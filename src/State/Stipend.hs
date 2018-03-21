{-# LANGUAGE OverloadedStrings #-}

module State.Stipend where

import           Control.Monad              (void)
import           Data.Maybe
import           Data.Monoid                ((<>))
import           Data.Pool
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Database.PostgreSQL.Simple

import           Context
import qualified State.Document
import qualified State.Reporter
import qualified Types.Document             as Document
import qualified Types.Reporter             as Reporter
import           Types.Stipend

computeAmount :: Stipend -> Int
computeAmount s = case summerTypical s of
                    FundedYearRound -> computeYearly s
                    FundedAcademic  -> computeYearly s * 2 `div` 3
                    FundedUnknown   -> amount s

computeYearly :: Stipend -> Int
computeYearly s = case period s of
                    Yearly      -> amount s
                    TwoSemester -> amount s * 3 `div` 2
                    Semester    -> amount s * 3
                    Monthly     -> amount s * 12
                    BiMonthly   -> amount s * 24

computeAmountNote :: Stipend -> Text
computeAmountNote s = case summerTypical s of
                        FundedYearRound -> ""
                        FundedAcademic -> "Summer funding not typical, so 8 month salary shown."
                        FundedUnknown -> "Unknown summer funding status, so " <> prettyPeriod (period s) <> " salary shown"

periodShort :: Stipend -> Text
periodShort s = case summerTypical s of
                  FundedAcademic -> "8mo"
                  FundedYearRound -> "12mo"
                  FundedUnknown -> case period s of
                                     Yearly      -> "12mo"
                                     TwoSemester -> "8mo"
                                     Semester    -> "4mo"
                                     Monthly     -> "1mo"
                                     BiMonthly   -> "0.5mo"

verifiedUI :: Ctxt -> Stipend -> IO (Text, Int, Int)
verifiedUI ctxt s = do
  let sawdoc = if sawDocument s then 1 else 0
  mr <- State.Reporter.get ctxt (reporterId s)
  let organizer = case mr >>= Reporter.trustedAt of
                    Nothing -> 0
                    Just _  -> 1
  ds <- State.Document.getForStipend ctxt (Types.Stipend.id s)
  let docs = case filter (\d -> isJust $ Document.verifiedAt d) ds of
               [] -> 0
               _  -> 1
  let count = sawdoc + organizer + docs
  let total = 3
  let message = case docs of
                  1 -> "Verified by supporting encrypted documents"
                  0 -> case organizer of
                         0 -> "Reported anonymously"
                         _ -> case count of
                                -- NOTE(dbp 2018-03-19): 0 & 3 shouldn't be able to
                                -- happen based on control flow above, but leaving it here in case this gets
                                -- reorganized (partiality isn't good).
                                0 -> "Unverified"
                                1 -> "Reported to an organizer"
                                2 -> "Documents seen by an organizer"
                                3 -> "Verified by encrypted documents"
  return (message, count, total)

create :: Ctxt -> Stipend -> IO (Maybe Int)
create ctxt stipend = withResource (Context.db ctxt) $ \c -> do
  r <- listToMaybe <$> query c "INSERT INTO stipends (amount, academic_year, period, summer_typical, year_in_program, department, reporter_id, saw_document, notes, verified_at) VALUES (?,?,?,?,?,?,?,?,?,?) RETURNING id" (amount stipend, academicYear stipend, period stipend, summerTypical stipend, yearInProgram stipend, department stipend, reporterId stipend, sawDocument stipend, notes stipend, verifiedAt stipend)
  case r of
    Just (Only r) -> return $ Just r
    Nothing       -> return Nothing


-- NOTE(dbp 2018-03-19): postgresql-simple only has ToRow instances up to 10
-- elements; to get more than that, we combine tuples of size <= 10 with :.
update :: Ctxt -> Stipend -> IO ()
update ctxt stip =
  withResource (Context.db ctxt) $ \c -> void $ execute c "UPDATE stipends SET amount = ?, academic_year = ?, period = ?, summer_typical = ?, year_in_program = ?, department = ?, reporter_id = ?, saw_document = ?, notes = ?, verified_at = ? where id = ?" ((amount stip, academicYear stip, period stip, summerTypical stip, yearInProgram stip, department stip) :. (reporterId stip, sawDocument stip, notes stip, verifiedAt stip, Types.Stipend.id stip))


get :: Ctxt -> Int -> IO (Maybe Stipend)
get ctxt id' = withResource (Context.db ctxt) $ \c -> listToMaybe <$> query c "SELECT id, created_at, token, amount, academic_year, period, summer_typical, year_in_program, department, reporter_id, saw_document, notes, verified_at FROM stipends WHERE id = ?" (Only id')

getByToken :: Ctxt -> Text -> IO (Maybe Stipend)
getByToken ctxt tok = withResource (Context.db ctxt) $ \c -> listToMaybe <$> query c "SELECT id, created_at, token, amount, academic_year, period, summer_typical, year_in_program, department, reporter_id, saw_document, notes, verified_at FROM stipends WHERE token = ?" (Only tok)

getByReporter :: Ctxt -> Int -> IO [Stipend]
getByReporter ctxt id' = withResource (Context.db ctxt) $ \c -> query c "SELECT id, created_at, token, amount, academic_year, period, summer_typical, year_in_program, department, reporter_id, saw_document, notes, verified_at FROM stipends WHERE reporter_id = ?" (Only id')


getWithUnverifiedDocuments :: Ctxt -> IO [Stipend]
getWithUnverifiedDocuments ctxt = withResource (Context.db ctxt) $ \c -> query_ c "SELECT S.id, S.created_at, S.token, S.amount, S.academic_year, S.period, S.summer_typical, S.year_in_program, S.department, S.reporter_id, S.saw_document, S.notes, S.verified_at FROM stipends as S JOIN documents as D on D.stipend_id = S.id WHERE D.verified_at IS NULL ORDER BY D.created_at DESC"

getUnverified :: Ctxt -> IO [Stipend]
getUnverified ctxt = withResource (Context.db ctxt) $ \c -> query_ c "SELECT id, created_at, token, amount, academic_year, period, summer_typical, year_in_program, department, reporter_id, saw_document, notes, verified_at FROM stipends WHERE verified_at IS NULL"


getAllVerified :: Ctxt -> IO [Stipend]
getAllVerified ctxt = withResource (Context.db ctxt) $ \c -> query_ c "SELECT id, created_at, token, amount, academic_year, period, summer_typical, year_in_program, department, reporter_id, saw_document, notes, verified_at FROM stipends WHERE verified_at IS NOT NULL"

delete :: Ctxt -> Int -> IO ()
delete ctxt id' = do
  State.Document.deleteByStipend ctxt id'
  withResource (Context.db ctxt) $ \c -> void $ execute c "DELETE FROM stipends WHERE id = ?" (Only id')
