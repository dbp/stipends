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
import qualified State.Types.Document       as Document
import qualified State.Types.Reporter       as Reporter
import           State.Types.Stipend

computeAmount :: Stipend -> Int
computeAmount s = case summerTypical s of
                    FundedYearRound -> computeYearly s
                    FundedAcademic  -> (computeYearly s * 3) `div` 4
                    FundedUnknown   -> computeYearly s

computeYearly :: Stipend -> Int
computeYearly s = case period s of
                    Yearly    -> amount s
                    Monthly   -> amount s * 12
                    BiMonthly -> amount s * 24

computeAmountNote :: Stipend -> Text
computeAmountNote s = case summerTypical s of
                        FundedYearRound -> ""
                        FundedAcademic -> "Summer funding not typical, so 9 month salary shown. 12 month salary is $" <> T.pack (show (computeYearly s))
                        FundedUnknown -> "Summer funding unknown, so salary may be $" <> T.pack (show (computeYearly s * 3 `div` 4))

verifiedUI :: Ctxt -> Stipend -> IO (Text, Int, Int)
verifiedUI ctxt s = do
  let sawdoc = if sawDocument s then 1 else 0
  mr <- State.Reporter.get ctxt (reporterId s)
  let organizer = case mr >>= Reporter.trustedAt of
                    Nothing -> 0
                    Just _  -> 1
  ds <- State.Document.getForStipend ctxt (State.Types.Stipend.id s)
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
  r <- listToMaybe <$> query c "INSERT INTO stipends (amount, academic_year, period, summer_typical, year_in_program, department, reporter_id, saw_document, notes) VALUES (?,?,?,?,?,?, ?,?,?) RETURNING id" (amount stipend, academicYear stipend, period stipend, summerTypical stipend, yearInProgram stipend, department stipend, reporterId stipend, sawDocument stipend, notes stipend)
  case r of
    Just (Only r) -> return $ Just r
    Nothing       -> return Nothing


get :: Ctxt -> Int -> IO (Maybe Stipend)
get ctxt id' = withResource (Context.db ctxt) $ \c -> listToMaybe <$> query c "SELECT id, created_at, token, amount, academic_year, period, summer_typical, year_in_program, department, reporter_id, saw_document, notes FROM stipends WHERE id = ?" (Only id')

getByToken :: Ctxt -> Text -> IO (Maybe Stipend)
getByToken ctxt tok = withResource (Context.db ctxt) $ \c -> listToMaybe <$> query c "SELECT id, created_at, token, amount, academic_year, period, summer_typical, year_in_program, department, reporter_id, saw_document, notes FROM stipends WHERE token = ?" (Only tok)


getWithUnverifiedDocuments :: Ctxt -> IO [Stipend]
getWithUnverifiedDocuments ctxt = withResource (Context.db ctxt) $ \c -> query_ c "SELECT S.id, S.created_at, S.token, S.amount, S.academic_year, S.period, S.summer_typical, S.year_in_program, S.department, S.reporter_id, S.saw_document, S.notes FROM stipends as S JOIN documents as D on D.stipend_id = S.id WHERE D.verified_at IS NULL ORDER BY D.created_at DESC"

-- NOTE(dbp 2018-03-18): This will probably become getAllVerified, once we are
-- verifying every stipend before showing publically (so people can't enter in
-- a stipend of $1 or $100,000 and mess everything up)
getAll :: Ctxt -> IO [Stipend]
getAll ctxt = withResource (Context.db ctxt) $ \c -> query_ c "SELECT id, created_at, token, amount, academic_year, period, summer_typical, year_in_program, department, reporter_id, saw_document, notes FROM stipends"
