{-# LANGUAGE OverloadedStrings #-}

module State.Stipend where

import           Control.Monad              (void)
import           Data.Maybe
import           Data.Pool
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple

import           Context
import           State.Types.Stipend


create :: Ctxt -> Stipend -> IO (Maybe Int)
create ctxt stipend = withResource (Context.db ctxt) $ \c -> do
  r <- listToMaybe <$> query c "INSERT INTO stipends (amount, academic_year, period, summer_guarantee, year_in_program, department, reporter_id, saw_document, notes) VALUES (?,?,?,?,?,?, ?,?,?) RETURNING id" (amount stipend, academicYear stipend, period stipend, summerGuarantee stipend, yearInProgram stipend, department stipend, reporterId stipend, sawDocument stipend, notes stipend)
  case r of
    Just (Only r) -> return $ Just r
    Nothing       -> return Nothing


get :: Ctxt -> Int -> IO (Maybe Stipend)
get ctxt id' = withResource (Context.db ctxt) $ \c -> listToMaybe <$> query c "SELECT id, created_at, token, amount, academic_year, period, summer_guarantee, year_in_program, department, reporter_id, saw_document, notes FROM stipends WHERE id = ?" (Only id')

getByToken :: Ctxt -> Text -> IO (Maybe Stipend)
getByToken ctxt tok = withResource (Context.db ctxt) $ \c -> listToMaybe <$> query c "SELECT id, created_at, token, amount, academic_year, period, summer_guarantee, year_in_program, department, reporter_id, saw_document, notes FROM stipends WHERE token = ?" (Only tok)


getWithUnverifiedDocuments :: Ctxt -> IO [Stipend]
getWithUnverifiedDocuments ctxt = withResource (Context.db ctxt) $ \c -> query_ c "SELECT S.id, S.created_at, S.token, S.amount, S.academic_year, S.period, S.summer_guarantee, S.year_in_program, S.department, S.reporter_id, S.saw_document, S.notes FROM stipends as S JOIN documents as D on D.stipend_id = S.id WHERE D.verified_at IS NULL ORDER BY D.created_at DESC"

-- NOTE(dbp 2018-03-18): This will probably become getAllVerified, once we are
-- verifying every stipend before showing publically (so people can't enter in
-- a stipend of $1 or $100,000 and mess everything up)
getAll :: Ctxt -> IO [Stipend]
getAll ctxt = withResource (Context.db ctxt) $ \c -> query_ c "SELECT id, created_at, token, amount, academic_year, period, summer_guarantee, year_in_program, department, reporter_id, saw_document, notes FROM stipends"
