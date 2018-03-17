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
