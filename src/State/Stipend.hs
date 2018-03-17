{-# LANGUAGE OverloadedStrings #-}

module State.Stipend where

import           Control.Monad              (void)
import           Data.Maybe
import           Data.Pool
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple

import           Context
import           State.Types.Stipend


create :: Ctxt -> Stipend -> IO ()
create ctxt stipend = withResource (Context.db ctxt) $ \c -> void $ execute c "INSERT INTO stipends (amount, academic_year, period, summer_guarantee, year_in_program, department, reporter_id, saw_document, notes) VALUES (?,?,?,?,?,?, ?,?,?)" (amount stipend, academicYear stipend, period stipend, summerGuarantee stipend, yearInProgram stipend, department stipend, reporterId stipend, sawDocument stipend, notes stipend)
