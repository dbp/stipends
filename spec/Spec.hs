{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Web.Cookie                         (setCookiePath)
import qualified Configuration.Dotenv               as Dotenv
import qualified Configuration.Dotenv.Types         as Dotenv
import           Database.PostgreSQL.Simple
import Control.Logging
import Test.Hspec
import Test.Hspec.Fn
import           Web.ClientSession                  (randomKey)
import           Network.Wai.Session                (withSession)
import           Network.Wai.Session.ClientSession  (clientsessionStore)
import           Data.Pool                          (withResource)
import System.Environment (setEnv)
import Web.Fn (toWAI)
import           Data.Default                       (def)
import Migrations (runMigrations)


import Context
import Site

main :: IO ()
main = withStdoutLogging $ do
  setEnv "DATABASE_URL" "postgres://stipends:111@localhost:5432/stipends_test"
  Dotenv.loadFile Dotenv.defaultConfig { Dotenv.configPath = [".env.test"] }
  ctxt <- initializer
  runMigrations (db ctxt) "migrations"
  withResource (db ctxt) $ \c -> execute_ c "DELETE FROM documents; DELETE FROM stipends; DELETE FROM reporters; DELETE FROM cache;"
  (_,k) <- randomKey
  let store = clientsessionStore k
  hspec $ fn (return ctxt) (\ctxt -> return $ (toWAI ctxt site)) [withSession store "_session" def {setCookiePath = Just "/"} (sess ctxt)]
                               (const $ return ()) $ do
    describe "index" $ do
      it "should have a form on the home page" $ do
        p <- get "/"
        shouldHaveSelector "form" p
