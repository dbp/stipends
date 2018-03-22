{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Configuration.Dotenv              as Dotenv
import qualified Configuration.Dotenv.Types        as Dotenv
import           Control.Logging
import           Control.Monad                     (void, when)
import           Data.Default                      (def)
import           Data.Maybe                        (isJust)
import           Data.Pool                         (withResource)
import           Data.Time.Clock                   (getCurrentTime)
import           Database.PostgreSQL.Simple
import           Migrations                        (runMigrations)
import           Network.Wai.Session               (withSession)
import           Network.Wai.Session.ClientSession (clientsessionStore)
import           System.Environment                (lookupEnv, setEnv)
import           Test.Hspec
import           Test.Hspec.Fn
import           Web.ClientSession                 (randomKey)
import           Web.Cookie                        (setCookiePath)
import           Web.Fn                            (toWAI)

import           Context
import           Site
import qualified State.Stipend
import qualified Types.Stipend                     as Stipend

clearAll :: Ctxt -> IO ()
clearAll ctxt = void $ withResource (db ctxt) $ \c -> execute_ c "DELETE FROM documents; DELETE FROM stipends; DELETE FROM reporters; DELETE FROM cache;"

main :: IO ()
main = withStdoutLogging $ do
  exists <- isJust <$> lookupEnv "DATABASE_URL"
  when (not exists) $
    setEnv "DATABASE_URL" "postgres://stipends:111@localhost:5432/stipends_test"
  Dotenv.loadFile Dotenv.defaultConfig { Dotenv.configPath = [".env.test"] }
  ctxt <- initializer
  runMigrations (db ctxt) "migrations"
  (_,k) <- randomKey
  let store = clientsessionStore k
  hspec $ fn (return ctxt) (\ctxt -> return $ (toWAI ctxt site)) [withSession store "_session" def {setCookiePath = Just "/"} (sess ctxt)]
                               (const $ return ()) $
    beforeEval clearAll $ do
    describe "index" $ do
      let form_data = [("add.amount", "21500")
                      ,("add.academic_year","2017")
                      ,("add.period","add.period.0")
                      ,("add.summer_typical","add.summer_typical.0")
                      ,("add.year_in_program","3")
                      ,("add.department","add.department.0")
                      ,("add.saw_document","add.saw_document.0")
                      ,("add.notes","")]
      it "should have a form on the home page" $ do
        p <- get "/"
        shouldHaveSelector "form" p
      it "posting should result in a new stipend" $ do
        count <- eval State.Stipend.count
        post "/" form_data >>= should300
        count' <- eval State.Stipend.count
        count' `shouldEqual` (count + 1)
      -- NOTE(dbp 2018-03-21): These are pretty brittle as a way of checking for
      -- presence of stipends...
      it "anonymously submitted stipends shouldn't show on home page" $ do
        post "/" form_data >>= should300
        get "/" >>= shouldNotHaveSelector ".department .year .amount"
      it "verified data should show up on home page" $ do
        post "/" form_data >>= should300
        [stipend] <- eval State.Stipend.getUnverified
        now <- eval (const getCurrentTime)
        eval (flip State.Stipend.update (stipend { Stipend.verifiedAt = Just now }))
        get "/" >>= shouldHaveSelector ".department .year .amount"
