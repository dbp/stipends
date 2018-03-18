{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Main where

import qualified Configuration.Dotenv               as Dotenv
import qualified Configuration.Dotenv.Types         as Dotenv
import           Control.Logging
import           Control.Monad
import           Data.Default                       (def)
import           Data.List                          (sort)
import qualified Data.Map                           as M
import           Data.Maybe
import           Data.Monoid
import           Data.Pool                          (Pool, createPool,
                                                     withResource)
import           Data.Serialize.Text                ()
import           Data.String                        (fromString)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
import           Data.Time.Clock
import           Data.Traversable
import qualified Data.Vault.Lazy                    as Vault
import           Database.PostgreSQL.Simple         (ConnectInfo (..),
                                                     Connection, Only (..),
                                                     close, connectPostgreSQL,
                                                     execute, execute_, query,
                                                     query_)
import           Database.PostgreSQL.Simple.FromRow
import           GHC.IO.Encoding                    (setLocaleEncoding, utf8)
import           Network.Wai                        (Response, pathInfo)
import           Network.Wai.Handler.Warp           (run)
import           Network.Wai.Middleware.Rollbar
import           Network.Wai.Session                (withSession)
import           Network.Wai.Session.ClientSession  (clientsessionStore)
import           Rollbar.Item.CodeVersion           (CodeVersion (SHA))
import           System.Directory                   (listDirectory)
import           System.Environment                 (getEnv, lookupEnv)
import           System.IO.Unsafe                   (unsafePerformIO)
import           Text.Digestive.Form
import           Text.Digestive.Larceny
import           Web.ClientSession                  (initKey, randomKey)
import           Web.Cookie                         (setCookiePath)
import           Web.Fn
import           Web.Fn.Extra.Digestive
import           Web.Heroku                         (parseDatabaseUrl)
import qualified Web.Larceny                        as L

import           Context
import           Site
import qualified State.Cache                        as Cache



main :: IO ()
main = withStdoutLogging $ do
  setLocaleEncoding utf8
  Dotenv.loadFile Dotenv.defaultConfig
  ctxt <- initializer
  runMigrations (db ctxt) "migrations"

  port <- maybe 3000 read <$> lookupEnv "PORT"
  log' $ "Listening on port " <> tshow port <>  "..."
  rb_token <- lookupEnv "ROLLBAR_ACCESS_TOKEN"
  sha <- fromMaybe "MISSING_SHA" <$> lookupEnv "HEROKU_SLUG_COMMIT"
  let rb = case rb_token of
             Nothing -> id
             Just tok -> exceptions (Settings (fromString tok) Nothing (Just (SHA (T.pack sha))) "production" :: Settings '[])
  mbs <- Cache.get' ctxt "session-key"
  let newkey = do (bs, k) <- randomKey
                  Cache.set' ctxt "session-key" bs
                  return k
  k <- case mbs of
         Nothing -> newkey
         Just bs ->
           case initKey bs of
             Right k -> return k
             Left _  -> newkey
  let store = clientsessionStore k


  run port $ rb $ (withSession store "_session" def {setCookiePath = Just "/"} (sess ctxt) (toWAI ctxt site))


runMigrations :: Pool Connection -> FilePath -> IO ()
runMigrations pgpool dir = do
  withResource pgpool $ \conn -> void $ execute_ conn "CREATE TABLE IF NOT EXISTS migrations (name TEXT NOT NULL PRIMARY KEY, created_at timestamptz NOT NULL DEFAULT now())"
  migrations <- sort <$> listDirectory dir
  forM_ migrations $ \m ->
    withResource pgpool $ \conn -> do
      (res :: [(Only Text)]) <- query conn "SELECT name FROM migrations WHERE name = ?" (Only m)
      when (length res == 0) $ do
        log' $ "Running migration " <> tshow m
        sql <- readFile (dir <> "/" <> m)
        void $ execute_ conn (fromString sql)
        void $ execute conn "INSERT INTO migrations (name) VALUES (?)" (Only m)
