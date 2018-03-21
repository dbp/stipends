{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Migrations where


import Control.Logging (log')
import           Control.Monad (void, when, forM_)
import Data.List (sort)
import System.Directory (listDirectory)
import Data.String (fromString)
import Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Pool                          (Pool, createPool,
                                                     withResource)
import           Data.Text                          (Text)
import           Database.PostgreSQL.Simple         (ConnectInfo (..),
                                                     Connection, Only (..),
                                                     close, connectPostgreSQL,
                                                     execute, execute_, query,
                                                     query_)



runMigrations :: Pool Connection -> FilePath -> IO ()
runMigrations pgpool dir = do
  withResource pgpool $ \conn -> void $ execute_ conn "CREATE TABLE IF NOT EXISTS migrations (name TEXT NOT NULL PRIMARY KEY, created_at timestamptz NOT NULL DEFAULT now())"
  migrations <- sort <$> listDirectory dir
  forM_ migrations $ \m ->
    withResource pgpool $ \conn -> do
      (res :: [(Only Text)]) <- query conn "SELECT name FROM migrations WHERE name = ?" (Only m)
      when (length res == 0) $ do
        log' $ "Running migration " <> T.pack (show m)
        sql <- readFile (dir <> "/" <> m)
        void $ execute_ conn (fromString sql)
        void $ execute conn "INSERT INTO migrations (name) VALUES (?)" (Only m)
