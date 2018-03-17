{-# LANGUAGE OverloadedStrings #-}

module State.Cache where

import           Control.Monad              (void)
import           Data.ByteString            (ByteString)
import           Data.Maybe
import           Data.Pool
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple

import           Context

get' :: Ctxt -> Text -> IO (Maybe ByteString)
get' ctxt key = withResource (Context.db ctxt) $ \c -> fmap fromOnly . listToMaybe <$> query c "SELECT value FROM cache WHERE key = ?" (Only key)

set' :: Ctxt -> Text -> ByteString -> IO ()
set' ctxt key value = withResource (Context.db ctxt) $ \c -> void $ execute c "INSERT INTO cache (key, value) VALUES (?,?) ON CONFLICT (key) DO UPDATE SET value = EXCLUDED.value" (key, Binary value)
