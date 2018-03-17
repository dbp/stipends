{-# LANGUAGE OverloadedStrings #-}

module State.Reporter where

import           Control.Monad              (void)
import           Data.Maybe
import           Data.Pool
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple

import           Context
import           State.Types.Reporter


get :: Ctxt -> Int -> IO (Maybe Reporter)
get ctxt id' = withResource (Context.db ctxt) $ \c -> listToMaybe <$> query c "SELECT id, created_at, fingerprint, token, name, trusted_at, curator_at FROM reporters WHERE id = ?" (Only id')

getByToken :: Ctxt -> Text -> IO (Maybe Reporter)
getByToken ctxt token = withResource (Context.db ctxt) $ \c -> listToMaybe <$> query c "SELECT id, created_at, fingerprint, token, name, trusted_at, curator_at FROM reporters WHERE token = ?" (Only token)


getAnonByFingerprint :: Ctxt -> Text -> IO (Maybe Reporter)
getAnonByFingerprint ctxt f = withResource (Context.db ctxt) $ \c -> listToMaybe <$> query c "SELECT id, created_at, fingerprint, token, name, trusted_at, curator_at FROM reporters WHERE fingerprint = ?" (Only f)


create :: Ctxt -> Reporter -> IO (Maybe Int)
create ctxt reporter = withResource (Context.db ctxt) $ \c -> do
  r<- listToMaybe <$> query c "INSERT INTO reporters (fingerprint, name) VALUES (?,?) RETURNING id" (fingerprint reporter, name reporter)
  case r of
    Just (Only r) -> return $ Just r
    Nothing       -> return Nothing
