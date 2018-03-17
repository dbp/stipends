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
get ctxt id' = withResource (Context.db ctxt) $ \c -> listToMaybe <$> query c "SELECT id, created_at, last_ip, token, name, trusted_at, curator_at FROM reporters WHERE id = ?" (Only id')


create :: Ctxt -> Reporter -> IO (Maybe Int)
create ctxt reporter = withResource (Context.db ctxt) $ \c -> do
  r<- listToMaybe <$> query c "INSERT INTO reporters (last_ip, name) VALUES (?,?) RETURNING id" (lastIp reporter, name reporter)
  case r of
    Just (Only r) -> return $ Just r
    Nothing       -> return Nothing
