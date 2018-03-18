{-# LANGUAGE OverloadedStrings #-}

module State.Document where

import           Control.Monad              (void)
import           Data.Maybe
import           Data.Pool
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple

import           Context
import           State.Types.Document


get :: Ctxt -> Int -> IO (Maybe Document)
get ctxt id' = withResource (Context.db ctxt) $ \c -> listToMaybe <$> query c "SELECT id, created_at, url, file_type, stipend_id, verified_at FROM documents WHERE id = ?" (Only id')

getForStipend :: Ctxt -> Int -> IO [Document]
getForStipend ctxt id' = withResource (Context.db ctxt) $ \c -> query c "SELECT id, created_at, url, file_type, stipend_id, verified_at FROM documents WHERE stipend_id = ?" (Only id')

create :: Ctxt -> Document -> IO (Maybe Int)
create ctxt document = withResource (Context.db ctxt) $ \c -> do
  r <- listToMaybe <$> query c "INSERT INTO documents (url, file_type, stipend_id, verified_at) VALUES (?,?,?,?) RETURNING id" (url document, fileType document, stipendId document, verifiedAt document)
  case r of
    Just (Only r) -> return $ Just r
    Nothing       -> return Nothing
