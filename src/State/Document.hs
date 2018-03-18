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
get ctxt id' = withResource (Context.db ctxt) $ \c -> listToMaybe <$> query c "SELECT id, created_at, object_key, decryption_key, file_type, stipend_id, verified_at FROM documents WHERE id = ?" (Only id')

getForStipend :: Ctxt -> Int -> IO [Document]
getForStipend ctxt id' = withResource (Context.db ctxt) $ \c -> query c "SELECT id, created_at, object_key, decryption_key, file_type, stipend_id, verified_at FROM documents WHERE stipend_id = ?" (Only id')

create :: Ctxt -> Document -> IO (Maybe Int)
create ctxt document = withResource (Context.db ctxt) $ \c -> do
  r <- listToMaybe <$> query c "INSERT INTO documents (object_key, decryption_key, file_type, stipend_id, verified_at) VALUES (?,?,?,?,?) RETURNING id" (objectKey document, Binary (decryptionKey document), fileType document, stipendId document, verifiedAt document)
  case r of
    Just (Only r) -> return $ Just r
    Nothing       -> return Nothing

update :: Ctxt -> Document -> IO ()
update ctxt doc =
  withResource (Context.db ctxt) $ \c -> void $ execute c "UPDATE documents SET object_key = ?, decryption_key = ?, file_type = ?, stipend_id = ?, verified_at = ? where id = ?" (objectKey doc, Binary (decryptionKey doc), fileType doc, stipendId doc, verifiedAt doc, State.Types.Document.id doc)
