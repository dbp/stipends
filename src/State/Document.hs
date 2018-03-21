{-# LANGUAGE OverloadedStrings #-}

module State.Document where

import           Control.Lens

import           Control.Monad              (void, when)
import           Data.Maybe
import           Data.Pool
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple
import           Network.AWS                hiding (Document, Error, Response)
import           Network.AWS.Data.Body
import           Network.AWS.S3             hiding (redirect)
import           System.IO

import           Context
import           Types.Document


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
  withResource (Context.db ctxt) $ \c -> void $ execute c "UPDATE documents SET object_key = ?, decryption_key = ?, file_type = ?, stipend_id = ?, verified_at = ? where id = ?" (objectKey doc, Binary (decryptionKey doc), fileType doc, stipendId doc, verifiedAt doc, Types.Document.id doc)

deleteByStipend :: Ctxt -> Int -> IO ()
deleteByStipend ctxt id' = do
  docs <- getForStipend ctxt id'
  when (length docs /= 0) $ do
    let keys = map (objectIdentifier . ObjectKey . objectKey) docs
    lgr  <- newLogger Debug stdout
    env  <- newEnv Discover
    runResourceT $ runAWS (env & envLogger .~ lgr) $
      within NorthVirginia $
      send (deleteObjects (BucketName $ Context.bucket ctxt) (delete' & dObjects .~ keys))
    return ()
  withResource (Context.db ctxt) $ \c -> void $ execute c "DELETE FROM documents WHERE stipend_id = ?" (Only id')
