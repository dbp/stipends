module Types.Document where

import           Data.ByteString                    (ByteString)
import           Data.Text                          (Text)
import           Data.Time.Clock

import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types


data Document =
     Document { id            :: Int
              , createdAt     :: UTCTime
              , objectKey     :: Text
              , decryptionKey :: ByteString
              , fileType      :: Text
              , stipendId     :: Int
              , verifiedAt    :: Maybe UTCTime
              } deriving (Eq, Show, Read)

instance FromRow Document where
  fromRow = Document <$> field
                     <*> field
                     <*> field
                     <*> field
                     <*> field
                     <*> field
                     <*> field
