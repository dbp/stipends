module State.Types.Reporter where

import           Data.Text                          (Text)
import           Data.Time.Clock

import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types


data Reporter =
     Reporter { id        :: Int
              , createdAt :: UTCTime
              , lastIp    :: Text
              , token     :: Text
              , name      :: Text
              , trustedAt :: Maybe UTCTime
              , curatorAt :: Maybe UTCTime
              } deriving (Eq, Show, Read)

instance FromRow Reporter where
  fromRow = Reporter <$> field
                     <*> field
                     <*> field
                     <*> field
                     <*> field
                     <*> field
                     <*> field
