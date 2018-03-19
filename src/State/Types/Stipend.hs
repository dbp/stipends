{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module State.Types.Stipend where

import           Control.Monad                        (mzero)
import           Data.Text                            (Text)
import           Data.Time.Clock

import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.Types

data SummerTypical = FundedYearRound
                   | FundedAcademic
                   | FundedUnknown
                   deriving (Eq, Show, Read)

tshowSummerTypical :: SummerTypical -> Text
tshowSummerTypical FundedYearRound = "yearround"
tshowSummerTypical FundedAcademic  = "academic"
tshowSummerTypical FundedUnknown   = "unknown"


instance FromField SummerTypical where
  fromField f mdata = do r <- fromField f mdata
                         case r :: Text of
                           "yearround" -> return FundedYearRound
                           "academic"  -> return FundedAcademic
                           "unknown"   -> return FundedUnknown
                           _           -> mzero
instance ToField SummerTypical where
  toField s = toField (tshowSummerTypical s)

data Period = Yearly | Monthly | BiMonthly deriving (Eq, Show, Read)

tshowPeriod :: Period -> Text
tshowPeriod Yearly    = "yearly"
tshowPeriod Monthly   = "monthly"
tshowPeriod BiMonthly = "bimonthly"

instance FromField Period where
  fromField f mdata = do r <- fromField f mdata
                         case r :: Text of
                           "yearly"    -> return Yearly
                           "monthly"   -> return Monthly
                           "bimonthly" -> return BiMonthly
                           _           -> mzero
instance ToField Period where
  toField p = toField (tshowPeriod p)


data Stipend =
     Stipend { id            :: Int
             , createdAt     :: UTCTime
             , token         :: Text
             , amount        :: Int
             , academicYear  :: Int
             , period        :: Period
             , summerTypical :: SummerTypical
             , yearInProgram :: Maybe Int
             , department    :: Text
             , reporterId    :: Int
             , sawDocument   :: Bool
             , notes         :: Text
             } deriving (Eq, Show, Read)

instance FromRow Stipend where
  fromRow = Stipend <$> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
