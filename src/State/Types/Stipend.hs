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

data SummerGuarantee = FundedYearRound
                     | FundedAcademic
                     | FundedUnknown
                     deriving (Eq, Show, Read)

tshowSummerGuarantee :: SummerGuarantee -> Text
tshowSummerGuarantee FundedYearRound = "yearround"
tshowSummerGuarantee FundedAcademic  = "academic"
tshowSummerGuarantee FundedUnknown   = "unknown"


instance FromField SummerGuarantee where
  fromField f mdata = do r <- fromField f mdata
                         case r :: Text of
                           "yearround" -> return FundedYearRound
                           "academic"  -> return FundedAcademic
                           "unknown"   -> return FundedUnknown
                           _           -> mzero
instance ToField SummerGuarantee where
  toField s = toField (tshowSummerGuarantee s)

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
     Stipend { id              :: Int
             , createdAt       :: UTCTime
             , token           :: Text
             , amount          :: Int
             , academicYear    :: Int
             , period          :: Period
             , summerGuarantee :: SummerGuarantee
             , yearInProgram   :: Maybe Int
             , department      :: Text
             , reporterId      :: Int
             , sawDocument     :: Bool
             , notes           :: Text
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
