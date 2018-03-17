{-# LANGUAGE OverloadedStrings #-}

module Handler.Home where

import           Control.Logging
import           Control.Monad.Trans       (liftIO)
import qualified Data.Map                  as M
import           Data.Maybe                (fromJust, fromMaybe)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import           Network.HTTP.Types.Method
import           Network.Wai
import           Text.Digestive.Form
import           Text.Digestive.Larceny
import           Web.Fn
import           Web.Fn.Extra.Digestive    (runForm)
import           Web.Larceny               (fillChildren, fillChildrenWith,
                                            mapSubs, subs, textFill)

import           Context
import qualified Handler.Reporter
import qualified State.Stipend
import qualified State.Types.Reporter      as Reporter
import qualified State.Types.Stipend       as Stipend

handle :: Ctxt -> IO (Maybe Response)
handle ctxt =
  runForm ctxt "add" (stipendForm ctxt) $
    \r ->
      case r of
        (v, Nothing)     -> renderWith ctxt (formFills v) "index"
        (_, Just stipend) -> do
          r <- Handler.Reporter.getReporter ctxt
          State.Stipend.create ctxt (stipend { Stipend.reporterId = Reporter.id r})
          redirect "/"

stipendForm :: Ctxt -> Form Text IO Stipend.Stipend
stipendForm ctxt =
  Stipend.Stipend
  <$> pure 0
  <*> pure (UTCTime (ModifiedJulianDay 0) 0)
  <*> "amount" .: stringRead "Must be a number (without commas or $), like 25400" Nothing
  <*> "academic_year" .: choice (map (\yr -> (yr, tshow yr <> "-" <> tshow (yr+1))) [2013..2019]) Nothing
  <*> "period" .: choice [(Stipend.Yearly, "Yearly")
                         ,(Stipend.Monthly, "Monthly")
                         ,(Stipend.BiMonthly, "BiMonthly")] Nothing
  <*> "summer_guarantee" .: choice [(Stipend.FundedAcademic, "Not guaranteed")
                                   ,(Stipend.FundedYearRound, "Summer guaranteed")
                                   ,(Stipend.FundedUnknown, "Unknown")] Nothing
  <*> "year_in_program" .: choice ([(Nothing, "")] ++ (map (\yr -> (Just yr, tshow yr <> " year in program")) [1..10])) Nothing
  <*> "department" .: choice (M.assocs (departments ctxt)) Nothing
  <*> "reporter" .: pure 0
  <*> "saw_document" .: choice [(True, "Yes"), (False, "No")] (Just False)
  <*> "notes" .: text Nothing
