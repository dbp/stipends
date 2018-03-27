{-# LANGUAGE OverloadedStrings #-}

module Handler.Reporter where

import           Control.Logging
import           Control.Monad.Trans       (liftIO)
import           Data.List                 (lookup)
import qualified Data.Map                  as M
import           Data.Maybe                (fromJust, fromMaybe)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import           Network.HTTP.Types.Method
import           Network.Wai
import           Text.Digestive.Form
import           Text.Digestive.Larceny    hiding (Substitutions)
import           Text.Read                 (readMaybe)
import           Web.Fn
import           Web.Fn.Extra.Digestive
import           Web.Larceny               (fillChildren, fillChildrenWith,
                                            mapSubs, subs, textFill)

import           Context
import qualified Handler.Stipend
import qualified State.Reporter            as State
import qualified State.Stipend
import           Types.Reporter

handle :: Ctxt -> IO (Maybe Response)
handle ctxt = route ctxt [ path "logout" ==> logoutH
                         , path "new" ==> newH
                         , segment // path "stipends" ==> stipendsH
                         ]

loginH :: Ctxt -> Text -> IO (Maybe Response)
loginH ctxt token = do
  mr <- State.getByToken ctxt token
  case mr of
    Nothing -> return ()
    Just r -> do
      setInSession ctxt reporterKey (tshow $ Types.Reporter.id r)
      setMessage ctxt $ "Successfully logged in as " <> (fromMaybe "" $ name r) <> "."
  redirect "/"

logoutH :: Ctxt -> IO (Maybe Response)
logoutH ctxt = do
  clearFromSession ctxt reporterKey
  setMessage ctxt "Logged out..."
  redirect "/"

getReporter :: Ctxt -> IO Reporter
getReporter ctxt = do
  mr <- Context.lookupReporter ctxt
  case mr of
    Just r  -> return r
    Nothing -> mkR
  where mkR = do
          let mip = T.decodeUtf8 <$> lookup "X-Forwarded-For" (requestHeaders (fst $ request ctxt))
          let muser = T.decodeUtf8 <$> lookup "User-Agent" (requestHeaders (fst $ request ctxt))
          case (mip, muser) of
            (Nothing, Nothing) -> do
              let f = "N/A"
              mkNewAnon f
            _ -> do
              let f = fromMaybe "" mip <> "///" <> fromMaybe "" muser
              mr <- State.getAnonByFingerprint ctxt f
              case mr of
                Just r  -> return r
                Nothing -> mkNewAnon f
        mkNewAnon ip = do
          -- NOTE(dbp 2018-03-17): We'll throw errors if either of these fail.
          -- These will be DB failures, which should be unlikely, and not much
          -- to do aside from report (which an exception will do). ¯\_(ツ)_/¯
          Just i <- State.create ctxt (Reporter 0 (UTCTime (ModifiedJulianDay 0) 0) ip "" Nothing Nothing Nothing)
          Just r <- State.get ctxt i
          return r

newH :: Ctxt -> IO (Maybe Response)
newH ctxt = requireCurator ctxt (return Nothing) $ do
  runForm ctxt "add" ("name" .: text Nothing) $
    \r ->
      case r of
        (v, Nothing)     -> renderWith ctxt (formFills v) "reporter/new"
        (_, Just name) -> do
          now <- getCurrentTime
          State.create ctxt (Reporter 0 (UTCTime (ModifiedJulianDay 0) 0) "N/A" "" (Just name) (Just now) Nothing)
          redirect "/curator/organizers"

stipendsH :: Ctxt -> Int -> IO (Maybe Response)
stipendsH ctxt id' = requireCurator ctxt (return Nothing) $ do
  stipends <- State.Stipend.getByReporter ctxt id'
  renderWith ctxt (subs [("stipends", mapSubs (Handler.Stipend.stipendSubs ctxt) stipends)]) "reporter/stipends"
