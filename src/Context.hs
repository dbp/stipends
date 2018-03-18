{-# LANGUAGE OverloadedStrings #-}
module Context where

import           Control.Monad              (join)
import           Control.Monad.Trans        (liftIO)
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                ((<>))
import           Data.Pool                  (Pool)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Time.Clock            (UTCTime)
import           Data.Time.Clock.POSIX      (utcTimeToPOSIXSeconds)
import           Data.Time.Format           (defaultTimeLocale, formatTime)
import qualified Data.Vault.Lazy            as Vault
import           Database.PostgreSQL.Simple (Connection)
import           Network.Wai                (Request (..), Response)
import           Network.Wai.Session        (Session)
import           System.Directory           (getModificationTime)
import           Web.Fn
import qualified Web.Larceny                (Fill, Library, Substitutions)
import qualified Web.Larceny                as L

type Fill = Web.Larceny.Fill ()
type Library = Web.Larceny.Library ()
type Substitutions = Web.Larceny.Substitutions ()

data Ctxt = Ctxt { request     :: FnRequest
                 , db          :: Pool Connection
                 , library     :: Library
                 , sess        :: Vault.Key (Session IO Text (Maybe Text))
                 , departments :: Map Text Text
                 , bucket      :: Text
                 }

instance RequestContext Ctxt where
  getRequest (Ctxt r _ _ _ _ _) = r
  setRequest (Ctxt _ p l s ds b) r = Ctxt r p l s ds b

render :: Ctxt -> Text -> IO (Maybe Response)
render ctxt = renderWith ctxt mempty

renderWith :: Ctxt -> Substitutions -> Text -> IO (Maybe Response)
renderWith ctxt subs tpl =
  do message <- getMessage ctxt
     t <- L.renderWith (library ctxt) (M.union (builtInSubs message) subs) () (T.splitOn "/" tpl)
     case t of
       Nothing -> return Nothing
       Just t' -> okHtml t'

builtInSubs :: Maybe Text -> Substitutions
builtInSubs message =
  L.subs [("render-message", L.textFill (fromMaybe "" message))
         ,("css", L.useAttrs (L.a "path") cssFill)]

dateFill :: UTCTime -> Fill
dateFill t = L.textFill (T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" t)

optionalDateFill :: Maybe UTCTime -> Fill
optionalDateFill (Just t) = dateFill t
optionalDateFill Nothing  = L.textFill ""

cssFill :: Text -> Fill
cssFill pth = L.rawTextFill' $ do
  mtime <- liftIO $ getModificationTime (T.unpack $ removeLeadingSlash $ T.replace "/%cache%/" "/" pth)
  return $ "<link rel='stylesheet' href='" <> (T.replace "%cache%" (T.pack $ show (utcTimeToPOSIXSeconds mtime)) pth) <> "'>"
  where removeLeadingSlash t = if "/" `T.isPrefixOf` t then T.drop 1 t else t

setMessage :: Ctxt -> Text -> IO ()
setMessage ctxt msg = setInSession ctxt "message" msg

getMessage :: Ctxt -> IO (Maybe Text)
getMessage ctxt = do msg <- getFromSession ctxt "message"
                     clearFromSession ctxt "message"
                     return msg

getFromSession :: Ctxt -> Text -> IO (Maybe Text)
getFromSession ctxt k =
  case Vault.lookup (sess ctxt) (vault (fst $ request ctxt)) of
    Just (getsess, _) ->
      join <$> getsess k
    Nothing ->
      error $ "getFromSession: no session in vault when looking for '" <> T.unpack k <> "'"

setInSession :: Ctxt -> Text -> Text -> IO ()
setInSession ctxt k v =
  case Vault.lookup (sess ctxt) (vault (fst $ request ctxt)) of
    Just (_, setsess) -> setsess k (Just v)
    Nothing ->
      error $ "setInSession: no session in vault when setting '" <> T.unpack k <> "' to '" <> T.unpack v <> "'"

clearFromSession :: Ctxt -> Text -> IO ()
clearFromSession ctxt k =
  let Just (_, setsess) = Vault.lookup (sess ctxt) (vault (fst $ request ctxt))
     in setsess k Nothing
