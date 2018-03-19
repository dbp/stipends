{-# LANGUAGE OverloadedStrings #-}
module Context where

import           Control.Monad              (join)
import           Control.Monad.Trans        (MonadIO, liftIO)
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe, isJust, isNothing,
                                             listToMaybe)
import           Data.Monoid                ((<>))
import           Data.Pool                  (Pool, withResource)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Time.Clock            (UTCTime)
import           Data.Time.Clock.POSIX      (utcTimeToPOSIXSeconds)
import           Data.Time.Format           (defaultTimeLocale, formatTime)
import qualified Data.Vault.Lazy            as Vault
import           Database.PostgreSQL.Simple (Connection, Only (..), query)
import           Network.Wai                (Request (..), Response)
import           Network.Wai.Session        (Session)
import           System.Directory           (getModificationTime)
import           Text.Read                  (readMaybe)
import           Web.Fn
import qualified Web.Larceny                (Fill, Library, Substitutions)
import qualified Web.Larceny                as L

import           State.Types.Reporter

type Fill = Web.Larceny.Fill ()
type Library = Web.Larceny.Library ()
type Substitutions = Web.Larceny.Substitutions ()

data Ctxt = Ctxt { request     :: FnRequest
                 , db          :: Pool Connection
                 , library     :: Library
                 , sess        :: Vault.Key (Session IO Text (Maybe Text))
                 , departments :: Map Text Text
                 , bucket      :: Text
                 , pubkey      :: Integer
                 }

instance RequestContext Ctxt where
  getRequest (Ctxt r _ _ _ _ _ _) = r
  setRequest (Ctxt _ p l s ds b k) r = Ctxt r p l s ds b k

render :: Ctxt -> Text -> IO (Maybe Response)
render ctxt = renderWith ctxt mempty

renderWith :: Ctxt -> Substitutions -> Text -> IO (Maybe Response)
renderWith ctxt subs tpl =
  do message <- getMessage ctxt
     t <- L.renderWith (library ctxt) (M.union (builtInSubs ctxt message) subs) () (T.splitOn "/" tpl)
     case t of
       Nothing -> return Nothing
       Just t' -> okHtml t'

builtInSubs :: Ctxt -> Maybe Text -> Substitutions
builtInSubs ctxt message =
  L.subs [("render-message", L.textFill (fromMaybe "" message))
         ,("css", L.useAttrs (L.a "path") cssFill)
         ,("is-curator", isCuratorFill ctxt)
         ,("is-organizer", isOrganizerFill ctxt)
         ]

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

reporterKey :: Text
reporterKey = "reporter_id"

getReporter :: Ctxt -> Int -> IO (Maybe Reporter)
getReporter ctxt id' = withResource (Context.db ctxt) $ \c -> listToMaybe <$> query c "SELECT id, created_at, fingerprint, token, name, trusted_at, curator_at FROM reporters WHERE id = ?" (Only id')

lookupReporter :: Ctxt -> IO (Maybe Reporter)
lookupReporter ctxt = do
  mrid <- (>>= readMaybe . T.unpack) <$> getFromSession ctxt reporterKey
  case mrid of
    Just rid -> getReporter ctxt rid
    Nothing  -> return Nothing


reporterSubs :: Reporter -> Substitutions
reporterSubs (Reporter i cr f t name trust cur) =
  L.subs [("id", L.textFill $ T.pack $ show i)
       ,("created-at",  dateFill cr)
       ,("fingerprint", L.textFill f)
       ,("token", L.textFill t)
       ,("name", L.textFill (fromMaybe "" name))
       ,("is-trusted", if isJust trust then L.fillChildren else L.textFill "")
       ,("not-trusted", if isNothing trust then L.fillChildren else L.textFill "")
       ,("trusted-at", optionalDateFill trust)
       ,("curator-at", optionalDateFill cur)
        ]


requireCurator :: MonadIO m => Ctxt -> m a -> m a -> m a
requireCurator ctxt not is =  do
  mrep <- (>>= curatorAt) <$> (liftIO $ lookupReporter ctxt)
  case mrep of
    Nothing -> not
    Just _  -> is

isCuratorFill :: Ctxt -> Fill
isCuratorFill ctxt = L.Fill $ \_s (pth, L.Template tpl) l -> do
  requireCurator ctxt (return "") (tpl pth mempty l)

isOrganizerFill :: Ctxt -> Fill
isOrganizerFill ctxt = L.Fill $ \attrs pt lib -> do
  mrep <- liftIO $ lookupReporter ctxt
  case mrep of
    Nothing -> return ""
    Just rep ->
      case trustedAt rep of
        Nothing -> return ""
        Just _ ->
          L.unFill (L.fillChildrenWith (reporterSubs rep)) attrs pt lib


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
