{-# LANGUAGE OverloadedStrings #-}

module Handler.Document where

import           Control.Lens
import           Control.Logging
import           Control.Monad.Trans       (liftIO)
import           Data.Conduit
import           Data.Conduit.Binary
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
import qualified Data.UUID                 as UUID
import qualified Data.UUID.V4              as UUID
import           Network.AWS               hiding (Document, Error, Response)
import           Network.AWS.Data.Body
import           Network.AWS.S3            hiding (redirect)
import           Network.HTTP.Types
import           Network.HTTP.Types.Method
import           Network.Wai
import           System.FilePath           (takeExtension)
import           System.IO
import           Text.Digestive.Form
import           Text.Digestive.Larceny    (formFills, tshow)
import           Text.Digestive.Types
import           Text.Read                 (readMaybe)
import           Web.Fn
import           Web.Fn.Extra.Digestive
import           Web.Larceny               (fillChildren, fillChildrenWith,
                                            fillChildrenWith', mapSubs, subs,
                                            textFill)
import qualified Web.Larceny               as L

import           Context
import qualified Handler.Reporter
import qualified Handler.Stipend
import qualified State.Document            as State
import qualified State.Stipend
import           State.Types.Document
import qualified State.Types.Reporter      as Reporter
import qualified State.Types.Stipend       as Stipend

handle :: Ctxt -> IO (Maybe Response)
handle ctxt = route ctxt [ path "add" // param "stipend" ==> addH
                         , segment ==> showH
                         ]

addH :: Ctxt -> Text -> IO (Maybe Response)
addH ctxt token = do
  mst <- State.Stipend.getByToken ctxt token
  case mst of
    Nothing -> do setMessage ctxt "No stipend found."
                  redirect "/"
    Just st ->
      runForm ctxt "add" (documentForm ctxt) $
        \r ->
          case r of
            (v, Nothing)       -> renderWith ctxt (formFills v) "document/add"
            (_, Just file) -> do
              uuid <- uploadFile ctxt file
              let filetype = T.pack $ takeExtension (file)
              State.create ctxt (Document 0 (UTCTime (ModifiedJulianDay 0) 0) uuid filetype (Stipend.id st) Nothing)
              setMessage ctxt "Added encrypted supporting document (viewable only to the curators). Thanks!"
              redirect (Handler.Stipend.url st)

documentForm :: Ctxt -> Form Text IO FilePath
documentForm ctxt = "file" .: (validate required Text.Digestive.Form.file)
  where required (Just v) = Success v
        required _        = Error "File is required."

uploadFile :: Ctxt -> FilePath -> IO Text
uploadFile ctxt path = do
  uuid <- UUID.toText <$> UUID.nextRandom
  lgr  <- newLogger Debug stdout
  env  <- newEnv Discover
  body <- chunkedFile defaultChunkSize path
  runResourceT $ runAWS (env & envLogger .~ lgr) $
        within NorthVirginia $
            send (putObject (BucketName $ Context.bucket ctxt) (ObjectKey uuid) body)
  return uuid

mimeMap :: [(Text,Text)]
mimeMap =  [
  ( ".txt"  , "text/plain"         ),
  ( ".doc"  , "application/msword" ),
  ( ".docx" , "application/vnd.openxmlformats-officedocument.wordprocessingml.document"),
  ( ".jpeg" , "image/jpeg"      ),
  ( ".jpg"  , "image/jpeg"      ),
  ( ".pdf"  , "application/pdf" ),
  ( ".png"  , "image/png"       )
  ]

showH :: Ctxt -> Int -> IO (Maybe Response)
showH ctxt id' = do
  mr <- Handler.Reporter.lookupReporter ctxt
  case mr of
    Nothing -> bounce
    Just rep ->
      case Reporter.curatorAt rep of
        Nothing -> bounce
        Just _ -> do
          mdoc <- State.get ctxt id'
          case mdoc of
            Nothing -> do setMessage ctxt "No document found"
                          redirectReferer ctxt
            Just doc -> do
              let content =
                    case lookup (fileType doc) mimeMap of
                      Nothing -> []
                      Just t  -> [("Content-Type", T.encodeUtf8 t)]
              lgr  <- newLogger Debug stdout
              env  <- newEnv Discover
              contents <- runResourceT $ do
                (RsBody body) <- runAWS (env & envLogger .~ lgr) $
                  within NorthVirginia $ do
                    rs <- send (getObject (BucketName $ Context.bucket ctxt) (ObjectKey $ url doc))
                    return (view gorsBody rs)
                body $$+- sinkLbs
              return (Just $ responseLBS status200 content contents)
  where bounce = do
         setMessage ctxt "Only curators can view supporting documents."
         redirectReferer ctxt
