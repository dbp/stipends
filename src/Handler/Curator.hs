{-# LANGUAGE OverloadedStrings #-}

module Handler.Curator where

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

handle :: Ctxt -> IO (Maybe Response)
handle ctxt = route ctxt [path "authenticate" // param "redirect" ==> authenticateH]

authenticateH :: Ctxt -> Int -> IO (Maybe Response)
authenticateH ctxt id' =
  runForm ctxt "add" ("key" .: stringRead "Must be an integer" Nothing) $
        \r ->
          case r of
            (v, Nothing) -> renderWith ctxt (formFills v) "curator/authenticate"
            (_, Just key) -> do
              setInSession ctxt "secret_key" (tshow $ (key :: Integer))
              redirect $ "/document/" <> tshow id'
