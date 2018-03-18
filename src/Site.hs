{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Site where

import           Control.Monad
import           Data.Default                       (def)
import           Data.List                          (sort)
import qualified Data.Map                           as M
import           Data.Maybe
import           Data.Monoid
import           Data.Pool                          (Pool, createPool,
                                                     withResource)
import           Data.Serialize.Text                ()
import           Data.String                        (fromString)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
import           Data.Time.Clock
import           Data.Traversable
import qualified Data.Vault.Lazy                    as Vault
import qualified Data.Yaml                          as Yaml
import           Database.PostgreSQL.Simple         (ConnectInfo (..),
                                                     Connection, Only (..),
                                                     close, connectPostgreSQL,
                                                     execute, execute_, query,
                                                     query_)
import           Database.PostgreSQL.Simple.FromRow
import           GHC.IO.Encoding                    (setLocaleEncoding, utf8)
import           Network.Wai                        (Response, pathInfo)
import           Network.Wai.Handler.Warp           (run)
import           Network.Wai.Middleware.Rollbar
import           Network.Wai.Session                (withSession)
import           Network.Wai.Session.ClientSession  (clientsessionStore)
import           Rollbar.Item.CodeVersion           (CodeVersion (SHA))
import           System.Directory                   (listDirectory)
import           System.Environment                 (getEnv, lookupEnv)
import           System.IO.Unsafe                   (unsafePerformIO)
import           Text.Digestive.Form
import           Text.Digestive.Larceny
import           Web.ClientSession                  (initKey, randomKey)
import           Web.Cookie                         (setCookiePath)
import           Web.Fn
import           Web.Fn.Extra.Digestive
import           Web.Heroku                         (parseDatabaseUrl)
import qualified Web.Larceny                        as L

import           Context
import qualified Handler.Document                   as Document
import qualified Handler.Home                       as Home
import qualified Handler.Reporter                   as Reporter
import qualified Handler.Stipend                    as Stipend
import qualified State.Cache                        as Cache

initializer :: IO Ctxt
initializer =
  do lib <- L.loadTemplates "templates" L.defaultOverrides
     Just depts <- Yaml.decodeFile "departments.yaml"
     u <- fmap parseDatabaseUrl <$> lookupEnv "DATABASE_URL"
     bucket <- T.pack <$> getEnv "BUCKET_NAME"
     let ps = fromMaybe [("host", "localhost")
                        ,("port", "5432")
                        ,("user", "stipends")
                        ,("password", "111")
                        ,("dbname", "stipends")]
              u
     pgpool <- createPool (connectPostgreSQL $ T.encodeUtf8 $ T.intercalate " " $ map (\(k,v) -> k <> "=" <> v) ps)
                        close 1 60 20


     session <- Vault.newKey

     return (Ctxt defaultFnRequest pgpool lib session depts bucket)

site :: Ctxt -> IO Response
site ctxt = route ctxt [ path "static" ==> staticServe "static"
                       -- NOTE(dbp 2018-03-17): The following allows url
                       -- busting, i.e., /static/TIMESTAMP/path/to/file.css
                       , path "static" // segment ==>
                         \ctxt (_ :: Text) -> staticServe "static" ctxt
                       , end ==> Home.handle
                       , path "reporter" ==> Reporter.handle
                       , path "stipend" ==> Stipend.handle
                       , path "document" ==> Document.handle
                       , anything ==> larcenyServe
                       ]
            `fallthrough` do r <- render ctxt "404"
                             case r of
                               Just r' -> return r'
                               Nothing -> notFoundText "Page not found"


larcenyServe :: Ctxt -> IO (Maybe Response)
larcenyServe ctxt = do
  let pth' = pathInfo (fst . getRequest $ ctxt)
  let pth = T.intercalate "/" pth'
  let idx = if T.length pth > 0 then pth <> "/index" else "index"
  if ((length pth' > 1) && "_" `T.isPrefixOf` (last pth')) || ".." `T.isInfixOf` pth
     then return Nothing
     else route ctxt [ anything ==> \ctxt -> render ctxt pth
                     , anything ==> \ctxt -> render ctxt idx
                     ]
