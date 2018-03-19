{-# LANGUAGE OverloadedStrings #-}

module Handler.Stipend where

import           Control.Logging
import           Control.Monad.Trans       (liftIO)
import           Data.List                 (lookup)
import qualified Data.Map                  as M
import           Data.Maybe                (fromJust, fromMaybe, isJust,
                                            isNothing)
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
import           Text.Digestive.Larceny    (tshow)
import           Text.Read                 (readMaybe)
import           Web.Fn
import           Web.Larceny               (fillChildren, fillChildrenWith,
                                            fillChildrenWith', mapSubs, subs,
                                            textFill)
import qualified Web.Larceny               as L

import           Context
import qualified State.Document
import qualified State.Reporter
import qualified State.Stipend             as State
import qualified State.Types.Document      as Document
import           State.Types.Stipend

url :: Stipend -> Text
url stipend = "/stipend/" <> token stipend

handle :: Ctxt -> IO (Maybe Response)
handle ctxt = route ctxt [ path "verify" // segment ==> verifyH
                         , segment ==> showH ]

verifyH :: Ctxt -> Int -> IO (Maybe Response)
verifyH ctxt id' =
  requireCurator ctxt (return Nothing) $ do
    mstip <- State.get ctxt id'
    case mstip of
      Nothing -> return Nothing
      Just stip -> do
        now <- getCurrentTime
        State.update ctxt (stip { verifiedAt = Just now })
        redirectReferer ctxt


showH :: Ctxt -> Text -> IO (Maybe Response)
showH ctxt tok = do
  mst <- State.getByToken ctxt tok
  case mst of
    Nothing -> do setMessage ctxt "No stipend found."
                  redirect "/"
    Just st -> renderWith ctxt (stipendSubs ctxt st) "stipends/show"

stipendSubs :: Ctxt -> Stipend -> Substitutions
stipendSubs ctxt (Stipend i cr t am acy per sg yr dep reprt saw notes ver) =
  subs [("id", textFill $ tshow i)
       ,("created-at",  dateFill cr)
       ,("token", textFill t)
       ,("amount", textFill $ "$" <> tshow am)
       ,("academic-year", textFill $ tshow acy <> "-" <> tshow (acy+1))
       ,("period", textFill $ tshowPeriod per)
       ,("summer-typical", textFill $ tshowSummerTypical sg)
       ,("year-in-program", textFill $ maybe "N/A" tshow yr)
       ,("department", textFill $ fromMaybe "ERROR CODE D22" $ M.lookup dep (departments ctxt))
       ,("reporter-id", textFill $ tshow reprt)
       ,("reporter", fillChildrenWith' $ do mrep <- liftIO $ State.Reporter.get ctxt reprt
                                            case mrep of
                                              Nothing -> return (subs [])
                                              Just rep -> return $ reporterSubs rep)
       ,("saw-document", textFill $ tshow saw)
       ,("notes", textFill notes)
       ,("documents", documentsFill ctxt i)
       ,("verified-at", optionalDateFill ver)
       ,("is-verified", if isJust ver then fillChildren else textFill "")
       ,("not-verified", if isNothing ver then fillChildren else textFill "")
       ]

documentsFill :: Ctxt -> Int -> Fill
documentsFill ctxt i = L.Fill $ \attrs pt lib -> do
  docs <- liftIO $ State.Document.getForStipend ctxt i
  let fill' =
        mapSubs (\(count, doc) ->
                    subs [("id", textFill (tshow $ Document.id doc))
                         ,("created-at", dateFill (Document.createdAt doc))
                         ,("object-key", textFill (Document.objectKey doc))
                         ,("file-type", textFill (Document.fileType doc))
                         ,("stipend-id", textFill (tshow (Document.stipendId doc)))
                         ,("verified-at", optionalDateFill (Document.verifiedAt doc))
                         ,("verified", if isJust (Document.verifiedAt doc) then fillChildren else textFill "")
                         ,("not-verified", if isNothing (Document.verifiedAt doc) then fillChildren else textFill "")
                         ,("counter", textFill (tshow count))
                         ]) (zip [1..] docs)
  L.unFill fill' attrs pt lib

