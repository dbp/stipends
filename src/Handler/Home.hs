{-# LANGUAGE OverloadedStrings #-}

module Handler.Home where

import           Control.Logging
import           Control.Monad.Trans       (liftIO)
import           Data.List                 (nub, sort, sortOn)
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
                                            fillChildrenWith', mapSubs, subs,
                                            textFill)

import           Context
import qualified Handler.Reporter
import qualified Handler.Stipend
import qualified State.Stipend
import qualified State.Types.Reporter      as Reporter
import qualified State.Types.Stipend       as Stipend

handle :: Ctxt -> IO (Maybe Response)
handle ctxt =
  runForm ctxt "add" (stipendForm ctxt) $
    \r ->
      case r of
        (v, Nothing)     -> do
          stipends <- State.Stipend.getAll ctxt
          let groups = groupStipends stipends
          renderWith ctxt (departmentsSubs ctxt groups <> formFills v) "index"
        (_, Just stipend) -> do
          r <- Handler.Reporter.getReporter ctxt
          Just id' <- State.Stipend.create ctxt (stipend { Stipend.reporterId = Reporter.id r})
          Just stipend <- State.Stipend.get ctxt id'
          setMessage ctxt "Submitted a new stipend. Thanks!"
          redirect $ Handler.Stipend.url stipend

type Department = Text
type Year = Int
groupStipends :: [Stipend.Stipend] -> [(Department, [(Year, [Stipend.Stipend])])]
groupStipends stipends =
  let depts = sort $ nub $ map (Stipend.department) stipends in
  map (\d -> (d, let stips = filter (\s -> Stipend.department s == d) stipends in
                 let years = sort $ nub $ map (Stipend.academicYear) stips in
                   map (\y -> (y, sortOn Stipend.amount $ filter (\s -> Stipend.academicYear s == y) stips)) years)) depts

departmentsSubs :: Ctxt -> [(Department, [(Year, [Stipend.Stipend])])] -> Context.Substitutions
departmentsSubs ctxt groups =
  subs [("departments",
         mapSubs (\(d, grps) ->
                     subs [("department", textFill ((departments ctxt) M.! d)),
                            ("years",
                              mapSubs (\(y,stips) ->
                                          subs [("year", textFill (tshow y)),
                                                 ("stipends",
                                                   mapSubs (\stip ->
                                                               subs [("amount", textFill (tshow $ State.Stipend.computeAmount stip))
                                                                    ,("amount-note", textFill (State.Stipend.computeAmountNote stip))
                                                                    ,("verified", fillChildrenWith' (liftIO $ verifiedSubstitutions ctxt stip)
                                                                         )
                                                                    ,("is-verified", if Stipend.sawDocument stip then fillChildren else textFill "")
                                                                    ,("not-verified", if Stipend.sawDocument stip then textFill "" else fillChildren)
                                                                    ])
                                                   stips)
                                               ])
                              grps)])
          groups)]

verifiedSubstitutions :: Ctxt -> Stipend.Stipend -> IO Context.Substitutions
verifiedSubstitutions ctxt s = do
  (note, count, total) <- State.Stipend.verifiedUI ctxt s
  return $ subs [("note", textFill note)
                ,("bullets", textFill $ T.concat $ (replicate count "●") ++ (replicate (total - count) "○"))]


stipendForm :: Ctxt -> Form Text IO Stipend.Stipend
stipendForm ctxt =
  Stipend.Stipend
  <$> pure 0
  <*> pure (UTCTime (ModifiedJulianDay 0) 0)
  <*> pure ""
  <*> "amount" .: stringRead "Must be a number (without commas or $), like 25400" Nothing
  <*> "academic_year" .: choice (map (\yr -> (yr, tshow yr <> "-" <> tshow (yr+1))) [2013..2019]) Nothing
  <*> "period" .: choice [(Stipend.Yearly, "Yearly")
                         ,(Stipend.Monthly, "Monthly")
                         ,(Stipend.BiMonthly, "BiMonthly")] Nothing
  <*> "summer_typical" .: choice [(Stipend.FundedAcademic, "Not typical")
                                   ,(Stipend.FundedYearRound, "Summer funding normally provided")
                                   ,(Stipend.FundedUnknown, "Unknown")] Nothing
  <*> "year_in_program" .: choice ([(Nothing, "")] ++ (map (\yr -> (Just yr, tshow yr <> " year in program")) [1..10])) Nothing
  <*> "department" .: choice (M.assocs (departments ctxt)) Nothing
  <*> "reporter" .: pure 0
  <*> "saw_document" .: choice [(True, "Yes"), (False, "No")] (Just False)
  <*> "notes" .: text Nothing
