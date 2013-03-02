{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Perse.Controllers where

import Perse.Model.Stats
import Perse.Types

import Control.Monads
import Data.Time
import Graphics.Google.Chart
import Prelude hiding (head,div)
import qualified Prelude as P
import Snap.App
import Text.Blaze.Extra
import Text.Blaze.Html5 hiding (output,map)
import Text.Blaze.Html5.Attributes

home :: Controller Config PState ()
home = do
  now <- io getCurrentTime
  let range = Range (addDays (-31) (utctDay now)) (utctDay now)
  stats <- model $ getStats range
  output $ html $ do
    head $ link ! rel "stylesheet" ! type_ "text/css" ! href "/css/bootstrap.min.css"
    body $ do
      div !. "container" $ do
        div !. "row" $ do
          div !. "span12" $ do
            summarize range stats
            mostActiveTimes stats
            dailyActivity range stats
            activeNicks range stats

summarize range stats = p $ do
  h1 $ do em "Per se"; " IRC stats"
  "During this "
  toHtml (show (diffDays (rangeTo range) (rangeFrom range)))
  "-day reporting period, a total of "
  em $ toHtml $ showCount (stNickCount stats)
  " different nicks spoke. "
  "For a total "
  em $ toHtml $ showCount (stEventCount stats)
  " IRC events, there were "
  em $ toHtml $ showCount (stMsgCount stats)
  " messages of conversation."

mostActiveTimes stats = do
  h2 "Most Active Times"
  p $ img ! src (toValue (chartURL chart))

  where chart = setAxisLabels [map (show . fst) (stActiveTimes stats)]
              $ setAxisTypes [AxisBottom]
              $ setData (encodeDataSimple [datas])
              $ setSize 700 257
              $ newBarChart Vertical Stacked
        datas = map (\x -> round ((fi x / maxcount) * 61)) times
        times = map snd (stActiveTimes stats)
        maxcount = fi (maximum times)

dailyActivity range stats = do
  h2 $ do "Daily Activity"
          " (last "
          toHtml (show (diffDays (rangeTo range) (rangeFrom range)))
          " days)"
  p $ img ! src (toValue (chartURL chart))

  where chart = setAxisLabels [map (show . fst) (stDailyAcitivty stats)]
              $ setAxisTypes [AxisBottom]
              $ setData (encodeDataSimple [datas])
              $ setSize 700 257
              $ newBarChart Vertical Stacked
        datas = map (\x -> round ((fi x / maxcount) * 61)) times
        times = map snd (stActiveTimes stats)
        maxcount = fi (maximum times)

activeNicks range stats = do
  h2 "Most Active Nicks (Top 50)"
  table $ do
    thead $ mapM_ th ["","Nick","Lines"]
    tbody $
      forM_ (zip [1..] (stActiveNicks stats)) $ \(i,(nick,lines)) ->
        tr $ do
          td $ toHtml (show i)
          td $ toHtml nick
          td $ toHtml (showCount lines)

showCount :: (Show n,Integral n) => n -> String
showCount = reverse . foldr merge "" . zip ("000,00,00,00"::String) . reverse . show where
  merge (f,c) rest | f == ',' = "," ++ [c] ++ rest
                   | otherwise = [c] ++ rest

fi = fromIntegral
