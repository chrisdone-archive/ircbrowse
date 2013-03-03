{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Perse.Controllers where

import           Perse.Model.Stats
import           Perse.Controllers.Cache
import           Perse.Types

import           Control.Monads
import           Data.List hiding (head)
import           Data.Time
import qualified Prelude as P
import           Prelude hiding (head,div)
import           Snap.App
import           Text.Blaze.Extra
import           Text.Blaze.Html5 hiding (output,map)
import           Text.Blaze.Html5.Attributes

home :: Controller Config PState ()
home = do
  now <- io getCurrentTime
  let range = Range (addDays (-31) (utctDay now)) (utctDay now)
  out <- cache (Home range) $ do
    stats <- model $ getStats range
    return $ Just $ html $ do
      head $ link ! rel "stylesheet" ! type_ "text/css" ! href "/css/bootstrap.min.css"
      body $ do
        div !. "container" $ do
          div !. "row" $ do
            div !. "span12" $ do
              summarize range stats
          div !. "row" $ do
            div !. "span6" $ do
              mostActiveTimes stats
            div !. "span6" $ do
              dailyActivity range stats
          div !. "row" $ do
            div !. "span12" $ do
              activeNicks range stats
        preEscapedText "<script type=\"text/javascript\"> var _gaq = _gaq \
                       \|| []; _gaq.push(['_setAccount', 'UA-38975161-1']);\
                       \ _gaq.push(['_trackPageview']); (function() {var ga\
                       \ = document.createElement('script'); ga.type = 'tex\
                       \t/javascript'; ga.async = true; ga.src = ('https:' \
                       \== document.location.protocol ? 'https://ssl' : \
                       \'http://www') + '.google-analytics.com/ga.js'; var\
                       \ s = document.getElementsByTagName('script')[0]; \
                       \s.parentNode.insertBefore(ga, s);})(); </script>"
  maybe (return ()) outputText out

summarize range stats = p $ do
  h1 $ do em "Per se"; " IRC"

  p $ do strong "Network(s): "
         toHtml (intercalate ", " (stNetworks stats))
  p $ do strong "Channel(s): "
         toHtml (intercalate ", " (stChannels stats))
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
  p "The most active times of the day in which anyone spoke."
  p $ img ! src (toValue url)

  where url = "http://chart.apis.google.com/chart?chxl=0:|" ++
              intercalate "|" labels ++
              "&chxt=x,y&chd=t:" ++
              intercalate "," datas ++
              "&chs=" ++ show w ++ "x" ++ show h ++ "&cht=bvs&chbh=a"
        labels = map show [0..23]
        datas = map (\x -> show (round ((fi x / maxcount) * 61))) times
        times = map snd (stActiveTimes stats)
        maxcount = fi (maximum times)
        w = 450
        h = 200

dailyActivity range stats = do
  h2 $ do "Daily Activity"
          " (last "
          toHtml (show (diffDays (rangeTo range) (rangeFrom range)))
          " days)"
  p "The amount of conversation per day in the past month."
  p $ img ! src (toValue url)

  where url = "http://chart.apis.google.com/chart?chxl=0:|" ++
              intercalate "|" labels ++
              "&chxt=x,y&chd=t:" ++
              intercalate "," datas ++
              "&chs=" ++ show w ++ "x" ++ show h ++ "&cht=bvs&chbh=a"
        labels = map (show.fst) (stDailyAcitivty stats)
        datas = map (\x -> show (round ((fi x / maxcount) * 61))) times
        times = map snd (stDailyAcitivty stats)
        maxcount = fi (maximum times)
        w = 450
        h = 200

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
