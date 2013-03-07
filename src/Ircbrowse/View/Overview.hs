{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fno-warn-unused-do-bind -fno-warn-type-defaults #-}

module Ircbrowse.View.Overview where

import Ircbrowse.View
import Ircbrowse.View.Template

overview :: Maybe String -> Maybe String -> Range -> Stats -> Html
overview network channel range stats = do
  template "overview" $ do
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
          activeNicks stats
    preEscapedText "<script type=\"text/javascript\"> var _gaq = _gaq \
                   \|| []; _gaq.push(['_setAccount', 'UA-38975161-1']);\
                   \ _gaq.push(['_trackPageview']); (function() {var ga\
                   \ = document.createElement('script'); ga.type = 'tex\
                   \t/javascript'; ga.async = true; ga.src = ('https:' \
                   \== document.location.protocol ? 'https://ssl' : \
                   \'http://www') + '.google-analytics.com/ga.js'; var\
                   \ s = document.getElementsByTagName('script')[0]; \
                   \s.parentNode.insertBefore(ga, s);})(); </script>"


summarize :: Range -> Stats -> Html
summarize range stats = p $ do
  h1 $ "IRC Browse"
  p $ do strong "Network(s): "
         toHtml (intercalate ", " (map snd (stNetworks stats)))
  p $ do strong "Channel(s): "
         forM_ (stChannels stats) $ \(network,name) ->
           a ! href (toValue ("/browse/" ++ network ++ "/" ++ name)) $ do "#"; toHtml name
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

mostActiveTimes :: Stats -> Html
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

dailyActivity :: Range -> Stats -> Html
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

activeNicks :: Stats -> Html
activeNicks stats = do
  h2 "Most Active Nicks (Top 50)"
  table $ do
    thead $ mapM_ th ["","Nick","Lines"]
    tbody $
      forM_ (zip [1..] (stActiveNicks stats)) $ \(i,(nick,linecount)) ->
        tr $ do
          td $ toHtml (show i)
          td $ toHtml nick
          td $ toHtml (showCount linecount)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
