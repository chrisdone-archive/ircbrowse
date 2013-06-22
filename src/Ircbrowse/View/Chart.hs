{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fno-warn-unused-do-bind -fno-warn-type-defaults #-}

-- | Render charts.

module Ircbrowse.View.Chart where

import Ircbrowse.View

barChart :: (Show a,Integral a) => [(String, a)] -> Html
barChart values = p $ barChartBy (450,200) values

barChartBy :: (Show a,Integral a) => (Int,Int) -> [(String, a)] -> Html
barChartBy (w,h) values = img ! src (toValue url)

  where url = "http://chart.apis.google.com/chart?" ++
              "chxl=0:|" ++ intercalate "|" xlabels ++
              "&chxt=x,y&chd=t:" ++
              intercalate "," datas ++
              "&chs=" ++ show w ++ "x" ++ show h ++ "&cht=bvs&chbh=a" ++
              "&chxr=0|1,0," ++ show maxcount
        xlabels = map fst values
        ylabels = map show [10,20,30,40,50]
        datas = map (\x -> show (round ((fi x / maxcount) * 100))) times
        times = map snd values
        maxcount = fi (maximum' times)
        maximum' [] = 0
        maximum' xs = maximum xs

radarChart :: (Show a,Integral a) => [(String, a)] -> Html
radarChart values = img ! src (toValue url)

  where url = "http://chart.apis.google.com/chart?" ++
              "chxl=0:|" ++ intercalate "|" xlabels ++
              "&chxt=x&chd=t:" ++
              intercalate "," (datas ++ take 1 datas) ++
              "&chs=" ++ show w ++ "x" ++ show h ++ "&cht=rs&" ++
              "&chxr=0|1,0," ++ show maxcount ++
              "&chm=B,FFCC3399,0,1.0,5.0"
        xlabels = map fst values
        ylabels = map show [10,20,30,40,50]
        datas = map (\x -> show (round ((fi x / maxcount) * 100))) times
        times = map snd values
        maxcount = fi (maximum' times)
        w = 200
        h = 200
        maximum' [] = 0
        maximum' xs = maximum xs
