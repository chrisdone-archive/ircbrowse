{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fno-warn-unused-do-bind -fno-warn-type-defaults #-}

-- | Render charts.

module Ircbrowse.View.Chart where

import Ircbrowse.View

barChart :: Integral a => [(String, a)] -> Html
barChart values = p $ img ! src (toValue url)

  where url = "http://chart.apis.google.com/chart?" ++
              "chyl=0:|" ++ intercalate "|" ylabels ++
              "&chxl=0:|" ++ intercalate "|" xlabels ++
              "&chxt=x,y&chd=t:" ++
              intercalate "," datas ++
              "&chs=" ++ show w ++ "x" ++ show h ++ "&cht=bvs&chbh=a"
        xlabels = map fst values
        ylabels = map show [10,20,30,40,50]
        datas = map (\x -> show (round ((fi x / maxcount) * 61))) times
        times = map snd values
        maxcount = fi (maximum times)
        w = 450
        h = 200
