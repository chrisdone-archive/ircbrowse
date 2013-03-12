{-# LANGUAGE OverloadedStrings #-}

-- | Generate colours for a nickname based on an X-Chat-like sum.

module Ircbrowse.View.NickColour where

import           Data.Monoid
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T

-- | Generate a Solarize colour from a nickname.
nickColour :: Text -> Text
nickColour = ("color:" <>) . sumIndex where
  -- This is based on X-Chat's algorithm. Nothing special.
  sumIndex = index . T.foldl' (\acc c -> acc + fromEnum c) 0
  index c = fromMaybe "#333333" (lookup (mod c (length colors)) colors)
  colors = zip [0..]
    [
    -- Tomorrow theme
    "#4d4d4c" -- Foreground
    ,"#8e908c" -- Comment
    ,"#c82829" -- Red
    ,"#f5871f" -- Orange
    ,"#eab700" -- Yellow
    ,"#718c00" -- Green
    ,"#3e999f" -- Aqua
    ,"#4271ae" -- Blue
    ,"#8959a8" -- Purple
    -- Solarized theme
    ,"#002b36" -- base03
    ,"#073642" -- base02
    ,"#586e75" -- base01
    ,"#657b83" -- base00
    ,"#839496" -- base0
    ,"#b58900" -- yellow
    ,"#cb4b16" -- orange
    ,"#dc322f" -- red
    ,"#d33682" -- magenta
    ,"#6c71c4" -- violet
    ,"#268bd2" -- blue
    ,"#2aa198" -- cyan
    ,"#859900" -- green
    ]
