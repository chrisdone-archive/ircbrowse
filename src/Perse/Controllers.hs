{-# LANGUAGE OverloadedStrings #-}

module Perse.Controllers where

import Perse.Model.Stats
import Perse.Types

import Snap.App
import Text.Blaze.Html5 hiding (output)
import Text.Blaze.Html5.Attributes
import Text.Blaze.Extra
import Prelude hiding (head,div)

home :: Controller Config PState ()
home = do
  stats <- model getStats
  output $ html $ do
    head $ link ! rel "stylesheet" ! type_ "text/css" ! href "/css/bootstrap.min.css"
    body $ do
      div !. "container" $ do
        div !. "row" $ do
          div !. "span12" $ do
            p $ do "Events: "; toHtml (show (stEventCount stats))
