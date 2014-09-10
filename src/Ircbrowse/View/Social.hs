{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fno-warn-unused-do-bind -fno-warn-type-defaults #-}

-- | A social graph.

module Ircbrowse.View.Social where

import Ircbrowse.View
import Ircbrowse.View.Template

import Data.Aeson
import Data.Aeson.Encode
import Data.Text (Text)
import Data.Text.Lazy.Builder

socialGraph :: [(Text,Text,Int)] -> Html
socialGraph graph = do
  template "nick-cloud" "Social Graph" graphScripts $ do
    containerFluid $ do
      row $ do
        span12 $ do
          h1 $ do
            a ! href "/" $ do "IRC Browse"
            ": Social Graph"
          p $ do "Below is a graph of communication between people in the channel."
          script $ preEscapedToHtml $ "drawGraph(" <> toLazyText (encodeToTextBuilder (toJSON spec)) <> ")"

  where spec = object [ "nodes" .= map makeNode nodes
                      , "width" .= (960::Int)
                      , "height" .= (600::Int)
                      , "edges" .= edges
                      ]
        makeNode label = object [ "label" .= label ]
        nodes = nub (map (\(x,_,_) -> x) limitedGraph ++ map (\(_,x,_) -> x) limitedGraph)
        edges = object (map (\(n1,n2,weight) -> (n1 <> ":" <> n2) .= weight)
                            limitedGraph)
        limitedGraph = take limit graph
        limit = 100

graphScripts :: Html
graphScripts =
  forM_ ["d3.v2","graph"] $ \name ->
    script ! src ("/js/" <> name <> ".js") $ mempty
