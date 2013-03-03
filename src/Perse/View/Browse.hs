{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fno-warn-unused-do-bind -fno-warn-type-defaults #-}

module Perse.View.Browse where

import Perse.View
import Perse.View.Template

import Data.Text (Text)
import Network.URI.Params
import Snap.App.Types

browse :: Maybe String -> Maybe String -> Range -> [Event] -> Pagination -> Html
browse network channel range events pagination =
  template "browse" $
    paginate pagination $
      table !. "events table" $
        forM_ events $ \event ->
          tr !. "event" $ do
            td  !. "timestamp" $ toHtml $ show (eventTimestamp event)
            if (eventType event == "talk")
              then do td !. "nick-wrap" $ do
                        " <"
                        span !. "nick" $ toHtml $ fromMaybe " " (eventNick event)
                        "> "
                      td !. "text" $ toHtml $ eventText event
              else do td !. "nick-wrap" $
                        span !. "nick" $ toHtml $ fromMaybe " " (eventNick event)
                      td !. "text" $ toHtml $ eventText event

-- | Render results with pagination.
paginate :: Pagination -> Html -> Html
paginate pn inner = do
  pnnav pn True
  inner
  pnnav pn False

-- | Show a pagination navigation, with results count, if requested.
pnnav :: Pagination -> Bool -> Html
pnnav pn@Pagination{..} showTotal = do
  div !. "container" $
    div !. "row12" $
      ul !. "pager" $ do
          when (pnPage-1 > 0) $ li !. "previous" $ navDirection pn (-1) "← Older"
          toHtml (" " :: Text)
          when (pnResults == pnLimit) $ li !. "next" $ navDirection pn 1 "Newer →"
          when showTotal $ do
            br
            toHtml $ results

    where results = unwords [showCount start ++ "—" ++ showCount end
                            ,"results of"
                            ,showCount pnTotal]
          start = 1 + (pnPage - 1) * pnResults
          end = pnPage * pnResults

-- | Link to change navigation page based on a direction.
navDirection :: Pagination -> Integer -> Text -> Html
navDirection Pagination{..} change caption = do
  a ! hrefURI uri $
    toHtml caption

  where uri = updateUrlParam "page"
  	      		     (show (pnPage + change))
			     pnURI
