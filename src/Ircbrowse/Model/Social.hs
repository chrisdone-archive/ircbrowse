{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Generate a graph of social interactions between users.

module Ircbrowse.Model.Social where

import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Ircbrowse.Monads
import           Ircbrowse.Types
import           Snap.App

getSocialGraph :: b -> r -> Model c s [(Text,Text,Int)]
getSocialGraph _ _ = do
  query ["select nick1,nick2,count"
        ,"from nick_to_nick"
        ,"order by count desc"]
        ()

-- generateGraph :: Config -> Pool -> IO ()
-- generateGraph config pool = db $ do
--   io $ putStrLn "Graph generation process started."
--   io $ putStrLn "Retrieving references ..."
--   events <- query ["select nick,text"
--                   ,"from event"
--                   ,"where type = 'talk'"
--                   ,"and text ~ E'^[a-zA-Z][a-zA-Z0-9\\\\-\\\\[\\\\]\\\\\\\\`^{}]*[,:] '"]
--                   ()
--   io $ putStrLn "Got events. Gonna generate a graph now."
--   let !m = collect events
--   io $ putStrLn "Processing query ..."
--   qins <- processQuery "INSERT INTO nick_to_nick_tmp (nick1,nick2,count) VALUES" ()
--   qrows <- forM (zip [0..] (M.toList m)) $ \(i,((nick1,nick2),count)) ->
--     processQuery (if i == 0 then "(?,?,?)" else ",(?,?,?)")
--                  (nick1,nick2,count)
--   io $ putStrLn "Running processed query ..."
--   [] :: [Only Int] <- queryProcessed (mappend qins (mconcat qrows))
--   io $ putStrLn "Done."
--   io $ putStrLn "Pruning invalid nicknames from the graph ..."
--   exec ["insert into nick_to_nick"
--        ,"select * from nick_to_nick_tmp"
--        ,"where nick1 in (select nick from event)"
--        ,"and   nick2 in (select nick from event);"
--        ,"delete from nick_to_nick_tmp;"]
--        ()
--   io $ putStrLn "Done!"
--   return ()

--   where db = runDB () config pool
--         collect :: [(Text,Text)] -> Map (Text,Text) Int
--         collect = foldl' extract M.empty
--         extract m (nick1,line) = M.insertWith (+) (makeEdge nick1 nick2) 1 m
--           where nick2 = T.takeWhile (\c -> c /= ':' && c /= ',') line
--                 makeEdge a b | a < b     = (a,b)
--                              | otherwise = (b,a)
