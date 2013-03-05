{-# LANGUAGE OverloadedStrings #-}

-- | The server's database migrations.

module Ircbrowse.Model.Migrations where

import Snap.App

-- | Migrations.
versions :: [(Int,Model c s Integer)]
versions = zip [1..] ms where
  ms = [exec ["CREATE TABLE event"
             ,"(id serial primary key"
             ,",timestamp timestamp with time zone not null"
             ,",network text not null"
             ,",channel text not null"
             ,",type text not null"
             ,",nick text"
             ,",text text not null"
             ,")"
             ]
             ()
       ,exec ["CREATE INDEX event_text_idx ON event USING gin(to_tsvector('english',text));"] ()
       ,exec ["CREATE INDEX event_network_idx ON event(network);"] ()
       ,exec ["CREATE INDEX event_channel_idx ON event(channel);"] ()
       ,exec ["CREATE INDEX event_nick_idx ON event(nick);"] ()
       ,exec ["CREATE INDEX event_type_idx ON event(type);"] ()
       ,exec ["CREATE INDEX event_timestamp_idx ON event(timestamp);"] ()
       ]
