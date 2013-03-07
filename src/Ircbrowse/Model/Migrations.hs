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
       ,do exec ["alter table event drop network;"] ()
           exec ["alter table event drop channel;"] ()
           exec ["alter table event add network integer not null default 1;"] ()
           exec ["alter table event add channel integer not null default 1;"] ()
	   exec ["create index event_network_idx on event(network);"] ()
	   exec ["create index event_channel_idx on event(channel);"] ()
       , do exec ["create table event_count (count integer not null default 0)"] ()
            exec ["insert into event_count values (0)"] ()
       , do exec ["create table network (name text not null, title text not null)"] ()
            exec ["create table channel (network text not null,name text not null)"] ()
            exec ["insert into network values ('freenode','Freenode')"] ()
            exec ["insert into channel values ('freenode','haskell')"] ()
        ,do exec ["drop index event_nick_idx"] ()
            exec ["drop index event_text_idx"] ()
       ]
