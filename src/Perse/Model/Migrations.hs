{-# LANGUAGE OverloadedStrings #-}

-- | The server's database migrations.

module Perse.Model.Migrations where

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
       ,exec ["CREATE INDEX event_type_idx ON event(type);"] ()
       ,exec ["CREATE INDEX event_timestamp_idx ON event(timestamp);"] ()
       ,exec ["CREATE INDEX event_nick_idx ON event(nick);"] ()
       ,exec ["CREATE INDEX event_network_idx ON event(network);"] ()
       ,exec ["CREATE INDEX event_channel_idx ON event(channel);"] ()
       ,do exec ["ALTER TABLE event ALTER channel TYPE INTEGER USING 1;"] ()
           exec ["CREATE TABLE channel (id SERIAL PRIMARY KEY NOT NULL, name TEXT NOT NULL);"] ()
           exec ["INSERT INTO channel (name) VALUES ('haskell');"] ()
           exec ["ALTER TABLE event"
                ,"ADD CONSTRAINT event_channel_fk"
                ,"FOREIGN KEY (channel) REFERENCES channel(id)"
                ,"ON DELETE CASCADE ON UPDATE CASCADE"
                ] ()
       ,do exec ["ALTER TABLE event ALTER network TYPE INTEGER USING 1;"] ()
           exec ["CREATE TABLE network (id SERIAL PRIMARY KEY NOT NULL, name TEXT NOT NULL);"] ()
           exec ["INSERT INTO network (name) VALUES ('freenode');"] ()
           exec ["ALTER TABLE event"
                ,"ADD CONSTRAINT event_network_fk"
                ,"FOREIGN KEY (network) REFERENCES network(id)"
                ,"ON DELETE CASCADE ON UPDATE CASCADE"
                ] ()
       ]
