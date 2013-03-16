{-# LANGUAGE OverloadedStrings #-}

-- | The server's database migrations.

module Ircbrowse.Model.Migrations where

import GHC.Int
import Snap.App

-- | Migrations.
versions :: [(Int,Model c s Int64)]
versions = zip [1..] ms where
  ms = [ex ["CREATE TABLE event"
             ,"(id serial primary key"
             ,",timestamp timestamp with time zone not null"
             ,",network text not null"
             ,",channel text not null"
             ,",type text not null"
             ,",nick text"
             ,",text text not null"
             ,")"
             ]
       ,ex ["CREATE INDEX event_text_idx ON event USING gin(to_tsvector('english',text));"]
       ,ex ["CREATE INDEX event_network_idx ON event(network);"]
       ,ex ["CREATE INDEX event_channel_idx ON event(channel);"]
       ,ex ["CREATE INDEX event_nick_idx ON event(nick);"]
       ,ex ["CREATE INDEX event_type_idx ON event(type);"]
       ,ex ["CREATE INDEX event_timestamp_idx ON event(timestamp);"]
       ,do ex ["alter table event drop network;"]
           ex ["alter table event drop channel;"]
           ex ["alter table event add network integer not null default 1;"]
           ex ["alter table event add channel integer not null default 1;"]
	   ex ["create index event_network_idx on event(network);"]
	   ex ["create index event_channel_idx on event(channel);"]
       , do ex ["create table event_count (count integer not null default 0)"]
            ex ["insert into event_count values (0)"]
       , do ex ["create table network (name text not null, title text not null)"]
            ex ["create table channel (network text not null,name text not null)"]
            ex ["insert into network values ('freenode','Freenode')"]
            ex ["insert into channel values ('freenode','haskell')"]
        ,do ex ["drop index event_nick_idx"]
            ex ["drop index event_text_idx"]
        ,do ex ["create table event_ as select * from event limit 0;"]
            ex ["alter table event_ add column number bigserial not null;"]
            ex ["insert into event_ select * from event order by timestamp asc;"]
            ex ["drop table event;"]
            ex ["alter table event_ rename to event;"]
            ex ["alter table event drop id;"]
            ex ["alter table event rename number to id;"]
            ex ["create index event_id_idx on event(id);"]
        ,do ex ["alter table event add constraint event_unique_message unique (network,channel,timestamp,nick,text)"]
        ,do ex ["create table conversation_by_year (year int not null unique, lines int not null);"]
            ex ["insert into conversation_by_year select date_part('year',timestamp),count(*) from event where type in ('talk','act') group by date_part('year',timestamp) order by 1;"]
            ex ["create table general_activity_by_year (year int not null unique, lines int not null);"]
            ex ["insert into general_activity_by_year select date_part('year',timestamp),count(*) from event group by date_part('year',timestamp) order by 1;"]
        ,do ex ["create table nick_to_nick (id serial primary key, nick1 text not null, nick2 text not null, count integer not null default 0);"]
            ex ["create table nick_to_nick_tmp (id serial primary key, nick1 text not null, nick2 text not null, count integer not null default 0);"]
         -- Wed Mar 13 20:40:10 CET 2013
         -- Here's where I start supporting multiple channels.
        ,do ex ["insert into channel values ('freenode','lisp');"]
            ex ["create index event_channel_idx on event(channel);"]
            ex ["alter table event_count add channel integer"]
            ex ["alter table channel add id serial not null primary key"]
            ex ["create table event_order_index (id int not null, origin int not null);"]
            ex ["alter table event_order_index add constraint event_order_id_origin unique (id,origin);"]
            ex ["create index event_order_idx on event_order_index(id);"]
            ex ["alter table event_order_index add idx int;"]
            ex ["create index event_order_idx_idx on event_order_index(idx);"]
            ex ["create index event_order_origin_dx on event_order_index(origin)"]
         -- This query might be relevant:
         --
         -- insert into event_order_index select rank() over(order by timestamp asc) as id,id as origin,1000 as idx from event where channel = 1 order by timestamp asc;
         --
         -- It's how the event order index is populated.
       ]

  ex q = exec q ()
