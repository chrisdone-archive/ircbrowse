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
       ]
