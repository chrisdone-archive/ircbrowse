{-# LANGUAGE OverloadedStrings #-}

-- | The server's database migrations.

module Perse.Model.Migrations where

import Snap.App

-- | Migrations.
versions :: [(Int,Model c s Integer)]
versions = zip [1..] ms where
  ms = []
