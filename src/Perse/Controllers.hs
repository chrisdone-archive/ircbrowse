{-# LANGUAGE OverloadedStrings #-}

module Perse.Controllers where

import Snap.App

home :: Controller c s ()
home = do
  writeText "Hello, World!"
