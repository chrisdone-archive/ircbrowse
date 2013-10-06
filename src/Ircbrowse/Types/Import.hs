module Ircbrowse.Types.Import where

-- | Show a channel.
showChan :: Channel -> String
showChan Haskell = "haskell"
showChan Lisp = "lisp"

-- | Show a channel.
showChanInt :: Channel -> Int
showChanInt Haskell = 1
showChanInt Lisp = 2

-- | Read a channel.
parseChan :: String -> Maybe Channel
parseChan "haskell" =  Just Haskell
parseChan "lisp" =  Just Lisp
parseChan _ = Nothing

idxNum :: Channel -> Int
idxNum Haskell = 1000
idxNum Lisp = 2000

-- | Possible supported channels.
data Channel = Haskell | Lisp
  deriving Enum
