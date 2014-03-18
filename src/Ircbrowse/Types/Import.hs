module Ircbrowse.Types.Import where

-- | Show a channel.
showChan :: Channel -> String
showChan Haskell = "haskell"
showChan Lisp = "lisp"
showChan HaskellGame = "haskell-game"
showChan Diagrams = "diagrams"
showChan Tasty = "tasty"
showChan HaskellDistributed = "haskell-distributed"
showChan NumericalHaskell = "numerical-haskell"

-- | Show a channel.
showChanInt :: Channel -> Int
showChanInt Haskell = 1
showChanInt Lisp = 2
showChanInt HaskellGame = 3
showChanInt Diagrams = 4
showChanInt Tasty = 5
showChanInt HaskellDistributed = 6
showChanInt NumericalHaskell = 7

-- | Read a channel.
parseChan :: String -> Maybe Channel
parseChan "haskell" =  Just Haskell
parseChan "lisp" =  Just Lisp
parseChan "diagrams" =  Just Diagrams
parseChan "haskell-game" = Just HaskellGame
parseChan "tasty" = Just Tasty
parseChan "haskell-distributed" = Just HaskellDistributed
parseChan "numerical-haskell" = Just NumericalHaskell
parseChan _ = Nothing

idxNum :: Channel -> Int
idxNum Haskell = 1000
idxNum Lisp = 2000
idxNum HaskellGame = 3000
idxNum Diagrams = 4000
idxNum Tasty = 5000
idxNum HaskellDistributed = 6000
idxNum NumericalHaskell = 7000

-- | Possible supported channels.
data Channel = Haskell | Lisp | HaskellGame | Diagrams | Tasty | HaskellDistributed | NumericalHaskell
  deriving Enum
