module Ircbrowse.Types.Import where

-- | Pretty print a channel in a human-representation.
prettyChan :: Channel -> String
prettyChan Haskell = "#haskell"
prettyChan Lisp = "#lisp"
prettyChan HaskellGame = "#haskell-game"
prettyChan Diagrams = "#diagrams"
prettyChan Tasty = "#tasty"
prettyChan HaskellDistributed = "#haskell-distributed"
prettyChan NumericalHaskell = "#numerical-haskell"
prettyChan FSharp = "##fsharp"
prettyChan Ghcjs = "#ghcjs"
prettyChan HaskellBeginners = "#haskell-beginners"
prettyChan HLedger = "#hledger"
prettyChan Typelevel = "#typelevel"
prettyChan Scalaz = "#scalaz"
prettyChan Shapeless = "#shapeless"
prettyChan Purescript = "#purescript"
prettyChan HaskellLens = "#haskell-lens"

-- | Show a channel.
showChan :: Channel -> String
showChan Haskell = "haskell"
showChan Lisp = "lisp"
showChan HaskellGame = "haskell-game"
showChan Diagrams = "diagrams"
showChan Tasty = "tasty"
showChan HaskellDistributed = "haskell-distributed"
showChan HaskellBeginners = "haskell-beginners"
showChan NumericalHaskell = "numerical-haskell"
showChan FSharp = "fsharp"
showChan Ghcjs = "ghcjs"
showChan HLedger = "hledger"
showChan Typelevel = "typelevel"
showChan Scalaz = "scalaz"
showChan Shapeless = "shapeless"
showChan Purescript = "purescript"
showChan HaskellLens = "haskell-lens"

-- | Show a channel.
showChanInt :: Channel -> Int
showChanInt Haskell = 1
showChanInt Lisp = 2
showChanInt HaskellGame = 3
showChanInt Diagrams = 4
showChanInt Tasty = 5
showChanInt HaskellDistributed = 6
showChanInt NumericalHaskell = 7
showChanInt FSharp = 8
showChanInt Ghcjs = 9
showChanInt HaskellBeginners = 10
showChanInt HLedger = 11
showChanInt Typelevel = 12
showChanInt Scalaz = 13
showChanInt Shapeless = 14
showChanInt Purescript = 15
showChanInt HaskellLens = 16

-- | Read a channel.
parseChan :: String -> Maybe Channel
parseChan "haskell" =  Just Haskell
parseChan "lisp" =  Just Lisp
parseChan "diagrams" =  Just Diagrams
parseChan "haskell-game" = Just HaskellGame
parseChan "tasty" = Just Tasty
parseChan "haskell-beginners" = Just HaskellBeginners
parseChan "haskell-distributed" = Just HaskellDistributed
parseChan "numerical-haskell" = Just NumericalHaskell
parseChan "fsharp" = Just FSharp
parseChan "ghcjs" = Just Ghcjs
parseChan "hledger" = Just HLedger
parseChan "typelevel" = Just Typelevel
parseChan "scalaz" = Just Scalaz
parseChan "shapeless" = Just Shapeless
parseChan "purescript" = Just Purescript
parseChan "haskell-lens" = Just HaskellLens
parseChan _ = Nothing

idxNum :: Channel -> Int
idxNum Haskell = 1000
idxNum Lisp = 2000
idxNum HaskellGame = 3000
idxNum Diagrams = 4000
idxNum Tasty = 5000
idxNum HaskellDistributed = 6000
idxNum NumericalHaskell = 7000
idxNum FSharp = 8000
idxNum Ghcjs = 9000
idxNum HaskellBeginners = 10000
idxNum HLedger = 11000
idxNum Typelevel = 12000
idxNum Scalaz = 13000
idxNum Shapeless = 14000
idxNum Purescript = 15000
idxNum HaskellLens = 16000

-- | Possible supported channels.
data Channel
  = Haskell
  | Lisp
  | HaskellGame
  | Diagrams
  | Tasty
  | HaskellDistributed
  | NumericalHaskell
  | FSharp
  | Ghcjs
  | HaskellBeginners
  | HLedger
  | Typelevel
  | Scalaz
  | Shapeless
  | Purescript
  | HaskellLens
  deriving (Enum)
