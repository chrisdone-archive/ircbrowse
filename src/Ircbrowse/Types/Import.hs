{-# LANGUAGE LambdaCase #-}

module Ircbrowse.Types.Import where

-- | Possible supported channels.
data Channel
  = Haskell
  | Lisp
  | HaskellGame
  | Diagrams
  | Tasty
  | HaskellDistributed
  | FSharp
  | Ghcjs
  | HaskellBeginners
  | HLedger
  | Typelevel
  | Scalaz
  | Shapeless
  | Purescript
  | HaskellCN
  | ReflexFrp
  | HaskellIdeEngine
  | HaskellStack
  | Snowdrift
  | LibReviews
  deriving (Enum)

-- | Pretty print a channel in a human-representation.
prettyChan :: Channel -> String
prettyChan = \case
  Haskell -> "#haskell"
  Lisp -> "#lisp"
  HaskellGame -> "#haskell-game"
  Diagrams -> "#diagrams"
  Tasty -> "#tasty"
  HaskellDistributed -> "#haskell-distributed"
  FSharp -> "##fsharp"
  Ghcjs -> "#ghcjs"
  HaskellBeginners -> "#haskell-beginners"
  HLedger -> "#hledger"
  Typelevel -> "#typelevel"
  Scalaz -> "#scalaz"
  Shapeless -> "#shapeless"
  Purescript -> "#purescript"
  HaskellCN -> "#haskell-cn"
  ReflexFrp -> "#reflex-frp"
  HaskellIdeEngine -> "#haskell-ide-engine"
  HaskellStack -> "#haskell-stack"
  Snowdrift -> "#snowdrift"
  LibReviews -> "#lib.reviews"

-- | Show a channel.
showChan :: Channel -> String
showChan = \case
  Haskell -> "haskell"
  Lisp -> "lisp"
  HaskellGame -> "haskell-game"
  Diagrams -> "diagrams"
  Tasty -> "tasty"
  HaskellDistributed -> "haskell-distributed"
  HaskellBeginners -> "haskell-beginners"
  FSharp -> "fsharp"
  Ghcjs -> "ghcjs"
  HLedger -> "hledger"
  Typelevel -> "typelevel"
  Scalaz -> "scalaz"
  Shapeless -> "shapeless"
  Purescript -> "purescript"
  HaskellCN -> "haskell-cn"
  ReflexFrp -> "reflex-frp"
  HaskellIdeEngine -> "haskell-ide-engine"
  HaskellStack -> "haskell-stack"
  Snowdrift -> "snowdrift"
  LibReviews -> "lib.reviews"

-- | Show a channel.
showChanInt :: Channel -> Int
showChanInt = \case
  Haskell -> 1
  Lisp -> 2
  HaskellGame -> 3
  Diagrams -> 4
  Tasty -> 5
  HaskellDistributed -> 6
  FSharp -> 8
  Ghcjs -> 9
  HaskellBeginners -> 10
  HLedger -> 11
  Typelevel -> 12
  Scalaz -> 13
  Shapeless -> 14
  Purescript -> 15
  HaskellCN -> 16
  ReflexFrp -> 17
  HaskellIdeEngine -> 18
  HaskellStack -> 19
  Snowdrift -> 20
  LibReviews -> 21

-- | Read a channel.
parseChan :: String -> Maybe Channel
parseChan = \case
  "haskell" ->  Just Haskell
  "lisp" ->  Just Lisp
  "diagrams" ->  Just Diagrams
  "haskell-game" -> Just HaskellGame
  "tasty" -> Just Tasty
  "haskell-beginners" -> Just HaskellBeginners
  "haskell-distributed" -> Just HaskellDistributed
  "fsharp" -> Just FSharp
  "ghcjs" -> Just Ghcjs
  "hledger" -> Just HLedger
  "typelevel" -> Just Typelevel
  "scalaz" -> Just Scalaz
  "shapeless" -> Just Shapeless
  "purescript" -> Just Purescript
  "haskell-cn" -> Just HaskellCN
  "reflex-frp" -> Just ReflexFrp
  "haskell-ide-engine" -> Just HaskellIdeEngine
  "haskell-stack" -> Just HaskellStack
  "snowdrift" -> Just Snowdrift
  "lib.reviews" -> Just LibReviews
  _ -> Nothing

idxNum :: Channel -> Int
idxNum = \case
  Haskell -> 1000
  Lisp -> 2000
  HaskellGame -> 3000
  Diagrams -> 4000
  Tasty -> 5000
  HaskellDistributed -> 6000
  FSharp -> 8000
  Ghcjs -> 9000
  HaskellBeginners -> 10000
  HLedger -> 11000
  Typelevel -> 12000
  Scalaz -> 13000
  Shapeless -> 14000
  Purescript -> 15000
  HaskellCN -> 16000
  ReflexFrp -> 17000
  HaskellIdeEngine -> 18000
  HaskellStack -> 19000
  Snowdrift -> 20000
  LibReviews -> 21000
