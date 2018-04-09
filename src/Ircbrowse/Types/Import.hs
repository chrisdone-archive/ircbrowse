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
  | CakeML
  | FSharp
  | Ghcjs
  | HaskellBeginners
  | HLedger
  | Typelevel
  | Scalaz
  | Shapeless
  | ProjectM36
  | Purescript
  | HaskellCN
  | ReflexFrp
  | HaskellIdeEngine
  | HaskellStack
  | Snowdrift
  | Servant
  | Ghc
  | Hackage
  | LibReviews
  | Yampa
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
  CakeML -> "#cakeml"
  FSharp -> "##fsharp"
  Ghcjs -> "#ghcjs"
  HaskellBeginners -> "#haskell-beginners"
  HLedger -> "#hledger"
  Typelevel -> "#typelevel"
  Scalaz -> "#scalaz"
  Shapeless -> "#shapeless"
  ProjectM36 -> "#project-m36"
  Purescript -> "#purescript"
  HaskellCN -> "#haskell-cn"
  ReflexFrp -> "#reflex-frp"
  HaskellIdeEngine -> "#haskell-ide-engine"
  HaskellStack -> "#haskell-stack"
  Snowdrift -> "#snowdrift"
  Servant -> "#servant"
  Ghc -> "#ghc"
  Hackage -> "#hackage"
  LibReviews -> "#lib.reviews"
  Yampa -> "#yampa"

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
  CakeML -> "cakeml"
  Ghcjs -> "ghcjs"
  HLedger -> "hledger"
  Typelevel -> "typelevel"
  Scalaz -> "scalaz"
  Shapeless -> "shapeless"
  ProjectM36 -> "project-m36"
  Purescript -> "purescript"
  HaskellCN -> "haskell-cn"
  ReflexFrp -> "reflex-frp"
  HaskellIdeEngine -> "haskell-ide-engine"
  HaskellStack -> "haskell-stack"
  Snowdrift -> "snowdrift"
  Servant -> "servant"
  Ghc -> "ghc"
  Hackage -> "hackage"
  LibReviews -> "lib.reviews"
  Yampa -> "yampa"

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
  Ghc -> 21
  Hackage -> 22
  Servant -> 23
  CakeML -> 24
  LibReviews -> 25
  ProjectM36 -> 26
  Yampa -> 28

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
  "cakeml" -> Just CakeML
  "fsharp" -> Just FSharp
  "ghcjs" -> Just Ghcjs
  "hledger" -> Just HLedger
  "typelevel" -> Just Typelevel
  "scalaz" -> Just Scalaz
  "shapeless" -> Just Shapeless
  "project-m36" -> Just ProjectM36
  "purescript" -> Just Purescript
  "haskell-cn" -> Just HaskellCN
  "reflex-frp" -> Just ReflexFrp
  "haskell-ide-engine" -> Just HaskellIdeEngine
  "haskell-stack" -> Just HaskellStack
  "snowdrift" -> Just Snowdrift
  "servant" -> Just Servant
  "ghc" -> Just Ghc
  "hackage" -> Just Hackage
  "lib.reviews" -> Just LibReviews
  "yampa" -> Just Yampa
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
  Ghc -> 21000
  Hackage -> 22000
  Servant -> 23000
  CakeML -> 24000
  LibReviews -> 25000
  ProjectM36 -> 26000
  Yampa -> 28000
