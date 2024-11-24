{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "react-basic-explore"
, dependencies =
  [ "console"
  , "control"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "free"
  , "freet"
  , "functors"
  , "identity"
  , "maybe"
  , "newtype"
  , "prelude"
  , "react"
  , "react-basic"
  , "react-basic-classic"
  , "react-basic-dom"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
