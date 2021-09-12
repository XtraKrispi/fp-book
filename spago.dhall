{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies = [ 
      "console"
    , "effect"
    , "integers"
    , "lists"
    , "maybe"
    , "newtype"
    , "prelude"
    , "psci-support" 
    , "strings"
    , "tuples"
    , "undefined"]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
