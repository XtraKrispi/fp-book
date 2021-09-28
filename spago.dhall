{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies = [ 
      "console"
    , "contravariant"
    , "effect"
    , "foldable-traversable"
    , "integers"
    , "lists"
    , "maybe"
    , "newtype"
    , "nonempty"
    , "prelude"
    , "profunctor"
    , "psci-support" 
    , "strings"
    , "tuples"
    , "undefined"]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
