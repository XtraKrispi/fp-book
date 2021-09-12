{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies = [ 
      "console"
    , "effect"
    , "lists"
    , "maybe"
    , "prelude"
    , "psci-support" 
    , "tuples"
    , "undefined"]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
