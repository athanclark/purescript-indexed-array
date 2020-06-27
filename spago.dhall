{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "foreign-object"
  , "generics-rep"
  , "psci-support"
  , "quickcheck-laws"
  , "quickcheck-utf8"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
