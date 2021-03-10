{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "ansi"
  , "console"
  , "debug"
  , "effect"
  , "fixed-points"
  , "matryoshka"
  , "psci-support"
  , "quickcheck"
  , "quickcheck-laws"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
