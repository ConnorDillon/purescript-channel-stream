{ name = "channel-stream"
, dependencies =
  [ "console"
  , "effect"
  , "psci-support"
  , "aff"
  , "avar"
  , "node-streams"
  , "node-buffer"
  , "channel"
  , "prelude"
  , "maybe"
  , "assert"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
