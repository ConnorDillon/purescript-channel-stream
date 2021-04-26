{ name = "channel-stream"
, dependencies =
  [ "console"
  , "effect"
  , "psci-support"
  , "contravariant"
  , "aff"
  , "avar"
  , "node-streams"
  , "node-fs"
  , "node-fs-aff"
  , "node-buffer"
  , "channel"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
