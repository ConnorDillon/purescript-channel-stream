let upstream = https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20210226/packages.dhall sha256:7e973070e323137f27e12af93bc2c2f600d53ce4ae73bb51f34eb7d7ce0a43ea
in upstream
  with channel = 
    { repo = "https://github.com/ConnorDillon/purescript-channel.git"
    , version = "v0.2.0"
    , dependencies = 
        [ "console"
        , "effect"
        , "psci-support"
        , "contravariant"
        , "aff"
        , "avar"
        , "newtype"
        , "control"
        , "exceptions"
        , "assert"
        ]
    }
