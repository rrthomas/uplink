package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary", "haskell_test"
)

haskell_binary(
  name = "uplink",
  srcs = ["Main.hs"],
  deps = [
      "@protolude//:protolude",
      "@List//:List",
      "//src:uplink-lib"
      ],
  compiler_flags = ['-O2', '-XNoImplicitPrelude', "-XLambdaCase", "-XRecordWildCards", "-XOverloadedStrings","-XFlexibleInstances", "-XFlexibleContexts", "-XRank2Types"],
  version = "1.2.0",
  prebuilt_dependencies = [
    "base-4.10.1.0",
    "optparse-applicative-0.14.0.0",
    #"List-0.6.2",
    "bytestring-0.10.8.2",
    #"protolude-0.2",
  ]

)


