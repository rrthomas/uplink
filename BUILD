package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary", "haskell_test"
)


load("//tools:settings.bzl", "ghc_path", "ghc_pkg_dbs")
load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_toolchain",
)

haskell_toolchain(
  name = "toolchain",
  version = "8.2.2",
  tools = "@ghc//:bin",
  #extra_ghc_package_dbs = ghc_pkg_dbs
)

uplink_version = "1.2.0"

filegroup(
  name = "golden-files",

  srcs = glob(["tests/golden/**/*.hs"]),
  visibility = ["//visibility:public"],
)

exports_files([
    "postgres/uplink.sql",
  "CHANGELOG.md",
  "uplink"
])


genrule(
    name = "uplink-config",
    outs = ["uplink-config.zip"],
    cmd = "zip -r $@ config/*",
    local = 1
)


