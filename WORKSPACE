load("//tools:settings.bzl", "ghc_path", "ghc_pkg_dbs")

http_archive(
  name = "io_tweag_rules_haskell",
  strip_prefix = "rules_haskell-master",
  urls = ["https://github.com/tweag/rules_haskell/archive/master.tar.gz"],
)

load("@io_tweag_rules_haskell//haskell:repositories.bzl", "haskell_repositories")

haskell_repositories()

register_toolchains("//:toolchain")

new_http_archive(
    name = "ghc_bindist",
    urls = [
      "https://downloads.haskell.org/~ghc/8.2.2/ghc-8.2.2-x86_64-deb8-linux.tar.xz",
    ],
    strip_prefix = "ghc-8.2.2",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

#genrule(
      #name = "ghc_bindist",
      #srcs = ["**/*"]
      #outs = ["out/bin/ghc"],
      #cmd = "$(location configure) --prefix=`pwd`/out && make install) > $@",
#)

#filegroup(
  #name = "bin",
  #srcs = glob([
    #"bin/ghc*",
    #"bin/hsc2hs",
    #"bin/haddock",
  #]),
  #deps = [":ghc_bindist"]
#)


genrule(
      name = "ghc_bindist",
      outs = ["out/bin/ghc"],
      cmd = "./configure --prefix=`pwd`/out && make install ",
      local = 1
)

filegroup(
    name = "bin",
    srcs = glob([
        "bin/*"
        ]
    ))


"""
)
new_local_repository(
  name = "ghc",
  #path = "/usr/",
  path = ghc_path,
  build_file_content = """
package(default_visibility = ["//visibility:public"])
filegroup(
  name = "bin",
  srcs = glob([
    "bin/ghc*",
    "bin/hsc2hs",
    "bin/haddock",
  ])
)

cc_library(
  name = "threaded-rts",
  srcs = glob(["lib/ghc-*/rts/libHSrts_thr-ghc*.so"]),
  hdrs = glob(["lib/ghc-*/include/**/*.h"]),
  strip_include_prefix = glob(["lib/ghc-*/include"], exclude_directories=0)[0],
)
"""
)


new_http_archive(
    name = "uplink_explorer",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

genrule(
    name = "uplink-explorer-zip",
    tools = ["."],
    outs = ["uplink-explorer.zip"],
    cmd = "zip -r $@ $(location .)*",
)

""",
    strip_prefix = "uplink-explorer-master",
    urls = [
        "https://github.com/adjoint-io/uplink-explorer/archive/master.zip"
    ],
)

new_http_archive(
    name = "uplink_sdk_python",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

genrule(
    name = "uplink-sdk-python-zip",
    tools = ["."],
    outs = ["uplink-sdk-python.zip"],
    cmd = "zip -r $@ $(location .)*",
)

""",
    strip_prefix = "uplink-sdk-python-master",
    urls = [
        "https://github.com/adjoint-io/uplink-sdk-python/archive/master.zip"
    ],
)


new_http_archive(
    name = "uplink_sdk_java",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

genrule(
    name = "uplink-sdk-java-zip",
    tools = ["."],
    outs = ["uplink-sdk-java.zip"],
    cmd = "zip -r $@ $(location .)*",
)

""",
    strip_prefix = "uplink-sdk-java-master",
    urls = [
        "https://github.com/adjoint-io/uplink-sdk-java/archive/master.zip"
    ],
)


new_http_archive(
    name = "List",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'List',
  src_strip_prefix="src",
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "transformers","base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "List-0.6.2",
    urls = [
        "https://hackage.haskell.org/package/List-0.6.2/List-0.6.2.tar.gz"
    ],
)

new_http_archive(
    name = "aeson",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

cc_library(
  name = "cbits",
  srcs = ["cbits/unescape_string.c"],
  hdrs = glob(["include/*.h"]),
  copts = ["-Iexternal/aeson/include"],
)

haskell_library(
  name = 'aeson',
  srcs = glob(['src/**/*.hs', 'attoparsec-iso8601/**/*.hs' ,'Data/**/*.hs', 'ffi/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "deepseq","template-haskell","time","bytestring","ghc-prim","base","containers"
  ],
  compiler_flags = ["-DCFFI"],
  deps = [
    ":cbits",
    "@dlist//:dlist",
    "@vector//:vector",
    "@text//:text",
    "@scientific//:scientific",
    "@hashable//:hashable",
    "@uuid_types//:uuid-types",
    "@unordered_containers//:unordered-containers",
    "@tagged//:tagged",
    "@base_compat//:base-compat",
    "@th_abstraction//:th-abstraction",
    "@time_locale_compat//:time-locale-compat",
    "@attoparsec//:attoparsec",
  ]
)
""",
    strip_prefix = "aeson-1.2.3.0",
    urls = [
        "https://hackage.haskell.org/package/aeson-1.2.3.0/aeson-1.2.3.0.tar.gz"
    ],
)

new_http_archive(
    name = "aeson_pretty",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'aeson_pretty',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "bytestring","vector","base"
  ],
  deps = [
    "@unordered_containers//:unordered_containers",
    "@scientific//:scientific",
    "@aeson//:aeson",
    "@text//:text",
    #"@base_compat//:base_compat"
  ]
)
""",
    strip_prefix = "aeson-pretty-0.8.5",
    urls = [
        "https://hackage.haskell.org/package/aeson-pretty-0.8.5/aeson-pretty-0.8.5.tar.gz"
    ],
)

new_http_archive(
    name = "ansi_terminal",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'ansi_terminal',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base"
  ],
  deps = [
    "@colour//:colour"
  ]
)
""",
    strip_prefix = "ansi-terminal-0.7.1.1",
    urls = [
        "https://hackage.haskell.org/package/ansi-terminal-0.7.1.1/ansi-terminal-0.7.1.1.tar.gz"
    ],
)

new_http_archive(
    name = "ansi_wl_pprint",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'ansi_wl_pprint',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base"
  ],
  deps = [
    "@ansi_terminal//:ansi_terminal"
  ]
)
""",
    strip_prefix = "ansi-wl-pprint-0.6.8.1",
    urls = [
        "https://hackage.haskell.org/package/ansi-wl-pprint-0.6.8.1/ansi-wl-pprint-0.6.8.1.tar.gz"
    ],
)

new_http_archive(
    name = "appar",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'appar',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Text/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base","bytestring"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "appar-0.1.4",
    urls = [
        "https://hackage.haskell.org/package/appar-0.1.4/appar-0.1.4.tar.gz"
    ],
)

new_http_archive(
    name = "arithmoi",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'arithmoi',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "integer-gmp","array","random","mtl","ghc-prim","base","containers"
  ],
  deps = [
    "@integer_logarithms//:integer-logarithms",
    "@exact_pi//:exact-pi"
  ]
)
""",
    strip_prefix = "arithmoi-0.6.0.0",
    urls = [
        "https://hackage.haskell.org/package/arithmoi-0.6.0.0/arithmoi-0.6.0.0.tar.gz"
    ],
)

new_http_archive(
    name = "array",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'array',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "array-0.5.2.0",
    urls = [
        "https://hackage.haskell.org/package/array-0.5.2.0/array-0.5.2.0.tar.gz"
    ],
)

new_http_archive(
    name = "asn1_encoding",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'asn1_encoding',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base","bytestring"
  ],
  deps = [
    "@asn1_types//:asn1_types",
    "@hourglass//:hourglass"
  ]
)
""",
    strip_prefix = "asn1-encoding-0.9.5",
    urls = [
        "https://hackage.haskell.org/package/asn1-encoding-0.9.5/asn1-encoding-0.9.5.tar.gz"
    ],
)

new_http_archive(
    name = "asn1_parse",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'asn1_parse',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base","bytestring"
  ],
  deps = [
    "@asn1_encoding//:asn1_encoding",
    "@asn1_types//:asn1_types"
  ]
)
""",
    strip_prefix = "asn1-parse-0.9.4",
    urls = [
        "https://hackage.haskell.org/package/asn1-parse-0.9.4/asn1-parse-0.9.4.tar.gz"
    ],
)

new_http_archive(
    name = "asn1_types",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'asn1_types',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base","bytestring"
  ],
  deps = [
    "@memory//:memory",
    "@hourglass//:hourglass"
  ]
)
""",
    strip_prefix = "asn1-types-0.3.2",
    urls = [
        "https://hackage.haskell.org/package/asn1-types-0.3.2/asn1-types-0.3.2.tar.gz"
    ],
)

new_http_archive(
    name = "async",
    build_file_content = """

package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'async',
  srcs = glob(['Control/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base", "stm"
  ],
  deps = [
  ]
)

""",
    strip_prefix = "async-2.1.1.1",
    urls = [
        "https://hackage.haskell.org/package/async-2.1.1.1/async-2.1.1.1.tar.gz"
    ],
)

new_http_archive(
    name = "attoparsec",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'attoparsec',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "deepseq","array","bytestring","transformers","base","containers"
  ],
  deps = [
    "@scientific//:scientific",
    "@text//:text"
  ]
)
""",
    strip_prefix = "attoparsec-0.13.2.0",
    urls = [
        "https://hackage.haskell.org/package/attoparsec-0.13.2.0/attoparsec-0.13.2.0.tar.gz"
    ],
)

new_http_archive(
    name = "auto_update",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'auto_update',
  srcs = glob(['Control/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "auto-update-0.1.4",
    urls = [
        "https://hackage.haskell.org/package/auto-update-0.1.4/auto-update-0.1.4.tar.gz"
    ],
)

new_http_archive(
    name = "base",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'base',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "integer-gmp","ghc-prim","base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "base-4.10.1.0",
    urls = [
        "https://hackage.haskell.org/package/base-4.10.1.0/base-4.10.1.0.tar.gz"
    ],
)

new_http_archive(
    name = "base_compat",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'base-compat',
  src_strip_prefix = "src",
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "unix","base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "base-compat-0.9.3",
    urls = [
        "https://hackage.haskell.org/package/base-compat-0.9.3/base-compat-0.9.3.tar.gz"
    ],
)

new_http_archive(
    name = "base58_bytestring",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'base58-bytestring',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base","bytestring"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "base58-bytestring-0.1.0",
    urls = [
        "https://hackage.haskell.org/package/base58-bytestring-0.1.0/base58-bytestring-0.1.0.tar.gz"
    ],
)

new_http_archive(
    name = "base64_bytestring",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'base64_bytestring',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base","bytestring"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "base64-bytestring-1.0.0.1",
    urls = [
        "https://hackage.haskell.org/package/base64-bytestring-1.0.0.1/base64-bytestring-1.0.0.1.tar.gz"
    ],
)

new_http_archive(
    name = "basement",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

cc_library(
  name = "cbits",
  srcs = glob(["cbits/*.c"]),
  hdrs = glob(["cbits/*.h"]),
  deps = ["@ghc//:threaded-rts"],
  copts = ["-fomit-frame-pointer"]
)

haskell_library(
  name = 'basement',
  srcs = glob(['Basement/**/*.hs','Basement/**/*.hsc', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  compiler_flags = ["-XNoImplicitPrelude", "-XRebindableSyntax", "-XTypeFamilies", "-XBangPatterns", "-XDeriveDataTypeable"],
  prebuilt_dependencies = [
    "ghc-prim","base"
  ],
  deps = [
    ":cbits"
  ]
)
""",
    strip_prefix = "basement-0.0.4",
    urls = [
        "https://hackage.haskell.org/package/basement-0.0.4/basement-0.0.4.tar.gz"
    ],
)

new_http_archive(
    name = "binary",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'binary',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "array","bytestring","base","containers"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "binary-0.8.5.1",
    urls = [
        "https://hackage.haskell.org/package/binary-0.8.5.1/binary-0.8.5.1.tar.gz"
    ],
)

new_http_archive(
    name = "blaze_builder",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'blaze_builder',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "deepseq","bytestring","text","base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "blaze-builder-0.4.0.2",
    urls = [
        "https://hackage.haskell.org/package/blaze-builder-0.4.0.2/blaze-builder-0.4.0.2.tar.gz"
    ],
)

new_http_archive(
    name = "byteable",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'byteable',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base","bytestring"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "byteable-0.1.1",
    urls = [
        "https://hackage.haskell.org/package/byteable-0.1.1/byteable-0.1.1.tar.gz"
    ],
)

new_http_archive(
    name = "byteorder",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'byteorder',
  srcs = glob(['System/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "byteorder-1.0.4",
    urls = [
        "https://hackage.haskell.org/package/byteorder-1.0.4/byteorder-1.0.4.tar.gz"
    ],
)

new_http_archive(
    name = "bytestring",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'bytestring',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "integer-gmp","deepseq","ghc-prim","base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "bytestring-0.10.8.2",
    urls = [
        "https://hackage.haskell.org/package/bytestring-0.10.8.2/bytestring-0.10.8.2.tar.gz"
    ],
)

new_http_archive(
    name = "bytestring_builder",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'bytestring_builder',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "deepseq","base","bytestring"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "bytestring-builder-0.10.8.1.0",
    urls = [
        "https://hackage.haskell.org/package/bytestring-builder-0.10.8.1.0/bytestring-builder-0.10.8.1.0.tar.gz"
    ],
)

new_http_archive(
    name = "case_insensitive",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'case_insensitive',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "deepseq","bytestring","text","base"
  ],
  deps = [
    "@hashable//:hashable"
  ]
)
""",
    strip_prefix = "case-insensitive-1.2.0.10",
    urls = [
        "https://hackage.haskell.org/package/case-insensitive-1.2.0.10/case-insensitive-1.2.0.10.tar.gz"
    ],
)

new_http_archive(
    name = "cereal",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'cereal',
  src_strip_prefix = "src",
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "containers","bytestring","ghc-prim","base","array"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "cereal-0.5.4.0",
    urls = [
        "https://hackage.haskell.org/package/cereal-0.5.4.0/cereal-0.5.4.0.tar.gz"
    ],
)

new_http_archive(
    name = "clock",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'clock',
  srcs = glob(['System/**/*.hsc', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base"
  ],
  compiler_flags = [
    "-XDeriveGeneric",
    "-XDeriveDataTypeable",
    "-XForeignFunctionInterface",
    "-XScopedTypeVariables",
    "-XViewPatterns"

  ],
  deps = [

  ]
)
""",
    strip_prefix = "clock-0.7.2",
    urls = [
        "https://hackage.haskell.org/package/clock-0.7.2/clock-0.7.2.tar.gz"
    ],
)

new_http_archive(
    name = "cmdargs",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'cmdargs',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "process","filepath","template-haskell","transformers","base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "cmdargs-0.10.18",
    urls = [
        "https://hackage.haskell.org/package/cmdargs-0.10.18/cmdargs-0.10.18.tar.gz"
    ],
)

new_http_archive(
    name = "colour",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'colour',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "colour-2.3.4",
    urls = [
        "https://hackage.haskell.org/package/colour-2.3.4/colour-2.3.4.tar.gz"
    ],
)

new_http_archive(
    name = "configurator",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'configurator',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "directory","bytestring","base", "unix-compat"
  ],
  deps = [
    "@unordered_containers//:unordered-containers",
    #"@unix_compat//:unix-compat",
    "@hashable//:hashable",
    "@attoparsec//:attoparsec",
    "@text//:text"
  ]
)
""",
    strip_prefix = "configurator-0.3.0.0",
    urls = [
        "https://hackage.haskell.org/package/configurator-0.3.0.0/configurator-0.3.0.0.tar.gz"
    ],
)

new_http_archive(
    name = "containers",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'containers',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "deepseq","ghc-prim","base","array"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "containers-0.5.10.2",
    urls = [
        "https://hackage.haskell.org/package/containers-0.5.10.2/containers-0.5.10.2.tar.gz"
    ],
)

new_http_archive(
    name = "cookie",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'cookie',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "deepseq","bytestring","text","base","time"
  ],
  deps = [
    "@blaze_builder//:blaze_builder",
    "@data_default_class//:data_default_class",
    "@old_locale//:old_locale"
  ]
)
""",
    strip_prefix = "cookie-0.4.3",
    urls = [
        "https://hackage.haskell.org/package/cookie-0.4.3/cookie-0.4.3.tar.gz"
    ],
)

new_http_archive(
    name = "cryptonite",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'cryptonite',
  srcs = glob(['Crypto/**/*.hs', 'Crypto/**/*.hsc' ,'Data/**/*.hs'], exclude=["Crypto/**/Windows*"]),
  # c_sources = glob(['cbits/**/*.c']),
  compiler_flags = ["-XCPP", "-XExistentialQuantification", "-XDeriveDataTypeable"],

  prebuilt_dependencies = [
    "integer-gmp","deepseq","bytestring","ghc-prim","base", "foundation-0.0.17", "memory-0.14.10"
  ],
  deps = [
    #"@memory//:memory",
    #"@foundation//:foundation"
  ]
)
""",
    strip_prefix = "cryptonite-0.24",
    urls = [
        "https://hackage.haskell.org/package/cryptonite-0.24/cryptonite-0.24.tar.gz"
    ],
)

new_http_archive(
    name = "data_accessor",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'data_accessor',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "containers","transformers","base","array"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "data-accessor-0.2.2.7",
    urls = [
        "https://hackage.haskell.org/package/data-accessor-0.2.2.7/data-accessor-0.2.2.7.tar.gz"
    ],
)

new_http_archive(
    name = "data_default",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'data_default',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base"
  ],
  deps = [
    "@data_default_instances_containers//:data_default_instances_containers",
    "@data_default_instances_dlist//:data_default_instances_dlist",
    "@data_default_class//:data_default_class",
    "@data_default_instances_old_locale//:data_default_instances_old_locale"
  ]
)
""",
    strip_prefix = "data-default-0.7.1.1",
    urls = [
        "https://hackage.haskell.org/package/data-default-0.7.1.1/data-default-0.7.1.1.tar.gz"
    ],
)

new_http_archive(
    name = "data_default_class",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'data_default_class',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "data-default-class-0.1.2.0",
    urls = [
        "https://hackage.haskell.org/package/data-default-class-0.1.2.0/data-default-class-0.1.2.0.tar.gz"
    ],
)

new_http_archive(
    name = "data_default_instances_containers",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'data_default_instances_containers',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base","containers"
  ],
  deps = [
    "@data_default_class//:data_default_class"
  ]
)
""",
    strip_prefix = "data-default-instances-containers-0.0.1",
    urls = [
        "https://hackage.haskell.org/package/data-default-instances-containers-0.0.1/data-default-instances-containers-0.0.1.tar.gz"
    ],
)

new_http_archive(
    name = "data_default_instances_dlist",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'data_default_instances_dlist',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base"
  ],
  deps = [
    "@dlist//:dlist",
    "@data_default_class//:data_default_class"
  ]
)
""",
    strip_prefix = "data-default-instances-dlist-0.0.1",
    urls = [
        "https://hackage.haskell.org/package/data-default-instances-dlist-0.0.1/data-default-instances-dlist-0.0.1.tar.gz"
    ],
)

new_http_archive(
    name = "data_default_instances_old_locale",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'data_default_instances_old_locale',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base"
  ],
  deps = [
    "@data_default_class//:data_default_class",
    "@old_locale//:old_locale"
  ]
)
""",
    strip_prefix = "data-default-instances-old-locale-0.0.1",
    urls = [
        "https://hackage.haskell.org/package/data-default-instances-old-locale-0.0.1/data-default-instances-old-locale-0.0.1.tar.gz"
    ],
)

new_http_archive(
    name = "datetimes",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'datetimes',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  src_strip_prefix = "src",
  prebuilt_dependencies = [
    "base","time", "aeson", "cereal", "hourglass"
  ],
  deps = [
    #"@cereal//:cereal",
    "@protolude//:protolude",
    #"@aeson//:aeson",
    #"@hourglass//:hourglass"
  ]
)
""",
    strip_prefix = "datetime-master",
    urls = [
        "https://github.com/adjoint-io/datetime/archive/master.zip"
    ],
)

new_http_archive(
    name = "deepseq",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'deepseq',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base","array"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "deepseq-1.4.3.0",
    urls = [
        "https://hackage.haskell.org/package/deepseq-1.4.3.0/deepseq-1.4.3.0.tar.gz"
    ],
)

new_http_archive(
    name = "directory",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'directory',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "filepath","unix","base","time"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "directory-1.3.0.2",
    urls = [
        "https://hackage.haskell.org/package/directory-1.3.0.2/directory-1.3.0.2.tar.gz"
    ],
)

new_http_archive(
    name = "distributed_process",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'distributed_process',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "transformers","deepseq","template-haskell","binary","random","time","bytestring","mtl","stm","base","containers"
  ],
  deps = [
    "@hashable//:hashable",
    "@distributed_static//:distributed_static",
    "@data_accessor//:data_accessor",
    "@syb//:syb",
    "@rank1dynamic//:rank1dynamic",
    "@exceptions//:exceptions",
    "@network_transport//:network_transport"
  ]
)
""",
    strip_prefix = "distributed-process-0.7.3",
    urls = [
        "https://hackage.haskell.org/package/distributed-process-0.7.3/distributed-process-0.7.3.tar.gz"
    ],
)

new_http_archive(
    name = "distributed_process_lifted",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'distributed_process_lifted',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "deepseq","mtl","transformers","base"
  ],
  deps = [
    "@monad_control//:monad_control",
    "@distributed_process_monad_control//:distributed_process_monad_control",
    "@distributed_process//:distributed_process",
    "@lifted_base//:lifted_base",
    "@network_transport//:network_transport",
    "@transformers_base//:transformers_base"
  ]
)
""",
    strip_prefix = "distributed-process-lifted-0.3.0.0",
    urls = [
        "https://hackage.haskell.org/package/distributed-process-lifted-0.3.0.0/distributed-process-lifted-0.3.0.0.tar.gz"
    ],
)

new_http_archive(
    name = "distributed_process_monad_control",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'distributed_process_monad_control',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "transformers","base"
  ],
  deps = [
    "@distributed_process//:distributed_process",
    "@monad_control//:monad_control",
    "@transformers_base//:transformers_base"
  ]
)
""",
    strip_prefix = "distributed-process-monad-control-0.5.1.3",
    urls = [
        "https://hackage.haskell.org/package/distributed-process-monad-control-0.5.1.3/distributed-process-monad-control-0.5.1.3.tar.gz"
    ],
)

new_http_archive(
    name = "distributed_static",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'distributed_static',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "containers","deepseq","bytestring","base","binary"
  ],
  deps = [
    "@rank1dynamic//:rank1dynamic"
  ]
)
""",
    strip_prefix = "distributed-static-0.3.8",
    urls = [
        "https://hackage.haskell.org/package/distributed-static-0.3.8/distributed-static-0.3.8.tar.gz"
    ],
)

new_http_archive(
    name = "dlist",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'dlist',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "deepseq","base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "dlist-0.8.0.3",
    urls = [
        "https://hackage.haskell.org/package/dlist-0.8.0.3/dlist-0.8.0.3.tar.gz"
    ],
)

new_http_archive(
    name = "easy_file",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'easy_file',
  srcs = glob(['System/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "directory","filepath","unix","base","time"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "easy-file-0.2.1",
    urls = [
        "https://hackage.haskell.org/package/easy-file-0.2.1/easy-file-0.2.1.tar.gz"
    ],
)

new_http_archive(
    name = "exact_pi",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'exact-pi',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base"
  ],
  deps = [
    "@numtype_dk//:numtype_dk"
  ]
)
""",
    strip_prefix = "exact-pi-0.4.1.2",
    urls = [
        "https://hackage.haskell.org/package/exact-pi-0.4.1.2/exact-pi-0.4.1.2.tar.gz"
    ],
)

new_http_archive(
    name = "exceptions",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'exceptions',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "stm","mtl","template-haskell","transformers","base"
  ],
  deps = [
    #"@transformers_compat//:transformers_compat"
  ]
)
""",
    strip_prefix = "exceptions-0.8.3",
    urls = [
        "https://hackage.haskell.org/package/exceptions-0.8.3/exceptions-0.8.3.tar.gz"
    ],
)

new_http_archive(
    name = "extensible_exceptions",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'extensible_exceptions',
  srcs = glob(['Control/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  compiler_flags = ["-XCPP", "-XExistentialQuantification", "-XDeriveDataTypeable"],
  prebuilt_dependencies = [
    "base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "extensible-exceptions-0.1.1.4",
    urls = [
        "https://hackage.haskell.org/package/extensible-exceptions-0.1.1.4/extensible-exceptions-0.1.1.4.tar.gz"
    ],
)

new_http_archive(
    name = "fail",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'fail',
  srcs = glob(['Control/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "fail-4.9.0.0",
    urls = [
        "https://hackage.haskell.org/package/fail-4.9.0.0/fail-4.9.0.0.tar.gz"
    ],
)

new_http_archive(
    name = "fast_logger",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'fast_logger',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "text","array","directory","filepath","bytestring","unix","base"
  ],
  deps = [
    "@unix_time//:unix_time",
    "@easy_file//:easy_file",
    "@auto_update//:auto_update"
  ]
)
""",
    strip_prefix = "fast-logger-2.4.10",
    urls = [
        "https://hackage.haskell.org/package/fast-logger-2.4.10/fast-logger-2.4.10.tar.gz"
    ],
)

new_http_archive(
    name = "fgl",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'fgl',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "containers","deepseq","transformers","base","array"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "fgl-5.5.4.0",
    urls = [
        "https://hackage.haskell.org/package/fgl-5.5.4.0/fgl-5.5.4.0.tar.gz"
    ],
)

new_http_archive(
    name = "file_embed",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'file_embed',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "directory","filepath","bytestring","template-haskell","base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "file-embed-0.0.10.1",
    urls = [
        "https://hackage.haskell.org/package/file-embed-0.0.10.1/file-embed-0.0.10.1.tar.gz"
    ],
)

new_http_archive(
    name = "filelock",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'filelock',
  srcs = glob(['System/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "unix","base"
  ],
  compiler_flags = ["-DUSE_FLOCK"],
  deps = [

  ]
)
""",
    strip_prefix = "filelock-0.1.1.2",
    urls = [
        "https://hackage.haskell.org/package/filelock-0.1.1.2/filelock-0.1.1.2.tar.gz"
    ],
)

new_http_archive(
    name = "filepath",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'filepath',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "filepath-1.4.1.2",
    urls = [
        "https://hackage.haskell.org/package/filepath-1.4.1.2/filepath-1.4.1.2.tar.gz"
    ],
)

new_http_archive(
    name = "foundation",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)
cc_library(
  name = "cbits",
  srcs = glob(["cbits/*.c"]),
  hdrs = glob(["cbits/*.h"]),
  deps = ["@ghc//:threaded-rts"],
  copts = ["-fomit-frame-pointer"]
)
haskell_library(
  name = 'foundation',
  srcs = glob(['Foundation/**/*.hs', 'Foundation/**/*.hsc', 'lib/**/*.hs' ,'Data/**/*.hs' ], exclude=["Foundation/**/Windows*"]),
  # c_sources = glob(['cbits/**/*.c']),
  compiler_flags = ["-XNoImplicitPrelude", "-XRebindableSyntax", "-XTypeFamilies", "-XBangPatterns", "-XDeriveDataTypeable"],
  prebuilt_dependencies = [
    "ghc-prim","base"
  ],
  deps = [
    "@basement//:basement",
    ":cbits"
  ]
)
""",
    strip_prefix = "foundation-0.0.17",
    urls = [
        "https://hackage.haskell.org/package/foundation-0.0.17/foundation-0.0.17.tar.gz"
    ],
)

new_http_archive(
    name = "ghc_boot_th",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'ghc_boot_th',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "ghc-boot-th-8.2.2",
    urls = [
        "https://hackage.haskell.org/package/ghc-boot-th-8.2.2/ghc-boot-th-8.2.2.tar.gz"
    ],
)

new_http_archive(
    name = "ghc_prim",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'ghc_prim',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "ghc-prim-0.5.1.1",
    urls = [
        "https://hackage.haskell.org/package/ghc-prim-0.5.1.1/ghc-prim-0.5.1.1.tar.gz"
    ],
)

new_http_archive(
    name = "gitrev",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'gitrev',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "directory","process","filepath","template-haskell","base"
  ],
  deps = [
    "@base_compat//:base-compat"
  ]
)
""",
    strip_prefix = "gitrev-1.3.1",
    urls = [
        "https://hackage.haskell.org/package/gitrev-1.3.1/gitrev-1.3.1.tar.gz"
    ],
)

new_http_archive(
    name = "hashable",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

cc_library(
  name = "fnv",
  srcs = ["cbits/fnv.c"],
)

haskell_library(
  name = 'hashable',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  version = "1.2.6.1",
  compiler_flags = ["-XTypeSynonymInstances", "-DGENERICS"],
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base","ghc-prim","integer-gmp","bytestring", "deepseq", "text-1.2.2.2"
  ],
  deps = [
    ":fnv",
    #"@text//:text",
  ]
)
""",
    strip_prefix = "hashable-1.2.6.1",
    urls = [
        "https://hackage.haskell.org/package/hashable-1.2.6.1/hashable-1.2.6.1.tar.gz"
    ],
)

new_http_archive(
    name = "haskeline",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'haskeline',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "process","terminfo","stm","directory","filepath","bytestring","transformers","unix","base","containers"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "haskeline-0.7.4.2",
    urls = [
        "https://hackage.haskell.org/package/haskeline-0.7.4.2/haskeline-0.7.4.2.tar.gz"
    ],
)

new_http_archive(
    name = "haskell_lexer",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'haskell_lexer',
  srcs = glob(['Language/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "haskell-lexer-1.0.1",
    urls = [
        "https://hackage.haskell.org/package/haskell-lexer-1.0.1/haskell-lexer-1.0.1.tar.gz"
    ],
)

new_http_archive(
    name = "hexpat",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'hexpat',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "transformers","deepseq","bytestring","text","base","containers"
  ],
  deps = [
    "@List//:List",
    "@utf8_string//:utf8_string"
  ]
)
""",
    strip_prefix = "hexpat-0.20.13",
    urls = [
        "https://hackage.haskell.org/package/hexpat-0.20.13/hexpat-0.20.13.tar.gz"
    ],
)

new_http_archive(
    name = "hexpat_pickle",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'hexpat_pickle',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "bytestring","text","base","containers"
  ],
  deps = [
    "@extensible_exceptions//:extensible_exceptions",
    "@utf8_string//:utf8_string",
    "@hexpat//:hexpat"
  ]
)
""",
    strip_prefix = "hexpat-pickle-0.6",
    urls = [
        "https://hackage.haskell.org/package/hexpat-pickle-0.6/hexpat-pickle-0.6.tar.gz"
    ],
)

new_http_archive(
    name = "hourglass",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'hourglass',
  srcs = glob(['System/**/*.hs', 'Time/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "deepseq","base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "hourglass-0.2.10",
    urls = [
        "https://hackage.haskell.org/package/hourglass-0.2.10/hourglass-0.2.10.tar.gz"
    ],
)

new_http_archive(
    name = "hslogger",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'hslogger',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "process","time","directory","mtl","unix","base","containers"
  ],
  deps = [
    "@network//:network",
    "@old_locale//:old_locale"
  ]
)
""",
    strip_prefix = "hslogger-1.2.10",
    urls = [
        "https://hackage.haskell.org/package/hslogger-1.2.10/hslogger-1.2.10.tar.gz"
    ],
)

new_http_archive(
    name = "http_client",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'http_client',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "deepseq","text","array","random","time","filepath","bytestring","transformers","ghc-prim","base","containers"
  ],
  deps = [
    "@cookie//:cookie",
    "@mime_types//:mime_types",
    "@http_types//:http_types",
    "@network//:network",
    "@case_insensitive//:case_insensitive",
    "@exceptions//:exceptions",
    "@network_uri//:network_uri",
    "@blaze_builder//:blaze_builder",
    "@streaming_commons//:streaming_commons",
    "@base64_bytestring//:base64_bytestring"
  ]
)
""",
    strip_prefix = "http-client-0.5.7.1",
    urls = [
        "https://hackage.haskell.org/package/http-client-0.5.7.1/http-client-0.5.7.1.tar.gz"
    ],
)

new_http_archive(
    name = "http_date",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'http_date',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "array","base","bytestring"
  ],
  deps = [
    "@attoparsec//:attoparsec"
  ]
)
""",
    strip_prefix = "http-date-0.0.6.1",
    urls = [
        "https://hackage.haskell.org/package/http-date-0.0.6.1/http-date-0.0.6.1.tar.gz"
    ],
)

new_http_archive(
    name = "http_types",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'http_types',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "bytestring","text","base","array"
  ],
  deps = [
    "@blaze_builder//:blaze_builder",
    "@case_insensitive//:case_insensitive"
  ]
)
""",
    strip_prefix = "http-types-0.9.1",
    urls = [
        "https://hackage.haskell.org/package/http-types-0.9.1/http-types-0.9.1.tar.gz"
    ],
)

new_http_archive(
    name = "http2",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'http2',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "containers","bytestring","stm","base","array"
  ],
  deps = [
    "@bytestring_builder//:bytestring_builder",
    "@case_insensitive//:case_insensitive",
    "@psqueues//:psqueues"
  ]
)
""",
    strip_prefix = "http2-1.6.3",
    urls = [
        "https://hackage.haskell.org/package/http2-1.6.3/http2-1.6.3.tar.gz"
    ],
)

new_http_archive(
    name = "integer_gmp",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'integer_gmp',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "ghc-prim","base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "integer-gmp-1.0.1.0",
    urls = [
        "https://hackage.haskell.org/package/integer-gmp-1.0.1.0/integer-gmp-1.0.1.0.tar.gz"
    ],
)

new_http_archive(
    name = "integer_logarithms",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'integer-logarithms',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  src_strip_prefix = "src",
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "integer-gmp","ghc-prim","base","array"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "integer-logarithms-1.0.2",
    urls = [
        "https://hackage.haskell.org/package/integer-logarithms-1.0.2/integer-logarithms-1.0.2.tar.gz"
    ],
)

new_http_archive(
    name = "iproute",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'iproute',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base","containers"
  ],
  deps = [
    "@byteorder//:byteorder",
    "@network//:network",
    "@appar//:appar"
  ]
)
""",
    strip_prefix = "iproute-1.7.1",
    urls = [
        "https://hackage.haskell.org/package/iproute-1.7.1/iproute-1.7.1.tar.gz"
    ],
)

new_http_archive(
    name = "leveldb_haskell",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'leveldb_haskell',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "filepath","bytestring","transformers","base"
  ],
  deps = [
    "@data_default//:data_default",
    "@resourcet//:resourcet",
    "@exceptions//:exceptions"
  ]
)
""",
    strip_prefix = "leveldb-haskell-0.6.5",
    urls = [
        "https://hackage.haskell.org/package/leveldb-haskell-0.6.5/leveldb-haskell-0.6.5.tar.gz"
    ],
)

new_http_archive(
    name = "lifted_base",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'lifted_base',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base"
  ],
  deps = [
    "@monad_control//:monad_control",
    "@transformers_base//:transformers_base"
  ]
)
""",
    strip_prefix = "lifted-base-0.2.3.11",
    urls = [
        "https://hackage.haskell.org/package/lifted-base-0.2.3.11/lifted-base-0.2.3.11.tar.gz"
    ],
)

new_http_archive(
    name = "memory",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'memory',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "deepseq","bytestring","ghc-prim","base", "basement", "foundation"
  ],
  deps = [
    #"@basement//:basement",
    #"@foundation//:foundation"
  ]
)
""",
    strip_prefix = "memory-0.14.11",
    urls = [
        "https://hackage.haskell.org/package/memory-0.14.11/memory-0.14.11.tar.gz"
    ],
)

new_http_archive(
    name = "merkle_tree",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'merkle-tree',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  src_strip_prefix = "src",
  prebuilt_dependencies = [
    "base","bytestring","random", "cryptonite", "memory-0.14.11"
  ],
  compiler_flags = ["-XNoImplicitPrelude", "-XOverloadedStrings"],
  deps = [
    "@cereal//:cereal",
    "@protolude//:protolude",
    #"@cryptonite//:cryptonite"
  ]
)
""",
    strip_prefix = "merkle-tree-0.1.0",
    urls = [
        "https://hackage.haskell.org/package/merkle-tree-0.1.0/merkle-tree-0.1.0.tar.gz"
    ],
)

new_http_archive(
    name = "mime_types",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'mime_types',
  srcs = glob(['Network/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "bytestring","base","containers"
  ],
  deps = [
    "@text//:text"

  ]
)
""",
    strip_prefix = "mime-types-0.1.0.7",
    urls = [
        "https://hackage.haskell.org/package/mime-types-0.1.0.7/mime-types-0.1.0.7.tar.gz"
    ],
)

new_http_archive(
    name = "mmorph",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'mmorph',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "transformers","base","mtl"
  ],
  deps = [
    #"@transformers_compat//:transformers_compat"
  ]
)
""",
    strip_prefix = "mmorph-1.1.0",
    urls = [
        "https://hackage.haskell.org/package/mmorph-1.1.0/mmorph-1.1.0.tar.gz"
    ],
)

new_http_archive(
    name = "monad_control",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'monad_control',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "stm","transformers","base"
  ],
  deps = [
    #"@transformers_compat//:transformers_compat",
    "@transformers_base//:transformers_base"
  ]
)
""",
    strip_prefix = "monad-control-1.0.2.2",
    urls = [
        "https://hackage.haskell.org/package/monad-control-1.0.2.2/monad-control-1.0.2.2.tar.gz"
    ],
)

new_http_archive(
    name = "mtl",
    build_file_content = """

package(default_visibility = ["//visibility:public"])
load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'mtl',
  version = "2.2.1",
  srcs = glob(['Control/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  compiler_flags = ["-XCPP", "-XMultiParamTypeClasses", "-XFunctionalDependencies", "-XFlexibleInstances" ],
  prebuilt_dependencies = [
    "base", "transformers"
  ],
  deps = [
  ]
)
""",
    strip_prefix = "mtl-2.2.1",
    urls = [
        "https://hackage.haskell.org/package/mtl-2.2.1/mtl-2.2.1.tar.gz"
    ],
)

new_http_archive(
    name = "mtl_compat",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'mtl_compat',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base","mtl"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "mtl-compat-0.2.1.3",
    urls = [
        "https://hackage.haskell.org/package/mtl-compat-0.2.1.3/mtl-compat-0.2.1.3.tar.gz"
    ],
)

new_http_archive(
    name = "nats",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'nats',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "nats-1.1.1",
    urls = [
        "https://hackage.haskell.org/package/nats-1.1.1/nats-1.1.1.tar.gz"
    ],
)

new_http_archive(
    name = "network",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'network',
  #srcs = glob(['Network/**/*.hsc', 'Network/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "unix","base","bytestring"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "network-2.6.3.2",
    urls = [
        "https://hackage.haskell.org/package/network-2.6.3.2/network-2.6.3.2.tar.gz"
    ],
)

new_http_archive(
    name = "network_transport",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'network_transport',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "deepseq","bytestring","transformers","base","binary"
  ],
  deps = [
    "@hashable//:hashable"
  ]
)
""",
    strip_prefix = "network-transport-0.5.2",
    urls = [
        "https://hackage.haskell.org/package/network-transport-0.5.2/network-transport-0.5.2.tar.gz"
    ],
)

new_http_archive(
    name = "network_transport_inmemory",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'network_transport_inmemory',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "bytestring","stm","base","containers"
  ],
  deps = [
    "@data_accessor//:data_accessor",
    "@network_transport//:network_transport"
  ]
)
""",
    strip_prefix = "network-transport-inmemory-0.5.2",
    urls = [
        "https://hackage.haskell.org/package/network-transport-inmemory-0.5.2/network-transport-inmemory-0.5.2.tar.gz"
    ],
)

new_http_archive(
    name = "network_transport_tcp",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'network_transport_tcp',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "bytestring","base","containers"
  ],
  deps = [
    "@data_accessor//:data_accessor",
    "@network//:network",
    "@network_transport//:network_transport"
  ]
)
""",
    strip_prefix = "network-transport-tcp-0.6.0",
    urls = [
        "https://hackage.haskell.org/package/network-transport-tcp-0.6.0/network-transport-tcp-0.6.0.tar.gz"
    ],
)

new_http_archive(
    name = "network_uri",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'network_uri',
  srcs = glob(['Network/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "deepseq","base","parsec"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "network-uri-2.6.1.0",
    urls = [
        "https://hackage.haskell.org/package/network-uri-2.6.1.0/network-uri-2.6.1.0.tar.gz"
    ],
)

new_http_archive(
    name = "numtype_dk",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'numtype_dk',
  srcs = glob(['Numeric/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "numtype-dk-0.5.0.1",
    urls = [
        "https://hackage.haskell.org/package/numtype-dk-0.5.0.1/numtype-dk-0.5.0.1.tar.gz"
    ],
)

new_http_archive(
    name = "old_locale",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'old_locale',
  srcs = glob(['System/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "old-locale-1.0.0.7",
    urls = [
        "https://hackage.haskell.org/package/old-locale-1.0.0.7/old-locale-1.0.0.7.tar.gz"
    ],
)

new_http_archive(
    name = "old_time",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'old_time',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base"
  ],
  deps = [
    "@old_locale//:old_locale"
  ]
)
""",
    strip_prefix = "old-time-1.1.0.3",
    urls = [
        "https://hackage.haskell.org/package/old-time-1.1.0.3/old-time-1.1.0.3.tar.gz"
    ],
)

new_http_archive(
    name = "optparse_applicative",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'optparse_applicative',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "transformers","process","base"
  ],
  deps = [
    #"@transformers_compat//:transformers_compat",
    "@ansi_wl_pprint//:ansi_wl_pprint"
  ]
)
""",
    strip_prefix = "optparse-applicative-0.14.0.0",
    urls = [
        "https://hackage.haskell.org/package/optparse-applicative-0.14.0.0/optparse-applicative-0.14.0.0.tar.gz"
    ],
)

new_http_archive(
    name = "parsec",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'parsec',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "bytestring","mtl","text","base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "parsec-3.1.11",
    urls = [
        "https://hackage.haskell.org/package/parsec-3.1.11/parsec-3.1.11.tar.gz"
    ],
)

new_http_archive(
    name = "pedersen_commitment",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'pedersen_commitment',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "bytestring","mtl","text","base","containers"
  ],
  deps = [
    "@protolude//:protolude",
    "@memory//:memory",
    "@cryptonite//:cryptonite"
  ]
)
""",
    strip_prefix = "pedersen-commitment-0.1.0",
    urls = [
        "https://hackage.haskell.org/package/pedersen-commitment-0.1.0/pedersen-commitment-0.1.0.tar.gz"
    ],
)

new_http_archive(
    name = "pem",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'pem',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base","bytestring","mtl"
  ],
  deps = [
    "@base64_bytestring//:base64_bytestring"
  ]
)
""",
    strip_prefix = "pem-0.2.2",
    urls = [
        "https://hackage.haskell.org/package/pem-0.2.2/pem-0.2.2.tar.gz"
    ],
)

new_http_archive(
    name = "postgres_tmp",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'postgres_tmp',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "text","base","bytestring"
  ],
  deps = [
    "@postgresql_simple//:postgresql_simple"
  ]
)
""",
    strip_prefix = "postgres-tmp-0.2.0",
    urls = [
        "https://hackage.haskell.org/package/postgres-tmp-0.2.0/postgres-tmp-0.2.0.tar.gz"
    ],
)

new_http_archive(
    name = "postgresql_libpq",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'postgresql_libpq',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base","bytestring"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "postgresql-libpq-0.9.3.1",
    urls = [
        "https://hackage.haskell.org/package/postgresql-libpq-0.9.3.1/postgresql-libpq-0.9.3.1.tar.gz"
    ],
)

new_http_archive(
    name = "postgresql_simple",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'postgresql_simple',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "vector","template-haskell","text","time","bytestring","transformers","base","containers"
  ],
  deps = [
    "@scientific//:scientific",
    "@hashable//:hashable",
    "@aeson//:aeson",
    "@uuid_types//:uuid_types",
    "@case_insensitive//:case_insensitive",
    #"@postgresql_libpq//:postgresql_libpq",
    "@bytestring_builder//:bytestring_builder",
    "@attoparsec//:attoparsec"
  ]
)
""",
    strip_prefix = "postgresql-simple-0.5.3.0",
    urls = [
        "https://hackage.haskell.org/package/postgresql-simple-0.5.3.0/postgresql-simple-0.5.3.0.tar.gz"
    ],
)

new_http_archive(
    name = "pretty",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'pretty',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "ghc-prim","base","deepseq"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "pretty-1.1.3.3",
    urls = [
        "https://hackage.haskell.org/package/pretty-1.1.3.3/pretty-1.1.3.3.tar.gz"
    ],
)

new_http_archive(
    name = "pretty_hex",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'pretty_hex',
  srcs = glob(['Hexdump.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base","bytestring"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "pretty-hex-1.0",
    urls = [
        "https://hackage.haskell.org/package/pretty-hex-1.0/pretty-hex-1.0.tar.gz"
    ],
)

new_http_archive(
    name = "pretty_show",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'pretty_show',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "filepath","pretty","ghc-prim","base","array"
  ],
  deps = [
    "@haskell_lexer//:haskell_lexer"
  ]
)
""",
    strip_prefix = "pretty-show-1.6.15",
    urls = [
        "https://hackage.haskell.org/package/pretty-show-1.6.15/pretty-show-1.6.15.tar.gz"
    ],
)

new_http_archive(
    name = "primitive",
    build_file_content = """

package(default_visibility = ["//visibility:public"])
load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary", "haskell_cc_import"
)


cc_library(
  name = "cbits",
  srcs = glob(["cbits/*.c"]),
  hdrs = glob(["cbits/*.h"]),
  deps = ["@ghc//:threaded-rts"],
  copts = ["-fomit-frame-pointer"]
)

haskell_library(
  name = 'primitive',
  version = "0.6.2.0",
  srcs = glob(['Control/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  prebuilt_dependencies = [
    "base", "transformers", "ghc-prim"
  ],
  deps = [
    ":cbits",
  ]
)

""",
    strip_prefix = "primitive-0.6.2.0",
    urls = [
        "https://hackage.haskell.org/package/primitive-0.6.2.0/primitive-0.6.2.0.tar.gz"
    ],
)

new_http_archive(
    name = "process",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'process',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "directory","deepseq","filepath","unix","base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "process-1.6.1.0",
    urls = [
        "https://hackage.haskell.org/package/process-1.6.1.0/process-1.6.1.0.tar.gz"
    ],
)

new_http_archive(
    name = "semigroups",
    build_file_content = """
package(default_visibility = ["//visibility:public"])
load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'semigroups',
  version = "2.2.1",
  src_strip_prefix = "src",
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  compiler_flags = ["-XCPP", "-XMultiParamTypeClasses", "-XFunctionalDependencies", "-XFlexibleInstances" ],
  prebuilt_dependencies = [
    "base", "transformers"
  ],
  deps = [
  ]
)
""",
    strip_prefix = "semigroups-0.18.3",
    urls = [
        "https://hackage.haskell.org/package/semigroups-0.18.3/semigroups-0.18.3.tar.gz"
    ],
)
new_http_archive(
    name = "protolude",
    build_file_content = """

package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'protolude',
  src_strip_prefix = "src",
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),

  # c_sources = glob(['cbits/**/*.c']),
  compiler_flags = ["-XMultiParamTypeClasses","-XFlexibleContexts","-XOverloadedStrings","-XNoImplicitPrelude"],
  prebuilt_dependencies = [
    "base","bytestring","array","ghc-prim","deepseq","containers","transformers", "text-1.2.2.2", "hashable", "mtl"
  ],
  deps = [
    "@safe//:safe",
    "@async//:async",
    #"@text//:text",
    #"@mtl//:mtl",
    "@stm//:stm",
    "@semigroups//:semigroups",
    #"@hashable//:hashable"
  ]
)
""",
    strip_prefix = "protolude-0.2.1",
    urls = [
        "https://hackage.haskell.org/package/protolude-0.2.1/protolude-0.2.1.tar.gz"
    ],
)
new_http_archive(
    name = "psqueues",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'psqueues',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "ghc-prim","base","deepseq"
  ],
  deps = [
    "@hashable//:hashable"
  ]
)
""",
    strip_prefix = "psqueues-0.2.4.0",
    urls = [
        "https://hackage.haskell.org/package/psqueues-0.2.4.0/psqueues-0.2.4.0.tar.gz"
    ],
)

new_http_archive(
    name = "random",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'random',
  srcs = glob(['System/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  compiler_flags = ["-XCPP"],
  prebuilt_dependencies = [
    "base","time"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "random-1.1",
    urls = [
        "https://hackage.haskell.org/package/random-1.1/random-1.1.tar.gz"
    ],
)

new_http_archive(
    name = "rank1dynamic",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'rank1dynamic',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base","binary"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "rank1dynamic-0.4.0",
    urls = [
        "https://hackage.haskell.org/package/rank1dynamic-0.4.0/rank1dynamic-0.4.0.tar.gz"
    ],
)

new_http_archive(
    name = "regex_base",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'regex_base',
  srcs = glob(['Text/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "containers","bytestring","mtl","base","array"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "regex-base-0.93.2",
    urls = [
        "https://hackage.haskell.org/package/regex-base-0.93.2/regex-base-0.93.2.tar.gz"
    ],
)

new_http_archive(
    name = "regex_compat",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'regex_compat',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base","array"
  ],
  deps = [
    "@regex_base//:regex_base",
    "@regex_posix//:regex_posix"
  ]
)
""",
    strip_prefix = "regex-compat-0.95.1",
    urls = [
        "https://hackage.haskell.org/package/regex-compat-0.95.1/regex-compat-0.95.1.tar.gz"
    ],
)

new_http_archive(
    name = "regex_posix",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'regex_posix',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "array","bytestring","base","containers"
  ],
  deps = [
    "@regex_base//:regex_base"
  ]
)
""",
    strip_prefix = "regex-posix-0.95.2",
    urls = [
        "https://hackage.haskell.org/package/regex-posix-0.95.2/regex-posix-0.95.2.tar.gz"
    ],
)

new_http_archive(
    name = "repline",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'repline',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "process","mtl","haskeline","base","containers"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "repline-0.1.7.0",
    urls = [
        "https://hackage.haskell.org/package/repline-0.1.7.0/repline-0.1.7.0.tar.gz"
    ],
)

new_http_archive(
    name = "resource_pool",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'resource_pool',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "transformers","vector","stm","base","time"
  ],
  deps = [
    "@hashable//:hashable",
    "@monad_control//:monad_control",
    "@transformers_base//:transformers_base"
  ]
)
""",
    strip_prefix = "resource-pool-0.2.3.2",
    urls = [
        "https://hackage.haskell.org/package/resource-pool-0.2.3.2/resource-pool-0.2.3.2.tar.gz"
    ],
)

new_http_archive(
    name = "resourcet",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'resourcet',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "mtl","transformers","base","containers"
  ],
  deps = [
    "@monad_control//:monad_control",
    "@lifted_base//:lifted_base",
    "@mmorph//:mmorph",
    "@unliftio_core//:unliftio_core",
    "@exceptions//:exceptions",
    #"@transformers_compat//:transformers_compat",
    "@transformers_base//:transformers_base"
  ]
)
""",
    strip_prefix = "resourcet-1.1.10",
    urls = [
        "https://hackage.haskell.org/package/resourcet-1.1.10/resourcet-1.1.10.tar.gz"
    ],
)

new_http_archive(
    name = "rts",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'rts',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "rts-1.0",
    urls = [
        "https://hackage.haskell.org/package/rts-1.0/rts-1.0.tar.gz"
    ],
)

new_http_archive(
    name = "safe",
    build_file_content = """

package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'safe',
  srcs = glob(['Safe/*.hs', 'Safe.hs']),
  prebuilt_dependencies = [
    "base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "safe-0.3.15",
    urls = [
        "https://hackage.haskell.org/package/safe-0.3.15/safe-0.3.15.tar.gz"
    ],
)

new_http_archive(
    name = "scientific",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'scientific',
  src_strip_prefix = "src",
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "integer-gmp","deepseq","binary","bytestring","base","containers"
  ],
  deps = [
    "@integer_logarithms//:integer-logarithms",
    "@hashable//:hashable",
    "@text//:text",
    "@primitive//:primitive"
  ]
)
""",
    strip_prefix = "scientific-0.3.5.2",
    urls = [
        "https://hackage.haskell.org/package/scientific-0.3.5.2/scientific-0.3.5.2.tar.gz"
    ],
)

new_http_archive(
    name = "scotty",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'scotty',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "transformers","bytestring","mtl","text","base"
  ],
  deps = [
    "@monad_control//:monad_control",
    "@aeson//:aeson",
    "@fail//:fail",
    "@http_types//:http_types",
    "@nats//:nats",
    "@network//:network",
    "@case_insensitive//:case_insensitive",
    "@data_default_class//:data_default_class",
    "@wai_extra//:wai_extra",
    "@blaze_builder//:blaze_builder",
    #"@transformers_compat//:transformers_compat",
    "@warp//:warp",
    "@regex_compat//:regex_compat",
    "@wai//:wai",
    "@transformers_base//:transformers_base"
  ]
)
""",
    strip_prefix = "scotty-0.11.0",
    urls = [
        "https://hackage.haskell.org/package/scotty-0.11.0/scotty-0.11.0.tar.gz"
    ],
)

new_http_archive(
    name = "simple_sendfile",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'simple_sendfile',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "unix","base","bytestring"
  ],
  deps = [
    "@network//:network"
  ]
)
""",
    strip_prefix = "simple-sendfile-0.2.26",
    urls = [
        "https://hackage.haskell.org/package/simple-sendfile-0.2.26/simple-sendfile-0.2.26.tar.gz"
    ],
)

new_http_archive(
    name = "stm",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'stm',
  version = "2.4.4.1",
  srcs = glob(['Control/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  #compiler_flags = ["-XCPP", "-XMultiParamTypeClasses", "-XDeriveDataTypeable", "-XFlexibleInstances", "-XMagicHash"],
  prebuilt_dependencies = [
    "base", "transformers", "array"
  ],
  deps = [
  ]
)

""",
    strip_prefix = "stm-2.4.4.1",
    urls = [
        "https://hackage.haskell.org/package/stm-2.4.4.1/stm-2.4.4.1.tar.gz"
    ],
)

new_http_archive(
    name = "streaming_commons",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'streaming_commons',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "transformers","process","text","array","random","directory","bytestring","stm","unix","base"
  ],
  deps = [
    "@zlib//:zlib",
    "@async//:async",
    "@blaze_builder//:blaze_builder",
    "@network//:network"
  ]
)
""",
    strip_prefix = "streaming-commons-0.1.18",
    urls = [
        "https://hackage.haskell.org/package/streaming-commons-0.1.18/streaming-commons-0.1.18.tar.gz"
    ],
)

new_http_archive(
    name = "stringsearch",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'stringsearch',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "containers","bytestring","base","array"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "stringsearch-0.3.6.6",
    urls = [
        "https://hackage.haskell.org/package/stringsearch-0.3.6.6/stringsearch-0.3.6.6.tar.gz"
    ],
)

new_http_archive(
    name = "syb",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'syb',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "syb-0.7",
    urls = [
        "https://hackage.haskell.org/package/syb-0.7/syb-0.7.tar.gz"
    ],
)

new_http_archive(
    name = "tagged",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'tagged',
  src_strip_prefix = "src",
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "deepseq","template-haskell","transformers","base"
  ],
  deps = [
    #"@transformers_compat//:transformers_compat"
  ]
)
""",
    strip_prefix = "tagged-0.8.5",
    urls = [
        "https://hackage.haskell.org/package/tagged-0.8.5/tagged-0.8.5.tar.gz"
    ],
)

new_http_archive(
    name = "template_haskell",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'template_haskell',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "ghc-boot-th","base","pretty"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "template-haskell-2.12.0.0",
    urls = [
        "https://hackage.haskell.org/package/template-haskell-2.12.0.0/template-haskell-2.12.0.0.tar.gz"
    ],
)

new_http_archive(
    name = "terminfo",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'terminfo',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "terminfo-0.4.1.0",
    urls = [
        "https://hackage.haskell.org/package/terminfo-0.4.1.0/terminfo-0.4.1.0.tar.gz"
    ],
)

new_http_archive(
    name = "text",
    build_file_content = """

package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

cc_library(
  name = "cbits",
  srcs = ["cbits/cbits.c"],
  hdrs = ["include/text_cbits.h"],
  deps = ["@ghc//:threaded-rts"],
  copts = ["-Iexternal/text/include"],
)

haskell_library(
  name = 'text',
  srcs = glob(['Control/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  version = "1.2.2.2",
  compiler_flags = ["-DINTEGER_GMP", "-DHAVE_DEEPSEQ"],
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base", "deepseq", "bytestring", "ghc-prim", "binary", "array", "integer-gmp"
  ],
  deps = [
    ":cbits",

  ]
)

""",
    strip_prefix = "text-1.2.2.2",
    urls = [
        "https://hackage.haskell.org/package/text-1.2.2.2/text-1.2.2.2.tar.gz"
    ],
)

new_http_archive(
    name = "th_abstraction",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'th-abstraction',
  src_strip_prefix = "src",
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "template-haskell","ghc-prim","base","containers"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "th-abstraction-0.2.6.0",
    urls = [
        "https://hackage.haskell.org/package/th-abstraction-0.2.6.0/th-abstraction-0.2.6.0.tar.gz"
    ],
)

new_http_archive(
    name = "time",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'time',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "deepseq","base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "time-1.8.0.2",
    urls = [
        "https://hackage.haskell.org/package/time-1.8.0.2/time-1.8.0.2.tar.gz"
    ],
)

new_http_archive(
    name = "time_locale_compat",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'time-locale-compat',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  src_strip_prefix = "src",
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base","time"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "time-locale-compat-0.1.1.3",
    urls = [
        "https://hackage.haskell.org/package/time-locale-compat-0.1.1.3/time-locale-compat-0.1.1.3.tar.gz"
    ],
)

new_http_archive(
    name = "tls",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'tls',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "bytestring","mtl","transformers","base"
  ],
  deps = [
    "@asn1_encoding//:asn1_encoding",
    "@x509_store//:x509_store",
    "@cereal//:cereal",
    "@memory//:memory",
    "@network//:network",
    "@asn1_types//:asn1_types",
    "@data_default_class//:data_default_class",
    "@cryptonite//:cryptonite",
    "@x509//:x509",
    "@x509_validation//:x509_validation",
    "@async//:async"
  ]
)
""",
    strip_prefix = "tls-1.4.0",
    urls = [
        "https://hackage.haskell.org/package/tls-1.4.0/tls-1.4.0.tar.gz"
    ],
)

new_http_archive(
    name = "tls_session_manager",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'tls_session_manager',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base"
  ],
  deps = [
    "@tls//:tls",
    "@clock//:clock",
    "@auto_update//:auto_update",
    "@psqueues//:psqueues"
  ]
)
""",
    strip_prefix = "tls-session-manager-0.0.0.2",
    urls = [
        "https://hackage.haskell.org/package/tls-session-manager-0.0.0.2/tls-session-manager-0.0.0.2.tar.gz"
    ],
)

new_http_archive(
    name = "transformers",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'transformers',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "transformers-0.5.2.0",
    urls = [
        "https://hackage.haskell.org/package/transformers-0.5.2.0/transformers-0.5.2.0.tar.gz"
    ],
)

new_http_archive(
    name = "transformers_base",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'transformers_base',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "transformers","stm","base"
  ],
  deps = [
    #"@transformers_compat//:transformers_compat"
  ]
)
""",
    strip_prefix = "transformers-base-0.4.4",
    urls = [
        "https://hackage.haskell.org/package/transformers-base-0.4.4/transformers-base-0.4.4.tar.gz"
    ],
)

new_http_archive(
    name = "transformers_compat",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'transformers_compat',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "transformers","ghc-prim","base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "transformers-compat-0.5.1.4",
    urls = [
        "https://hackage.haskell.org/package/transformers-compat-0.5.1.4/transformers-compat-0.5.1.4.tar.gz"
    ],
)

new_http_archive(
    name = "unix",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'unix',
  srcs = glob(['System/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "time","base","bytestring"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "unix-2.7.2.2",
    urls = [
        "https://hackage.haskell.org/package/unix-2.7.2.2/unix-2.7.2.2.tar.gz"
    ],
)

new_http_archive(
    name = "unix_compat",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'unix-compat',
  src_strip_prefix = "src",
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base", "time", "unix", "unix-compat"
  ],
  deps = [
  ]
)
""",
    strip_prefix = "unix-compat-0.5.0.1",
    urls = [
        "https://hackage.haskell.org/package/unix-compat-0.5.0.1/unix-compat-0.5.0.1.tar.gz"
    ],
)

new_http_archive(
    name = "unix_time",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'unix_time',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "binary","base","bytestring"
  ],
  deps = [
    "@old_time//:old_time"
  ]
)
""",
    strip_prefix = "unix-time-0.3.7",
    urls = [
        "https://hackage.haskell.org/package/unix-time-0.3.7/unix-time-0.3.7.tar.gz"
    ],
)

new_http_archive(
    name = "unliftio_core",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'unliftio_core',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "transformers","base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "unliftio-core-0.1.0.0",
    urls = [
        "https://hackage.haskell.org/package/unliftio-core-0.1.0.0/unliftio-core-0.1.0.0.tar.gz"
    ],
)

new_http_archive(
    name = "unordered_containers",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'unordered-containers',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "deepseq","base"
  ],
  deps = [
    "@hashable//:hashable"
  ]
)
""",
    strip_prefix = "unordered-containers-0.2.8.0",
    urls = [
        "https://hackage.haskell.org/package/unordered-containers-0.2.8.0/unordered-containers-0.2.8.0.tar.gz"
    ],
)

#new_http_archive(
    #name = "uplink",
    #build_file_content = """
#package(default_visibility = ["//visibility:public"])

#load(
  #"@io_tweag_rules_haskell//haskell:haskell.bzl",
  #"haskell_library","haskell_binary"
#)

#haskell_library(
  #name = 'uplink',
  #srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  ## c_sources = glob(['cbits/**/*.c']),
  #prebuilt_dependencies = [
    #"process","deepseq","text","haskeline","binary","random","stm","bytestring","unix","parsec","array","time","directory","filepath","mtl","transformers","base","containers"
  #],
  #deps = [
    #"@hashable//:hashable",
    #"@repline//:repline",
    #"@http_client//:http_client",
    #"@aeson//:aeson",
    #"@fgl//:fgl",
    #"@gitrev//:gitrev",
    #"@unordered_containers//:unordered_containers",
    #"@base58_bytestring//:base58_bytestring",
    #"@merkle_tree//:merkle_tree",
    #"@filelock//:filelock",
    #"@hexpat_pickle//:hexpat_pickle",
    #"@hourglass//:hourglass",
    #"@scotty//:scotty",
    #"@arithmoi//:arithmoi",
    #"@http_types//:http_types",
    #"@cereal//:cereal",
    #"@memory//:memory",
    #"@network//:network",
    #"@asn1_types//:asn1_types",
    #"@exceptions//:exceptions",
    #"@network_uri//:network_uri",
    #"@ansi_terminal//:ansi_terminal",
    #"@async//:async",
    #"@pem//:pem",
    #"@wai_logger//:wai_logger",
    #"@asn1_encoding//:asn1_encoding",
    #"@dlist//:dlist",
    #"@file_embed//:file_embed",
    #"@leveldb_haskell//:leveldb_haskell",
    #"@monad_control//:monad_control",
    #"@scientific//:scientific",
    #"@pretty_show//:pretty_show",
    #"@aeson_pretty//:aeson_pretty",
    #"@network_transport_inmemory//:network_transport_inmemory",
    #"@distributed_process_lifted//:distributed_process_lifted",
    #"@warp_tls//:warp_tls",
    #"@cryptonite//:cryptonite",
    #"@hslogger//:hslogger",
    #"@hexpat//:hexpat",
    #"@distributed_process//:distributed_process",
    #"@resource_pool//:resource_pool",
    #"@pretty_hex//:pretty_hex",
    #"@network_transport//:network_transport",
    #"@postgresql_simple//:postgresql_simple",
    #"@transformers_base//:transformers_base",
    #"@network_transport_tcp//:network_transport_tcp",
    #"@protolude//:protolude",
    #"@pedersen_commitment//:pedersen_commitment",
    #"@base64_bytestring//:base64_bytestring",
    #"@configurator//:configurator",
    #"@datetimes//:datetimes",
    #"@wl_pprint_text//:wl_pprint_text",
    #"@safe//:safe",
    #"@x509//:x509",
    #"@lifted_base//:lifted_base",
    #"@warp//:warp",
    #"@wai//:wai",
    #"@postgres_tmp//:postgres_tmp"
  #]
#)
#""",
    #strip_prefix = "uplink-0.1.2",
    #urls = [
        #"https://hackage.haskell.org/package/uplink-0.1.2/uplink-0.1.2.tar.gz"
    #],
#)

new_http_archive(
    name = "utf8_string",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'utf8_string',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base","bytestring"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "utf8-string-1.0.1.1",
    urls = [
        "https://hackage.haskell.org/package/utf8-string-1.0.1.1/utf8-string-1.0.1.1.tar.gz"
    ],
)

new_http_archive(
    name = "uuid_types",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'uuid-types',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "deepseq","bytestring","base","binary"
  ],
  deps = [
    "@hashable//:hashable",
    "@text//:text",
    "@random//:random"
  ]
)
""",
    strip_prefix = "uuid-types-1.0.3",
    urls = [
        "https://hackage.haskell.org/package/uuid-types-1.0.3/uuid-types-1.0.3.tar.gz"
    ],
)

new_http_archive(
    name = "vault",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'vault',
  src_strip_prefix ="src",
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base","containers"
  ],
  compiler_flags = ["-DUseGHC", "-XCPP", "-DLAZINESS=Lazy"],
  deps = [
    "@unordered_containers//:unordered-containers",
    "@hashable//:hashable"
  ]
)
""",
    strip_prefix = "vault-0.3.0.7",
    urls = [
        "https://hackage.haskell.org/package/vault-0.3.0.7/vault-0.3.0.7.tar.gz"
    ],
)

new_http_archive(
    name = "vector",
    build_file_content = """


package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

cc_library(
  name = "cbits",
  hdrs = glob(["include/*.h", "internal/*"]),
  #data = ["internal/unbox-tuple-instances"],
  #copts = ["-Iexternal/text/include"],
)


haskell_library(
  name = 'vector',
  version = "0.12.0.1",
  srcs = glob(['Control/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  #compiler_flags = ["-XCPP", "-XMultiParamTypeClasses", "-XFunctionalDependencies", "-XFlexibleInstances", "-Wall -fno-warn-unused-imports -fno-warn-warnings-deprecations -Wcompat -Wnoncanonical-monad-instances -Wnoncanonical-monadfail-instances"],
  prebuilt_dependencies = [
    "base", "deepseq", "ghc-prim"
  ],
  deps = [
    ":cbits",
    "@primitive//:primitive",
    "@semigroups//:semigroups"
  ]

)

""",
    strip_prefix = "vector-0.12.0.1",
    urls = [
        "https://hackage.haskell.org/package/vector-0.12.0.1/vector-0.12.0.1.tar.gz"
    ],
)

new_http_archive(
    name = "void",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'void',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "void-0.7.2",
    urls = [
        "https://hackage.haskell.org/package/void-0.7.2/void-0.7.2.tar.gz"
    ],
)

new_http_archive(
    name = "wai",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'wai',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "bytestring","transformers","base"
  ],
  deps = [
    "@http_types//:http_types",
    "@network//:network",
    "@bytestring_builder//:bytestring_builder",
    "@blaze_builder//:blaze_builder",
    "@text//:text",
    #"@vault//:vault"

  ]
)
""",
    strip_prefix = "wai-3.2.1.1",
    urls = [
        "https://hackage.haskell.org/package/wai-3.2.1.1/wai-3.2.1.1.tar.gz"
    ],
)

new_http_archive(
    name = "wai_extra",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)



haskell_library(
  name = 'wai_extra',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "deepseq","time","directory","bytestring","transformers","unix","base","containers"
  ],
  deps = [
    ":cbits",
    "@cookie//:cookie",
    "@aeson//:aeson",
    "@text//:text",
    "@case_insensitive//:case_insensitive",
    "@http_types//:http_types",
    "@resourcet//:resourcet",
    "@network//:network",
    "@zlib//:zlib",
    "@void//:void",
    "@vault//:vault",
    "@ansi_terminal//:ansi_terminal",
    "@wai_logger//:wai_logger",
    "@iproute//:iproute",
    "@blaze_builder//:blaze_builder",
    "@streaming_commons//:streaming_commons",
    "@base64_bytestring//:base64_bytestring",
    "@unix_compat//:unix_compat",
    "@data_default_class//:data_default_class",
    "@stringsearch//:stringsearch",
    "@lifted_base//:lifted_base",
    "@word8//:word8",
    "@fast_logger//:fast_logger",
    "@wai//:wai",
    "@old_locale//:old_locale"
  ]
)
""",
    strip_prefix = "wai-extra-3.0.20.2",
    urls = [
        "https://hackage.haskell.org/package/wai-extra-3.0.20.2/wai-extra-3.0.20.2.tar.gz"
    ],
)

new_http_archive(
    name = "wai_logger",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'wai_logger',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "unix","base","bytestring"
  ],
  deps = [
    "@unix_time//:unix_time",
    "@http_types//:http_types",
    "@byteorder//:byteorder",
    "@network//:network",
    "@case_insensitive//:case_insensitive",
    "@blaze_builder//:blaze_builder",
    "@fast_logger//:fast_logger",
    "@wai//:wai"
  ]
)
""",
    strip_prefix = "wai-logger-2.3.0",
    urls = [
        "https://hackage.haskell.org/package/wai-logger-2.3.0/wai-logger-2.3.0.tar.gz"
    ],
)

new_http_archive(
    name = "warp",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'warp',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "text","array","bytestring","ghc-prim","stm","unix","base","containers", "vault-0.3.0.7"
  ],
  deps = [
    "@wai//:wai",
    "@hashable//:hashable",
    "@blaze_builder//:blaze_builder",
    "@iproute//:iproute",
    "@http_types//:http_types",
    "@simple_sendfile//:simple_sendfile",
    "@streaming_commons//:streaming_commons",
    "@network//:network",
    "@http2//:http2",
    "@case_insensitive//:case_insensitive",
    #"@vault//:vault",
    "@unix_compat//:unix_compat",
    "@async//:async",
    "@bytestring_builder//:bytestring_builder",
    "@word8//:word8",
    "@http_date//:http_date",
    "@auto_update//:auto_update"
  ]
)
""",
    strip_prefix = "warp-3.2.13",
    urls = [
        "https://hackage.haskell.org/package/warp-3.2.13/warp-3.2.13.tar.gz"
    ],
)

new_http_archive(
    name = "warp_tls",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'warp_tls',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base","bytestring"
  ],
  deps = [
    "@tls_session_manager//:tls_session_manager",
    "@tls//:tls",
    "@network//:network",
    "@data_default_class//:data_default_class",
    "@cryptonite//:cryptonite",
    "@warp//:warp",
    "@wai//:wai",
    "@streaming_commons//:streaming_commons"
  ]
)
""",
    strip_prefix = "warp-tls-3.2.4",
    urls = [
        "https://hackage.haskell.org/package/warp-tls-3.2.4/warp-tls-3.2.4.tar.gz"
    ],
)

new_http_archive(
    name = "wl_pprint_text",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'wl_pprint_text',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base"
  ],
  deps = [
    #"@base_compat//:base_compat"
    "@text//:text",
  ]
)
""",
    strip_prefix = "wl-pprint-text-1.1.1.0",
    urls = [
        "https://hackage.haskell.org/package/wl-pprint-text-1.1.1.0/wl-pprint-text-1.1.1.0.tar.gz"
    ],
)

new_http_archive(
    name = "word8",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'word8',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "word8-0.1.3",
    urls = [
        "https://hackage.haskell.org/package/word8-0.1.3/word8-0.1.3.tar.gz"
    ],
)

new_http_archive(
    name = "x509",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'x509',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "bytestring","mtl","base","containers"
  ],
  deps = [
    "@asn1_encoding//:asn1_encoding",
    "@memory//:memory",
    "@asn1_types//:asn1_types",
    "@cryptonite//:cryptonite",
    "@hourglass//:hourglass",
    "@asn1_parse//:asn1_parse",
    "@pem//:pem"
  ]
)
""",
    strip_prefix = "x509-1.7.2",
    urls = [
        "https://hackage.haskell.org/package/x509-1.7.2/x509-1.7.2.tar.gz"
    ],
)

new_http_archive(
    name = "x509_store",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'x509_store',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "directory","filepath","bytestring","mtl","base","containers"
  ],
  deps = [
    "@asn1_encoding//:asn1_encoding",
    "@asn1_types//:asn1_types",
    "@x509//:x509",
    "@cryptonite//:cryptonite",
    "@pem//:pem"
  ]
)
""",
    strip_prefix = "x509-store-1.6.5",
    urls = [
        "https://hackage.haskell.org/package/x509-store-1.6.5/x509-store-1.6.5.tar.gz"
    ],
)

new_http_archive(
    name = "x509_validation",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'x509_validation',
  srcs = glob(['src/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "bytestring","mtl","base","containers"
  ],
  deps = [
    "@asn1_encoding//:asn1_encoding",
    "@x509_store//:x509_store",
    "@memory//:memory",
    "@asn1_types//:asn1_types",
    "@byteable//:byteable",
    "@data_default_class//:data_default_class",
    "@x509//:x509",
    "@hourglass//:hourglass",
    "@cryptonite//:cryptonite",
    "@pem//:pem"
  ]
)
""",
    strip_prefix = "x509-validation-1.6.9",
    urls = [
        "https://hackage.haskell.org/package/x509-validation-1.6.9/x509-validation-1.6.9.tar.gz"
    ],
)

new_http_archive(
    name = "zlib",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library","haskell_binary"
)

haskell_library(
  name = 'zlib',
  srcs = glob(['Codec/**/*.hs', 'lib/**/*.hs' ,'Data/**/*.hs']),
  # c_sources = glob(['cbits/**/*.c']),
  prebuilt_dependencies = [
    "base","bytestring"
  ],
  deps = [

  ]
)
""",
    strip_prefix = "zlib-0.6.1.2",
    urls = [
        "https://hackage.haskell.org/package/zlib-0.6.1.2/zlib-0.6.1.2.tar.gz"
    ],
)
