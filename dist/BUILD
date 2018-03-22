package(default_visibility = ["//visibility:public"])

load("@bazel_tools//tools/build_defs/pkg:pkg.bzl", "pkg_tar", "pkg_deb")
load("@bazel_tools//tools/build_defs/pkg:rpm.bzl", "pkg_rpm")


pkg_tar(
    name = "uplink-bin",
    package_dir = "/usr/bin/",
    srcs = ["//:uplink"],
    mode = "0755",
    extension = "tar.gz",
)

pkg_deb(
    name = "uplink-debian",
    architecture = "amd64",
    data = ":uplink-bin",
    depends = [
        "libleveldb-dev",
        "libexpat1",
        "libgmp-dev",
    ],
    description = "debian/description",
    #homepage = "http://bazel.build",
    maintainer = "",
    package = "uplink",
    version = "0.1.2",
)

pkg_rpm(
    name = "uplink-rpm",
    spec_file = "uplink-rpm.spec",
    version = "0.1.2",
    data = ["//:uplink"],
    changelog = "//:CHANGELOG.md"
)



#container_push(
   #name = "push_foo",
   #image = ":foo",
   #format = "Docker",
   #registry = "gcr.io",
   #repository = "my-project/my-image",
   #tag = "dev",
#)


genrule(
    name = "checksum-bin",
    srcs = [":uplink-bin"],
    outs = ["uplink-bin.tar.gz.checksum"],
    cmd = "sha256sum $(location :uplink-bin) > \"$@\"",
)

genrule(
    name = "checksum-config",
    srcs = ["//:uplink-config"],
    outs = ["uplink-config.zip.checksum"],
    cmd = "sha256sum $(location //:uplink-config) > \"$@\"",
)

genrule(
    name = "checksum-debian",
    srcs = [":uplink-debian"],
    outs = ["uplink-debian.deb.checksum"],
    cmd = "sha256sum $(locations :uplink-debian) > \"$@\"",
)

genrule(
    name = "checksum-rpm",
    srcs = [":uplink-rpm"],
    outs = ["uplink-rpm.rpm.checksum"],
    cmd = "sha256sum $(locations :uplink-rpm) > \"$@\"",
)

filegroup(
  name = "artifacts",
  srcs = [
      "@uplink_explorer//:uplink-explorer-zip",
      "@uplink_sdk_python//:uplink-sdk-python-zip",
      "@uplink_sdk_java//:uplink-sdk-java-zip",
      "//dist:uplink-bin",
      "//dist:uplink-debian",
      "//dist:uplink-rpm",
      "//dist:checksum-debian",
      "//dist:checksum-rpm",
      "//:uplink-config",
      "//dist:checksum-config"
  ],
)



