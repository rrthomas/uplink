set -e
bazel build //dist:artifacts

mkdir -p release/$1

cp bazel-bin/dist/uplink-debian.deb release/$1/uplink-$1.deb
cp bazel-genfiles/dist/uplink-debian.deb.checksum release/$1/uplink-$1.deb.checksum


cp bazel-genfiles/uplink-config.zip release/$1/uplink-config-$1.zip
cp bazel-genfiles/dist/uplink-config.zip.checksum release/$1/uplink-config-$1.zip.checksum

cp bazel-bin/dist/uplink-rpm.rpm release/$1/uplink_$1.rpm
cp bazel-genfiles/dist/uplink-rpm.rpm.checksum release/$1/uplink-$1.rpm.checksum


cp bazel-genfiles/external/uplink_explorer/uplink-explorer.zip release/$1/uplink-explorer-$1.zip
cp bazel-genfiles/external/uplink_sdk_python/uplink-sdk-python.zip release/$1/uplink-sdk-python-$1.zip
cp bazel-genfiles/external/uplink_sdk_java/uplink-sdk-java.zip release/$1/uplink-sdk-java-$1.zip

