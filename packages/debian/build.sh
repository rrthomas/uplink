#!/usr/bin/env bash

cp ../../config/node.config uplink/etc/node.config
cp -R ../../.stack-work/image/0/usr uplink/

dpkg-deb -Zgzip --build uplink
alien --to-rpm uplink.deb
alien -t uplink.deb

echo "SHA Hashes:"
sha256sum uplink.deb

echo "Signature:"
gpg --yes --armor --detach-sig uplink.deb
gpg --yes --armor --detach-sig uplink-1.0-2.x86_64.rpm
