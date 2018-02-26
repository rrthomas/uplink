#!/bin/bash

# ASSUMPTIONS: - togo is installed https://github.com/genereese/togo
#              - uplink has been build
#              - key for signing was imported with gpg2

NAME=uplink

echo '%_signature     gpg2
%_gpg_path      ~/.gnupg
%_gpg_name      Stephen Diehl <stephen@adjoint.io>
%_gpgbin        /usr/bin/gpg2' > ~/.rpmmacros

togo configure -n "Stephen Diehl" -e "stephen@adjoint.io"
cd $NAME
mkdir -p root/usr/local/bin
mkdir -p root/etc/init

cp -R ../../etc root/etc/
cp ../../etc/node.config root/etc/
cp -R ../../../../.stack-work/install/x86_64-linux/lts-7.16/8.0.1/bin/uplink root/usr/local/bin

togo file exclude root/usr/local/bin
togo file exclude root/etc

togo build package

echo "SHA Hashes:"
sha256sum rpms/uplink-1.0-1.x86_64.rpm

echo "Signature:"
gpg --armor --detach-sig rpms/uplink-1.0-1.x86_64.rpm
rpm --addsign rpms/uplink-1.0-1.x86_64.rpm

cd ..
mv $NAME/rpms/* .
rm -rf $NAME/root/*
rm -rf $NAME/rpms/*
