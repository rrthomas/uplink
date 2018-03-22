version = 1.2

.PHONY: default

default: deb rpm pacman config
.PHONY: dist

dist:
	rm -rf root/
	mkdir -p root/usr/local/bin
	mkdir -p root/etc/uplink/
	cp ../uplink root/usr/local/bin

clean:
	rm -f *.deb *.rpm *.xz *.pkg



config:
	mkdir config
	mkdir config/validators
	cp -p ../config/node.config.local config/node.config
	cp -p ../config/chain.config.local config/chain.config
	cp -rp ../config/validators config/
	zip -r config-$(version).zip config/

deb: dist
	fpm --verbose --vendor "Adjoint Inc." --license "APACHE2" -s dir -t deb -n "uplink" -d libleveldb-dev -d libexpat1 -d libgmp-dev -v 1.2 -C root .

rpm: dist
	fpm --verbose  --vendor "Adjoint Inc." --license "APACHE2" -s dir -t rpm -n "uplink" -d leveldb -d expat -d gmp -v 1.2 -C root  .

pacman: dist
	fpm --verbose  --vendor "Adjoint Inc." --license "APACHE2" -s dir -t pacman -n "uplink" -d leveldb -d expat -d gmp -v 1.2  -C root .

osx: dist
	fpm --verbose -s dir -t osxpkg -n "uplink"  -v $(version)  -C root .
