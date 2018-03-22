Summary: uplink
Name: uplink
Version: 1.2.0
License: APACHE2
Requires: leveldb, expat, gmp
Release: 1
%description
%install
install -m 755 -d %{buildroot}%{_bindir}
install -p -m 755 -t %{buildroot}%{_bindir} uplink

%files
%{_bindir}/uplink
