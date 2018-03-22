
cat > tools/settings.bzl <<EOF
version = "1.2.0"
ghc_pkg_dbs = [
  "`stack path --ghc-package-path`"
]
ghc_path = "`stack path --compiler-bin`/../"
EOF

