#!/usr/bin/env bash
set -euo pipefail

GETTEXT_DIR="$(brew --prefix gettext)"
LIBPNG_DIR="$(brew --prefix libpng)"

mkdir -p ~/.R
cat > ~/.R/Makevars << EOF
LDFLAGS=-L${GETTEXT_DIR}/lib -L${LIBPNG_DIR}/lib
CPPFLAGS=-I${GETTEXT_DIR}/include -I${LIBPNG_DIR}/include
EOF

cat ~/.R/Makevars
