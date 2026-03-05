#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SPEC_FILE="$ROOT_DIR/packaging/rpm/nltc-acrt.spec"
VERSION="$(awk -F'"' '/__version__/ {print $2; exit}' "$ROOT_DIR/src/acrt_pkg/__init__.py")"
PKG_NAME="nltc-acrt"
TARBALL_NAME="$PKG_NAME-$VERSION.tar.gz"
RPM_TOPDIR="${RPM_TOPDIR:-$ROOT_DIR/.rpmbuild}"

mkdir -p "$RPM_TOPDIR"/{BUILD,BUILDROOT,RPMS,SOURCES,SPECS,SRPMS}

if ! git -C "$ROOT_DIR" diff --quiet || ! git -C "$ROOT_DIR" diff --cached --quiet; then
  echo "Working tree is not clean. Commit or stash changes before building RPM."
  exit 1
fi

# Create clean source tarball from HEAD so RPM builds are reproducible.
git -C "$ROOT_DIR" archive --format=tar.gz --prefix="$PKG_NAME-$VERSION/" -o "$RPM_TOPDIR/SOURCES/$TARBALL_NAME" HEAD

# Keep spec version in sync with Python package version.
sed -E "s/^Version:.*/Version:        $VERSION/" "$SPEC_FILE" > "$RPM_TOPDIR/SPECS/$PKG_NAME.spec"

rpmbuild \
  --define "_topdir $RPM_TOPDIR" \
  -ba "$RPM_TOPDIR/SPECS/$PKG_NAME.spec"

echo "RPMs built under: $RPM_TOPDIR/RPMS"
