#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SPEC_FILE="$ROOT_DIR/packaging/rpm/nltc-acrt.spec"
VERSION="$(awk -F'"' '/__version__/ {print $2; exit}' "$ROOT_DIR/src/acrt_pkg/__init__.py")"
PKG_NAME="nltc-acrt"
TARBALL_NAME="$PKG_NAME-$VERSION.tar.gz"
RPM_TOPDIR="${RPM_TOPDIR:-$ROOT_DIR/.rpmbuild}"
RPMBUILD_OPTS=()

mkdir -p "$RPM_TOPDIR"/{BUILD,BUILDROOT,RPMS,SOURCES,SPECS,SRPMS}

if git -C "$ROOT_DIR" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  if ! git -C "$ROOT_DIR" diff --quiet || ! git -C "$ROOT_DIR" diff --cached --quiet; then
    echo "Working tree is not clean. Commit or stash changes before building RPM."
    exit 1
  fi

  # Create clean source tarball from HEAD when git metadata is available.
  git -C "$ROOT_DIR" archive --format=tar.gz --prefix="$PKG_NAME-$VERSION/" -o "$RPM_TOPDIR/SOURCES/$TARBALL_NAME" HEAD
else
  echo "No git metadata found. Building source tarball from filesystem snapshot."
  tar \
    -C "$ROOT_DIR" \
    --exclude-vcs \
    --exclude '.rpmbuild' \
    --exclude '__pycache__' \
    --exclude '*.pyc' \
    --transform "s,^\./,$PKG_NAME-$VERSION/," \
    -czf "$RPM_TOPDIR/SOURCES/$TARBALL_NAME" \
    .
fi

# Keep spec version in sync with Python package version.
sed -E "s/^Version:.*/Version:        $VERSION/" "$SPEC_FILE" > "$RPM_TOPDIR/SPECS/$PKG_NAME.spec"

if [[ "${RPMBUILD_NODEPS:-0}" == "1" ]]; then
  RPMBUILD_OPTS+=(--nodeps)
fi

rpmbuild \
  --define "_topdir $RPM_TOPDIR" \
  "${RPMBUILD_OPTS[@]}" \
  -ba "$RPM_TOPDIR/SPECS/$PKG_NAME.spec"

echo "RPMs built under: $RPM_TOPDIR/RPMS"
