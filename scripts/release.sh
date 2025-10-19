#!/bin/bash
set -e

# Get version from cabal file and create release tag
VERSION=$(grep "^version:" blogroll.cabal | awk '{print $2}')

git tag "v$VERSION"
git push origin "v$VERSION"

echo "Released v$VERSION"