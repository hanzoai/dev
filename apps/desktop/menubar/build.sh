#!/bin/sh
# Builds HanzoMenubar.app into the directory given as $1 (default .build/app).
set -eu
here=$(cd "$(dirname "$0")" && pwd)
out=${1:-$here/.build/app}
app="$out/HanzoMenubar.app"
cd "$here"
archs=${HANZO_MENUBAR_ARCHS:-"--arch arm64 --arch x86_64"}
swift build -c release $archs
rm -rf "$app"
mkdir -p "$app/Contents/MacOS" "$app/Contents/Resources"
cp "$(swift build -c release $archs --show-bin-path)/HanzoMenubar" "$app/Contents/MacOS/"
cp Fonts/*.ttf "$app/Contents/Resources/"
cp Info.plist "$app/Contents/"
codesign --force --sign "${HANZO_SIGN_IDENTITY:--}" "$app"
echo "$app"
