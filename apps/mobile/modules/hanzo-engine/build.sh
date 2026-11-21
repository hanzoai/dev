#!/usr/bin/env bash
# Build ios/HanzoEngineFFI.xcframework from hanzoai/engine (hanzo-engine-ffi, Metal).
# usage: build.sh <path to a hanzoai/engine checkout>
set -euo pipefail
ENGINE="${1:?path to a hanzoai/engine checkout}"
HERE="$(cd "$(dirname "$0")" && pwd)"
TARGETS="${CARGO_TARGET_DIR:-$ENGINE/target}"
for t in aarch64-apple-ios aarch64-apple-ios-sim; do
  sdk=iphoneos; [ "$t" = aarch64-apple-ios-sim ] && sdk=iphonesimulator
  (cd "$ENGINE" && SDKROOT="$(xcrun --sdk $sdk --show-sdk-path)" \
    cargo build --release -p hanzo-engine-ffi --features metal --target "$t")
done
HEADERS="$(mktemp -d)"
trap 'rm -rf "$HEADERS"' EXIT
cp "$ENGINE/hanzo-engine-ffi/include/hanzo_engine_ffi.h" "$HEADERS/"
printf 'module HanzoEngineFFI {\n  header "hanzo_engine_ffi.h"\n  export *\n}\n' > "$HEADERS/module.modulemap"
rm -rf "$HERE/ios/HanzoEngineFFI.xcframework"
xcodebuild -create-xcframework \
  -library "$TARGETS/aarch64-apple-ios/release/libhanzo_engine_ffi.a" -headers "$HEADERS" \
  -library "$TARGETS/aarch64-apple-ios-sim/release/libhanzo_engine_ffi.a" -headers "$HEADERS" \
  -output "$HERE/ios/HanzoEngineFFI.xcframework"
