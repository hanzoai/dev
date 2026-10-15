#!/usr/bin/env bash
# Fetch the V8 build upstream's JavaScript runtime links, for `make test-upstream`.
#
# Upstream publishes the archive beside a manifest of SHA-256 digests, and the
# digest of that manifest is pinned in the checkout, so every byte is checked
# against the gitlink and nothing trusts the network. Writes target/v8-env.sh.
#
# usage: scripts/v8.sh [TARGET]   (default: the host's Rust target)
set -euo pipefail

root=$(cd "$(dirname "$0")/.." && pwd)
codex=$root/upstream/codex
target=${1:-$(rustc -vV | sed -n 's/^host: //p')}
case "$target" in *[!a-z0-9_-]*|"") echo "invalid Rust target: $target" >&2; exit 1 ;; esac

version=$(sed -n 's/^v8 = "=\(.*\)"$/\1/p' "$codex/codex-rs/Cargo.toml")
[ -n "$version" ] || { echo "no exact v8 pin in upstream's workspace manifest" >&2; exit 1; }
trusted=$codex/third_party/v8/rusty_v8_${version//./_}_release_manifests.sha256
[ -f "$trusted" ] || { echo "$trusted: upstream does not publish V8 $version" >&2; exit 1; }

profile=ptrcomp_sandbox_release
checksums=rusty_v8_${profile}_${target}.sha256
case "$target" in
  *windows-msvc) archive=rusty_v8_${profile}_${target}.lib.gz ;;
  *) archive=librusty_v8_${profile}_${target}.a.gz ;;
esac
binding=src_binding_${profile}_${target}.rs
base=https://github.com/openai/codex/releases/download/rusty-v8-v$version
dir=$root/target/v8/$target
mkdir -p "$dir"

# fetch NAME DIGEST: download NAME into $dir unless it is there with DIGEST.
fetch() {
  local path=$dir/$1
  if [ -f "$path" ] && [ "$(sha256sum "$path" | cut -d' ' -f1)" = "$2" ]; then return; fi
  curl -fsSL --retry 4 --connect-timeout 20 "$base/$1" -o "$path.part"
  [ "$(sha256sum "$path.part" | cut -d' ' -f1)" = "$2" ] || { echo "checksum mismatch: $1" >&2; rm -f "$path.part"; exit 1; }
  mv "$path.part" "$path"
}
digest() { tr -d '\r' < "$1" | awk -v n="$2" '$2 == n { print $1 }'; }

want=$(digest "$trusted" "$checksums")
[ -n "$want" ] || { echo "no upstream V8 checksum for $target" >&2; exit 1; }
fetch "$checksums" "$want"
[ "$(tr -d '\r' < "$dir/$checksums" | grep -c .)" -eq 2 ] || { echo "expected exactly the archive and binding checksums" >&2; exit 1; }
for name in "$archive" "$binding"; do
  sum=$(digest "$dir/$checksums" "$name")
  [ -n "$sum" ] || { echo "$checksums names no $name" >&2; exit 1; }
  fetch "$name" "$sum"
done

printf 'export RUSTY_V8_ARCHIVE=%s\nexport RUSTY_V8_SRC_BINDING_PATH=%s\n' "$dir/$archive" "$dir/$binding" > "$root/target/v8-env.sh"
echo "verified upstream V8 $version for $target"
