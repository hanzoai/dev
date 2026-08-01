#!/usr/bin/env bash
set -euo pipefail

# Generate the Homebrew formula from the latest GitHub release.
# Writes Formula/hanzo-dev.rb into the repo root (not a tap); release.yml copies
# it into hanzoai/homebrew-tap as Formula/hanzo-dev.rb.

owner_repo="hanzoai/dev"
version="${1:-}"
if [ -z "$version" ] && [ -f "code-rs/Cargo.toml" ]; then
  version="$(awk -F '"' '/^\[workspace.package\]/{f=1; next} f && $1 ~ /version/ {print $2; exit}' code-rs/Cargo.toml)"
fi
if [ -z "$version" ] && [ -f "codex-cli/package.json" ]; then
  version="$(jq -r .version codex-cli/package.json)"
fi
if [ -z "$version" ]; then
  echo "Unable to infer release version; pass it as \$1 or ensure code-rs/Cargo.toml or codex-cli/package.json are available." >&2
  exit 1
fi

# Optional directory where CI placed artifacts (step: Prepare release assets)
RELEASE_ASSETS_DIR=${RELEASE_ASSETS_DIR:-"release-assets"}

# Every platform the tap formula serves. Dropping one silently narrows the
# formula, so all four are required to produce a publishable file.
assets=(
  "dev-aarch64-apple-darwin.tar.gz"
  "dev-x86_64-apple-darwin.tar.gz"
  "dev-aarch64-unknown-linux-musl.tar.gz"
  "dev-x86_64-unknown-linux-musl.tar.gz"
)

placeholder_for() {
  case "$1" in
    dev-aarch64-apple-darwin.tar.gz)       echo "__SHA_MACOS_ARM__" ;;
    dev-x86_64-apple-darwin.tar.gz)        echo "__SHA_MACOS_INTEL__" ;;
    dev-aarch64-unknown-linux-musl.tar.gz) echo "__SHA_LINUX_ARM__" ;;
    dev-x86_64-unknown-linux-musl.tar.gz)  echo "__SHA_LINUX_INTEL__" ;;
    *)                                     echo "" ;;
  esac
}

sha256_file() {
  local f="$1"
  if command -v shasum >/dev/null 2>&1; then
    shasum -a 256 "$f" | awk '{print $1}'
  elif command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$f" | awk '{print $1}'
  else
    echo ""; return 1
  fi
}

# Retry a command with backoff: retry <max_tries> <sleep_seconds> <cmd...>
retry() {
  local max="$1"; shift
  local sleep_s="$1"; shift
  local n=1
  while :; do
    if "$@"; then return 0; fi
    if [ "$n" -ge "$max" ]; then return 1; fi
    n=$((n+1))
    sleep "$sleep_s"
  done
}

mkdir -p Formula
formula="Formula/hanzo-dev.rb"

cat > "$formula" <<'RUBY'
class HanzoDev < Formula
  desc "Local AI coding agent for your terminal, powered by Hanzo AI"
  homepage "https://github.com/hanzoai/dev"
  version "__VERSION__"
  license "Apache-2.0"

  on_macos do
    on_arm do
      url "https://github.com/hanzoai/dev/releases/download/v#{version}/dev-aarch64-apple-darwin.tar.gz"
      sha256 "__SHA_MACOS_ARM__"
    end
    on_intel do
      url "https://github.com/hanzoai/dev/releases/download/v#{version}/dev-x86_64-apple-darwin.tar.gz"
      sha256 "__SHA_MACOS_INTEL__"
    end
  end

  on_linux do
    on_arm do
      url "https://github.com/hanzoai/dev/releases/download/v#{version}/dev-aarch64-unknown-linux-musl.tar.gz"
      sha256 "__SHA_LINUX_ARM__"
    end
    on_intel do
      url "https://github.com/hanzoai/dev/releases/download/v#{version}/dev-x86_64-unknown-linux-musl.tar.gz"
      sha256 "__SHA_LINUX_INTEL__"
    end
  end

  livecheck do
    url :stable
    strategy :github_latest
  end

  def install
    # Installs `dev` ONLY. It must not also write a `hanzo` shim: `hanzo` is the
    # Hanzo CLI (Formula/hanzo.rb), a different program. A shim here would make
    # `hanzo` mean "the dev coding assistant" for anyone who installed this
    # formula first, and would collide with hanzo.rb on the same filename.
    bin.install Dir["dev-*"].first => "dev"
  end

  test do
    system "#{bin}/dev", "--help"
  end
end
RUBY

sed -i.bak "s#__VERSION__#${version}#" "$formula"

for a in "${assets[@]}"; do
  url="https://github.com/${owner_repo}/releases/download/v${version}/${a}"
  tmp="${TMPDIR:-/tmp}/${a}"
  sha=""

  # Prefer local artifact if available to avoid CDN propagation races
  local_path="${RELEASE_ASSETS_DIR}/${a}"
  if [ -f "$local_path" ]; then
    echo "Using local asset for sha256: ${local_path}" >&2
    sha=$(sha256_file "$local_path") || sha=""
  fi

  # Fallback to remote download (with retries) if local missing or sha empty
  if [ -z "$sha" ]; then
    echo "Downloading ${url} (fallback for sha256)..." >&2
    if ! retry 12 5 curl -fsSL "${url}" -o "${tmp}"; then
      echo "ERROR: could not download ${url} to compute sha256." >&2
    else
      sha=$(sha256_file "$tmp" || true)
    fi
  fi

  # A formula shipped with an unresolved placeholder is a broken tap: `brew
  # install` fails on every machine until the next release. Refuse to emit one.
  if [ -z "$sha" ]; then
    echo "ERROR: no sha256 for ${a}; refusing to write a formula with an unresolved placeholder." >&2
    rm -f "$formula" "$formula.bak"
    exit 1
  fi

  ph="$(placeholder_for "$a")"
  sed -i.bak "s#${ph}#${sha}#" "$formula"
done

rm -f "$formula.bak"
echo "Wrote ${formula} for v${version}" >&2

# Optional: best-effort HEAD check to surface propagation status without failing CI
for a in "${assets[@]}"; do
  url="https://github.com/${owner_repo}/releases/download/v${version}/${a}"
  if ! retry 6 5 bash -c "curl -fsI \"$url\" >/dev/null"; then
    echo "WARN: ${a} not yet available at ${url} (HEAD 404). Likely CDN propagation; continuing." >&2
  fi
done
