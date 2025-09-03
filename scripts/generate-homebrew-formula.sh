#!/usr/bin/env bash
set -euo pipefail

# Generate a minimal Homebrew formula from the latest GitHub release.
# Writes Formula/Code.rb into the repo root (not a tap); you can copy it
# into a tap repo to publish.

owner_repo="just-every/code"
version="${1:-$(jq -r .version codex-cli/package.json)}"

assets=(
  "code-aarch64-apple-darwin.tar.gz"
  "code-x86_64-apple-darwin.tar.gz"
)

mkdir -p Formula
cat > Formula/Code.rb <<'RUBY'
class Code < Formula
  desc "Terminal coding agent"
  homepage "https://github.com/just-every/code"
RUBY

echo "  version \"v${version}\"" >> Formula/Code.rb

cat >> Formula/Code.rb <<'RUBY'
  on_macos do
    if Hardware::CPU.arm?
      url "__URL_ARM64__"
      sha256 "__SHA_ARM64__"
    else
      url "__URL_X64__"
      sha256 "__SHA_X64__"
    end
  end

  def install
    bin.install Dir["code-*"].first => "code"
    # Provide a compatibility shim
    (bin/"coder").write <<~EOS
      #!/bin/bash
      exec "#{bin}/code" "$@"
    EOS
  end

  test do
    system "#{bin}/code", "--help"
  end
end
RUBY

for a in "${assets[@]}"; do
  url="https://github.com/${owner_repo}/releases/download/v${version}/${a}"
  tmp="/tmp/${a}"
  echo "Downloading ${url}..." >&2
  curl -fsSL "${url}" -o "${tmp}"
  sha=$(shasum -a 256 "${tmp}" | awk '{print $1}')
  if [[ "${a}" == *"aarch64-apple-darwin"* ]]; then
    sed -i.bak "s#__URL_ARM64__#${url}#" Formula/Code.rb
    sed -i.bak "s#__SHA_ARM64__#${sha}#" Formula/Code.rb
  else
    sed -i.bak "s#__URL_X64__#${url}#" Formula/Code.rb
    sed -i.bak "s#__SHA_X64__#${sha}#" Formula/Code.rb
  fi
done

rm -f Formula/Code.rb.bak
echo "Wrote Formula/Code.rb for v${version}" >&2

