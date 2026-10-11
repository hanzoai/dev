class HanzoDev < Formula
  desc "Local AI coding agent for your terminal, powered by Hanzo AI"
  homepage "https://github.com/hanzoai/dev"
  version "0.6.122"
  license "Apache-2.0"

  on_macos do
    on_arm do
      url "https://github.com/hanzoai/dev/releases/download/v#{version}/dev-aarch64-apple-darwin.tar.gz"
      sha256 "88b6f017c2f4e5e3f256d5ac84a010e287089ea4a6e73f77cc506d122f5d4ec1"
    end
    on_intel do
      url "https://github.com/hanzoai/dev/releases/download/v#{version}/dev-x86_64-apple-darwin.tar.gz"
      sha256 "4038f0b8cd034d64018cede8064316ea8210d26b8d6598610b25e6e08d311c3f"
    end
  end

  on_linux do
    on_arm do
      url "https://github.com/hanzoai/dev/releases/download/v#{version}/dev-aarch64-unknown-linux-musl.tar.gz"
      sha256 "f2e75ffcdde3acb66eeb882a31a99bed21458fb98da0c0d38e5da8c4f7a7022c"
    end
    on_intel do
      url "https://github.com/hanzoai/dev/releases/download/v#{version}/dev-x86_64-unknown-linux-musl.tar.gz"
      sha256 "900d0185b13c46d07ac7b5049985f3a0311b57fb42a1946f4d8e468f13816335"
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
