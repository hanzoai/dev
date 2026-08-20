class HanzoDev < Formula
  desc "Local AI coding agent for your terminal, powered by Hanzo AI"
  homepage "https://github.com/hanzoai/dev"
  version "0.6.102"
  license "Apache-2.0"

  on_macos do
    on_arm do
      url "https://github.com/hanzoai/dev/releases/download/v#{version}/dev-aarch64-apple-darwin.tar.gz"
      sha256 "cb34654c2089ddd26e2d63eb75c27b902a42ca394d92dd1f5c1afb944463a5a7"
    end
    on_intel do
      url "https://github.com/hanzoai/dev/releases/download/v#{version}/dev-x86_64-apple-darwin.tar.gz"
      sha256 "c27d73b32f46308eb88eef334894a670a5ad9a9124bf8274baf065864d42e2eb"
    end
  end

  on_linux do
    on_arm do
      url "https://github.com/hanzoai/dev/releases/download/v#{version}/dev-aarch64-unknown-linux-musl.tar.gz"
      sha256 "4d04236b98de6fdf617df3170ef60576aa7481bbe4911376b1f1a8b310ea7c92"
    end
    on_intel do
      url "https://github.com/hanzoai/dev/releases/download/v#{version}/dev-x86_64-unknown-linux-musl.tar.gz"
      sha256 "816223bc8f12c9b560373d771bdacdef959e654b4634a3c896600617cbe4063b"
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
