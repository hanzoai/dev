class Code < Formula
  desc "Terminal coding agent"
  homepage "https://github.com/just-every/code"
  version "v0.2.67"
  on_macos do
    if Hardware::CPU.arm?
      url "https://github.com/just-every/code/releases/download/v0.2.67/code-aarch64-apple-darwin.tar.gz"
      sha256 "ed8fbb68d8cff0f76d28c9c9b69445dab66f8e645613e9145061e950e8cf7507"
    else
      url "https://github.com/just-every/code/releases/download/v0.2.67/code-x86_64-apple-darwin.tar.gz"
      sha256 "642f656b1d45fe305738f519b5d44c8329bd9963f3e3781ee542a4313d2102f7"
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
