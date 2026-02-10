{
  description = "Development Nix flake for OpenAI Codex CLI";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, flake-utils, rust-overlay, ... }: 
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        pkgsWithRust = import nixpkgs {
          inherit system;
          overlays = [ rust-overlay.overlays.default ];
        };
        monorepo-deps = with pkgs; [
          # for precommit hook
          pnpm
          husky
        ];
        code-rs = import ./code-rs {
          pkgs = pkgsWithRust;
          inherit monorepo-deps;
        };
      in
      {
        packages = {
          code-rs = code-rs.package;
          default = code-rs.package;
        };

        devShells = {
          code-rs = code-rs.devShell;
          default = code-rs.devShell;
        };

        apps = {
          code-rs = code-rs.app;
          default = code-rs.app;
        };
      }
    );
}
