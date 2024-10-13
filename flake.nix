{
  description = "chokan - chotto kanzen";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-24.05";
    # Rust tool management
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, fenix }:let
    overlays = [
      fenix.overlays.default
    ];

    pkgs = import nixpkgs {
      system = "x86_64-linux";
      inherit overlays;
    };
  in {

    packages.x86_64-linux.default = pkgs;

    devShells.x86_64-linux.default = pkgs.mkShell {
      buildInputs = [
        (pkgs.fenix.stable.withComponents [
          "cargo"
          "clippy"
          "rust-src"
          "rustc"
          "rustfmt"
        ])
        pkgs.rust-analyzer-nightly
        pkgs.gcc
        pkgs.pre-commit
        pkgs.go-task
      ];

      shelHook = ''
 pre-commit install
      '';
    };
  };
}
