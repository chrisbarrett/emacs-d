{
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];

      perSystem = { pkgs, ... }:
        let
          treesit-grammars = pkgs.emacsPackages.treesit-grammars.with-all-grammars;
        in {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [ prek ];

          shellHook = ''
            export TREESIT_EXTRA_LOAD_PATH="${treesit-grammars}/lib"
            prek install
          '';
        };
      };
    };
}
