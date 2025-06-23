(nil
 '(progn
    (setq directory-name (file-name-nondirectory (directory-file-name default-directory))))
 "{
  inputs = {
    nixpkgs.url = \"github:nixos/nixpkgs/nixpkgs-unstable\";
    flake-utils.url = \"github:numtide/flake-utils\";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        name = \"" directory-name "\";
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            " _ "
          ];
        };
      });
}"

 '(progn
    (makunbound 'directory-name)))
