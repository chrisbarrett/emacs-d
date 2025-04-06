(nil
 '(progn
    (setq directory-name (file-name-nondirectory (directory-file-name default-directory))))
 "{
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
