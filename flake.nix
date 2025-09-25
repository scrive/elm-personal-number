{
  description = "A developer shell for working on elm-personal-number.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          name = "elm-personal-number";

          packages = with pkgs.elmPackages; [
            elm
            elm-doc-preview
            elm-format
            elm-optimize-level-2
            elm-test-rs
          ];
        };

        packages = {
          test = pkgs.stdenv.mkDerivation {
            pname = "elm-personal-number-test";
            version = "1.0.0";
            # point to elm.json
            src = ./.;
            
            nativeBuildInputs = with pkgs.elmPackages; [
              elm
              elm-test-rs
            ];
            
            buildPhase = ''
              # Run elm-test
              elm-test
            '';
            
            installPhase = ''
              # Create output directory and mark success
              mkdir -p $out
              echo "Tests completed successfully" > $out/test-results
            '';
            dontStrip = true;
          };
        };
        
        checks = {
          test = self.packages.${system}.test;
        };
      }
    );
}