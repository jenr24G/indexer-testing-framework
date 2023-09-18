{
  description = "Monorepo for the Pantheon Programming Language";
  
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = 
    { self
    , nixpkgs
    , flake-utils
    , flake-compat 
    }: flake-utils.lib.eachSystem 
      [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" "x86_64-windows"] (
      system: let
        pkgs = import nixpkgs
          { inherit system; };
      
        haskellPackages = pkgs.haskell.packages."ghc925";

        runtimeDependencies = with pkgs; [

        ];

        buildDependencies = with pkgs; [
          haskellPackages.ghc # GHC compiler in the desired version (will be available on PATH)
          zlib
        ] ++ runtimeDependencies ++ lib.optionals stdenv.isDarwin
        [
          frameworks.Security
          frameworks.CoreServices
        ];

        developmentDependencies = with pkgs; [
          nixd nixpkgs-fmt alejandra
          haskellPackages.haskell-language-server # LSP server for editor
          haskellPackages.implicit-hie # auto generate LSP hie.yaml file from cabal
          haskellPackages.ghcid # Continuous terminal Haskell compile checker
          haskellPackages.ormolu # Haskell formatter
          haskellPackages.hlint # Haskell codestyle checker
          haskellPackages.hoogle # Lookup Haskell documentation
          haskellPackages.retrie # Haskell refactoring tool
          stack-wrapped
        ] ++ buildDependencies ++ runtimeDependencies;

        stack-wrapped = pkgs.symlinkJoin {
          name = "stack"; # will be available as the usual `stack` in terminal
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };

      in with pkgs; {
        packages = flake-utils.lib.flattenTree rec {};

        devShells = flake-utils.lib.flattenTree rec {
          default = pkgs.mkShell {
            shellHook = '''';
            packages = developmentDependencies;
          };
        };
      }
    );
}
