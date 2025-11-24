{
  description = "agda2lambox";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    agda = {
      url = "github:agda/agda";
      flake = false;
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }@inputs:
    let
      overlay-agda2lambox =
        final: prev:
        let
          hLib = prev.haskell.lib;
          lib = prev.lib;
        in
        {
          haskellPackages = prev.haskellPackages.extend (
            hfinal: hprev: {
              sexpresso = hLib.doJailbreak (hLib.markUnbroken hprev.sexpresso);
              Agda = hLib.dontCheck (hLib.dontHaddock (hfinal.callCabal2nix "Agda" inputs.agda { }));
              agda2lambox = hLib.doJailbreak (hfinal.callCabal2nix "agda2lambox" ./. { });
            }
          );
        };
    in
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            overlay-agda2lambox
          ];
        };
        agda2lambox = pkgs.haskellPackages.agda2lambox;
      in
      {
        packages = {
          inherit agda2lambox;
          default = agda2lambox;
        };
        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [ p.agda2lambox ];
          buildInputs = with pkgs.haskellPackages; [
            cabal-install
            cabal2nix
            haskell-language-server
          ];
        };
      }
    );
}
