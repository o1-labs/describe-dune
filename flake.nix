{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }: {
    overlay = final: prev: {
      describe-dune = final.ocamlPackages.callPackage ./describe-dune.nix { };
    };
  } // flake-utils.lib.eachDefaultSystem (system:
    {
      packages."${system}".describe-dune =
        (nixpkgs.legacyPackages."${system}".extend self.overlay).describe-dune;
      defaultPackage."${system}" = self.packages."${system}".describe-dune;
    }
  );
}
