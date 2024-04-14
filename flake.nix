{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }: {
    overlay = final: prev: {
      describe-dune = final.ocamlPackages.callPackage ./describe-dune.nix { };
    };
    packages.x86_64-linux.describe-dune =
      (nixpkgs.legacyPackages.x86_64-linux.extend self.overlay).describe-dune;
    defaultPackage.x86_64-linux = self.packages.x86_64-linux.describe-dune;
  };
}
