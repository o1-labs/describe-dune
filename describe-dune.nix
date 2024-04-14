{ stdenv, fetchFromGitHub, opam-installer, ocaml, findlib, yojson, cmdliner, parsexp, base, stdio }:
stdenv.mkDerivation {
  pname = "describe-dune";
  version = "dev";

  src = ./.;

  buildInputs = [ yojson parsexp cmdliner base stdio ];
  nativeBuildInputs = [ ocaml findlib opam-installer ];

  preInstall = "export PREFIX=$out";
}
