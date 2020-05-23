with import ./nixpkgs.nix {};

let
  ghcWithPackages =
    haskellPackages.ghcWithPackages (p: with p; [
      free
      QuickCheck
      quickcheck-transformer
      ghcid
      containers
      dependent-map
    ]);
in

mkShell {
  buildInputs = [ ghcWithPackages ];
}
