{
  description = "Hacker News - TUI over JSON";

  inputs.nixpkgs.url = "nixpkgs";
  outputs = { self, nixpkgs }:
    let
      forAllSystems = f:
        nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system:
      supportedSystems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];
        import nixpkgs {
          system = system;
          overlays = [ self.overlays.default ];
        });
    in {
      formatter = forAllSystems (system: nixpkgsFor.${system}.nixfmt);
      overlays.default = (final: prev: {
        dist = final.haskellPackages.callCabal2nix "hnj" ./. { };
      });
      packages = forAllSystems (system:
        let pkgs = nixpkgsFor.${system};
        in {
          default = self.packages.${system}.dist;
          dist = pkgs.dist;
          # checklist-trunc-tables =
          #             pkgs.writeShellApplication {
          #               name = "kk-trunc-tables";
          #               runtimeInputs = [ pkgs.postgresql ];
          #               text = "psql -h 192.168.1.36 -U todo todo -c 'TRUNCATE Lists CASCADE; ALTER SEQUENCE lists_listid_seq RESTART WITH 1; ALTER SEQUENCE listentries_entryid_seq RESTART WITH 1;' ";
          #             };
        });
      checks = self.packages;

      devShells = forAllSystems (system:
        let haskellPackages = nixpkgsFor.${system}.haskellPackages;
        in {
          default = haskellPackages.shellFor {
            packages = p: [ self.packages.${system}.dist ];
            withHoogle = true;
            buildInputs = [
              haskellPackages.haskell-language-server
              haskellPackages.cabal-install
              haskellPackages.ormolu

              nixpkgsFor.${system}.zlib

              # self.packages.${system}.checklist-trunc-tables
            ];

            nativeBuildInputs = [ haskellPackages.hsc2hs ];
          };
        });
    };
}
