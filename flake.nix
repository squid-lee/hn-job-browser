{
  description = "Hacker News - TUI over JSON";

  inputs.nixpkgs.url = "nixpkgs";
  outputs =
    { self, nixpkgs }:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems f;
      nixpkgsFor = forAllSystems (
        system:
        import nixpkgs {
          inherit system;
          overlays = [ self.overlays.default ];
        }
      );
    in
    {
      formatter = forAllSystems (
        system:
        let
          pkgs = nixpkgsFor.${system};
        in
        pkgs.writeShellApplication {
          name = "fmt";
          runtimeInputs = [
            pkgs.git
            pkgs.haskellPackages.ormolu
            pkgs.haskellPackages.hlint
            pkgs.nixfmt-rfc-style
            pkgs.statix
          ];
          text = ''
            git ls-files -z '*.hs' | xargs -0 ormolu --mode inplace
            git ls-files -z '*.hs' | xargs -0 hlint
            git ls-files -z '*.nix' | xargs -0 nixfmt
            git ls-files -z '*.nix' | xargs -0 statix check
          '';
        }
      );
      overlays.default = final: prev: { dist = final.haskellPackages.callCabal2nix "hnj" ./. { }; };
      packages = forAllSystems (
        system:
        let
          pkgs = nixpkgsFor.${system};
        in
        {
          default = self.packages.${system}.dist;
          inherit (pkgs) dist;
          # checklist-trunc-tables =
          #             pkgs.writeShellApplication {
          #               name = "kk-trunc-tables";
          #               runtimeInputs = [ pkgs.postgresql ];
          #               text = "psql -h 192.168.1.36 -U todo todo -c 'TRUNCATE Lists CASCADE; ALTER SEQUENCE lists_listid_seq RESTART WITH 1; ALTER SEQUENCE listentries_entryid_seq RESTART WITH 1;' ";
          #             };
        }
      );
      checks = forAllSystems (
        system:
        let
          pkgs = nixpkgsFor.${system};
        in
        {
          build = self.packages.${system}.dist;
          lint =
            pkgs.runCommand "lint"
              {
                nativeBuildInputs = [
                  pkgs.haskellPackages.hlint
                  pkgs.statix
                ];
              }
              ''
                cd ${self}
                hlint app
                statix check .
                touch $out
              '';
        }
      );

      devShells = forAllSystems (
        system:
        let
          inherit (nixpkgsFor.${system}) haskellPackages;
        in
        {
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
        }
      );
    };
}
