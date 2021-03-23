{
  inputs = {
    utils.url = "github:numtide/flake-utils";
    naersk.url = "github:nmattia/naersk";
  };

  outputs = { self, nixpkgs, utils, naersk }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages."${system}";
        naersk-lib = naersk.lib."${system}";
      in rec {
        # `nix build`
        packages.wired = naersk-lib.buildPackage {
          pname = "wired";
          root = ./.;
          nativeBuildInputs = with pkgs; [ pkgconfig ];
          buildInputs = with pkgs; [ gtk3 ];
        };
        defaultPackage = packages.wired;

        # `nix run`
        apps.wired = utils.lib.mkApp { drv = packages.wired; };
        defaultApp = apps.wired;

        # `nix develop`
        devShell = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [ rustc cargo rls pkgconfig ];
          buildInputs = with pkgs; [ gtk3 ];
        };
      });
}
