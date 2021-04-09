{
  inputs = { utils.url = "github:numtide/flake-utils"; };

  outputs = { self, nixpkgs, utils }:
    utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages."${system}";
      in rec {
        # `nix develop`
        devShell = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [ nodejs jre ];
          shellHook = "export PATH=$PWD/node_modules/.bin:$PATH";
        };
      });
}
