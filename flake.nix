{
  description = "purescript-react-basic-explore";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };


  outputs = { self, nixpkgs, ... }@inputs:
    let
      name = "iios";

      supportedSystems = [
        "aarch64-darwin"
        "x86_64-darwin"
        "x86_64-linux"
      ];

      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
    in
    {
      devShell = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        pkgs.mkShell {
          inherit name;
          buildInputs = (with pkgs; [
            nodejs-18_x
            nixpkgs-fmt
            purescript
            spago
            bun
            esbuild
          ]) ++ (pkgs.lib.optionals (system == "aarch64-darwin")
            (with pkgs.darwin.apple_sdk.frameworks; [
              Cocoa
              CoreServices
            ]));
        });
    };
}
