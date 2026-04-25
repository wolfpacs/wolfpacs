{
  description = "WolfPACS development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        beamPkgs = pkgs.beam.packages.erlang_28;
        erlang = pkgs.beam.interpreters.erlang_28;

        pythonEnv = pkgs.python3.withPackages (ps: with ps; [
          robotframework
          pydicom
          pynetdicom
        ]);
      in
      {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            erlang
            beamPkgs.rebar3
            git
            cacert
            dcmtk
            gnumake

            # For validation tests (Robot Framework)
            pythonEnv
          ];
        };
      });
}
