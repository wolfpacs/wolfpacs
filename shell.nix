{ pkgs ? import <nixpkgs> { } }:

let
  pythonEnv = pkgs.python3.withPackages (ps: with ps; [
    robotframework
    pydicom
    pynetdicom
  ]);
in
pkgs.mkShell {
  packages = with pkgs; [
    beam.interpreters.erlang_28
    beam.packages.erlang_28.rebar3
    git
    cacert
    dcmtk
    gnumake
    pythonEnv
  ];
}
