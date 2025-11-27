{
  perSystem =
    { pkgs, ... }:
    let
      opkgs = pkgs.ocamlPackages;
    in
    {
      packages.ppx_monad = opkgs.buildDunePackage {
        pname = "ppx_monad";
        version = "dev";
        src = ./..;
        propagatedBuildInputs = [ opkgs.ppxlib ];
      };
    };
}
