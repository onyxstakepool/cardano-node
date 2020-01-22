{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "2.0";
      identifier = { name = "lobemo-backend-ekg"; version = "0.1.0.1"; };
      license = "Apache-2.0";
      copyright = "2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "Alexander Diemand";
      homepage = "https://github.com/input-output-hk/iohk-monitoring-framework";
      url = "";
      synopsis = "provides a backend implementation to EKG";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.iohk-monitoring)
          (hsPkgs.aeson)
          (hsPkgs.async)
          (hsPkgs.bytestring)
          (hsPkgs.ekg)
          (hsPkgs.ekg-core)
          (hsPkgs.safe-exceptions)
          (hsPkgs.snap-core)
          (hsPkgs.snap-server)
          (hsPkgs.stm)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.unordered-containers)
          ];
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/iohk-monitoring-framework";
      rev = "72818632b88dde4c8c65c744b580a738983c7c48";
      sha256 = "1kx7bbn2dwss47n9dvj48xcqf655nyzw87pxs12wcx918w23kmc3";
      });
    postUnpack = "sourceRoot+=/plugins/backend-ekg; echo source root reset to \$sourceRoot";
    }