{
  description = "Berkeley DB (libdb) — historical archive and living fork";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        version = "5.3.29";
      in {
        packages = rec {
          # Default: the Meson/Ninja build of the core C library.
          libdb-meson = pkgs.stdenv.mkDerivation {
            pname = "libdb";
            inherit version;
            src = ./.;
            nativeBuildInputs = [ pkgs.meson pkgs.ninja pkgs.python3 pkgs.pkg-config ];
            # meson.build is at the repo root; nix's meson hooks drive it.
          };

          # The reference Autoconf build (build_unix), full feature set.
          libdb-autoconf = pkgs.stdenv.mkDerivation {
            pname = "libdb-autoconf";
            inherit version;
            src = ./.;
            enableParallelBuilding = true;
            configurePhase = ''
              runHook preConfigure
              cd build_unix
              ../dist/configure --prefix=$out --enable-shared --disable-static
              runHook postConfigure
            '';
            # buildPhase / installPhase use the default make in build_unix.
          };

          default = libdb-meson;
        };

        devShells.default = pkgs.mkShell {
          packages = [
            pkgs.meson pkgs.ninja pkgs.python3 pkgs.pkg-config
            pkgs.gcc pkgs.clang pkgs.autoconf pkgs.gnumake
            pkgs.tcl   # for the TCL test harness (--enable-test)
          ];
          shellHook = ''
            echo "libdb dev shell — Meson: 'meson setup build && ninja -C build'"
            echo "                  Autoconf: 'cd build_unix && ../dist/configure && make'"
          '';
        };

        # `nix flake check` smoke: the default package builds.
        checks.default = self.packages.${system}.default;
      });
}
