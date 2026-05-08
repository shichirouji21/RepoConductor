{
  description = "RepoConductor - terminal dashboard for many Git repositories";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        haskellPackages = pkgs.haskellPackages;

        repoconductorRaw =
          haskellPackages.callCabal2nix "repoconductor" (pkgs.lib.cleanSource ./.) { };

        repoconductor = pkgs.haskell.lib.compose.justStaticExecutables (
          repoconductorRaw.overrideAttrs (old: {
            postInstall = (old.postInstall or "") + ''
              install -Dm644 ${./completions/repoconductor.bash} \
                $out/share/bash-completion/completions/repoconductor
              install -Dm644 ${./completions/_repoconductor} \
                $out/share/zsh/site-functions/_repoconductor
              install -Dm644 ${./completions/repoconductor.fish} \
                $out/share/fish/vendor_completions.d/repoconductor.fish
            '';
          })
        );
      in {
        packages.default = repoconductor;
        packages.repoconductor = repoconductor;

        apps.default = flake-utils.lib.mkApp {
          drv = repoconductor;
          name = "repoconductor";
        };

        devShells.default = haskellPackages.shellFor {
          packages = _: [ repoconductorRaw ];
          buildInputs = with pkgs; [
            cabal-install
            haskell-language-server
            git
          ];
          withHoogle = false;
        };
      });
}
