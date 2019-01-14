with builtins;
with rec {
  # A known-good pinned nixpkgs version
  nixpkgs = overlays: import (fetchTarball {
    name   = "nixpkgs1709";
    url    = https://github.com/NixOS/nixpkgs/archive/17.09.tar.gz;
    sha256 = "0kpx4h9p1lhjbn1gsil111swa62hmjs9g93xmsavfiki910s73sh";
  }) { inherit overlays; config = {}; };

  # Avoid rebuilding if only our metadata has changed
  src = filterSource (path: _: !(elem (baseNameOf path)) [
                       ".git" ".gitignore" ".issues" "dist" "dist-newstyle"
                       "nixpkgs.nix" "README" "release.nix" "shell.nix"
                     ])
                     ./.;

  # Overrides the pinned haskellPackages set. We don't do it directly in an
  # overlay, since that can break the dependencies of cabal2nix.
  haskellPackages =
    with nixpkgs [];
    haskell.packages.ghc7103.override (old: {
      overrides = self: super: {
        lazy-lambda-calculus = self.callPackage (super.haskellSrc2nix {
          inherit src;
          name = "lazy-lambda-calculus";
        }) {};

        lazysmallcheck2012 = self.callPackage (super.haskellSrc2nix {
          name = "lazysmallcheck2012";
          src  = fetchgit {
            url    = http://chriswarbo.net/git/lazy-smallcheck-2012.git;
            rev    = "dbd6fba";
            sha256 = "1i3by7mp7wqy9anzphpxfw30rmbsk73sb2vg02nf1mfpjd303jj7";
          };
        }) {};

        syb = self.callHackage "syb" "0.6" {};
      };
    });
};
nixpkgs [
  (self: super: { inherit haskellPackages; })
]
