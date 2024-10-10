{
  description = "A json to nix converter written in Haskell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "aarch64-linux" "aarch64-darwin" "x86_64-linux" "x86_64-darwin" ];
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, ... }: {

        # Typically, you just want a single project named "default". But
        # multiple projects are also possible, each using different GHC version.
        haskellProjects.default = {
          # The base package set representing a specific GHC version.
          # By default, this is pkgs.haskellPackages.
          # You may also create your own. See https://community.flake.parts/haskell-flake/package-set
          # basePackages = pkgs.haskellPackages;

          # Extra package information. See https://community.flake.parts/haskell-flake/dependency
          #
          # Note that local packages are automatically included in `packages`
          # (defined by `defaults.packages` option).
          #
          packages = {
            # aeson.source = "1.5.0.0";      # Override aeson to a custom version from Hackage
            # shower.source = inputs.shower; # Override shower to a custom source path
          };
          settings = {
            #  aeson = {
            #    check = false;
            #  };
            #  relude = {
            #    haddock = false;
            #    broken = false;
            #  };
          };

          devShell = {
            # Enabled by default
            # enable = true;

            # Programs you want to make available in the shell.
            # Default programs can be disabled by setting to 'null'
            # tools = hp: { fourmolu = hp.fourmolu; ghcid = null; };

            hlsCheck.enable = pkgs.stdenv.isDarwin; # On darwin, sandbox is disabled, so HLS can use the network.
          };
        };

        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.json2nix;
        packages.yaml2nix = pkgs.writeShellScriptBin "yaml2nix" ''
          cmd="${pkgs.yq}/bin/yq"
          if [ -n "$1" ] && [ "$1" != "-" ]; then
            cmd="$cmd \"$1\""
          fi

          cmd="$cmd | ${self'.packages.json2nix}/bin/json2nix"
          if [ -n "$2" ]; then
            cmd="$cmd - \"$2\""
          fi

          eval $cmd
        '';
        packages.toml2nix = pkgs.writeShellScriptBin "toml2nix" ''
          cmd="${pkgs.yq}/bin/tomlq"
          if [ -n "$1" ] && [ "$1" != "-" ]; then
            cmd="$cmd \"$1\""
          fi

          cmd="$cmd | ${self'.packages.json2nix}/bin/json2nix"
          if [ -n "$2" ]; then
            cmd="$cmd - \"$2\""
          fi

          eval $cmd
        '';
      };
    };
}
