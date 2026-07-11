{
  description = "theseus: a reversible language, compiler, and (eventually) adiabatic silicon";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    crane.url = "github:ipetkov/crane";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, fenix, crane, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # ADR-0019 (amended): single pinned nightly. "Nightly" here is a
        # snapshot — fully reproducible. Source stays stable-compatible
        # (no feature gates without a ratified ADR); the nightly channel
        # exists so Miri and cargo-fuzz are first-class in the default shell.
        # Bumping this pin is a deliberate PR; the T1 determinism suite is
        # the canary.
        rustToolchain = fenix.packages.${system}.complete.withComponents [
          "cargo" "rustc" "rust-src" "rustfmt" "clippy" "miri" "rust-analyzer"
        ];

        craneLib = (crane.mkLib pkgs).overrideToolchain rustToolchain;

        src = craneLib.cleanCargoSource ./.;
        commonArgs = { inherit src; strictDeps = true; };

        # One deps derivation shared by every check (crane's cargoArtifacts).
        cargoArtifacts = craneLib.buildDepsOnly commonArgs;

        theseus = craneLib.buildPackage (commonArgs // { inherit cargoArtifacts; });

        # ADR-0019: external tools reach tests through the TargetTools seam,
        # never PATH-luck. These env vars are that seam's Nix-side population.
        targetToolsEnv = {
          THESEUS_VERILATOR = "${pkgs.verilator}/bin/verilator";
          THESEUS_YOSYS = "${pkgs.yosys}/bin/yosys";
        };
      in {
        packages.default = theseus;

        checks = {
          inherit theseus;
          clippy = craneLib.cargoClippy (commonArgs // {
            inherit cargoArtifacts;
            cargoClippyExtraArgs = "--all-targets -- --deny warnings";
          });
          fmt = craneLib.cargoFmt { inherit src; };
          # Fast tier (ADR-0018): units + small-type exhaustive + fixed-seed
          # properties + snapshots. Nightly tier runs outside `flake check`
          # via scheduled CI (fuzzing, mutation, SAT obligations, Verilator).
          test = craneLib.cargoNextest (commonArgs // {
            inherit cargoArtifacts;
            env = targetToolsEnv;
          });
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [ theseus ];
          env = targetToolsEnv;
          packages = with pkgs; [
            rustToolchain

            # hardware flow (L2/L3)
            verilator
            yosys
            nextpnr           # provides ecp5 arch for ULX3S; icestorm variant if iCEBreaker
            trellis
            openfpgaloader
            surfer
            gtkwave

            # solvers: primary verification is in-process CNF (ADR-0020,
            # amended); z3 kept for the SMT-LIB *debug export* path only.
            z3

            # rust dev / test discipline (ADR-0018)
            cargo-nextest
            cargo-insta
            cargo-mutants
            cargo-fuzz
            cargo-deny
            cargo-audit

            # docs & hygiene
            mdbook
            structurizr-cli   # ADR-0021 (amended): diagrams are Structurizr DSL
            treefmt
            alejandra
          ];
        };

        # Proofs shell (ledger TH-*): Lean 4 + Mathlib. We ship `elan` rather
        # than nixpkgs' `lean4` on purpose — Mathlib's binary cache
        # (`lake exe cache get`) is keyed to the *official* release toolchain
        # named in `theseus-proofs/lean-toolchain`, which a nixpkgs-built lean
        # cannot consume. elan fetches that exact toolchain into ~/.elan; git +
        # curl + cacert are what `lake`/`cache get` need to resolve deps and
        # pull the cache over TLS.
        devShells.proofs = pkgs.mkShell {
          packages = with pkgs; [ elan git curl cacert ];
          # Point TLS at the Nix-provided CA bundle so `cache get` (and any
          # `lake` git fetch over https) verifies inside a pure-ish shell.
          SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
        };

        # Thin silicon shell, awake since the pivot (ADR-0022/0024/0025).
        # PDK: pin sky130A via open_pdks/volare; export PDK_ROOT here once
        # pinned. OpenLane/layout tools stay out until the parasitics rung
        # (ADR-0032 rung 4).
        devShells.silicon = pkgs.mkShell {
          packages = with pkgs; [ ngspice xschem netgen gaw ];
          # env.PDK_ROOT = "...pinned sky130A store path...";
        };
      });
}
