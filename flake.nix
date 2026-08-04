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

        # SkyWater sky130 PDK (primitive devices), pinned via ciel-releases.
        # ADR-0024: PDK version pinned in the flake like the toolchain.
        sky130-pdk = import ./nix/sky130-pdk.nix { inherit pkgs; };

        # cleanCargoSource keeps only Rust/Cargo files, but the build and test
        # graph also pull in non-Rust assets at compile time via include_str!
        # (theseus-check's stdlib/prelude.thse, theseus-spice's golden
        # rung0-chardb.ron) and the conformance corpus the test binaries embed
        # (*.thse/*.expect/*.diag). Dropping those makes the sandboxed
        # nix-flake-check build fail where a local `cargo build` (which sees the
        # whole tree) succeeds. Keep Cargo sources plus those asset extensions.
        src = pkgs.lib.cleanSourceWith {
          src = ./.;
          name = "theseus-source";
          filter = path: type:
            (craneLib.filterCargoSources path type)
            || pkgs.lib.hasSuffix ".thse" path
            || pkgs.lib.hasSuffix ".expect" path
            || pkgs.lib.hasSuffix ".diag" path
            || pkgs.lib.hasSuffix ".ron" path;
        };
        commonArgs = { inherit src; strictDeps = true; };

        # One deps derivation shared by every check (crane's cargoArtifacts).
        cargoArtifacts = craneLib.buildDepsOnly commonArgs;

        theseus = craneLib.buildPackage (commonArgs // { inherit cargoArtifacts; });

        # ADR-0019: external tools reach tests through the TargetTools seam,
        # never PATH-luck. These env vars are that seam's Nix-side population.
        targetToolsEnv = {
          THESEUS_VERILATOR = "${pkgs.verilator}/bin/verilator";
          THESEUS_YOSYS = "${pkgs.yosys}/bin/yosys";
          # Physics seam: the SPICE harness locates ngspice through this, never
          # PATH-luck. Populated for every shell so the maestro daemon (which
          # runs from the default shell and whose workers inherit its env) can
          # both find the harness's simulator AND let a worker invoke `ngspice`
          # directly during circuit iteration (allowlisted in .claude/settings.json).
          THESEUS_NGSPICE = "${pkgs.ngspice}/bin/ngspice";
          # Cap glibc malloc to ONE arena for every process in this seam (the
          # maestro daemon, its workers, the gate's `cargo test`, and dev
          # shells all inherit it). The energy tests
          # (crates/theseus-tisc/tests/energy.rs) elaborate CPU-step netlists
          # (~GB) into `OnceLock` statics while building large `balance()`
          # transients; libtest's default `--test-threads=ncpu` × glibc's
          # per-thread arenas keep freed pages unshared, fragmenting peak RSS
          # to ~86 GB and SIGKILL-ing the gate under memory pressure. One arena
          # lets the threads share freed memory: peak ~48 GB (measured), no
          # wall-clock regression. Neither `.cargo/config.toml [env]` (build
          # only) nor a target `runner` (not applied to the host target without
          # `--target`) reaches the test runtime — this env seam does.
          MALLOC_ARENA_MAX = "1";
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

            # physics flow (M3a): SPICE simulator for worker circuit iteration
            # and the harness. On PATH here so daemon-spawned workers inherit it.
            ngspice

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

        # Silicon shell, awake since the pivot (ADR-0022/0024/0025). sky130 PDK
        # pinned via ciel-releases (nix/sky130-pdk.nix). OpenLane/layout tools
        # stay out until the parasitics rung (ADR-0032 rung 4).
        devShells.silicon = pkgs.mkShell {
          packages = with pkgs; [ ngspice xschem netgen gaw sky130-pdk ];
          env = targetToolsEnv // {
            PDK_ROOT = "${sky130-pdk}";
            # The harness reaches the sky130 ngspice models through this seam
            # (mirrors THESEUS_NGSPICE); use `.lib "$THESEUS_SKY130_LIB" tt`.
            THESEUS_SKY130_LIB = "${sky130-pdk}/sky130A/libs.tech/ngspice/sky130.lib.spice";
            # INV-STAMP (ADR-0032): the pinned PDK build hash for energy stamps.
            THESEUS_SKY130_VERSION = sky130-pdk.pdkVersion;
          };
        };

        # Physics-track shell (M3a): the Rust toolchain PLUS the SPICE
        # simulator, so the theseus-spice harness can `cargo test` AND shell out
        # to `ngspice -b`. The M3a minimal money plot (2LAL slope −1) uses
        # generic MOSFET models, so no sky130 PDK is required here yet; the full
        # DUT matrix will add the pinned PDK (see devShells.silicon).
        devShells.physics = pkgs.mkShell {
          inputsFrom = [ theseus ];
          env = targetToolsEnv;
          packages = with pkgs; [ rustToolchain cargo-nextest ngspice ];
          # THESEUS_NGSPICE comes from targetToolsEnv (shared across all shells).
        };

        # Characterization shell (M3a rung1): the physics shell PLUS the pinned
        # sky130 PDK, so `cargo test` can run sky130-cell characterization decks
        # through ngspice. (.#physics stays PDK-free for the rung0 harness;
        # .#silicon has the PDK but no Rust.) This is the gate for rung1 work.
        devShells.char = pkgs.mkShell {
          inputsFrom = [ theseus ];
          packages = with pkgs; [ rustToolchain cargo-nextest ngspice sky130-pdk ];
          env = targetToolsEnv // {
            PDK_ROOT = "${sky130-pdk}";
            THESEUS_SKY130_LIB = "${sky130-pdk}/sky130A/libs.tech/ngspice/sky130.lib.spice";
            THESEUS_SKY130_VERSION = sky130-pdk.pdkVersion;
          };
        };
      });
}
