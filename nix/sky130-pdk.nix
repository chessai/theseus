# SkyWater sky130 PDK — pinned, reproducible, primitive-devices only.
#
# sky130 is not in nixpkgs, so we fetch the prebuilt PDK from the fossi-foundation
# ciel-releases (ciel = the volare successor). The release TAG encodes the exact
# open_pdks build, giving reproducibility (ADR-0024 B2.2: PDK version pinned in the
# flake like the toolchain). We fetch only the two assets a transistor-level custom
# flow needs — `sky130_fd_pr` (foundry PRIMITIVE devices = the FET spice models,
# 14 MB) and `common` (shared corner/model files, 6.3 MB) — NOT the ~600 MB
# standard-cell libraries, which the adiabatic custom cells do not use.
#
# Output layout: $out/sky130A and $out/sky130B, each a normal PDK tree. The ngspice
# models live at $out/sky130A/libs.tech/ngspice/sky130.lib.spice (corners via
# relative .include). Consumers set PDK_ROOT=$out and reference the tt corner with
#   .lib "$THESEUS_SKY130_LIB" tt
#
# Stamp (INV-STAMP / ADR-0032 rung1): pdkVersion below is the open_pdks build hash.
#
# PARKING LOT (deferred, not needed for rung1): a "real" derivation would build the
# PDK from the OFFICIAL source — open_pdks assembling google/skywater-pdk — rather
# than fetching a prebuilt ciel-releases tarball. That buys full source provenance
# and independence from the ciel release cadence, at the cost of packaging magic/
# open_pdks under nix. The prebuilt fetch here is reproducible (pinned tag + hash)
# and sufficient for the rung1 characterization work; revisit if provenance or a
# custom device set demands from-source assembly.
{ pkgs }:

let
  # ciel-releases tag == open_pdks build hash. Bumping the PDK is a one-line change
  # here (tag + the two hashes) — a deliberate PR, like the rust toolchain pin.
  pdkVersion = "ff08c23db8359afce3f134c454e7930586d0641c";
  tag = "sky130-${pdkVersion}";
  base = "https://github.com/fossi-foundation/ciel-releases/releases/download/${tag}";

  fdPr = pkgs.fetchurl {
    url = "${base}/sky130_fd_pr.tar.zst";
    hash = "sha256-pr2rLm/4+O64GImLK0hB2dU/o9/vbdJUg4CuMvNE608=";
  };
  common = pkgs.fetchurl {
    url = "${base}/common.tar.zst";
    hash = "sha256-f36Ny7e5irFaX57QrNO9P5M/GSUXFySpryWxihzW9Fc=";
  };
in
pkgs.stdenvNoCC.mkDerivation {
  pname = "sky130-pdk";
  version = pdkVersion;

  dontUnpack = true;
  nativeBuildInputs = [ pkgs.zstd pkgs.gnutar ];

  # Both assets extract into a shared sky130A/ + sky130B/ tree (fd_pr provides the
  # device models; common provides the corners/ and r+c/ trees they .include).
  buildPhase = ''
    runHook preBuild
    mkdir -p pdk && cd pdk
    tar --zstd -xf ${fdPr}
    tar --zstd -xf ${common}
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out
    cp -r sky130A sky130B $out/
    # Fail loudly if the ngspice model entrypoint is missing (structure changed).
    test -f "$out/sky130A/libs.tech/ngspice/sky130.lib.spice" \
      || { echo "sky130.lib.spice missing — PDK layout changed"; exit 1; }
    runHook postInstall
  '';

  # Expose the pinned version to Nix consumers (e.g. the INV-STAMP pdk field).
  passthru.pdkVersion = pdkVersion;

  meta = {
    description = "SkyWater sky130 PDK (primitive devices + common), pinned via ciel-releases";
    homepage = "https://github.com/fossi-foundation/ciel-releases";
  };
}
