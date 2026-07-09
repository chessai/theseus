workspace "theseus" "Reversible language, compiler, and hardware stack (ADR-0021, Structurizr amendment)" {

  model {
    dev = person "Language user" "Writes theseus programs; reads witness-bearing diagnostics"

    theseus = softwareSystem "theseus toolchain" "Compiler, interpreter, simulators, verifier" {
      cli = container "theseus-cli" "Driver + REPL (:step/:unstep); invariant-ledger validation" "Rust"
      syntax = container "theseus-syntax" "Lexer, recursive-descent parser, pretty" "Rust"
      check = container "theseus-check" "Elaboration; bidirectional coverage + linearity (ADR-0004)" "Rust"
      core = container "theseus-core" "Core Pi; adjoint involution; fixed-order rewrites (ADR-0010)" "Rust"
      eval = container "theseus-eval" "Fuel interpreter; exhaustive enumerator; oracle root" "Rust"
      netlist = container "theseus-netlist" "Typed hierarchical IR; canonicalize->Permute (ADR-0014); schedule/balance; typed+bit simulators" "Rust"
      emit = container "theseus-emit" "Encoding lowering (ADR-0011/0012); typed Verilog mini-AST; Verilator harness" "Rust"
      verify = container "theseus-verify" "Miter + Tseitin + CnfSolver/ObligationOracle (ADR-0020)" "Rust"
      spice = container "theseus-spice" "Phased cell netlist + tick sim (INV-TOKEN); SPICE deck emission; characterization DB; energy reports (ADR-0025/0026/0028)" "Rust"
    }

    verilator = softwareSystem "Verilator" "Oracle-chain terminus; framed stdio protocol" "external"
    fpgaFlow = softwareSystem "Yosys / nextpnr" "L3 synthesis and place-and-route" "external"
    ngspice = softwareSystem "ngspice + Sky130A models" "Physics oracle terminus; characterization bench (ADR-0024/0025)" "external"
    silicon = softwareSystem "Sky130 shuttle flow" "M5 tape-out (rung 4, ADR-0032)" "external"

    dev -> cli "compiles / REPLs"
    cli -> syntax "parse"
    syntax -> check "surface AST"
    check -> core "elaborated core Pi"
    core -> eval "interpretation"
    core -> netlist "canonicalization"
    netlist -> emit "flat typed netlist"
    emit -> verilator "verilated model + protocol"
    emit -> fpgaFlow "structural Verilog"
    netlist -> verify "lowering lemma obligations"
    netlist -> spice "flat typed netlist (dual-rail lowering, ADR-0031)"
    spice -> ngspice "decks; transients decoded back through typed layer (B5.3)"
    spice -> silicon "rung-4 handoff (M5, optional)"
  }

  views {
    systemContext theseus "context" { include *; autoLayout lr }
    container theseus "pipeline" { include *; autoLayout lr }
    styles {
      element "external" { background #999999 }
    }
  }
}
