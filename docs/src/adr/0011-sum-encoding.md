# ADR-0011: Sums stay typed in the netlist IR; encoding is backend lowering

**Status:** Accepted

## Context
`a+b` has |a|+|b| inhabitants but any w-bit bundle has 2^w raw states, so
some patterns encode nothing -- and in a reversible circuit every bit is
somebody's input in the other direction: padding must be *reconstructed*
exactly on the reverse pass. Candidate encodings: (A) tag + shared padded
payload, width 1+max; (B) disjoint payloads, width 1+|a|+|b|, exponential
under nesting; (C) dense arithmetic, optimal width but structural
combinators become adders/comparators/multiplications; (D) one-hot /
dual-rail -- the native idiom of adiabatic logic families.

## Decision
Don't fix an encoding in the IR. Sums are first-class typed ports;
injections and case-splits are typed nodes. Encoding is per-backend
lowering: FPGA default = (A) with a machine-checked canonicity invariant
(ADR-0012), (B) available as a local heuristic for shallow sums; (D)
earmarked for the adiabatic backend; (C) documented-rejected.

## Consequences
One extra lowering pass to write and verify; WCET/width reporting is
backend-relative. In exchange: simulator checks and differential tests
operate on typed values above the bit level, and the adiabatic backend
arrives without an IR migration.
