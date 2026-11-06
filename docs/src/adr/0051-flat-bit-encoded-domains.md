# ADR-0051: Flat bit-encoded domains for admission and truth tables

**Status:** Accepted -- **implemented** (`theseus-core::domain`,
`theseus-core::prim::admit`, `theseus-netlist::lower` (`prim_from_bits`,
`prim_matches`, B4.2 synthesis), `theseus-tisc::bveq_netlist::synthesize_gate`).

## Context

Every whole-domain check over a prim walked boxed `Value` trees:
`enumerate_values(ty)`, then `BTreeMap<Value, Value>` lookups, then `bits_of`.
A `Flat(8)` value is a thermometer `Right^k(Left(Unit))`, up to 256 boxes, and
each map lookup compares such trees about `log2 |domain|` times. After ADR-0049
made `admit`'s spec==table check lane-parallel, `admit`'s mutual-inverse loops
(ADR-0013) were its whole cost: ~5 s for one `Flat(8) x Flat(8)` table.
Nothing over these domains could be vectorized while the domain was a list of
trees rather than an integer range.

## Decision

1. **One encoding, packed.** A type's inhabitants are `u64` indices: the
   `theseus_netlist::bits_of` bit string packed little-endian (bit `j` of the
   index is `bits_of(v)[j]`). Sum tag at the low bit, `1 = Left`; payload
   `max` of the arms, narrower arm zero-padded, uninhabited arm 0 bits;
   products concatenate. `bits_of` has no rails for `Flat(n)`; the index
   extends it with the thermometer depth `k` as `n` bits (the
   `value_to_leaf_bytes` leaf byte). `bits_of` stays the definition: the
   index is property-tested against it (same width as `rail_width`, same
   bits, same failures), not a second encoding. Theseus-core owns the index
   (`bit_width`, `index_of`, `value_of_index`, `valid_indices`), since
   `admit` lives there and cannot depend on theseus-netlist.
2. **Order is enumeration order.** `valid_indices` yields in
   `enumerate_values` order, which on one type's inhabitants is also
   `Value`'s derived `Ord`, i.e. `BTreeMap` order (tested). Numeric index
   order differs (the tag is the low bit), so every "first failure" scan
   walks the valid list. That is how errors stay identical.
3. **`BitTable`.** A prim's maps are read once (one pass over each
   `BTreeMap`, no lookups) into `fwd`/`bwd: Vec<u32>` indexed by index, with
   sentinels for "no entry" and "image does not inhabit the target type".
   `is_bijection` is three slice loops -- totality, a bitset of hit output
   indices whose popcount must equal the codomain's size, and
   `bwd[fwd[i]] == i` -- and, only when one fails, a replay of `admit`'s four
   `Value` loops in index space and enumeration order, so the `AdmitError`
   variant and offending `Value` (rebuilt with `value_of_index`) are the old
   ones. Ill-typed images are rare and kept faithful: the old loops looked
   them up in the other map as values, so their outcome is recorded at
   build time.
4. **Callers.** `admit` runs its mutual-inverse check on a `BitTable`, and
   feeds the ADR-0049 lane check from the index range (input leaf bytes from
   the index, spec output read back to an index and compared) when the
   table's keys are exactly the domain. `prim_matches` and the B4.2 synthesis
   in `lower_prim` share one table per prim; `prim_from_bits` enumerates the
   index range; `synthesize_gate` reads its rows from a table. The
   `Value` walks remain as test oracles and as the path for types a table
   does not fit.
5. **Dense only.** A side is tabled only if it is at most 24 bits wide and
   has at most 16 slots per inhabitant past 2^16 (`numeral_ty(k)` is `k` bits
   for `k` values). Otherwise `BitTable::from_def` is `None` and callers use
   the `Value` walk (in `synthesize_gate`, which has none, an error -- gate
   prims are products of bits, always dense).

## Not done

- `compile::structural_iso_is_identity` is a whole-domain walk but not of
  this shape: it applies a `Value -> Value` structural rewrite and compares
  TISC leaf bytes, with no table. Converting it means index-space versions of
  the six rewrites; left as is.
- Rank-indexed tables (dense in `0..|domain|`) would cover sparse types too,
  but lose the bit patterns gate synthesis needs.
- `admit` accepts some maps that are mutual inverses only through keys and
  images outside the declared types (e.g. `1 <-> Bool` closed by an
  ill-typed key). The table path reproduces that verdict rather than
  changing admission in a performance change.

## Consequences

`admit` on a `Flat(8) x Flat(8)` table drops from ~5 s to ~0.25 s (about
20x), with or without a spec; what remains is reading the maps (one walk of
each boxed `Value`, memory-bound). `prim_from_bits` on a 2^16-entry prim runs
about 2x faster and `lower` of one about 5x. New whole-domain checks over a
prim should take a `BitTable`.
