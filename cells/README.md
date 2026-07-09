# Cell library (ADR-0027)

Hand-authored `.subckt` files — the only hand-authored SPICE in the repo
(ADR-0025 B5.2). One logic cell family: the k-controlled dual-rail swap,
k ∈ {0,1,2}; plus bench source/sink and baseline comparators (static
CMOS, PFAL) that exist to lose gracefully in the money plot.

Admission = the B5 harness suite passing: conservation bookkeeping,
functional verdict via typed decode, reverse-drive (TH-6), and
characterization across the ramp × load × Vt × body sweep (ADR-0024).
Every cell ships the ADR-0027 B4.3 contract.
