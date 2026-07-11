//! De-risk: confirm serde derive + ron round-trip build and run under the gate.
//! (Throwaway proof for the deps adoption; the real consolidation replaces the
//! hand-rolled RON in chardb/artifact with this mechanism.)

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
struct CharPointProbe {
    t_ramp: f64,
    energy: f64,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
struct CellCharProbe {
    key: String,
    points: Vec<CharPointProbe>,
}

#[test]
fn serde_ron_round_trip() {
    let cell = CellCharProbe {
        key: "buffer_std_body-to-rail".to_string(),
        points: vec![
            CharPointProbe { t_ramp: 1e-9, energy: 3.2e-15 },
            CharPointProbe { t_ramp: 1e-8, energy: 3.3e-16 },
        ],
    };
    let text = ron::ser::to_string_pretty(&cell, ron::ser::PrettyConfig::default())
        .expect("serialize");
    let back: CellCharProbe = ron::from_str(&text).expect("deserialize");
    assert_eq!(cell, back, "ron round-trip must be lossless");
    // Determinism: same value serializes byte-identically.
    let text2 = ron::ser::to_string_pretty(&cell, ron::ser::PrettyConfig::default())
        .expect("serialize");
    assert_eq!(text, text2, "serialization must be deterministic");
}
