//! Regenerate the golden rung-0 characterization DB (`data/rung0-chardb.ron`).
//!
//! This runs [`build_char_db`] with `DeviceRung::Rung0Level1` over
//! periods `[1e-8, 1e-7, 1e-6, 1e-5]` and prints the resulting RON to
//! stdout. Redirect to update the committed artifact:
//!
//! ```sh
//! cargo run -p theseus-spice --example characterize_rung0 > crates/theseus-spice/data/rung0-chardb.ron
//! ```
//!
//! **Warning:** this runs live ngspice sweeps over all admitted
//! reversible cells and takes ~15 minutes.

use theseus_spice::adiabatic::DeviceRung;
use theseus_spice::chardb::build_char_db;

fn main() {
    let periods = [1e-8, 1e-7, 1e-6, 1e-5];
    let db = build_char_db(DeviceRung::Rung0Level1, &periods);
    print!("{}", db.to_ron());
}
