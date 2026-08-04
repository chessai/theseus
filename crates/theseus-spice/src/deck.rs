//! Shared SPICE deck assembly helpers.

/// Append the standard control-block/deck footer (`.endc` then `.end`) to a
/// deck's line buffer and join it into the final deck text (newline-separated,
/// trailing newline). Every deck emitter ends this way; the bytes are
/// snapshot-tested, so this must reproduce them exactly.
pub(crate) fn finalize_deck(mut lines: Vec<String>) -> String {
    lines.push(".endc".into());
    lines.push(".end".into());
    lines.join("\n") + "\n"
}
