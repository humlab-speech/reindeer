# Whole-file vs windowed DSP (F1.3)

Reproduce: `Rscript tools/fidelity-whole-file.R` (`ae`, 253 Phonetic segments,
`.at = 0.5`, `.mode = "bundle"` vs `"window"`, rows where both modes give a
non-zero value). Measured 2026-10-11, reindeer 3.0.0, superassp 3.x.

| Routine | Column | n | median abs diff | median rel diff |
|---|---|---|---|---|
| `trk_formant_forest` | F1-F4, B1-B4 (Hz) | 175-248 | 0 | 0 |
| `trk_pitch_ksv` | fo_Hz | 62 | 17.1 Hz | n/a |
| `trk_pitch_ac` | F0 | 99 | 18.4 Hz | n/a |
| `trk_spectral_moments` | cog | 253 | 117.6 Hz | 20 % |
| `trk_spectral_moments` | sd / skewness / kurtosis | 253 | 109 Hz / 1.1 / 11.4 | 17-69 % |
| `trk_intensity` | intensity | 252 | 6.4 dB | n/a |
| `trk_cpps` | cpp | 77 | 9.7 | n/a |
| `trk_pitch_rapt` | | | `.at` selects a frame but the routine returned no track | |

## Reading

- **Forest** is identical: the routine handles its own begin/end window, so
  both modes give the same numbers.
- **Everything else differs, and the windowed value is the less faithful one.**
  A windowed call gives the routine only the segment: no left context for
  pitch trackers (octave and voicing errors at onsets), a single short frame
  for spectral moments, a different level reference for intensity. emuR's
  semantics are whole-file SSFF tracks sliced per segment, which is exactly
  what `.mode = "bundle"` does.
- **Decision:** `"bundle"` stays the default for every routine; no allow-list
  of windowed routines is needed. `.mode = "window"` remains for users who
  want the segment analysed in isolation (e.g. when the routine is expensive
  and segments are few, 0.73 s vs 0.3 s in `RESULTS_2.5.0.md`), and the
  `quantify()` reference now says that results differ by routine.
- `frame_time` is file-relative in bundle mode and window-relative in window
  mode; it is excluded from the comparison.
