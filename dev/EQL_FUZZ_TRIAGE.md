# EQL fuzzer triage

`Rscript tools/fuzz-eql.R [n] [seed]` generates random simple, sequence,
dominance, conjunction, projection and `Num()` queries over `ae` and compares
reindeer with emuR (row count, labels, start, end). Local only: it needs emuR.
Workflow: a mismatch that is a reindeer bug gets a case in
`tools/make-eql-golden.R` (then regenerate the golden file) and a fix; an emuR
quirk is listed here and not "fixed".

## Fixed

- `Num(parent, child)` counted link paths, not distinct children. In `ae`
  two Phonemes of one Syllable link to the same Phonetic (msajc010, item 111),
  so reindeer reported 2 where emuR reports 1. Now `COUNT(DISTINCT ...)`;
  three golden cases added. (Seeds 1, 2; 300-400 queries each.)

## emuR quirks (not reindeer bugs)

- `Num(Word, Text)`: `Text` is an attribute of `Word`, not a level. emuR
  aborts; reindeer resolves the attribute to its level and returns rows.
  Kept deliberately (attribute-to-level resolution is documented).
