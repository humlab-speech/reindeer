# Remove MOMEL/INTSINT Code Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Remove all MOMEL/INTSINT-related bundled files from the reindeer package and clean up vignette references.

**Architecture:** Pure deletion + minimal edits. No new code. No tests to write (tests don't cover the deleted directories). Verify with `devtools::load_all()` and `devtools::check()` after removal.

**Tech Stack:** R package structure, git rm, devtools

---

### Task 1: Delete inst/pymomelintsint/

**Files:**
- Delete: `inst/pymomelintsint/` (entire directory)

**Step 1: Remove the directory from git**

```bash
git rm -r inst/pymomelintsint/
```

Expected: git stages all deletions under `inst/pymomelintsint/`.

**Step 2: Verify removal**

```bash
ls inst/pymomelintsint 2>/dev/null || echo "gone"
```

Expected: `gone`

---

### Task 2: Delete inst/praat/Momel-Intsint/

**Files:**
- Delete: `inst/praat/Momel-Intsint/` (entire directory)

**Step 1: Remove the directory from git**

```bash
git rm -r "inst/praat/Momel-Intsint/"
```

Expected: git stages all deletions under `inst/praat/Momel-Intsint/`.
Note: the PDF and files with spaces in names are handled automatically by `git rm -r`.

**Step 2: Verify removal**

```bash
ls "inst/praat/Momel-Intsint" 2>/dev/null || echo "gone"
```

Expected: `gone`

---

### Task 3: Update inst/agents/PRAAT_MODIFICATIONS.md

This doc was written specifically to document the now-deleted code. Remove it entirely.

**Files:**
- Delete: `inst/agents/PRAAT_MODIFICATIONS.md`

**Step 1: Remove from git**

```bash
git rm inst/agents/PRAAT_MODIFICATIONS.md
```

---

### Task 4: Update vignettes/reindeer_workflow.qmd

Remove the two-line "Prosodic Analysis with MOMEL/INTSINT" subsection (lines 493-495).

**Files:**
- Modify: `vignettes/reindeer_workflow.qmd`

**Step 1: Delete the subsection**

Remove these exact lines:
```
## Prosodic Analysis with MOMEL/INTSINT

For MOMEL/INTSINT prosodic annotation, see the [protoscribe](https://github.com/humlab-speech/protoscribe) companion package.
```

---

### Task 5: Update vignettes/cache_management.Rmd

Remove the `momel_intsint_20251020.sqlite` line from the example output block (line 79).
The example is showing a cache listing — this file would no longer appear since the draft cache system is in protoscribe, not reindeer.

**Files:**
- Modify: `vignettes/cache_management.Rmd`

**Step 1: Delete the example line**

Remove this line from within the fenced code block:
```
momel_intsint_20251020.sqlite (245.67 MB) [draft]
```

---

### Task 6: Update vignettes/transcription_workflow.Rmd

The vignette already says "this has moved to protoscribe" — only the code example calls `protoscribe::draft_momel_intsint()`. Since that's a protoscribe call (not reindeer), no reindeer code changes here.

However, the example implies reindeer bundles momel/intsint. Replace the code example with a generic protoscribe pointer that does not mention momel/intsint specifically.

**Files:**
- Modify: `vignettes/transcription_workflow.Rmd`

**Step 1: Replace the code chunk**

Replace the current `{r protoscribe-example}` chunk (lines 30-49):
```r
library(reindeer)
library(protoscribe)

# Load corpus with reindeer
corp <- corpus("path/to/database_emuDB")

# Use protoscribe for annotation drafting
suggestions <- protoscribe::draft_momel_intsint(
  corpus = corp,
  session = "session001",
  bundle = "bundle001"
)

# Validate suggestions
suggestions <- protoscribe::assess(suggestions)

# Apply to database
protoscribe::transcribe(suggestions)
```

With:
```r
library(reindeer)
library(protoscribe)

# Load corpus with reindeer
corp <- corpus("path/to/database_emuDB")

# Use protoscribe for annotation drafting
# See protoscribe documentation for available draft_*() functions
suggestions <- protoscribe::draft_vad(corpus = corp)

# Validate suggestions
suggestions <- protoscribe::assess(suggestions)

# Apply to database
protoscribe::transcribe(suggestions)
```

---

### Task 7: Update inst/agents/AGENT_GUIDE.md

The guide was just updated (in v0.4.9) and does not directly reference the deleted directories. However, verify no mention of momel/intsint remains.

**Files:**
- Modify: `inst/agents/AGENT_GUIDE.md` (if references found)

**Step 1: Check for references**

```bash
grep -n "momel\|intsint\|MOMEL\|INTSINT\|pymomelintsint\|Momel-Intsint" inst/agents/AGENT_GUIDE.md
```

If any lines are found, remove them or replace with a note pointing to protoscribe.

---

### Task 8: Update PRAAT_MODIFICATIONS reference in AGENT_GUIDE.md

Since `PRAAT_MODIFICATIONS.md` is being deleted, remove any reference to it from `AGENT_GUIDE.md`.

**Files:**
- Modify: `inst/agents/AGENT_GUIDE.md`

**Step 1: Check for references**

```bash
grep -n "PRAAT_MODIFICATIONS" inst/agents/AGENT_GUIDE.md
```

Remove any lines found.

---

### Task 9: Update CLAUDE.md references

**Files:**
- Modify: `CLAUDE.md`

**Step 1: Check for references**

```bash
grep -n "momel\|intsint\|MOMEL\|INTSINT\|pymomelintsint\|Momel-Intsint\|PRAAT_MODIFICATIONS" CLAUDE.md
```

Remove or update any lines that refer to the deleted files/directories.

---

### Task 10: Verify package loads cleanly

```bash
Rscript -e "devtools::load_all(quiet=TRUE); cat('OK\n')"
```

Expected: `OK` with no errors.

---

### Task 11: Bump version and commit

**Step 1: Bump version to 0.4.10 in DESCRIPTION and inst/CITATION**

In `DESCRIPTION`: change `Version: 0.4.9` → `Version: 0.4.10`
In `inst/CITATION`: change both occurrences of `0.4.9` → `0.4.10`

**Step 2: Commit everything**

```bash
git add -A
git commit -m "chore: remove MOMEL/INTSINT bundled code (v0.4.10)

- Delete inst/pymomelintsint/ (Python reimplementation, Perl scripts, binaries)
- Delete inst/praat/Momel-Intsint/ (Hirst plugin, momel binaries, C sources)
- Delete inst/agents/PRAAT_MODIFICATIONS.md (now obsolete)
- Clean up vignette references to momel/intsint
- Bump version 0.4.9 → 0.4.10"
```
