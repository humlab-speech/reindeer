# Converting `create_ae_db()` to `data(ae)` Pattern

## Current Implementation

Currently, the `ae` demo database is accessed via:

```r
reindeer:::create_ae_db() -> ae
```

This function:
1. Creates the emuR demo data in `tempdir()` using `emuR::create_emuRdemoData()`
2. Loads the `ae_emuDB` database using `emuR::load_emuDB()`
3. Returns an `emuDBhandle` object

## Could This Be a `data()` Package?

**Short Answer**: Not directly, but there are better alternatives.

## Why `data(ae)` Won't Work Directly

The `data()` function in R is designed for:
- Loading **serialized R objects** (`.rda`, `.RData` files)
- Objects that can be saved with `save()` and loaded with `load()`
- Typically: data frames, vectors, lists, matrices

**Problems with emuDB databases:**

1. **Size**: The `ae_emuDB` is ~5.4 MB with 29 files
   - Too large for package data (CRAN limit is 5 MB for entire package)
   - Would bloat the package significantly

2. **Structure**: emuDB databases are directory-based with:
   - JSON configuration files (`_DBconfig.json`)
   - Annotation files (`_annot.json`)
   - Audio files (`.wav`)
   - Signal files (`.fms`, `.dft`)
   - Metadata files (`.meta_json`)
   - SQLite cache (`_emuDBcache.sqlite`)
   
   This directory structure can't be serialized as a simple R object.

3. **File Paths**: emuDBhandle objects contain absolute paths:
   ```r
   handle$basePath  # e.g., "/path/to/ae_emuDB"
   ```
   These paths would be invalid after serialization.

4. **Active Connections**: The handle may contain database connections that can't be serialized.

## Alternative Approaches

### Option 1: Ship Compressed Database (Recommended for Internal Use)

Store a compressed version in `inst/extdata/`:

```r
# In package structure:
inst/
  extdata/
    ae_emuDB.tar.gz  # Compressed database

# Access function:
#' @export
get_ae_db <- function(verbose = TRUE) {
  # Extract to temp directory
  db_archive <- system.file("extdata", "ae_emuDB.tar.gz", package = "reindeer")
  temp_dir <- tempfile("reindeer_ae_")
  dir.create(temp_dir)
  
  # Extract
  utils::untar(db_archive, exdir = temp_dir)
  
  # Load and return
  db_path <- file.path(temp_dir, "ae_emuDB")
  emuR::load_emuDB(db_path, verbose = verbose)
}

# Usage:
library(reindeer)
ae <- get_ae_db()
corp <- corpus(ae)
```

**Pros:**
- Database is always available (no external dependency on emuR's demo data)
- Extraction is fast (~1 second)
- Can customize the database for reindeer-specific examples

**Cons:**
- Adds ~5 MB to package (might exceed CRAN limits if package is already large)
- Users still need to manage temporary directories

### Option 2: Lazy Loading with corpus() Constructor (Best)

Since reindeer uses the `corpus` S7 class, you can create a pre-built corpus object:

```r
# Create a serializable corpus reference
# Save this in data/ae_corpus.rda:

ae_corpus_ref <- list(
  source = "emuR::create_emuRdemoData",
  db_name = "ae",
  description = "Acoustic-phonetic database for demos"
)

# Then create a smart constructor:
#' @export
ae <- function(verbose = FALSE) {
  # Use emuR's demo data
  demodir <- file.path(tempdir(), "emuR_demoData")
  if (!dir.exists(demodir)) {
    if (verbose) message("Creating emuR demo data...")
    emuR::create_emuRdemoData()
  }
  
  db <- emuR::load_emuDB(file.path(demodir, "ae_emuDB"), verbose = verbose)
  corpus(db, verbose = verbose)
}

# Usage:
library(reindeer)
corp <- ae()  # Returns a corpus object directly!
glimpse(corp)
```

**Pros:**
- No package size increase
- Returns a reindeer `corpus` object (not just emuDBhandle)
- Consistent with reindeer's design
- Clean, simple API: `ae()` instead of `reindeer:::create_ae_db()`

**Cons:**
- Still depends on emuR's demo data
- First call creates data in tempdir (but cached for session)

### Option 3: Data Package Reference (For Documentation)

If you want `data(ae)` to work for documentation purposes:

```r
# In data/ae.R (using roxygen2 for documentation):
#' AE Demo Corpus Reference
#'
#' Reference object for the ae demo corpus. Use ae() function to load.
#'
#' @format A list with corpus metadata
#' @source emuR package demo data
#' @seealso [ae()] to load the corpus
#' @examples
#' \dontrun{
#' # Load the ae corpus
#' corp <- ae()
#' glimpse(corp)
#' }
"ae_ref"

# In R/data.R:
ae_ref <- list(
  name = "ae",
  description = "American English vowel corpus",
  sessions = 1,
  bundles = 7,
  source = "emuR::create_emuRdemoData()"
)

# Save with:
# usethis::use_data(ae_ref, overwrite = TRUE)

# Then users can do:
data(ae_ref)  # Loads metadata
corp <- ae()  # Loads actual corpus
```

**Pros:**
- Follows R conventions for data packages
- Good for documentation
- Minimal package size impact

**Cons:**
- `data(ae_ref)` doesn't load the corpus itself (just metadata)
- Might confuse users expecting `data()` to load everything

## Recommendation: Hybrid Approach

**Best solution for reindeer:**

1. **Export `ae()` as a function** (Option 2):
   ```r
   #' @export
   ae <- function(verbose = FALSE) {
     # Implementation as shown above
   }
   ```

2. **Update all examples** to use `ae()` instead of `reindeer:::create_ae_db()`:
   ```r
   # Old:
   reindeer:::create_ae_db() -> ae_handle
   corp <- corpus(ae_handle)
   
   # New:
   corp <- ae()  # Much cleaner!
   ```

3. **Optionally add `data(ae_info)`** for documentation:
   ```r
   #' @export
   #' @rdname ae
   ae_info <- list(
     bundles = 7,
     sessions = 1,
     duration = "~3 minutes of speech",
     speakers = 1,
     annotation_levels = c("Utterance", "Intonational", "Intermediate", 
                           "Word", "Syllable", "Phoneme", "Phonetic", "Tone", "Foot")
   )
   ```

## Implementation Steps

### Step 1: Create the `ae()` function

```r
# In R/reindeer_demo_data.R

#' Load the AE Demo Corpus
#'
#' Loads the ae (American English) demo corpus from emuR package and returns
#' a reindeer corpus object ready for analysis.
#'
#' @param verbose Logical. Show loading messages? Default FALSE.
#' @return A corpus object
#' 
#' @details
#' The ae corpus contains:
#' - 7 bundles from 1 session
#' - ~3 minutes of speech
#' - Phonetic, phonemic, syllabic, and prosodic annotations
#' 
#' Data is loaded from emuR's demo data and cached in the temporary directory
#' for the duration of the R session.
#'
#' @examples
#' # Load the corpus
#' corp <- ae()
#' 
#' # View structure
#' glimpse(corp)
#' 
#' # Query phonetic segments
#' segs <- ask_for(corp, "Phonetic == n")
#' print(segs)
#'
#' @export
ae <- function(verbose = FALSE) {
  demodir <- file.path(tempdir(), "emuR_demoData")
  
  if (!dir.exists(demodir)) {
    if (verbose) {
      cli::cli_alert_info("Creating emuR demo data...")
    }
    emuR::create_emuRdemoData()
  }
  
  db <- emuR::load_emuDB(
    file.path(demodir, "ae_emuDB"), 
    verbose = verbose
  )
  
  corpus(db, verbose = verbose)
}
```

### Step 2: Update Examples Throughout Package

Replace all instances of:
```r
reindeer:::create_ae_db() -> ae
corp <- corpus(ae)
```

With:
```r
corp <- ae()
```

### Step 3: Update Tests

```r
# In tests/testthat/test_*.R

test_that("corpus creation works", {
  corp <- ae()
  expect_s3_class(corp, "reindeer::corpus")
})
```

### Step 4: Optional Data Documentation

```r
# In R/data.R

#' AE Corpus Information
#'
#' Metadata about the ae (American English) demo corpus.
#'
#' @format A list with corpus characteristics:
#' \describe{
#'   \item{bundles}{Number of bundles (7)}
#'   \item{sessions}{Number of sessions (1)}
#'   \item{duration}{Approximate speech duration}
#'   \item{speakers}{Number of speakers (1)}
#'   \item{annotation_levels}{Vector of annotation level names}
#' }
#' @source emuR package demo data
#' @seealso [ae()] to load the corpus
"ae_info"
```

## Comparison Summary

| Approach | API | Size Impact | Complexity | Recommended |
|----------|-----|-------------|------------|-------------|
| `create_ae_db()` (current) | `reindeer:::create_ae_db()` | None | Low | ❌ (internal) |
| Ship compressed DB | `get_ae_db()` | +5 MB | Medium | ⚠️ (if needed) |
| **Function constructor** | **`ae()`** | **None** | **Low** | ✅ **Best** |
| data() reference | `data(ae_ref)` then `ae()` | Minimal | Medium | ⚠️ (optional) |

## Conclusion

**The `ae()` function approach is the best solution** because:

1. ✅ **Clean API**: `corp <- ae()` is much better than `reindeer:::create_ae_db()`
2. ✅ **No size penalty**: Uses emuR's existing demo data
3. ✅ **Returns corpus object**: Users get the reindeer S7 class directly
4. ✅ **Easy to implement**: Just one exported function
5. ✅ **Follows R conventions**: Functions like `data()`, `example()` are common patterns
6. ✅ **Well-documented**: Can have comprehensive roxygen2 documentation

Traditional `data(ae)` loading doesn't make sense for database objects with complex file structures and active connections. The function pattern is more appropriate and actually provides better UX.
