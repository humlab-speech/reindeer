# Metadata System Update

## Overview

The reindeer metadata system has been updated to store database-level defaults in a separate `.meta_json` file rather than in the database configuration file. This improves compatibility with other EMU tools.

## Changes

### Previous Behavior
Database-level metadata defaults were stored in the `metadataDefaults` field of `<dbname>_DBconfig.json`:

```json
{
  "name": "ae",
  "UUID": "...",
  "metadataDefaults": {
    "Accent": "Northern",
    "Elicitation": "Scripted"
  }
}
```

### New Behavior
Database-level metadata defaults are now stored in `<dbname>.meta_json` in the database root directory:

```
ae_emuDB/
├── ae_DBconfig.json          # Configuration only, no metadata
├── ae.meta_json              # Database-level defaults
├── 0000_ses/
│   ├── 0000.meta_json        # Session-level defaults
│   └── msajc003_bndl/
│       └── msajc003.meta_json # Bundle-specific metadata
```

## Benefits

1. **Better separation of concerns**: Configuration separate from metadata
2. **Improved compatibility**: Other EMU tools don't expect `metadataDefaults` in DBconfig
3. **Consistent structure**: All metadata levels use `.meta_json` files

## API Compatibility

All existing functions work identically:

```r
# Add database-level metadata (now writes to <dbname>.meta_json)
add_metadata(ae_db, list("Accent" = "Northern", "Elicitation" = "Scripted"))

# Export/import still work the same way
export_metadata(ae_db, "metadata.xlsx")
import_metadata(ae_db, "metadata.xlsx")

# Get metadata still returns the same structure
meta <- get_metadata(ae_db)
```

## Migration

Existing databases with `metadataDefaults` in DBconfig are not automatically migrated. If needed, you can manually migrate:

```r
# Read from DBconfig
config <- jsonlite::read_json(file.path(db_path, "ae_DBconfig.json"))
if (!is.null(config$metadataDefaults)) {
  # Write to .meta_json
  jsonlite::write_json(
    config$metadataDefaults, 
    file.path(db_path, "ae.meta_json"),
    auto_unbox = TRUE, 
    pretty = TRUE
  )
  
  # Optionally remove from config
  config$metadataDefaults <- NULL
  jsonlite::write_json(config, file.path(db_path, "ae_DBconfig.json"), 
                      auto_unbox = TRUE, pretty = TRUE)
}
```

## Files Modified

- `R/reindeer_metadata.R`: Updated old implementation
- `R/reindeeR_metadata_optimized.R`: Updated optimized implementation

Both implementations now use `<dbname>.meta_json` for database-level defaults.
