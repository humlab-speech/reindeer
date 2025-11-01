# reindeer 0.2.2

## New Features

* Added `create_cmdi_metadata()` function for generating CLARIN-compliant CMDI XML files
  - Supports multiple CMDI profiles (media-corpus, speech-corpus, speech-corpus-dlu)
  - Automatically collects metadata from database structure
  - Reads participant information from .meta_json files at session/bundle level
  - Generates comprehensive metadata including participants, resources, annotations
  - Includes placeholders for planned metadata additions

* Added CMDI validation script (`inst/scripts/validate-cmdi.sh`)
  - Validates XML well-formedness
  - Checks CMDI namespace compliance
  - Verifies required elements
  - Detects placeholder fields
  - Assesses metadata completeness
  - Validates resource references

## Documentation

* Added comprehensive CMDI generation guide (`inst/doc/CMDI_METADATA_GENERATION.md`)
  - Complete function usage examples
  - .meta_json file format specifications
  - CMDI profile descriptions
  - Placeholder field specifications (PROJECT.json, PUBLICATIONS.json, ETHICS.json, QUALITY.json)
  - Integration guide for CLARIN repositories
  - Best practices and troubleshooting

* Added metadata templates (`inst/templates/`)
  - session_meta_template.json
  - bundle_meta_template.json
  - PROJECT.json

## Metadata Infrastructure

* Session-level metadata support via `.meta_json` files
  - Participant demographics (age, gender, language, dialect)
  - Recording details (date, location, equipment, sample rate)
  - Session information (task, duration, notes)

* Bundle-level metadata support
  - Stimulus information
  - Repetition tracking
  - Annotation quality metrics

* Planned additions (with placeholders):
  - Project funding information
  - Related publications
  - Ethical approval details
  - Quality control procedures

## CLARIN Integration

* Full CMDI 1.2 specification compliance
* Three supported profiles:
  - media-corpus (clarin.eu:cr1:p_1387365569699)
  - SpeechCorpusWithParticipants (clarin.eu:cr1:p_1392642184799)
  - SpeechCorpus-DLU (clarin.eu:cr1:p_1381926654456)

* Generated CMDI files ready for upload to:
  - The Language Archive (TLA)
  - LINDAT/CLARIAH-CZ
  - Other CLARIN repositories

# reindeer 0.2.1

## Documentation

* Added comprehensive EAF validation documentation for `convert_emu_to_eaf()` function
  - **EMUR_TO_EAF_VALIDATION_GUIDE.md**: Complete EAF 3.0 specification and validation rules
  - **EMUR_TO_EAF_CHECKLIST.md**: Quick reference validation checklist
  - **CONVERT_FUNCTION_FIXES_REQUIRED.md**: Implementation requirements and testing guide

* These documents ensure the `convert_emu_to_eaf()` function produces valid EAF 3.0 files for all emuR annotation files, supporting both `align_items=TRUE` and `align_items=FALSE` modes

## Validation

* Documentation includes:
  - Complete EAF 3.0 specification requirements
  - Mode-specific conversion rules (ALIGNABLE_ANNOTATION vs REF_ANNOTATION)
  - Test cases with expected outputs
  - Common errors and troubleshooting guide
  - Integration with EAF validator tools
  - Priority-ordered implementation requirements

## Key Features Documented

* **align_items=TRUE**: ITEMs receive start/end times from dominated SEGMENTS/POINTs → `ALIGNABLE_ANNOTATION`
* **align_items=FALSE**: ITEMs become symbolic references → `REF_ANNOTATION`
* Proper TIME_ORDER generation with unique TIME_SLOT_IDs
* LINGUISTIC_TYPE definitions with correct TIME_ALIGNABLE settings
* TIER hierarchy with proper PARENT_REF and CONSTRAINTS attributes
* Time slot sharing for Time_Subdivision constraints
* PREVIOUS_ANNOTATION chains for Symbolic_Subdivision

# reindeer 0.2.0

Previous release (see prior commit history)
