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
