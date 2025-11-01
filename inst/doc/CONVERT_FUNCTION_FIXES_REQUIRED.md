# CRITICAL FIXES REQUIRED for convert_emu_to_eaf()

## Overview
This document specifies the exact requirements for the `convert_emu_to_eaf()` function in the reindeer R package to ensure it produces valid EAF 3.0 files for ALL annot.json files in ae_emuDB, regardless of align_items mode.

---

## MANDATORY FIXES

### 1. ROOT ELEMENT - ANNOTATION_DOCUMENT

**MUST HAVE these attributes:**
```xml
<ANNOTATION_DOCUMENT
    AUTHOR="emuR"                                    ← REQUIRED
    DATE="2025-11-01T15:00:00+00:00"                ← REQUIRED (ISO 8601 with timezone)
    FORMAT="3.0"                                     ← REQUIRED
    VERSION="3.0"                                    ← REQUIRED
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xsi:noNamespaceSchemaLocation="http://www.mpi.nl/tools/elan/EAFv3.0.xsd">
```

**Current Issues to Fix:**
- [ ] Ensure DATE has timezone (use `format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")`)
- [ ] Ensure VERSION="3.0" (not missing or wrong value)
- [ ] Ensure FORMAT="3.0" is present
- [ ] Ensure xmlns declarations are correct

---

### 2. TIME_ORDER Element

**CRITICAL REQUIREMENTS:**

```r
# Time conversion formula (MUST BE EXACT):
time_ms <- round((sampleStart / sampleRate) * 1000)

# For end time:
end_time_ms <- round(((sampleStart + sampleDur) / sampleRate) * 1000)

# VALIDATION:
# - end_time_ms MUST BE >= start_time_ms
# - All TIME_SLOT_IDs MUST be unique
# - TIME_VALUE must be non-negative integer
```

**Required Structure:**
```xml
<TIME_ORDER>
    <TIME_SLOT TIME_SLOT_ID="ts1" TIME_VALUE="0"/>
    <TIME_SLOT TIME_SLOT_ID="ts2" TIME_VALUE="500"/>
    ...
</TIME_ORDER>
```

**Fixes Needed:**
- [ ] Collect ALL unique time points before creating TIME_ORDER
- [ ] De-duplicate time slots with same TIME_VALUE
- [ ] Ensure TIME_SLOT_IDs are globally unique (use counter)
- [ ] Sort time slots by TIME_VALUE (recommended)
- [ ] Verify no time slot has TIME_VALUE where start > end

---

### 3. LINGUISTIC_TYPE Definitions

**MUST create one per tier BEFORE creating tiers:**

```xml
<!-- For SEGMENT tiers or align_items=TRUE -->
<LINGUISTIC_TYPE 
    LINGUISTIC_TYPE_ID="word-lt"
    TIME_ALIGNABLE="true"
    CONSTRAINTS="Time_Subdivision"      ← Add if tier has parent
    GRAPHIC_REFERENCES="false"/>

<!-- For ITEM tiers with align_items=FALSE -->
<LINGUISTIC_TYPE 
    LINGUISTIC_TYPE_ID="word-lt"
    TIME_ALIGNABLE="false"
    CONSTRAINTS="Symbolic_Subdivision"   ← Add if tier has parent
    GRAPHIC_REFERENCES="false"/>
```

**Decision Tree for TIME_ALIGNABLE:**
```r
if (level_type == "SEGMENT" || level_type == "EVENT") {
    time_alignable <- TRUE
} else if (level_type == "ITEM") {
    if (align_items == TRUE && item_has_linked_segment) {
        time_alignable <- TRUE
    } else {
        time_alignable <- FALSE
    }
}
```

**Decision Tree for CONSTRAINTS:**
```r
if (tier_has_parent) {
    if (time_alignable == TRUE) {
        # Check if children fill parent completely
        if (children_fill_parent_no_gaps) {
            constraint <- "Time_Subdivision"
        } else {
            constraint <- "Included_In"
        }
    } else {
        # REF_ANNOTATION mode
        if (multiple_children_per_parent_ordered) {
            constraint <- "Symbolic_Subdivision"
        } else {
            constraint <- "Symbolic_Association"
        }
    }
}
```

**Fixes Needed:**
- [ ] Analyze emuR links to determine parent-child relationships
- [ ] Set TIME_ALIGNABLE correctly based on level type and align_items
- [ ] Add CONSTRAINTS attribute for child tiers
- [ ] Ensure all LINGUISTIC_TYPE_IDs are unique

---

### 4. TIER Element Structure

**REQUIRED attributes:**
```xml
<TIER 
    TIER_ID="Word"                          ← REQUIRED, unique
    LINGUISTIC_TYPE_REF="word-lt"           ← REQUIRED, must exist
    PARENT_REF="Utterance">                 ← REQUIRED if tier has parent
```

**Fixes Needed:**
- [ ] Parse emuR `linkDefinitions` to determine tier hierarchy
- [ ] Map emuR level names to TIER_IDs (use level name directly)
- [ ] Set PARENT_REF for child tiers based on links
- [ ] Create tiers in correct order (parents before children)
- [ ] Ensure no circular dependencies

---

### 5. ALIGNABLE_ANNOTATION (align_items=TRUE mode)

**REQUIRED structure:**
```xml
<ANNOTATION>
    <ALIGNABLE_ANNOTATION 
        ANNOTATION_ID="a1"              ← REQUIRED, globally unique
        TIME_SLOT_REF1="ts1"            ← REQUIRED, must exist in TIME_ORDER
        TIME_SLOT_REF2="ts2">           ← REQUIRED, must exist in TIME_ORDER
        <ANNOTATION_VALUE>hello</ANNOTATION_VALUE>
    </ALIGNABLE_ANNOTATION>
</ANNOTATION>
```

**Critical Logic:**
```r
# For SEGMENT/EVENT:
start_ms <- round((item$sampleStart / sample_rate) * 1000)
end_ms <- round(((item$sampleStart + item$sampleDur) / sample_rate) * 1000)

# For ITEM with align_items=TRUE:
# 1. Find linked SEGMENT/POINT via emuR links
# 2. Extract that segment's sampleStart and sampleDur
# 3. Use those values for time calculation
# 4. Create ALIGNABLE_ANNOTATION with those times
```

**Fixes Needed:**
- [ ] For ITEMs, traverse emuR links to find dominated segments
- [ ] Use dominated segment's timing for ITEM when align_items=TRUE
- [ ] Ensure TIME_SLOT_REF1 and TIME_SLOT_REF2 point to existing TIME_SLOTs
- [ ] Verify TIME_VALUE(REF1) <= TIME_VALUE(REF2)
- [ ] Generate unique ANNOTATION_IDs (use global counter)
- [ ] Handle multiple labels (use first label value)

---

### 6. REF_ANNOTATION (align_items=FALSE mode)

**REQUIRED structure:**
```xml
<ANNOTATION>
    <REF_ANNOTATION 
        ANNOTATION_ID="a2"              ← REQUIRED, globally unique
        ANNOTATION_REF="a1"             ← REQUIRED, parent annotation's ID
        PREVIOUS_ANNOTATION="a_prev">   ← OPTIONAL, for ordering
        <ANNOTATION_VALUE>word1</ANNOTATION_VALUE>
    </REF_ANNOTATION>
</ANNOTATION>
```

**Critical Logic:**
```r
# Map item IDs to annotation IDs created earlier
item_to_annotation_map <- list()

# For each ITEM with align_items=FALSE:
# 1. Find parent item via emuR links (fromID -> toID)
# 2. Look up parent's annotation ID from map
# 3. Set ANNOTATION_REF to parent's annotation ID
# 4. If Symbolic_Subdivision, set PREVIOUS_ANNOTATION to previous sibling
```

**Fixes Needed:**
- [ ] Build mapping from emuR item IDs to EAF annotation IDs
- [ ] Parse emuR links to find parent annotations
- [ ] Set ANNOTATION_REF correctly
- [ ] For Symbolic_Subdivision, maintain PREVIOUS_ANNOTATION chain
- [ ] Ensure first child has NO PREVIOUS_ANNOTATION
- [ ] Each subsequent child points to previous sibling

---

### 7. Annotation Value Handling

**REQUIRED:**
```xml
<ANNOTATION_VALUE>text value here</ANNOTATION_VALUE>
```

**Must handle:**
- Empty values: `<ANNOTATION_VALUE></ANNOTATION_VALUE>` (valid, don't omit)
- Multiple labels: Use first label's value
- Special characters: XML escape (< > & " ')

**Fixes Needed:**
- [ ] Always create ANNOTATION_VALUE element (even if empty)
- [ ] Use first label if multiple labels present
- [ ] XML-escape special characters
- [ ] Handle NULL or missing labels gracefully

---

### 8. Time Slot Sharing (Time_Subdivision)

**CRITICAL for adjacent segments:**

```r
# Adjacent children on Time_Subdivision tier MUST share boundary time slot
# Example:
# Word1: ts1 (0ms) -> ts2 (500ms)
# Word2: ts2 (500ms) -> ts3 (1000ms)
#         ↑ SHARED TIME SLOT

# Algorithm:
# 1. Sort children by start time
# 2. For each adjacent pair:
#    - Child1 end time == Child2 start time
#    - Use SAME time slot ID for both
```

**Fixes Needed:**
- [ ] Detect Time_Subdivision tiers
- [ ] Sort child annotations by time
- [ ] Reuse time slot IDs at shared boundaries
- [ ] Verify no gaps (child2.start == child1.end)
- [ ] Verify no overlaps (child2.start >= child1.end)

---

### 9. HEADER and MEDIA_DESCRIPTOR

**REQUIRED if audio file exists:**
```xml
<HEADER MEDIA_FILE="" TIME_UNITS="milliseconds">
    <MEDIA_DESCRIPTOR 
        MEDIA_URL="file:///path/to/audio.wav"
        MIME_TYPE="audio/x-wav"
        RELATIVE_MEDIA_URL="./audio.wav"/>
</HEADER>
```

**Fixes Needed:**
- [ ] Extract media filename from annot.json `annotates` field
- [ ] Create absolute path for MEDIA_URL
- [ ] Create relative path for RELATIVE_MEDIA_URL
- [ ] Set MIME_TYPE="audio/x-wav" for WAV files
- [ ] Handle missing media gracefully

---

## TESTING REQUIREMENTS

### Test Case 1: Simple SEGMENT Tier
```r
# Input: One tier with SEGMENT type
# Expected: ALIGNABLE_ANNOTATION with correct times
# Validation: Must pass validator with exit code 0
```

### Test Case 2: ITEM with align_items=TRUE
```r
# Input: ITEM tier linked to SEGMENT tier
# Expected: ITEM gets ALIGNABLE_ANNOTATION with segment's times
# Validation: Times must match dominated segment
```

### Test Case 3: ITEM with align_items=FALSE
```r
# Input: ITEM tier linked to parent
# Expected: ITEM gets REF_ANNOTATION with ANNOTATION_REF
# Validation: Reference must point to existing annotation
```

### Test Case 4: Three-Level Hierarchy
```r
# Input: Utterance -> Word -> Phoneme
# Expected: Proper parent-child hierarchy in EAF
# Validation: All PARENT_REF attributes correct
```

### Test Case 5: Time_Subdivision
```r
# Input: Parent with non-overlapping children filling duration
# Expected: Shared time slots at boundaries
# Validation: No gaps, no overlaps
```

### Test Case 6: All ae_emuDB Files
```r
# Input: Every annot.json in ae_emuDB
# Expected: Valid EAF for both TRUE and FALSE modes
# Validation: ALL must pass validator
```

---

## VALIDATION WORKFLOW

```bash
# For each annot.json file:
for json_file in ae_emuDB/**/*_annot.json; do
    # Test TRUE mode
    Rscript -e "convert_emu_to_eaf('$json_file', 'output_TRUE.eaf', TRUE)"
    ./validate-eaf.sh output_TRUE.eaf || echo "FAILED: $json_file TRUE mode"
    
    # Test FALSE mode
    Rscript -e "convert_emu_to_eaf('$json_file', 'output_FALSE.eaf', FALSE)"
    ./validate-eaf.sh output_FALSE.eaf || echo "FAILED: $json_file FALSE mode"
done
```

---

## ERROR CHECKLIST

Run validator and check for these specific errors:

- [ ] "annotation has end time < begin time" → Fix time calculation
- [ ] "tier has parent but no stereotype CONSTRAINT" → Add CONSTRAINTS
- [ ] "time slot reference does not exist" → Fix TIME_SLOT_ID generation
- [ ] "tier type mismatch" → Fix TIME_ALIGNABLE setting
- [ ] "overlapping annotations" → Fix time slot sharing
- [ ] XML schema validation error → Check mandatory attributes

---

## IMPLEMENTATION PRIORITY

### Priority 1 (Critical - Will cause validation failure):
1. Correct time conversion (samples to milliseconds)
2. Unique ANNOTATION_IDs, TIME_SLOT_IDs, TIER_IDs
3. Valid TIME_SLOT references
4. Correct TIME_ALIGNABLE setting
5. Mandatory XML attributes (AUTHOR, DATE, VERSION)

### Priority 2 (Important - Best practices):
1. CONSTRAINTS attribute for child tiers
2. Time slot sharing for Time_Subdivision
3. PREVIOUS_ANNOTATION chain for Symbolic_Subdivision
4. Proper parent-child hierarchy
5. HEADER with MEDIA_DESCRIPTOR

### Priority 3 (Nice to have):
1. Sort TIME_SLOTs by value
2. Meaningful TIER_IDs
3. Relative media URLs
4. Proper XML indentation

---

## SUCCESS CRITERIA

✅ **ALL** annot.json files in ae_emuDB must:
1. Convert without errors (both modes)
2. Pass EAF validator with exit code 0
3. Have no ERROR messages in validator output
4. Open successfully in ELAN application
5. Display correct tier hierarchy
6. Show annotations at correct times
7. Preserve all annotation text values

---

## DEBUGGING TIPS

1. **If time values are wrong:**
   - Check sample rate is from annot.json, not hardcoded
   - Verify formula: `(samples / rate) * 1000`
   - Check for integer overflow

2. **If references don't exist:**
   - Print all TIME_SLOT_IDs before referencing
   - Print all ANNOTATION_IDs before referencing
   - Check for typos in ID generation

3. **If TIME_ALIGNABLE is wrong:**
   - Check level type (SEGMENT/EVENT → true, ITEM → depends)
   - Check align_items parameter
   - Verify link structure for ITEMs

4. **If hierarchy is wrong:**
   - Parse linkDefinitions from emuR
   - Map fromID/toID to tier names
   - Check parent tiers exist before children

---

## CONTACT FOR ISSUES

If validation fails after implementing these fixes:
1. Run test_all_conversions.R script
2. Check validation_results.csv for specific failures
3. Run validator manually to see detailed errors
4. Consult EMUR_TO_EAF_VALIDATION_GUIDE.md for details

---

*Document Version: 1.0*  
*Date: November 1, 2025*  
*Critical for: reindeer R package convert_emu_to_eaf() function*
