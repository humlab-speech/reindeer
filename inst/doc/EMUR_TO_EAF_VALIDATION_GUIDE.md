# emuR to EAF Conversion Validation Guide

## Overview

This document provides validation rules for the `convert_emu_to_eaf()` function from the reindeer R package, which converts emuR `*_annot.json` files to EAF (ELAN Annotation Format) 3.0 format.

**Two Conversion Modes:**
1. **`align_items=TRUE`**: ITEMs receive start/end times from SEGMENTS/POINTs they dominate → `ALIGNABLE_ANNOTATION`
2. **`align_items=FALSE`**: ITEMs become symbolic references → `REF_ANNOTATION`

---

## Part 1: Mandatory EAF 3.0 Structure

### Root Element: `<ANNOTATION_DOCUMENT>`

**Required Attributes:**
```xml
<ANNOTATION_DOCUMENT 
    AUTHOR="[creator name or program]"           ← REQUIRED
    DATE="2025-11-01T14:00:00Z"                  ← REQUIRED (xsd:dateTime format)
    VERSION="3.0"                                 ← REQUIRED
    FORMAT="3.0"                                  ← RECOMMENDED
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xsi:noNamespaceSchemaLocation="http://www.mpi.nl/tools/elan/EAFv3.0.xsd">
```

**Validation Checks:**
- ✅ `AUTHOR` must be present and non-empty
- ✅ `DATE` must follow ISO 8601 format with timezone
- ✅ `VERSION` must be "3.0"

---

### `<HEADER>` Element

**Structure:**
```xml
<HEADER MEDIA_FILE="" TIME_UNITS="milliseconds">
    <MEDIA_DESCRIPTOR 
        MEDIA_URL="file:///path/to/audio.wav"    ← REQUIRED if media exists
        MIME_TYPE="audio/x-wav"                  ← REQUIRED if media exists
        RELATIVE_MEDIA_URL="./audio.wav"/>       ← OPTIONAL
    <PROPERTY NAME="URN">urn:...</PROPERTY>      ← OPTIONAL
</HEADER>
```

**Validation Checks:**
- ✅ If audio file referenced in emuR bundle, `<MEDIA_DESCRIPTOR>` must be present
- ✅ `MEDIA_URL` must point to existing WAV file
- ✅ `MIME_TYPE` must match actual file type (typically `audio/x-wav` for WAV)
- ✅ `TIME_UNITS` should be "milliseconds" (standard)

---

### `<TIME_ORDER>` Element

**Structure:**
```xml
<TIME_ORDER>
    <TIME_SLOT TIME_SLOT_ID="ts1" TIME_VALUE="0"/>
    <TIME_SLOT TIME_SLOT_ID="ts2" TIME_VALUE="1000"/>
    <TIME_SLOT TIME_SLOT_ID="ts3" TIME_VALUE="1500"/>
</TIME_ORDER>
```

**Validation Checks:**
- ✅ Exactly ONE `<TIME_ORDER>` per document
- ✅ Each `TIME_SLOT_ID` must be unique across entire document
- ✅ `TIME_VALUE` must be non-negative integer (milliseconds)
- ✅ `TIME_VALUE` should be ordered (recommended, not required)
- ✅ For `ALIGNABLE_ANNOTATION`, both referenced time slots MUST have `TIME_VALUE`
- ✅ `TIME_VALUE` of start slot ≤ `TIME_VALUE` of end slot

**emuR Mapping:**
- Sample rate from `sampleRate` field in `*_annot.json`
- Convert sample points to milliseconds: `(sampleStart / sampleRate) * 1000`
- Duration: `(sampleDur / sampleRate) * 1000`

---

## Part 2: TIER and LINGUISTIC_TYPE

### `<LINGUISTIC_TYPE>` Definitions

**Must define before use in tiers:**
```xml
<LINGUISTIC_TYPE 
    LINGUISTIC_TYPE_ID="default-lt"              ← REQUIRED, unique
    TIME_ALIGNABLE="true"                        ← REQUIRED (true/false)
    CONSTRAINTS="Time_Subdivision"               ← OPTIONAL
    GRAPHIC_REFERENCES="false"/>                 ← OPTIONAL
```

**Constraint Types:**
1. **None** - Top-level tier (no parent)
2. **Time_Subdivision** - Child segments parent's time completely, no gaps
3. **Included_In** - Child within parent's time, gaps allowed
4. **Symbolic_Subdivision** - Ordered symbolic children (uses REF_ANNOTATION)
5. **Symbolic_Association** - One-to-one symbolic link (uses REF_ANNOTATION)

**Validation Checks:**
- ✅ Each `LINGUISTIC_TYPE_ID` must be unique
- ✅ `TIME_ALIGNABLE` must match annotation type on tiers using this type
  - `true` → tier contains `ALIGNABLE_ANNOTATION`
  - `false` → tier contains `REF_ANNOTATION`
- ✅ If `CONSTRAINTS` is set, it must be one of the 4 valid values

---

### `<TIER>` Elements

**Structure:**
```xml
<TIER 
    TIER_ID="Word"                               ← REQUIRED, unique
    LINGUISTIC_TYPE_REF="default-lt"             ← REQUIRED, must exist
    PARENT_REF="Utterance"                       ← OPTIONAL (for child tiers)
    PARTICIPANT="speaker1">                      ← OPTIONAL
```

**Validation Checks:**
- ✅ Each `TIER_ID` must be unique across entire document
- ✅ `LINGUISTIC_TYPE_REF` must reference an existing `LINGUISTIC_TYPE_ID`
- ✅ If `PARENT_REF` is set, it must reference an existing `TIER_ID`
- ✅ Cannot mix `ALIGNABLE_ANNOTATION` and `REF_ANNOTATION` on same tier
- ✅ Tier hierarchy must not create cycles

**emuR Mapping:**
- Each level in emuR's `levelDefinitions` → one TIER
- Parent-child links in emuR's `linkDefinitions` → `PARENT_REF` attribute

---

## Part 3: Annotations (Mode-Specific)

### Mode 1: `align_items=TRUE` → `ALIGNABLE_ANNOTATION`

**Structure:**
```xml
<TIER TIER_ID="Word" LINGUISTIC_TYPE_REF="alignable-lt">
    <ANNOTATION>
        <ALIGNABLE_ANNOTATION 
            ANNOTATION_ID="a1"                   ← REQUIRED, unique
            TIME_SLOT_REF1="ts1"                 ← REQUIRED, must exist
            TIME_SLOT_REF2="ts2">                ← REQUIRED, must exist
            <ANNOTATION_VALUE>hello</ANNOTATION_VALUE>
        </ALIGNABLE_ANNOTATION>
    </ANNOTATION>
</TIER>
```

**Validation Checks:**
- ✅ `ANNOTATION_ID` unique across entire document
- ✅ `TIME_SLOT_REF1` and `TIME_SLOT_REF2` must reference existing `TIME_SLOT_ID`s
- ✅ Both time slots must have `TIME_VALUE` defined
- ✅ `TIME_VALUE(REF1) <= TIME_VALUE(REF2)`
- ✅ Tier's `LINGUISTIC_TYPE` must have `TIME_ALIGNABLE="true"`
- ✅ `<ANNOTATION_VALUE>` must be present (can be empty string)

**emuR Conversion Logic:**
```
For each ITEM in level:
  1. Find linked SEGMENT or POINT it dominates
  2. Extract sampleStart and sampleDur
  3. Convert to milliseconds
  4. Create/reuse TIME_SLOTs
  5. Create ALIGNABLE_ANNOTATION with TIME_SLOT_REF1/REF2
```

**Constraint Handling:**
- **Time_Subdivision**: Adjacent items must share time slots (no gaps/overlaps)
  - Item1: ts1→ts2, Item2: ts2→ts3 (share ts2)
- **Included_In**: Items can have gaps, don't need to share time slots
  - Item1: ts1→ts2, Item2: ts4→ts5 (gap allowed)

---

### Mode 2: `align_items=FALSE` → `REF_ANNOTATION`

**Structure:**
```xml
<TIER TIER_ID="Word" LINGUISTIC_TYPE_REF="ref-lt" PARENT_REF="Utterance">
    <ANNOTATION>
        <REF_ANNOTATION 
            ANNOTATION_ID="a2"                   ← REQUIRED, unique
            ANNOTATION_REF="a1"                  ← REQUIRED, parent's ID
            PREVIOUS_ANNOTATION="a_prev">        ← OPTIONAL, for ordering
            <ANNOTATION_VALUE>word1</ANNOTATION_VALUE>
        </REF_ANNOTATION>
    </ANNOTATION>
</TIER>
```

**Validation Checks:**
- ✅ `ANNOTATION_ID` unique across entire document
- ✅ `ANNOTATION_REF` must reference an existing `ANNOTATION_ID`
- ✅ Referenced annotation must be on parent tier
- ✅ Tier must have `PARENT_REF` attribute
- ✅ Tier's `LINGUISTIC_TYPE` must have `TIME_ALIGNABLE="false"`
- ✅ If `PREVIOUS_ANNOTATION` is set, it must reference existing `ANNOTATION_ID` on same tier

**emuR Conversion Logic:**
```
For each ITEM in level:
  1. Check it has NO direct time alignment
  2. Find parent annotation via emuR links
  3. Create REF_ANNOTATION with ANNOTATION_REF to parent
  4. For Symbolic_Subdivision: set PREVIOUS_ANNOTATION to create order chain
```

**Constraint Handling:**
- **Symbolic_Subdivision**: Ordered sequence using `PREVIOUS_ANNOTATION`
  - First item: no `PREVIOUS_ANNOTATION`
  - Each subsequent: `PREVIOUS_ANNOTATION` points to previous sibling
  - Creates linked list: a1 → a2 → a3
- **Symbolic_Association**: One-to-one, no `PREVIOUS_ANNOTATION` needed

---

## Part 4: Common Conversion Errors

### Error 1: Mixed Annotation Types
```xml
❌ INVALID:
<TIER TIER_ID="mixed">
    <ANNOTATION>
        <ALIGNABLE_ANNOTATION .../>
    </ANNOTATION>
    <ANNOTATION>
        <REF_ANNOTATION .../>      ← Error: mixing types
    </ANNOTATION>
</TIER>
```

**Fix**: Separate into two tiers with appropriate `LINGUISTIC_TYPE`

---

### Error 2: TIME_ALIGNABLE Mismatch
```xml
❌ INVALID:
<LINGUISTIC_TYPE LINGUISTIC_TYPE_ID="type1" TIME_ALIGNABLE="false"/>
<TIER TIER_ID="tier1" LINGUISTIC_TYPE_REF="type1">
    <ANNOTATION>
        <ALIGNABLE_ANNOTATION .../>  ← Error: type says false, annotation is alignable
    </ANNOTATION>
</TIER>
```

**Fix**: Set `TIME_ALIGNABLE="true"` in LINGUISTIC_TYPE

---

### Error 3: Missing/Invalid Time Slots
```xml
❌ INVALID:
<ALIGNABLE_ANNOTATION 
    TIME_SLOT_REF1="ts99"          ← Error: ts99 doesn't exist
    TIME_SLOT_REF2="ts2">
```

**Fix**: Ensure all referenced `TIME_SLOT_ID`s exist in `<TIME_ORDER>`

---

### Error 4: Time Value Ordering
```xml
❌ INVALID:
<TIME_SLOT TIME_SLOT_ID="ts1" TIME_VALUE="2000"/>
<TIME_SLOT TIME_SLOT_ID="ts2" TIME_VALUE="1000"/>
<ALIGNABLE_ANNOTATION 
    TIME_SLOT_REF1="ts1"           ← 2000 > 1000 (invalid)
    TIME_SLOT_REF2="ts2">
```

**Fix**: Ensure start time ≤ end time

---

### Error 5: Broken Hierarchy
```xml
❌ INVALID:
<TIER TIER_ID="child" PARENT_REF="nonexistent">  ← Error: parent doesn't exist
<TIER TIER_ID="grandchild" PARENT_REF="child">
```

**Fix**: Ensure parent tiers exist before children, no circular references

---

### Error 6: Symbolic_Subdivision Without Ordering
```xml
❌ INCOMPLETE (for Symbolic_Subdivision):
<REF_ANNOTATION ANNOTATION_ID="a1" ANNOTATION_REF="parent1">word1</...>
<REF_ANNOTATION ANNOTATION_ID="a2" ANNOTATION_REF="parent1">word2</...>
                ↑ Missing PREVIOUS_ANNOTATION="a1" for proper ordering
```

**Fix**: Add `PREVIOUS_ANNOTATION` chain for ordered subdivisions

---

## Part 5: Validation Test Cases

### Test Case 1: Simple Two-Level Hierarchy (align_items=TRUE)

**Input (emuR annot.json):**
```json
{
  "levels": [
    {
      "name": "Utterance",
      "type": "SEGMENT",
      "items": [
        {"id": 101, "sampleStart": 0, "sampleDur": 20000, "labels": [{"value": "hello world"}]}
      ]
    },
    {
      "name": "Word",
      "type": "SEGMENT",
      "items": [
        {"id": 201, "sampleStart": 0, "sampleDur": 10000, "labels": [{"value": "hello"}]},
        {"id": 202, "sampleStart": 10000, "sampleDur": 10000, "labels": [{"value": "world"}]}
      ]
    }
  ],
  "links": [
    {"fromID": 201, "toID": 101},
    {"fromID": 202, "toID": 101}
  ],
  "sampleRate": 20000
}
```

**Expected EAF Output:**
```xml
<TIME_ORDER>
    <TIME_SLOT TIME_SLOT_ID="ts1" TIME_VALUE="0"/>
    <TIME_SLOT TIME_SLOT_ID="ts2" TIME_VALUE="500"/>
    <TIME_SLOT TIME_SLOT_ID="ts3" TIME_VALUE="1000"/>
</TIME_ORDER>

<LINGUISTIC_TYPE LINGUISTIC_TYPE_ID="utterance-lt" TIME_ALIGNABLE="true"/>
<LINGUISTIC_TYPE LINGUISTIC_TYPE_ID="word-lt" TIME_ALIGNABLE="true" CONSTRAINTS="Time_Subdivision"/>

<TIER TIER_ID="Utterance" LINGUISTIC_TYPE_REF="utterance-lt">
    <ANNOTATION>
        <ALIGNABLE_ANNOTATION ANNOTATION_ID="a1" TIME_SLOT_REF1="ts1" TIME_SLOT_REF2="ts3">
            <ANNOTATION_VALUE>hello world</ANNOTATION_VALUE>
        </ALIGNABLE_ANNOTATION>
    </ANNOTATION>
</TIER>

<TIER TIER_ID="Word" LINGUISTIC_TYPE_REF="word-lt" PARENT_REF="Utterance">
    <ANNOTATION>
        <ALIGNABLE_ANNOTATION ANNOTATION_ID="a2" TIME_SLOT_REF1="ts1" TIME_SLOT_REF2="ts2">
            <ANNOTATION_VALUE>hello</ANNOTATION_VALUE>
        </ALIGNABLE_ANNOTATION>
    </ANNOTATION>
    <ANNOTATION>
        <ALIGNABLE_ANNOTATION ANNOTATION_ID="a3" TIME_SLOT_REF1="ts2" TIME_SLOT_REF2="ts3">
            <ANNOTATION_VALUE>world</ANNOTATION_VALUE>
        </ALIGNABLE_ANNOTATION>
    </ANNOTATION>
</TIER>
```

**Validation Checks:**
- ✅ Words share time slot ts2 (Time_Subdivision constraint)
- ✅ Word times fill parent Utterance completely
- ✅ All TIME_SLOTs have values, ordered correctly
- ✅ All ANNOTATION_IDs unique
- ✅ Tier hierarchy correct (Word → Utterance)

---

### Test Case 2: Items as References (align_items=FALSE)

**Input (emuR annot.json):**
```json
{
  "levels": [
    {
      "name": "Utterance",
      "type": "SEGMENT",
      "items": [
        {"id": 101, "sampleStart": 0, "sampleDur": 20000, "labels": [{"value": "sentence"}]}
      ]
    },
    {
      "name": "Word",
      "type": "ITEM",
      "items": [
        {"id": 201, "labels": [{"value": "hello"}]},
        {"id": 202, "labels": [{"value": "world"}]}
      ]
    }
  ],
  "links": [
    {"fromID": 201, "toID": 101},
    {"fromID": 202, "toID": 101}
  ],
  "sampleRate": 20000
}
```

**Expected EAF Output:**
```xml
<LINGUISTIC_TYPE LINGUISTIC_TYPE_ID="utterance-lt" TIME_ALIGNABLE="true"/>
<LINGUISTIC_TYPE LINGUISTIC_TYPE_ID="word-lt" TIME_ALIGNABLE="false" CONSTRAINTS="Symbolic_Subdivision"/>

<TIER TIER_ID="Utterance" LINGUISTIC_TYPE_REF="utterance-lt">
    <ANNOTATION>
        <ALIGNABLE_ANNOTATION ANNOTATION_ID="a1" TIME_SLOT_REF1="ts1" TIME_SLOT_REF2="ts2">
            <ANNOTATION_VALUE>sentence</ANNOTATION_VALUE>
        </ALIGNABLE_ANNOTATION>
    </ANNOTATION>
</TIER>

<TIER TIER_ID="Word" LINGUISTIC_TYPE_REF="word-lt" PARENT_REF="Utterance">
    <ANNOTATION>
        <REF_ANNOTATION ANNOTATION_ID="a2" ANNOTATION_REF="a1">
            <ANNOTATION_VALUE>hello</ANNOTATION_VALUE>
        </REF_ANNOTATION>
    </ANNOTATION>
    <ANNOTATION>
        <REF_ANNOTATION ANNOTATION_ID="a3" ANNOTATION_REF="a1" PREVIOUS_ANNOTATION="a2">
            <ANNOTATION_VALUE>world</ANNOTATION_VALUE>
        </REF_ANNOTATION>
    </ANNOTATION>
</TIER>
```

**Validation Checks:**
- ✅ Words use REF_ANNOTATION (not time-aligned)
- ✅ All reference parent annotation a1
- ✅ Second word has PREVIOUS_ANNOTATION to first (ordering)
- ✅ LINGUISTIC_TYPE has TIME_ALIGNABLE="false"
- ✅ Symbolic_Subdivision constraint specified

---

### Test Case 3: Three-Level Hierarchy

**Input:** Utterance → Word → Phoneme

**Expected Behavior:**
- Utterance: SEGMENT → ALIGNABLE_ANNOTATION
- Word: SEGMENT (if align_items=TRUE) → ALIGNABLE_ANNOTATION, PARENT_REF="Utterance"
- Phoneme: SEGMENT (if align_items=TRUE) → ALIGNABLE_ANNOTATION, PARENT_REF="Word"

**Validation:**
- ✅ Three tiers with proper parent chain
- ✅ Each level's annotations reference correct parent
- ✅ Time slots properly shared for Time_Subdivision
- ✅ No gaps/overlaps where constrained

---

### Test Case 4: Multiple Labels per Item

**Input (emuR):**
```json
{
  "items": [
    {"id": 101, "labels": [{"value": "hello"}, {"value": "greeting"}]}
  ]
}
```

**Expected Behavior:**
- Use first label for `<ANNOTATION_VALUE>`
- OR concatenate labels with delimiter
- OR create warning about multiple labels

**Validation:**
- ✅ Check conversion handles multiple labels consistently
- ✅ Document which label is used

---

## Part 6: Integration with EAF Validator

### Using the Validator

After conversion, validate with:
```bash
./validate-eaf.sh output.eaf
```

### Expected Validator Output

**For valid EAF:**
```
Validating EAF file: output.eaf
================================================================================
++ Start of XML validation by SAXParser
Resolved schema to local resource: /mpi/eudico/resources/EAFv3.0.xsd
Received 0 warnings and 0 errors
-- End of XML validation by SAXParser

Checking contents of the EAF file
++ Start of Tier Types
++ Start of Tiers
++ Start of Controlled Vocabularies
================================================================================
Validation completed successfully.
```

**Common Errors to Check:**
1. "ERROR: tier ... has parent tier ... but has no stereotype CONSTRAINT"
2. "ERROR: annotation with id=... has an end time < begin time"
3. "ERROR: overlapping annotations found"
4. "ERROR: tier type mismatch"

---

## Part 7: Conversion Function Requirements

### Minimum Requirements for `convert_emu_to_eaf()`

**Function Signature:**
```r
convert_emu_to_eaf(annot_json_path, 
                   output_eaf_path, 
                   align_items = TRUE,
                   author = "emuR",
                   ...)
```

**Must Produce:**
1. ✅ Valid XML with proper namespace declarations
2. ✅ Complete `<ANNOTATION_DOCUMENT>` structure
3. ✅ Proper `<HEADER>` with media reference
4. ✅ Complete `<TIME_ORDER>` with all time slots
5. ✅ `<LINGUISTIC_TYPE>` definitions before tier usage
6. ✅ Properly structured `<TIER>` elements
7. ✅ Correct annotation type based on `align_items` parameter
8. ✅ Valid hierarchy matching emuR link structure

**Must Handle:**
- ✅ Sample rate conversion to milliseconds
- ✅ Time slot deduplication (same times → same slot)
- ✅ Time slot sharing for Time_Subdivision
- ✅ Proper PREVIOUS_ANNOTATION chaining for Symbolic_Subdivision
- ✅ Multiple labels per item (defined behavior)
- ✅ Empty annotation values
- ✅ Missing parent links (top-level tiers)

**Should Validate:**
- ✅ All ANNOTATION_IDs unique
- ✅ All TIME_SLOT_IDs unique
- ✅ All TIER_IDs unique
- ✅ All references point to existing entities
- ✅ Time ordering correct
- ✅ No circular dependencies in tier hierarchy

---

## Part 8: Testing Checklist

### Pre-Conversion Checks
- [ ] emuR database loads without errors
- [ ] All annotation tiers present
- [ ] Links between tiers are valid
- [ ] Sample rate is defined

### Post-Conversion Checks
- [ ] EAF file is valid XML
- [ ] All mandatory elements present
- [ ] All IDs are unique
- [ ] Time slots ordered correctly
- [ ] Tier hierarchy matches emuR structure
- [ ] Annotation types match align_items setting
- [ ] LINGUISTIC_TYPE TIME_ALIGNABLE matches annotation types
- [ ] All references (time slots, annotations, tiers) resolve
- [ ] File validates with ELAN validator (exit code 0)

### Mode-Specific Checks

**align_items=TRUE:**
- [ ] All ITEMs have time values
- [ ] Time slots created for all segment boundaries
- [ ] Adjacent segments share time slots (if Time_Subdivision)
- [ ] No time gaps/overlaps (if constrained)

**align_items=FALSE:**
- [ ] ITEMs become REF_ANNOTATIONs
- [ ] All REF_ANNOTATIONs have valid ANNOTATION_REF
- [ ] PREVIOUS_ANNOTATION chain correct (if Symbolic_Subdivision)
- [ ] Parent tier exists and is time-aligned

---

## Summary: Critical Validation Points

### Must Have (Breaking errors if missing):
1. ✅ Valid XML structure
2. ✅ AUTHOR, DATE, VERSION in ANNOTATION_DOCUMENT
3. ✅ TIME_ORDER with unique TIME_SLOT_IDs
4. ✅ LINGUISTIC_TYPE definitions
5. ✅ Unique TIER_IDs, ANNOTATION_IDs
6. ✅ Valid time slot references
7. ✅ Correct annotation type for TIME_ALIGNABLE setting

### Should Have (Best practices):
1. ✅ TIME_SLOTs ordered by value
2. ✅ PREVIOUS_ANNOTATION for ordered sequences
3. ✅ CONSTRAINTS attribute on dependent tiers
4. ✅ RELATIVE_MEDIA_URL for portability
5. ✅ Meaningful TIER_IDs and ANNOTATION_IDs

### Must Avoid (Common errors):
1. ❌ Mixing ALIGNABLE and REF annotations on same tier
2. ❌ TIME_ALIGNABLE mismatch with annotation type
3. ❌ Invalid time ordering (start > end)
4. ❌ Dangling references (non-existent IDs)
5. ❌ Circular tier dependencies
6. ❌ Duplicate IDs

---

*Document Version: 1.0*  
*Date: November 1, 2025*  
*For: reindeer R package `convert_emu_to_eaf()` function*  
*EAF Spec: Version 3.0*
