# emuR to EAF Conversion: Quick Validation Checklist

## Pre-Flight Check (Before Conversion)

- [ ] emuR database loads successfully
- [ ] `*_annot.json` file exists and is valid JSON
- [ ] Sample rate is defined (`sampleRate` field)
- [ ] All levels have valid type (SEGMENT, EVENT, or ITEM)
- [ ] Links array is present (for hierarchical structure)

---

## Conversion Mode Decision

**Choose `align_items=TRUE` if:**
- ✅ ITEMs have linked SEGMENTS with timing
- ✅ You want items to have explicit start/end times
- ✅ Items will become `ALIGNABLE_ANNOTATION`

**Choose `align_items=FALSE` if:**
- ✅ ITEMs are purely symbolic (no direct timing)
- ✅ You want items to reference parent annotations
- ✅ Items will become `REF_ANNOTATION`

---

## Post-Conversion Validation

### 1. XML Structure
- [ ] File is well-formed XML
- [ ] Has XML declaration: `<?xml version="1.0" encoding="UTF-8"?>`
- [ ] Has proper namespace declarations
- [ ] Schema reference present

### 2. Root Element
- [ ] `<ANNOTATION_DOCUMENT>` is root
- [ ] `AUTHOR` attribute present and non-empty
- [ ] `DATE` attribute in ISO 8601 format (YYYY-MM-DDThh:mm:ssZ)
- [ ] `VERSION="3.0"`

### 3. Header
- [ ] `<HEADER>` element present
- [ ] `<MEDIA_DESCRIPTOR>` present if audio file referenced
- [ ] `MEDIA_URL` points to audio file
- [ ] `MIME_TYPE` is correct (usually `audio/x-wav`)

### 4. Time Order
- [ ] Exactly ONE `<TIME_ORDER>` element
- [ ] All `TIME_SLOT_ID` values are unique
- [ ] All `TIME_VALUE` values are non-negative integers
- [ ] Time slots roughly ordered by TIME_VALUE (best practice)

### 5. Linguistic Types
- [ ] All `LINGUISTIC_TYPE_ID` values are unique
- [ ] `TIME_ALIGNABLE` attribute present (true/false)
- [ ] If tier has children, `CONSTRAINTS` attribute set correctly
- [ ] Defined BEFORE any tier uses them

### 6. Tiers
- [ ] All `TIER_ID` values are unique
- [ ] Each `LINGUISTIC_TYPE_REF` references existing LINGUISTIC_TYPE
- [ ] If `PARENT_REF` set, parent tier exists
- [ ] No circular dependencies in tier hierarchy

### 7. Annotations (General)
- [ ] All `ANNOTATION_ID` values unique across entire document
- [ ] Each annotation has `<ANNOTATION_VALUE>` element
- [ ] Tier does not mix ALIGNABLE and REF annotations

---

## Mode-Specific Checks

### For `align_items=TRUE` (ALIGNABLE_ANNOTATION)

#### Time Slots
- [ ] Both `TIME_SLOT_REF1` and `TIME_SLOT_REF2` are set
- [ ] Both referenced time slots exist in `<TIME_ORDER>`
- [ ] Both time slots have `TIME_VALUE` defined
- [ ] `TIME_VALUE(REF1) <= TIME_VALUE(REF2)`

#### Linguistic Type Match
- [ ] Tier's LINGUISTIC_TYPE has `TIME_ALIGNABLE="true"`
- [ ] All annotations on tier are `<ALIGNABLE_ANNOTATION>`

#### Time_Subdivision Constraint (if applicable)
- [ ] Adjacent child annotations share time slots
- [ ] No gaps between child annotations
- [ ] No overlaps between child annotations
- [ ] Child annotations fill parent's duration exactly

#### Included_In Constraint (if applicable)
- [ ] Child annotations within parent's time range
- [ ] Gaps between children are allowed

---

### For `align_items=FALSE` (REF_ANNOTATION)

#### References
- [ ] `ANNOTATION_REF` attribute is set
- [ ] Referenced annotation exists
- [ ] Referenced annotation is on parent tier
- [ ] Tier has `PARENT_REF` attribute set

#### Linguistic Type Match
- [ ] Tier's LINGUISTIC_TYPE has `TIME_ALIGNABLE="false"`
- [ ] All annotations on tier are `<REF_ANNOTATION>`

#### Symbolic_Subdivision (if applicable)
- [ ] First child has no `PREVIOUS_ANNOTATION`
- [ ] Each subsequent child has `PREVIOUS_ANNOTATION`
- [ ] PREVIOUS_ANNOTATION chain is correct (no breaks)
- [ ] All PREVIOUS_ANNOTATION references exist

#### Symbolic_Association (if applicable)
- [ ] At most one child per parent
- [ ] No `PREVIOUS_ANNOTATION` needed

---

## Automated Validation

### Using ELAN Validator

```bash
# From elan-7.0 directory
./validate-eaf.sh path/to/output.eaf

# Expected output for valid file:
# "Validation completed successfully."
# Exit code: 0

# Expected output for invalid file:
# "Validation completed with ERRORS."
# Exit code: 1
```

### Critical Error Messages to Watch For

**Schema Validation:**
- "ERROR: element X not allowed here"
- "ERROR: attribute Y is required"

**Content Validation:**
- "ERROR: tier ... has parent tier ... but has no stereotype CONSTRAINT"
- "ERROR: annotation with id=... has an end time < begin time"
- "ERROR: overlapping annotations found"
- "ERROR: time slot reference ... does not exist"

---

## Common Conversion Errors

### ❌ Error 1: Missing TIME_VALUE
**Problem:** ALIGNABLE_ANNOTATION references time slot without TIME_VALUE
**Fix:** Ensure all segments have sampleStart and sampleDur in emuR
**Check:** All time slots used by ALIGNABLE_ANNOTATION have values

### ❌ Error 2: Wrong Annotation Type
**Problem:** ITEM without timing becomes ALIGNABLE_ANNOTATION
**Fix:** Use `align_items=FALSE` for ITEMs without direct timing
**Check:** ITEMs → REF_ANNOTATION when align_items=FALSE

### ❌ Error 3: Missing Parent Tier
**Problem:** Child tier's PARENT_REF doesn't exist
**Fix:** Ensure parent tier created before children
**Check:** All PARENT_REF values reference existing TIER_ID

### ❌ Error 4: TIME_ALIGNABLE Mismatch
**Problem:** Tier with ALIGNABLE_ANNOTATIONs uses TIME_ALIGNABLE="false"
**Fix:** Set TIME_ALIGNABLE to match annotation type
**Check:** 
- ALIGNABLE → TIME_ALIGNABLE="true"
- REF → TIME_ALIGNABLE="false"

### ❌ Error 5: Broken Ordering
**Problem:** Symbolic_Subdivision missing PREVIOUS_ANNOTATION chain
**Fix:** Link annotations with PREVIOUS_ANNOTATION
**Check:** First child has none, others form chain

### ❌ Error 6: Time Ordering
**Problem:** Start time > end time
**Fix:** Verify sampleStart and sampleDur conversion
**Check:** All TIME_SLOT pairs satisfy start ≤ end

### ❌ Error 7: Duplicate IDs
**Problem:** Same ID used for multiple elements
**Fix:** Generate unique IDs (use counter or UUID)
**Check:** 
- All TIME_SLOT_IDs unique
- All ANNOTATION_IDs unique
- All TIER_IDs unique

---

## Testing Strategy

### 1. Unit Tests (Simple Cases)

**Test A: Single Segment Tier**
- Input: One SEGMENT tier, one annotation
- Expected: One tier with one ALIGNABLE_ANNOTATION
- Validate: Time slots exist, values correct

**Test B: Single ITEM Tier (FALSE)**
- Input: One ITEM tier, no parent
- Expected: Error or warning (ITEMs need parent)
- Validate: Proper error handling

**Test C: Two-Level Hierarchy (TRUE)**
- Input: Parent SEGMENT, child SEGMENT
- Expected: Two tiers, both ALIGNABLE, proper parent link
- Validate: Time_Subdivision constraint, shared slots

**Test D: Two-Level Hierarchy (FALSE)**
- Input: Parent SEGMENT, child ITEM
- Expected: Parent ALIGNABLE, child REF
- Validate: ANNOTATION_REF correct, ordering if needed

### 2. Integration Tests (Complex Cases)

**Test E: Three-Level Hierarchy**
- Utterance → Word → Phoneme
- Mix of TRUE/FALSE modes
- Validate: Complete hierarchy, all constraints

**Test F: Multiple Labels**
- Items with multiple label values
- Expected: Defined behavior (first? concatenated?)
- Validate: Consistency across all items

**Test G: Gaps in Child Segments**
- Parent: 0-1000ms, Children: 0-300ms, 600-1000ms (gap!)
- Expected: Included_In constraint (not Time_Subdivision)
- Validate: Proper constraint type selected

**Test H: Real emuR Database**
- Use actual ae_emuDB bundle
- Convert with both TRUE and FALSE
- Validate: Both pass validator

### 3. Edge Cases

**Test I: Empty Annotations**
- Items with empty labels
- Expected: `<ANNOTATION_VALUE></ANNOTATION_VALUE>`
- Validate: Not missing, just empty

**Test J: Single Sample Point**
- sampleStart=1000, sampleDur=1
- Expected: TIME_VALUE(REF1)=50, TIME_VALUE(REF2)=50.05 (or 50 and 50)
- Validate: Handles tiny durations

**Test K: Very Long Recording**
- Hours of audio, millions of samples
- Expected: TIME_VALUE up to millions (OK)
- Validate: No integer overflow

---

## Final Validation Command

```bash
# 1. Convert emuR to EAF
convert_emu_to_eaf("bundle_annot.json", "output.eaf", align_items=TRUE)

# 2. Validate with ELAN validator
./validate-eaf.sh output.eaf

# 3. Check exit code
if [ $? -eq 0 ]; then
    echo "✅ VALID EAF file"
else
    echo "❌ INVALID EAF file - check errors above"
fi

# 4. Optional: Open in ELAN to verify visually
# elan output.eaf
```

---

## Success Criteria

A valid conversion must:
1. ✅ Pass XML schema validation
2. ✅ Pass ELAN content validation (no errors)
3. ✅ Open correctly in ELAN application
4. ✅ Display all tiers with correct hierarchy
5. ✅ Show all annotations with correct timing
6. ✅ Preserve all emuR annotation values
7. ✅ Maintain parent-child relationships

---

## Troubleshooting

**If validation fails:**
1. Check validator error messages (most informative)
2. Verify TIME_ORDER has all referenced time slots
3. Check all IDs are unique
4. Verify TIME_ALIGNABLE matches annotation types
5. Check parent references are valid
6. Verify time slot ordering (start ≤ end)
7. Look for missing LINGUISTIC_TYPE definitions

**For debugging:**
- Save intermediate XML to file
- Use XML validator separately
- Check against test-sample.eaf for structure
- Compare with test-invalid.eaf to see errors

---

*Quick Reference Version*  
*See EMUR_TO_EAF_VALIDATION_GUIDE.md for full details*  
*Date: November 1, 2025*
