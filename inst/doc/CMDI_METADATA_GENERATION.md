# CMDI Metadata Generation for EMU Speech Corpora

## Overview

The `create_cmdi_metadata()` function generates CLARIN-compliant CMDI (Component Metadata Infrastructure) XML files for EMU speech databases. This enables corpus discoverability and sharing through CLARIN repositories.

---

## Function Usage

```r
library(reindeer)

# Load corpus
corpus <- load_emuDB("/path/to/ae_emuDB")

# Generate CMDI with defaults
create_cmdi_metadata(corpus)

# Generate CMDI with full metadata
create_cmdi_metadata(
  corpus,
  corpus_title = "American English Speech Corpus",
  corpus_description = "A corpus of American English speech containing...",
  author = "John Doe",
  institution = "University of Example",
  contact_email = "contact@example.edu",
  license = "CC-BY-4.0",
  availability = "available",
  profile = "speech-corpus",
  include_placeholders = TRUE
)
```

---

## Metadata Sources

The function collects metadata from three sources:

### 1. Database Structure (Automatic)
- Database name, UUID
- Number of sessions and bundles
- Annotation level definitions
- Link definitions (tier hierarchy)
- SSFF track definitions
- Media file information
- Total corpus duration

### 2. .meta_json Files (User-provided)
- Participant information (age, gender, language)
- Recording information (date, location, equipment)
- Session information (tasks, conditions)

### 3. Function Parameters (User-provided)
- Corpus title and description
- Author and institution
- Contact information
- License and availability

---

## .meta_json File Format

Place `.meta_json` files at:
- **Session level**: `database/SESSION_NAME_ses/.meta_json`
- **Bundle level**: `database/SESSION_NAME_ses/BUNDLE_NAME_bndl/.meta_json`

### Example Session-level .meta_json

```json
{
  "participant": {
    "id": "P001",
    "age": 45,
    "gender": "Female",
    "language": "English",
    "dialect": "American English",
    "education": "Bachelor's degree",
    "occupation": "Teacher"
  },
  "recording": {
    "date": "2023-09-15",
    "location": "Laboratory",
    "equipment": "Shure SM58",
    "sampleRate": 16000,
    "channels": 1,
    "recordingConditions": "Sound-treated booth"
  },
  "session": {
    "name": "0000_ses",
    "task": "Reading task",
    "duration_minutes": 30,
    "notes": "Clear recording, no issues"
  }
}
```

### Example Bundle-level .meta_json

```json
{
  "participant": {
    "id": "P001",
    "age": 45,
    "gender": "Female"
  },
  "recording": {
    "date": "2023-09-15",
    "stimulus": "Passage A",
    "repetition": 1
  },
  "annotation": {
    "annotator": "JD",
    "annotation_date": "2023-10-01",
    "quality": "high"
  }
}
```

---

## CMDI Profiles Supported

### 1. media-corpus (Default)
General media corpus profile suitable for audio/video collections.

**Profile ID**: `clarin.eu:cr1:p_1387365569699`

**Use when**: You have a general media collection with annotations.

### 2. speech-corpus
Speech corpus with detailed participant information.

**Profile ID**: `clarin.eu:cr1:p_1392642184799`

**Use when**: You have detailed participant demographics and want to emphasize the speech aspect.

### 3. speech-corpus-dlu
Speech corpus variant from CLARIN-D DLU working group.

**Profile ID**: `clarin.eu:cr1:p_1381926654456`

**Use when**: You need DLU-specific metadata fields.

---

## Generated CMDI Structure

### Header Section
- **MdCreator**: Author of the metadata
- **MdCreationDate**: Date metadata was created
- **MdProfile**: CMDI profile ID
- **MdCollectionDisplayName**: Display name for the corpus

### Resources Section
- **ResourceProxyList**: References to all media files
- Each recording listed with:
  - Resource type (audio/wav, etc.)
  - Resource reference (filename)

### Components Section

#### GeneralInfo
- Corpus name
- UUID
- Description

#### Access
- Availability status
- License information
- Contact information

#### Creation
- Creator(s) name and organization
- Creation date

#### Project (Placeholder)
- Project name
- Project description
- Funding information
- Grant number

#### Publications (Placeholder)
- Related publications
- Authors
- Publication year

#### SubjectLanguages
- Languages represented in corpus
- Derived from participant metadata

#### SpeechCorpusSpecific
- Modality (SpokenLanguage)
- Corpus type (AnnotatedSpeech)
- Size (number of recordings)
- Total duration
- Annotation levels and types

#### Participants
- Number of participants
- Per-participant information:
  - ID
  - Age
  - Gender
  - Mother tongue
  - (from .meta_json files)

#### TechnicalInfo
- Media format
- Sample rate
- SSFF tracks

#### QualityControl (Placeholder)
- Quality control methods
- Description

#### EthicalApproval (Placeholder)
- Ethics approval number
- Ethics committee institution

---

## Placeholders for Planned Additions

When `include_placeholders = TRUE`, the function adds placeholder fields for future metadata:

### Corpus Description File (DESCRIPTION.txt)
To be placed at: `database/DESCRIPTION.txt`

**Should contain:**
```
Title: [Corpus Title]
Description: [Full corpus description]
Purpose: [Research purpose]
Languages: [Languages included]
Annotation Scheme: [Description of annotation tiers and conventions]
Data Collection: [How data was collected]
Time Period: [When data was collected]
Geographic Coverage: [Where data was collected]
Transcription Conventions: [Conventions used]
Citation: [How to cite this corpus]
```

### Project Information (PROJECT.json)
To be placed at: `database/PROJECT.json`

```json
{
  "project": {
    "name": "Project Name",
    "description": "Project description",
    "acronym": "PROJ",
    "startDate": "2020-01-01",
    "endDate": "2024-12-31",
    "website": "https://project.example.com"
  },
  "funding": {
    "funder": "National Science Foundation",
    "grantNumber": "NSF-12345",
    "amount": "500000",
    "currency": "USD"
  },
  "team": [
    {
      "name": "Principal Investigator",
      "role": "PI",
      "affiliation": "University of Example"
    }
  ]
}
```

### Publications (PUBLICATIONS.json)
To be placed at: `database/PUBLICATIONS.json`

```json
{
  "publications": [
    {
      "title": "Paper about this corpus",
      "authors": ["Doe, J.", "Smith, A."],
      "year": 2023,
      "venue": "Journal of Speech Research",
      "doi": "10.1234/jsr.2023.001",
      "type": "journal article"
    }
  ]
}
```

### Ethics Information (ETHICS.json)
To be placed at: `database/ETHICS.json`

```json
{
  "ethics": {
    "approvalNumber": "IRB-2023-001",
    "institution": "University of Example IRB",
    "approvalDate": "2023-01-15",
    "consent": "Written informed consent obtained",
    "dataProtection": "Pseudonymized participant IDs",
    "retention": "10 years"
  }
}
```

### Quality Control (QUALITY.json)
To be placed at: `database/QUALITY.json`

```json
{
  "qualityControl": {
    "transcription": {
      "method": "Double-blind transcription",
      "interRaterReliability": 0.95,
      "tool": "ELAN"
    },
    "annotation": {
      "method": "Expert annotation with guidelines",
      "guidelines": "Available at URL",
      "training": "Annotators received 10 hours training"
    },
    "validation": {
      "automated": "EAF validator",
      "manual": "Spot checks by supervisor"
    }
  }
}
```

---

## Example Workflow

### Step 1: Prepare Metadata Files

```bash
# Create session metadata
cd /path/to/database/0000_ses/
cat > .meta_json << 'EOF'
{
  "participant": {
    "id": "P001",
    "age": 25,
    "gender": "Female",
    "language": "English"
  },
  "recording": {
    "date": "2023-09-15",
    "location": "Laboratory"
  }
}
EOF

# Create bundle metadata
cd msajc003_bndl/
cat > .meta_json << 'EOF'
{
  "participant": {
    "id": "P001"
  },
  "recording": {
    "stimulus": "Reading passage A"
  }
}
EOF
```

### Step 2: Generate CMDI in R

```r
library(reindeer)

# Load database
corpus <- load_emuDB("/path/to/database")

# Generate CMDI
create_cmdi_metadata(
  corpus,
  corpus_title = "My Speech Corpus",
  corpus_description = "A corpus of spoken language",
  author = "Jane Smith",
  institution = "University of Example",
  contact_email = "jane.smith@example.edu",
  license = "CC-BY-4.0",
  availability = "available"
)
```

### Step 3: Review and Edit

The generated CMDI file will be at: `database/database_name_cmdi.xml`

**Review for:**
- All placeholders filled with actual information
- Participant information correctly extracted
- Duration and size information accurate
- Resource references correct

### Step 4: Upload to CLARIN Repository

The CMDI file can now be uploaded to a CLARIN repository along with your corpus data.

---

## Complete Example

```r
library(reindeer)

# Load corpus
ae <- load_emuDB("/Users/frkkan96/Downloads/ae_emuDB")

# Generate full CMDI metadata
create_cmdi_metadata(
  ae,
  output_file = "/Users/frkkan96/Downloads/ae_emuDB/ae_cmdi.xml",
  profile = "speech-corpus",
  corpus_title = "AE Demo Database - American English",
  corpus_description = paste(
    "A demonstration database of American English speech",
    "containing multi-level annotations of phonetic, phonological,",
    "and prosodic features. The database includes formant tracks",
    "and various derived signal processing measures."
  ),
  author = "EMU-SDMS Team",
  institution = "Institute for Phonetics and Speech Processing, LMU Munich",
  contact_email = "emu@phonetik.uni-muenchen.de",
  license = "CC-BY-4.0",
  availability = "available",
  include_placeholders = TRUE,
  verbose = TRUE
)
```

**Output:**
```
Creating CMDI metadata for database: ae
Database UUID: 0fc618dc-8980-414d-8c7a-144a649ce199
Collecting database metadata...
  Sessions: 1
  Bundles: 7
  Annotation levels: 5
  Total duration: 00:00:45
Collecting participant metadata...
  Found metadata for 1 participants
  Age range: 25 - 25 years
  Gender distribution: Female 
Generating CMDI XML...
CMDI file written to: /Users/frkkan96/Downloads/ae_emuDB/ae_cmdi.xml
```

---

## Integration with Repository Upload

Once you have generated a CMDI file, you can upload it to CLARIN repositories:

### The Language Archive (TLA)
1. Create account at https://archive.mpi.nl/
2. Upload corpus files
3. Upload CMDI metadata file
4. TLA will validate and index your metadata

### LINDAT/CLARIAH-CZ Repository
1. Create account at https://lindat.mff.cuni.cz/
2. Submit corpus with CMDI file
3. Repository validates metadata
4. Corpus becomes discoverable via CLARIN

### Local CLARIN Centre
Contact your national CLARIN centre for specific upload procedures.

---

## Validation

Before uploading, validate your CMDI file:

### Online Validator
https://cmdi-validator.clarin.eu/

### Command Line
```bash
# Using xmllint
xmllint --noout --schema cmd-envelop.xsd your_corpus_cmdi.xml
```

---

## Best Practices

1. **Provide rich .meta_json files** - The more metadata you provide, the more discoverable your corpus
2. **Use consistent participant IDs** - Same ID across sessions for the same person
3. **Include bundle-level metadata** - For stimulus-specific information
4. **Fill in placeholders** - Replace all PLACEHOLDER_ fields before upload
5. **Use standard license codes** - CC-BY-4.0, CC-BY-NC-4.0, etc.
6. **Validate before upload** - Use CMDI validator
7. **Keep CMDI file updated** - Regenerate when corpus changes

---

## Troubleshooting

### "No participant metadata found"
- Check that .meta_json files exist in session/bundle directories
- Verify JSON syntax is valid
- Ensure participant object exists in JSON

### "Missing required fields"
- Review function parameters
- Check that corpus_title, author, etc. are provided
- Set include_placeholders=FALSE if you've filled all fields

### "XML validation fails"
- Check for special characters in metadata (escape &, <, >)
- Verify profile ID is correct
- Ensure all required CMDI elements are present

---

*Documentation Version: 1.0*  
*Date: November 1, 2025*  
*For: reindeer R package v0.2.1+*
