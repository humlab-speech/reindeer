# CMDI validation in reindeer

reindeer generates [CLARIN CMDI](https://www.clarin.eu/content/cmdi-component-metadata-infrastructure)
1.2 records for a corpus (via `create_cmdi_metadata()` / `describe_corpus()`).
CMDI validation has three layers; reindeer covers the first two in-package and
documents how to run the third externally.

## The three layers

| Layer | What it checks | Where |
|-------|----------------|-------|
| **1. Structural** | CMD 1.2 envelope, `CMDVersion`, required `Header` fields, `ResourceProxyList`, a profile-bound `xsi:schemaLocation`, and exactly one profile root component under `Components`. | `validate_cmdi()` — always, offline |
| **2. XSD** | The instance validates against the profile's generated XSD (element names, order, cardinality, datatypes). | `validate_cmdi(online = TRUE)` when reachable; otherwise **conformant by construction** — the `media-corpus` payload is generated to match the profile XSD |
| **3. Schematron** | CLARIN best-practice assertions beyond XSD (e.g. controlled vocabularies, self-link rules). | External Java **CMDI Instance Validator** / online **Curation Module** |

## What reindeer guarantees

- **Envelope (Phase 1):** the record declares the profile payload namespace
  (`cmdp:`) and a dual `xsi:schemaLocation` binding both the CMD envelope schema
  and the profile XSD, so any CMDI validator can resolve the schema. `MdSelfLink`
  is populated (pass a PID via `create_cmdi_metadata(self_link = ...)`).
- **Components:** the default profile is **speech-corpus-with-participants**
  (`clarin.eu:cr1:p_1392642184799`), generated to match the profile XSD
  (verified element names, sequence order, cardinality, content models,
  closed-vocabulary values). It is **fully populated** from reindeer data:
  - `GeneralInfo` (name/title/class/location), `Access` (Availability, Licence,
    Contact), `Creation` (Creators), `Project` (name/funder/url/institution),
    `SubjectLanguages` (with ISO 639-3 codes), `SpeechCorpusSpecific`
    (Modalities, MediaType, `NumberOfSpeakers`, Multilinguality, `Size`),
    `TechnicalInfo` (LanguageScripts).
  - `AnnotationTypes` are mapped from the database's annotation tiers.
  - Per-bundle Age/Gender are **aggregated** into `Participants`
    (`AgeDistribution` mean/range, sex breakdown) — all three candidate
    profiles are corpus-level, so per-speaker records are aggregate by design.
  - Any user metadata field without a matching component is folded into
    `GeneralInfo/Descriptions` as `"field: value"`, so nothing is dropped.

  `media-corpus` (`clarin.eu:cr1:p_1387365569699`) remains available via
  `profile = "media-corpus"` with its own conformant (but sparser) tree.

```r
res <- validate_cmdi("mycorpus_cmdi.xml")
res$structural   # TRUE
res$problems     # character(0)
res$xsd          # NA offline; TRUE/FALSE when a schema is available
```

## Collection hierarchy: corpus → session → bundle

`describe_corpus(..., formats = c("cmdi", "session-cmdi"))` emits, in addition to
the single corpus record, a **media-session-profile**
(`clarin.eu:cr1:p_1336550377513`) record per EMU session and per bundle:

```
<db>_cmdi.xml                                    speech-corpus (corpus)
  └─ Metadata proxy →  0000_ses/0000.cmdi.xml    media-session (session)
        └─ Metadata proxy →  0000_ses/msajcXXX_bndl/msajcXXX.cmdi.xml   (bundle)
              └─ Resource → msajcXXX.wav + msajcXXX.eaf
```

Each session/bundle record carries an actor per speaker with **Age, Sex,
Education, Dialect** (from resolved metadata) and a `media-annotation-bundle`
per recording pointing at its audio and its **EAF** (as a `WrittenResource`).
This gives the metadata-less ELAN `.eaf` exports a proper, per-session CMDI
description, and lets a repository harvest the whole tree from the corpus record
down. Records are written into the session/bundle directories next to the media
and EAF files.

## Running full XSD + Schematron validation (Layer 2 + 3)

Full validation needs the CLARIN schema set (the profile XSD imports the CMD
envelope schema and `xml.xsd`) plus the Schematron rules. Do this outside R:

1. **Get the toolkit schemas** (bundles the envelope + component schemas offline):
   <https://github.com/clarin-eric/cmdi-toolkit>

2. **XSD validation with `xmllint`** — the instance's `xsi:schemaLocation`
   points `xmllint` at both schemas:
   ```sh
   xmllint --noout --schema \
     https://infra.clarin.eu/CMDI/1.2/xsd/cmd-envelop.xsd \
     mycorpus_cmdi.xml
   ```
   Offline, replace the URL with the toolkit's local `cmd-envelop.xsd` and use an
   XML catalog to resolve the profile XSD import.

3. **Schematron / full compliance** — run the CLARIN **CMDI Instance Validator**
   (Java) or upload to the online **Curation Module**:
   - Instance validator: <https://github.com/clarin-eric/cmdi-toolkit>
   - Curation Module: <https://curate.acdh.oeaw.ac.at/>

## Suggested CI step (non-blocking)

Add a job that generates a record from the demo corpus and runs `xmllint`
against the fetched schema. Keep it non-blocking until the Components mapping is
complete for every profile you emit:

```yaml
- name: CMDI XSD validation (non-blocking)
  continue-on-error: true
  run: |
    Rscript -e 'reindeer::describe_corpus(reindeer:::create_ae_db(), formats="cmdi")'
    xmllint --noout --schema \
      https://infra.clarin.eu/CMDI/1.2/xsd/cmd-envelop.xsd \
      *_cmdi.xml
```

## Extending to other profiles

`speech-corpus` (default) and `media-corpus` are generated conformantly. To add
another profile (e.g. `speech-recordings-DLU`), fetch its XSD from the Component
Registry, read its root component tree (names, order, cardinality, content
models, and any closed-vocabulary enumerations), and add a builder mirroring
`.add_speech_corpus_participants_components()`, then route it in
`generate_cmdi_xml()` by profile id.
