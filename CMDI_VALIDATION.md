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
- **Components (Phase 2):** for the default `media-corpus` profile
  (`clarin.eu:cr1:p_1387365569699`) the `Components` subtree is generated to
  match the profile XSD — verified element names, sequence order, cardinality,
  and content models. Corpus-level fields map to `Collection > GeneralInfo`
  (`Name`, `Title`, `Owner`, `PublicationYear`) and the speaker count to
  `SpeechCorpus > NumberOfSpeakers`. Per-speaker attributes (Age, Gender, …)
  are **not** in the CMDI — the media-corpus profile has no collection-level
  slot for them; they belong to a media-session profile and remain available in
  the README and DataCite outputs.

```r
res <- validate_cmdi("mycorpus_cmdi.xml")
res$structural   # TRUE
res$problems     # character(0)
res$xsd          # NA offline; TRUE/FALSE when a schema is available
```

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

Only `media-corpus` is currently generated conformantly. To add another profile
(e.g. `speech-corpus`), fetch its XSD from the Component Registry, read its root
component tree (names, order, cardinality, content models), and add a builder
mirroring `.add_media_corpus_components()`.
