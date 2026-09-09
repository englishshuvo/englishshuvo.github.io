# Do not publish checklist

Run through this before every push to main.

## Data

- [ ] No protected health information of any kind, including de-identified record-level extracts.
- [ ] No row-level or case-level data from PDMP, ESSENCE, EMS, hospital discharge, vital records, NVDRS, SUDORS, or ODMAP.
- [ ] No screenshots of internal dashboards, internal Power BI or Tableau reports, or internal ArcGIS layers.
- [ ] No small-cell counts from any state data source. Suppression rules for public reporting apply here too.
- [ ] No survey instruments or raw responses from the dissertation studies unless already published.
- [ ] Every dataset used in a notebook is downloadable by a stranger from a public URL.

## Employer and role

- [ ] No claim to speak for Nebraska DHHS, UNMC, or IEDCR.
- [ ] Job titles on the site match the official titles, not the resume's JD-aligned framing.
- [ ] Employers and dates match the locked resume and LinkedIn exactly.

## Claims

- [ ] No invented percentages, effect sizes, or impact numbers.
- [ ] No tool listed as a skill that has not actually been used: Databricks, Snowflake, Sentinel, Flatiron, Komodo, HealthVerity, TriNetX, TreeAge, Epic build.
- [ ] No AMCP dossier, global value dossier, or HTA submission described as completed work.
- [ ] No implied years inside a pharmaceutical sponsor.
- [ ] No degree described as a PhD in Epidemiology. The degree is a PhD in Public Health.
- [ ] Publication titles, authors, and DOIs copied from the journal record, never reconstructed.

## Scope

- [ ] No Bangladesh political content, Health Minister material, #AntiVIP, or JOSS movement content on this domain.
- [ ] No blog, no opinion posts, no commentary on current events.
- [ ] No personal contact details beyond the professional email and phone already on the resume.

## Build

- [ ] `quarto render` completes with no errors.
- [ ] No file in the repo contains a real credential, API key, or internal URL.
- [ ] Every "PENDING" and "PLACEHOLDER" string is gone.

```bash
grep -rn "PENDING\|PLACEHOLDER\|TODO" --include="*.qmd" --include="*.yml" .
```
