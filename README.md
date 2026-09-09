# rishadahmed.com

Quarto website for Rishad Ahmed, PhD, MBA. Applied epidemiology, observational studies on linked administrative and surveillance data.

## Structure

```
_quarto.yml                     site config, navbar, footer
_variables.yml                  email, github, domain, phone
index.qmd                       home
cv.qmd                          CV page, links to files/rishad-ahmed-cv.pdf
papers.qmd                      citations and DOIs
methods.qmd                     linkage, confounding, cohort notes
work/rwe.qmd                    P1 observational and RWE
work/exposure.qmd               P2 occupational and environmental
work/surveillance.qmd           P3 surveillance systems
work/outcomes.qmd               P4 outcomes and decision evidence
notebooks/rwe-protocol/
  protocol.qmd                  study protocol on MEPS public data
  analysis.qmd                  analysis notebook implementing it
files/                          CV PDF, favicon, other static assets
styles.css                      site styles
.github/workflows/publish.yml   render and deploy to GitHub Pages
CNAME                           custom domain for Pages
DO-NOT-PUBLISH.md               pre-push checklist
```

## Local development

```bash
quarto preview
```

## Before first deploy

1. Replace every `PENDING` and `PLACEHOLDER` string. See `DO-NOT-PUBLISH.md`.
2. Drop the CV PDF at `files/rishad-ahmed-cv.pdf`.
3. Confirm `CNAME` matches the domain actually registered.
