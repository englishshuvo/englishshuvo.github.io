# Usual sleep and frequent mental distress, BRFSS 2022

Independent sample analysis of the public 2022 BRFSS file. The question, the population, and the estimators are locked in the protocol and the statistical analysis plan before the contrast is fit.

- [Protocol](../../notebooks/brfss-2022-sleep-fmd/protocol.qmd)
- [Statistical analysis plan](../../notebooks/brfss-2022-sleep-fmd/sap.qmd)

This is not employer work, not a regulated submission, and not a claims-database study. Frequent mental distress is the CDC Healthy Days item (14 or more days in the past 30), not a diagnosis.

## Run

From the repository root, with R and the packages named in the plan:

```bash
Rscript studies/brfss-2022-sleep-fmd/run.R
```

That command tests the recodes and the estimators, verifies the CDC zip against a pinned SHA-256, audits the file, fits the prespecified contrasts, and writes aggregate output to `output/`. The microdata stay in `data/`, which is gitignored.

Stages, if you want only one piece:

```bash
Rscript studies/brfss-2022-sleep-fmd/run.R test
Rscript studies/brfss-2022-sleep-fmd/run.R audit
Rscript studies/brfss-2022-sleep-fmd/run.R estimate
Rscript studies/brfss-2022-sleep-fmd/run.R check
```

`audit` stops before any sleep-by-distress contrast. `check` confirms that the case study contains the key strings written by the estimation run.

The first time on a new machine, install `haven`, `survey`, `ggplot2`, and `jsonlite` at the versions in `renv.lock` when that lockfile is present (`renv::restore()` from this directory).

## What is committed

Code, tests, the protocol, the plan, and aggregate outputs (counts, weighted estimates, the figure). Not the transport file, not a row-level extract, and not a cache of the microdata.
