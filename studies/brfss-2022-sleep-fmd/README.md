# Usual sleep and frequent mental distress, BRFSS 2022

Independent sample analysis of the public 2022 BRFSS file. The question, the population, and the estimators are locked in the protocol and the statistical analysis plan before the contrast is fit.

- [Protocol](../../notebooks/brfss-2022-sleep-fmd/protocol.qmd)
- [Statistical analysis plan](../../notebooks/brfss-2022-sleep-fmd/sap.qmd)
- [Amendments after the first run](../../notebooks/brfss-2022-sleep-fmd/amendments.qmd)

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

On a new machine, from the repository root:

```bash
Rscript -e 'install.packages("renv", repos="https://cloud.r-project.org")'
Rscript -e 'renv::restore(project="studies/brfss-2022-sleep-fmd", prompt=FALSE)'
Rscript studies/brfss-2022-sleep-fmd/run.R
Rscript studies/brfss-2022-sleep-fmd/run.R check
quarto render
```

The review restore used an empty library at `/tmp/brfss-isolated/library` so an existing library was left in place:

```bash
Rscript -e 'renv::restore(project="studies/brfss-2022-sleep-fmd", library="/tmp/brfss-isolated/library", prompt=FALSE, clean=FALSE)'
R_LIBS=/tmp/brfss-isolated/library R_LIBS_USER=/tmp/brfss-isolated/library Rscript studies/brfss-2022-sleep-fmd/run.R
```

## What is committed

Code, tests, the protocol, the plan, and aggregate outputs (counts, weighted estimates, the figure). Not the transport file, not a row-level extract, and not a cache of the microdata.
