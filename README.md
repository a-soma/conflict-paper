# R Analysis Code - Conflict in the Liptako-Gourma Border Zone

Reproduces every table and figure in the paper.

## Project layout

```
revision/
├── .here              <- project-root marker (used by the `here` package)
├── R/
│   ├── run_all.R      <- run this to execute the full pipeline
│   ├── 01_data_construction.R
│   ├── 02_core_panel_model.R
│   ├── 03_placebo_bootstrap.R
│   ├── 04_crossborder_model.R
│   └── 05_figures.R
└── data/              <- inputs (see below); outputs also land here
```

## How to run

Open R with the working directory set anywhere inside this project (e.g.
open `revision/` as an RStudio project, or `setwd()` into it), then:

```r
source("R/run_all.R")
```

All scripts use `here::here()` for paths, so they resolve correctly
regardless of where R was launched from, you no longer need `data/` to be
your literal working directory. `run_all.R` calls `here::i_am("R/run_all.R")`
to anchor the project root; the `.here` file is a fallback anchor if you run
an individual script on its own without going through `run_all.R`.

## What each script does

- `01_data_construction.R`, builds `BF_panel_full_2000_2025.csv` (13 BF
  regions x 2000-2025) and `tricountry_admin1_year_panel.csv` (31 admin1
  units across BF/Mali/Niger) from the raw ACLED extract and region-level
  covariates.
- `02_core_panel_model.R`, Table 4 (Models A-E: baseline, full controls,
  excl. Sahel, fatalities DV, PPML), Table 5 (pooled OLS benchmark), and the
  VIF diagnostics.
- `03_placebo_bootstrap.R`, Table 6 (placebo test, fake breaks restricted to
  2000-2014), Figure 6 data (gold-site tercile dose-response), and Table 7
  (cluster/pairs bootstrap on gold and cropland).
- `04_crossborder_model.R`, border-proximity escalation effect estimated
  separately for Burkina Faso, Mali, and Niger.
- `05_figures.R`, regenerates all five figures (written to `figures_R/`) in
  the paper's muted navy/grey academic style.

## Requirements

`here`, `readr`, `readxl`, `dplyr`, `tidyr`, `plm`, `lmtest`, `sandwich`,
`car`, `ggplot2`. All are on CRAN:

```r
install.packages(c("here", "readr", "readxl", "dplyr", "tidyr",
                    "plm", "lmtest", "sandwich", "car", "ggplot2"))
```

Every script, including `run_all.R`, launched from an unrelated working
directory, was run end-to-end and cross-checked numerically against the
paper's reported results before being deposited here.
