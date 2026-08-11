# =============================================================================
# run_all.R
#
# Master script: runs the full analysis pipeline end to end, in order.
# Uses the `here` package so paths resolve correctly regardless of where R
# is launched from (RStudio, Rscript, source(), etc.) -- as long as this
# file stays at R/run_all.R relative to the project root (the folder that
# contains R/ and data/ as siblings).
#
# Usage: open this project in RStudio (or setwd() into the project root)
# and run: source("R/run_all.R")
# =============================================================================

here::i_am("R/run_all.R")

source(here::here("R", "01_data_construction.R"))
source(here::here("R", "02_core_panel_model.R"))
source(here::here("R", "03_placebo_bootstrap.R"))
source(here::here("R", "04_crossborder_model.R"))
source(here::here("R", "05_figures.R"))

cat("\n=== Pipeline complete ===\n")
