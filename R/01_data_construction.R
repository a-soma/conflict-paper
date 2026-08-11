# =============================================================================
# 01_data_construction.R
#
# Builds the two analysis panels used throughout the paper from raw inputs:
#   (A) BF_panel_full_2000_2025.csv    - Burkina Faso, 13 regions x 2000-2025
#   (B) tricountry_admin1_year_panel.csv - Burkina Faso + Mali + Niger,
#                                          31 admin1 units x 2000-2025
#
# Inputs expected in ./data/:
#   - ACLED_BFA_MLI_NER_updated_to_2025-07.csv
#       Raw ACLED extract (global export, 7 July 2026) already filtered to
#       Burkina Faso / Mali / Niger. NOT yet deduplicated: ACLED sometimes
#       reports more than one row per event (event_id_cnty) when both sides
#       of an interaction are separately coded.
#   - ACLED_tricountry_with_borderdist.csv
#       Same extract, already deduplicated by event_id_cnty, with an added
#       column dist_border_km = distance (km) from the event to the nearest
#       national boundary of the *other two* study countries. This was
#       computed once via a spatial join (see the commented-out appendix at
#       the bottom of this script) and is shipped as a precomputed input so
#       this script does not require the `sf` package.
#   - data_light_crop_final.csv
#       Region-level cropland / night-lights / unemployment / gold-site
#       (BENKADI) cross-section for Burkina Faso's 13 regions.
#   - exploitation_or_region.xlsx
#       Region-level panel: number of gold mines 2003-2012, average mining
#       sites before/after 2015, population density, HDI (2017), and
#       terrorism-situation counts before/after 2015.
#
# Outputs written to ./data/:
#   - BF_panel_full_2000_2025.csv
#   - tricountry_admin1_year_panel.csv
# =============================================================================

library(here)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)

data_dir <- here::here("data")

# -----------------------------------------------------------------------
# Part A: Burkina Faso region-year panel (2000-2025)
# -----------------------------------------------------------------------

acled_raw <- read_csv(file.path(data_dir, "ACLED_BFA_MLI_NER_updated_to_2025-07.csv"),
                       guess_max = 100000, show_col_types = FALSE)

# Canonicalise region-name spelling variants across sources
name_map <- c(
  "Boucle de Mouhoun" = "Boucle du Mouhoun",
  "Boucle du Mouhoun" = "Boucle du Mouhoun",
  "Haut-Bassins"       = "Hauts-Bassins",
  "Hauts-Bassins"      = "Hauts-Bassins",
  "Plateau Central"    = "Plateau-Central",
  "Plateau-Central"    = "Plateau-Central"
)
canon_region <- function(x) ifelse(x %in% names(name_map), name_map[x], x)

bf <- acled_raw %>%
  filter(country == "Burkina Faso") %>%
  distinct(event_id_cnty, .keep_all = TRUE) %>%   # de-duplicate events
  mutate(region = canon_region(admin1))

cat("Deduplicated Burkina Faso events, 2000-2025:", nrow(bf), "\n")

# Region x year conflict counts (balanced panel, 2000-2025)
bf_panel <- bf %>%
  group_by(region, year) %>%
  summarise(events = n(), fatalities = sum(fatalities, na.rm = TRUE), .groups = "drop")

regions_13 <- unique(bf_panel$region)
years_all  <- 2000:2025
bf_panel <- expand_grid(region = regions_13, year = years_all) %>%
  left_join(bf_panel, by = c("region", "year")) %>%
  mutate(events = replace_na(events, 0),
         fatalities = replace_na(fatalities, 0),
         post2015 = as.integer(year >= 2015))

# Static region-level covariates: cropland, night lights, unemployment, gold sites (BENKADI)
statics <- read_csv(file.path(data_dir, "data_light_crop_final.csv"), show_col_types = FALSE) %>%
  rename(region = NAME_1) %>%
  mutate(region = canon_region(region)) %>%
  select(region, light_mean, cropl_mean, taux_chomage, nbr_site, taux_site)

# Region-level gold-mining panel (BEFORE/AFTER 2015), density, HDI
# The source spreadsheet has multi-line column headers; read by position instead.
gold_raw <- read_excel(file.path(data_dir, "exploitation_or_region.xlsx"),
                        sheet = "Feuil1", col_names = FALSE, skip = 2)
# Column layout (17 columns, no header row after skip=2):
#   ...1  region name
#   ...2  to ...11   annual gold-mine counts, 2003-2012 (10 columns)
#   ...12 average mining sites before 2015
#   ...13 average mining sites after 2015
#   ...14 population density (hab/km2)
#   ...15 HDI (2017)
#   ...16 number of terrorism situations before 2015
#   ...17 number of terrorism situations from 2015 onward
# (Verified against the spreadsheet directly -- an earlier version of this
# script had these shifted by one column; double-check against the raw file
# if the source spreadsheet layout ever changes.)
gold_panel <- gold_raw %>%
  transmute(
    region              = canon_region(...1),
    gold_avg_pre2015     = as.numeric(...12),
    gold_avg_post2015    = as.numeric(...13),
    density_hab_km2      = as.numeric(...14),
    hdi_2017             = as.numeric(...15),
    terror_pre2015       = as.numeric(...16),
    terror_post2015      = as.numeric(...17)
  )

bf_panel <- bf_panel %>%
  left_join(statics, by = "region") %>%
  left_join(gold_panel, by = "region") %>%
  mutate(gold_sites = ifelse(post2015 == 1, gold_avg_post2015, gold_avg_pre2015))

write_csv(bf_panel, file.path(data_dir, "BF_panel_full_2000_2025.csv"))
cat("Wrote", file.path(data_dir, "BF_panel_full_2000_2025.csv"),
    "(", nrow(bf_panel), "rows )\n")

# -----------------------------------------------------------------------
# Part B: Burkina Faso + Mali + Niger admin1-year panel (2000-2025)
# -----------------------------------------------------------------------

tri <- read_csv(file.path(data_dir, "ACLED_tricountry_with_borderdist.csv"),
                 guess_max = 100000, show_col_types = FALSE)

tri_panel <- tri %>%
  group_by(country, admin1, year) %>%
  summarise(events = n(), fatalities = sum(fatalities, na.rm = TRUE),
            dist_border_km = mean(dist_border_km, na.rm = TRUE), .groups = "drop")

admin1_dist <- tri %>%
  group_by(country, admin1) %>%
  summarise(dist_border_km = mean(dist_border_km, na.rm = TRUE), .groups = "drop")

tri_full <- expand_grid(admin1 = admin1_dist$admin1, year = years_all) %>%
  left_join(admin1_dist, by = "admin1") %>%
  left_join(tri_panel %>% select(admin1, year, events, fatalities),
            by = c("admin1", "year")) %>%
  mutate(events = replace_na(events, 0),
         fatalities = replace_na(fatalities, 0),
         post2015 = as.integer(year >= 2015))

write_csv(tri_full, file.path(data_dir, "tricountry_admin1_year_panel.csv"))
cat("Wrote", file.path(data_dir, "tricountry_admin1_year_panel.csv"),
    "(", nrow(tri_full), "rows )\n")

# =============================================================================
# Appendix (not executed): how dist_border_km was originally computed.
# Requires the `sf` package and an Africa admin0 boundary shapefile
# (africa_adm0.shp, no CRS written in the file -> set to EPSG:4326 manually).
# =============================================================================
# library(sf)
# africa <- st_read("africa_adm0.shp") |> st_set_crs(4326)
# three  <- africa[toupper(trimws(africa$COUNTRY)) %in%
#                     c("BURKINA FASO", "MALI", "NIGER"), ]
# three_utm <- st_transform(three, 32630)  # UTM zone 30N, metres
# boundaries <- lapply(split(three_utm, three_utm$COUNTRY), st_boundary)
#
# dist_to_others <- function(pt, country) {
#   others <- setdiff(c("BURKINA FASO", "MALI", "NIGER"), toupper(country))
#   min(sapply(others, function(o) as.numeric(st_distance(pt, boundaries[[o]])))) / 1000
# }
# # Apply dist_to_others() row-wise to each event's point geometry (st_point),
# # transformed to EPSG:32630, to reproduce the dist_border_km column.
