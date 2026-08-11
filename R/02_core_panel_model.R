# =============================================================================
# 02_core_panel_model.R
#
# Core two-way fixed-effects panel model (Table 4 in the paper) and the
# pooled cross-sectional benchmark (Table 5), estimated on the Burkina Faso
# region-year panel built by 01_data_construction.R.
#
# Packages: dplyr, plm, lmtest, sandwich, car
# =============================================================================

library(here)
library(readr)
library(dplyr)
library(plm)
library(lmtest)
library(sandwich)
library(car)     # for vif()

df <- read_csv(here::here("data", "BF_panel_full_2000_2025.csv"), show_col_types = FALSE) %>%
  mutate(
    log_events      = log(events + 1),
    log_fatal       = log(fatalities + 1),
    cropl_mean_post = cropl_mean * post2015,
    nbr_site_post   = nbr_site   * post2015,   # gold sites (static BENKADI count) x post-2015
    hdi_2017_post   = hdi_2017   * post2015,
    density_post    = density_hab_km2 * post2015,
    unemp_post      = taux_chomage * post2015,
    light_post      = light_mean * post2015
  )

pdata <- pdata.frame(df, index = c("region", "year"))
cluster_se <- function(mod) coeftest(mod, vcov = vcovHC(mod, method = "arellano", cluster = "group"))

# --- Model A: baseline (gold, cropland, HDI), two-way FE -------------------
modA <- plm(log_events ~ cropl_mean_post + nbr_site_post + hdi_2017_post,
            data = pdata, model = "within", effect = "twoways")
cat("\n=== Model A: baseline ===\n"); print(cluster_se(modA))

# --- Model B: full controls (+ density, unemployment, night lights) --------
modB <- plm(log_events ~ cropl_mean_post + nbr_site_post + hdi_2017_post +
              density_post + unemp_post + light_post,
            data = pdata, model = "within", effect = "twoways")
cat("\n=== Model B: full controls ===\n"); print(cluster_se(modB))

# --- Model C: excluding Sahel (outlier check) -------------------------------
pdata_noSahel <- pdata.frame(df %>% filter(region != "Sahel"), index = c("region", "year"))
modC <- plm(log_events ~ cropl_mean_post + nbr_site_post + hdi_2017_post,
            data = pdata_noSahel, model = "within", effect = "twoways")
cat("\n=== Model C: excluding Sahel ===\n"); print(cluster_se(modC))

# --- Model D: fatalities as alternative DV ----------------------------------
modD <- plm(log_fatal ~ cropl_mean_post + nbr_site_post + hdi_2017_post,
            data = pdata, model = "within", effect = "twoways")
cat("\n=== Model D: fatalities as DV ===\n"); print(cluster_se(modD))

# --- Model E: PPML (Poisson), region+year FE, cluster by region ------------
# Implemented with base glm() + region/year factor dummies (rather than the
# `fixest` package) so this script only depends on packages available from
# CRAN/Ubuntu's r-cran-* archive. If you have `fixest` installed, this is
# equivalent to: fepois(events ~ ... | region + year, data = df, cluster = ~region)
df <- df %>% mutate(region_f = factor(region), year_f = factor(year))
modE <- glm(events ~ cropl_mean_post + nbr_site_post + hdi_2017_post + region_f + year_f,
            data = df, family = poisson())
modE_cl <- coeftest(modE, vcov = vcovCL(modE, cluster = df$region, type = "HC1"))
cat("\n=== Model E: PPML (Poisson, region+year FE) ===\n")
print(modE_cl[c("cropl_mean_post", "nbr_site_post", "hdi_2017_post"), ])

# =============================================================================
# Table 5: pooled cross-sectional benchmark (n = 13 regions, no time dimension)
# Uses the same four covariates originally hypothesised: night lights,
# cropland, unemployment, gold sites. Shows that cropland is not significant
# even in the simplest possible specification (see Section 5.2 of the paper).
# =============================================================================
region_level <- df %>%
  group_by(region) %>%
  summarise(total_events = sum(events),
            light_mean   = first(light_mean),
            cropl_mean   = first(cropl_mean),
            taux_chomage = first(taux_chomage),
            nbr_site     = first(nbr_site))

naive_ols <- lm(total_events ~ light_mean + cropl_mean + taux_chomage + nbr_site,
                 data = region_level)
cat("\n=== Table 5: pooled cross-sectional OLS benchmark ===\n")
print(coeftest(naive_ols, vcov = vcovHC(naive_ols, type = "HC1")))
cat("R-squared:", summary(naive_ols)$r.squared, "\n")

# =============================================================================
# Variance inflation factors for Model B (checks the collinearity flagged in
# the paper: only the gold-site interaction stays comfortably below VIF 10)
# =============================================================================
lm_B_for_vif <- lm(log_events ~ cropl_mean_post + nbr_site_post + hdi_2017_post +
                      density_post + unemp_post + light_post,
                    data = df)
cat("\n=== VIF check (Model B controls) ===\n")
print(vif(lm_B_for_vif))

# Pairwise correlations referenced in Section 5.2 of the paper
cat("\n=== Correlations (region-level) ===\n")
cat("corr(cropland, gold sites)      =",
    cor(region_level$cropl_mean, region_level$nbr_site), "\n")
cat("corr(cropland, total events)    =",
    cor(region_level$cropl_mean, region_level$total_events), "\n")
cat("corr(gold sites, total events)  =",
    cor(region_level$nbr_site, region_level$total_events), "\n")
