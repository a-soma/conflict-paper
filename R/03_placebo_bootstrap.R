# =============================================================================
# 03_placebo_bootstrap.R
#
# Validity checks for the pre/post-2015 design used in 02_core_panel_model.R:
#   (1) A placebo test restricted to 2000-2014, using fake break years, to
#       confirm the gold-site effect is specific to 2015 (Table 6).
#   (2) A dose-response check: mean events by gold-site tercile, before vs.
#       after 2015 (Figure 6).
#   (3) A cluster (pairs) bootstrap on the gold-site coefficient, since only
#       13 regions are available for cluster-robust inference (Table 7).
#
# Packages: dplyr, plm, lmtest, sandwich
# =============================================================================

library(here)
library(readr)
library(dplyr)
library(plm)
library(lmtest)
library(sandwich)

df <- read_csv(here::here("data", "BF_panel_full_2000_2025.csv"), show_col_types = FALSE) %>%
  mutate(log_events = log(events + 1))

cluster_se <- function(mod) coeftest(mod, vcov = vcovHC(mod, method = "arellano", cluster = "group"))

# -----------------------------------------------------------------------
# (1) Placebo test: fake break years, restricted to the 2000-2014 window
#     (restricting to the pre-2015 window is essential -- see the comment
#     in the paper's Section 4.2: allowing the "post" period to extend into
#     2015-2025 would let every fake break inherit the true escalation)
# -----------------------------------------------------------------------
pre_only <- df %>% filter(year < 2015)

placebo_results <- lapply(c(2005, 2008, 2010, 2012), function(fake_year) {
  d <- pre_only %>%
    mutate(fake_post = as.integer(year >= fake_year),
           gold_fake_post = nbr_site * fake_post)
  pdx <- pdata.frame(d, index = c("region", "year"))
  m <- plm(log_events ~ gold_fake_post, data = pdx, model = "within", effect = "twoways")
  res <- cluster_se(m)
  data.frame(fake_break_year = fake_year,
             coefficient = res["gold_fake_post", "Estimate"],
             p_value     = res["gold_fake_post", "Pr(>|t|)"])
}) %>% bind_rows()

cat("=== Table 6: Placebo test (fake breaks, 2000-2014 window only) ===\n")
print(placebo_results)

# True 2015 break, for comparison, estimated over the full sample
full_pdx <- pdata.frame(df %>% mutate(gold_post = nbr_site * as.integer(year >= 2015)),
                         index = c("region", "year"))
true_break <- plm(log_events ~ gold_post, data = full_pdx, model = "within", effect = "twoways")
cat("\nTrue 2015 break (full sample):\n")
print(cluster_se(true_break))

# -----------------------------------------------------------------------
# (2) Dose-response: mean events by gold-site tercile, pre vs. post 2015
# -----------------------------------------------------------------------
df <- df %>% mutate(gold_tercile = ntile(nbr_site, 3))

cat("\n=== Figure 6 data: mean events by gold tercile, pre-2015 ===\n")
print(df %>% filter(year < 2015) %>% group_by(gold_tercile) %>%
        summarise(mean_events = mean(events)))

cat("\n=== Figure 6 data: mean events by gold tercile, post-2015 ===\n")
print(df %>% filter(year >= 2015) %>% group_by(gold_tercile) %>%
        summarise(mean_events = mean(events)))

# -----------------------------------------------------------------------
# (3) Cluster (pairs) bootstrap on the gold-site x post-2015 coefficient
#     (resamples the 13 regions with replacement; addresses the small
#     number of clusters available for the usual clustered-SE asymptotics)
# -----------------------------------------------------------------------
set.seed(42)
n_boot <- 500
regions_list <- unique(df$region)
boot_gold <- numeric(0)
boot_crop <- numeric(0)

df <- df %>% mutate(gold_post = nbr_site * post2015,
                     cropl_mean_post = cropl_mean * post2015,
                     hdi_2017_post = hdi_2017 * post2015)

# NOTE: we bootstrap the full Model A specification (gold, cropland, HDI
# jointly), not gold alone, so the cropland/HDI coefficients are resampled
# under the same specification reported in Table 4. This also serves as an
# implementation-robustness check: asymptotic clustered standard errors for
# the cropland coefficient differ somewhat between software packages when
# there are only 13 clusters (a known small-sample issue with clustered SEs);
# the bootstrap, which does not rely on those asymptotics, is the more
# reliable arbiter and is what the paper reports as the final word on both
# coefficients.
for (b in 1:n_boot) {
  sampled <- sample(regions_list, length(regions_list), replace = TRUE)
  parts <- lapply(seq_along(sampled), function(i) {
    df %>% filter(region == sampled[i]) %>%
      mutate(region_bs = paste0(sampled[i], "_", i))
  })
  dboot <- bind_rows(parts)
  pdx <- pdata.frame(dboot, index = c("region_bs", "year"))
  m <- tryCatch(
    plm(log_events ~ gold_post + cropl_mean_post + hdi_2017_post,
        data = pdx, model = "within", effect = "twoways"),
    error = function(e) NULL
  )
  if (!is.null(m)) {
    cf <- coef(m)
    if (all(c("gold_post", "cropl_mean_post") %in% names(cf))) {
      boot_gold <- c(boot_gold, cf["gold_post"])
      boot_crop <- c(boot_crop, cf["cropl_mean_post"])
    }
  }
}

cat(sprintf("\n=== Table 7: cluster (pairs) bootstrap, %d valid replications ===\n",
            length(boot_gold)))
cat(sprintf("GOLD  mean = %.5f, 95%% CI = [%.5f, %.5f], share <= 0 = %.1f%%\n",
            mean(boot_gold), quantile(boot_gold, 0.025), quantile(boot_gold, 0.975),
            100 * mean(boot_gold <= 0)))
cat(sprintf("CROP  mean = %.5f, 95%% CI = [%.5f, %.5f], two-sided bootstrap p = %.3f\n",
            mean(boot_crop), quantile(boot_crop, 0.025), quantile(boot_crop, 0.975),
            2 * min(mean(boot_crop <= 0), mean(boot_crop >= 0))))
