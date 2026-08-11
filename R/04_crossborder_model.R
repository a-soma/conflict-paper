# =============================================================================
# 04_crossborder_model.R
#
# Cross-border extension: does proximity to the nearest of the other two
# study countries (Burkina Faso, Mali, Niger) predict a larger post-2015
# escalation? Estimated separately by country on the 31-unit admin1 panel
# built by 01_data_construction.R (Table for Section 4.6 / Figure 7).
#
# Packages: dplyr, plm, lmtest, sandwich
# =============================================================================

library(here)
library(readr)
library(dplyr)
library(plm)
library(lmtest)
library(sandwich)

df <- read_csv(here::here("data", "tricountry_admin1_year_panel.csv"), show_col_types = FALSE) %>%
  mutate(log_events = log(events + 1),
         dist_post  = dist_border_km * post2015)

cluster_se <- function(mod) coeftest(mod, vcov = vcovHC(mod, method = "arellano", cluster = "group"))

countries <- c("Burkina Faso", "Mali", "Niger")
results <- lapply(countries, function(cty) {
  sub <- df %>% filter(country == cty)
  pdx <- pdata.frame(sub, index = c("admin1", "year"))
  m <- plm(log_events ~ dist_post, data = pdx, model = "within", effect = "twoways")
  res <- cluster_se(m)
  data.frame(country = cty,
             coefficient = res["dist_post", "Estimate"],
             std_error   = res["dist_post", "Std. Error"],
             p_value     = res["dist_post", "Pr(>|t|)"],
             n           = nobs(m))
}) %>% bind_rows()

cat("=== Border-proximity escalation effect, by country ===\n")
print(results)

# Interpretation (see Section 5.3 of the paper): a significant negative
# coefficient means units closer to the border of one of the other two
# countries saw a larger post-2015 escalation. Burkina Faso and Mali show
# this pattern; Niger does not, consistent with an additional, distinct
# driver of Niger's crisis (the Lake Chad basin front) that is unrelated to
# proximity to Burkina Faso or Mali.
