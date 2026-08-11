# =============================================================================
# 05_figures.R
#
# Reproduces the five figures used in the paper (temporal evolution,
# pre/post-2015 split, disorder-type frequency, gold-tercile dose-response,
# and the border-proximity coefficient plot), using the muted navy/grey,
# serif-font style used throughout the manuscript's figures.
#
# Packages: dplyr, ggplot2
# =============================================================================

library(here)
library(readr)
library(dplyr)
library(ggplot2)

NAVY  <- "#33415c"
GREY  <- "#9aa5b1"
LGREY <- "#d9d9d9"

academic_theme <- theme_minimal(base_family = "serif", base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = LGREY, size = 0.3),
    axis.line = element_line(color = "grey30", size = 0.3),
    plot.title = element_blank()   # captions live in LaTeX, not in the image
  )

fig_dir <- here::here("figures_R")
dir.create(fig_dir, showWarnings = FALSE)

# --- Figure 1: conflict events in Burkina Faso, 2000-2025 ------------------
bf <- read_csv(here::here("data", "BF_panel_full_2000_2025.csv"), show_col_types = FALSE)
yearly <- bf %>% group_by(year) %>% summarise(events = sum(events))

p1 <- ggplot(yearly, aes(x = year, y = events)) +
  geom_col(fill = NAVY, width = 0.65) +
  geom_vline(xintercept = 2014.5, linetype = "dashed", size = 0.5) +
  annotate("text", x = 2014.7, y = max(yearly$events) * 0.9, label = "2015",
           hjust = 0, family = "serif", size = 3) +
  labs(x = "Year", y = "Number of ACLED events") +
  academic_theme
ggsave(file.path(fig_dir, "temp_evolution_2025.png"), p1, width = 6.5, height = 3.6, dpi = 300)

# --- Figure 2: pre/post-2015 split -----------------------------------------
pre  <- sum(bf$events[bf$year < 2015])
post <- sum(bf$events[bf$year >= 2015])
pie_df <- data.frame(
  group = c("2015 onward", "Before 2015"),
  value = c(post, pre)
) %>% mutate(pct = round(100 * value / sum(value), 1),
             label = sprintf("%s\n(%.1f%%)", group, pct))

p2 <- ggplot(pie_df, aes(x = "", y = value, fill = group)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("2015 onward" = NAVY, "Before 2015" = LGREY)) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5),
            family = "serif", size = 3.2) +
  theme_void(base_family = "serif") +
  theme(legend.position = "none")
ggsave(file.path(fig_dir, "pre_post_2015.png"), p2, width = 4.3, height = 4.3, dpi = 300)

# --- Figure 3: disorder-type frequency --------------------------------------
# (Percentages as reported in Table 2 of the paper; recompute from the raw
# ACLED extract with `count(disorder_type)` if you want to regenerate them.)
disorder_df <- data.frame(
  disorder_type = factor(c("Political\nviolence", "Strategic\ndevelopments",
                            "Demonstrations", "Pol. violence;\nDemonstrations"),
                          levels = c("Political\nviolence", "Strategic\ndevelopments",
                                     "Demonstrations", "Pol. violence;\nDemonstrations")),
  pct = c(67.0, 19.8, 13.1, 0.1)
)

p3 <- ggplot(disorder_df, aes(x = disorder_type, y = pct, fill = disorder_type)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = paste0(pct, "%")), vjust = -0.6, family = "serif", size = 3) +
  scale_fill_manual(values = c(NAVY, "#4d5d75", GREY, LGREY)) +
  labs(x = NULL, y = "Share of all recorded events (%)") +
  ylim(0, 78) +
  academic_theme +
  theme(legend.position = "none")
ggsave(file.path(fig_dir, "disorder_type.png"), p3, width = 5.6, height = 3.6, dpi = 300)

# --- Figure 6: gold-site tercile dose-response ------------------------------
bf <- bf %>% mutate(gold_tercile = ntile(nbr_site, 3))
tercile_df <- bind_rows(
  bf %>% filter(year < 2015) %>% group_by(gold_tercile) %>%
    summarise(mean_events = mean(events)) %>% mutate(period = "Before 2015"),
  bf %>% filter(year >= 2015) %>% group_by(gold_tercile) %>%
    summarise(mean_events = mean(events)) %>% mutate(period = "2015 onward")
) %>%
  mutate(gold_tercile = factor(gold_tercile, labels = c("Low", "Medium", "High")),
         period = factor(period, levels = c("Before 2015", "2015 onward")))

p4 <- ggplot(tercile_df, aes(x = gold_tercile, y = mean_events, fill = period)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.55) +
  scale_fill_manual(values = c("Before 2015" = GREY, "2015 onward" = NAVY)) +
  labs(x = "Gold-site density tercile", y = "Mean conflict events per region-year",
       fill = NULL) +
  academic_theme +
  theme(legend.position = c(0.25, 0.85))
ggsave(file.path(fig_dir, "gold_tercile_doseresponse.png"), p4, width = 5.2, height = 3.6, dpi = 300)

# --- Figure 7: border-proximity coefficient by country ----------------------
# (Coefficients as estimated in 04_crossborder_model.R)
border_df <- data.frame(
  country = factor(c("Burkina Faso", "Mali", "Niger"),
                    levels = c("Burkina Faso", "Mali", "Niger")),
  coef = c(-0.01128, -0.00316, 0.00064),
  p_label = c("p<0.0001", "p=0.0101", "p=0.6576")
)

p5 <- ggplot(border_df, aes(x = country, y = coef, fill = country)) +
  geom_col(width = 0.5) +
  geom_hline(yintercept = 0, size = 0.4) +
  geom_text(aes(label = p_label, vjust = ifelse(coef < 0, 1.6, -0.8)),
            family = "serif", size = 3) +
  scale_fill_manual(values = c("Burkina Faso" = NAVY, "Mali" = NAVY, "Niger" = GREY)) +
  labs(x = NULL, y = expression("Coefficient: distance-to-border" ~ "×" ~ "post-2015")) +
  expand_limits(y = c(min(border_df$coef) * 1.18, max(border_df$coef) * 1.6)) +
  academic_theme +
  theme(legend.position = "none",
        axis.title.y = element_text(margin = margin(r = 8)),
        plot.margin = margin(t = 12, r = 10, b = 5, l = 5))
ggsave(file.path(fig_dir, "border_spillover_by_country.png"), p5, width = 5.8, height = 4.0, dpi = 300)

cat("Figures written to", fig_dir, "\n")
