# ── Phase 3: EDA Visualisations ───────────────────────────────────────────────

library(tidyverse)
library(scales)     # for nicer axis formatting
library(patchwork)  # for combining multiple plots into one figure

# install.packages("scales")
# install.packages("patchwork")


# ── 1. Setup ──────────────────────────────────────────────────────────────────

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("DataLoading.R")
source("DataCleaning.R")   # loads all the clean datasets from Phase 2

# Shared colour palette used consistently across every chart
COL_POS    <- "#E24B4A"   # red    — positive gap (men earn more)
COL_NEG    <- "#1D9E75"   # green  — negative gap (women earn more)
COL_ALL    <- "#378ADD"   # blue   — "All employees" series
COL_FT     <- "#1D9E75"   # teal   — Full-Time series
COL_PT     <- "#BA7517"   # amber  — Part-Time series
COL_MEDIAN <- "#378ADD"   # blue   — median metric
COL_MEAN   <- "#D85A30"   # coral  — mean metric

# Shared ggplot2 theme applied to every chart
theme_gpg <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title       = element_text(size = 14, face = "bold", margin = margin(b = 6)),
      plot.subtitle    = element_text(size = 11, colour = "grey40", margin = margin(b = 12)),
      plot.caption     = element_text(size = 9,  colour = "grey50", margin = margin(t = 10)),
      axis.title       = element_text(size = 11, colour = "grey30"),
      axis.text        = element_text(size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey92"),
      legend.position  = "bottom",
      legend.title     = element_blank()
    )
}


# ── 2. Plot 1 — Overall GPG: median vs mean by employment type ────────────────


p1_data <- bind_rows(
  df_age_clean %>%
    filter(description == "All employees") %>%
    select(emp_type, gpg_median, gpg_mean)
) %>%
  pivot_longer(cols = c(gpg_median, gpg_mean),
               names_to = "metric", values_to = "value") %>%
  mutate(
    metric   = recode(metric, gpg_median = "Median GPG", gpg_mean = "Mean GPG"),
    emp_type = factor(emp_type, levels = c("All", "Full-Time", "Part-Time"))
  )

p1 <- ggplot(p1_data, aes(x = emp_type, y = value, fill = metric)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_text(aes(label = paste0(round(value, 1), "%"),
                vjust = ifelse(value >= 0, -0.4, 1.3)),
            position = position_dodge(width = 0.6), size = 3.5) +
  scale_fill_manual(values = c("Median GPG" = COL_MEDIAN, "Mean GPG" = COL_MEAN)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = "UK gender pay gap by employment type, 2025",
    subtitle = "Positive values = men earn more; negative = women earn more",
    x        = "Employment type",
    y        = "Gender pay gap (%)",
    caption  = "Source: ONS Annual Survey of Hours and Earnings (ASHE), 2025"
  ) +
  theme_gpg()

print(p1)
ggsave("plot1_overall.png", p1, width = 8, height = 5, dpi = 150)


# ── 3. Plot 2 — GPG by age group ─────────────────────────────────────────────

p2_data <- df_age_clean %>%
  filter(emp_type == "All", description != "All employees")

p2 <- ggplot(p2_data, aes(x = description, y = gpg_median,
                          fill = gpg_median)) +
  geom_col(show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_text(aes(label = paste0(round(gpg_median, 1), "%"),
                vjust = ifelse(gpg_median >= 0, -0.4, 1.3)),
            size = 3.5) +
  scale_fill_gradient2(low = COL_NEG, mid = "grey90",
                       high = COL_POS, midpoint = 0) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(-2, 23)) +
  labs(
    title    = "Gender pay gap by age group, 2025 (all employees, median)",
    subtitle = "The gap rises sharply from the 30s — consistent with career interruption and the motherhood penalty",
    x        = "Age group",
    y        = "Median gender pay gap (%)",
    caption  = "Source: ONS ASHE 2025"
  ) +
  theme_gpg()

print(p2)
ggsave("plot2_age.png", p2, width = 9, height = 5, dpi = 150)


# ── 4. Plot 3 — Age gap: all vs full-time vs part-time comparison ─────────────

p3_data <- df_age_clean %>%
  filter(description != "All employees") %>%
  mutate(emp_type = factor(emp_type,
                           levels = c("All", "Full-Time", "Part-Time")))

p3 <- ggplot(p3_data, aes(x = description, y = gpg_median,
                          colour = emp_type, group = emp_type)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  scale_colour_manual(values = c("All"       = COL_ALL,
                                 "Full-Time" = COL_FT,
                                 "Part-Time" = COL_PT)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Gender pay gap by age group and employment type, 2025",
    subtitle = "Part-time workers show a flat or negative gap — a structural contrast to full-time employees",
    x        = "Age group",
    y        = "Median gender pay gap (%)",
    caption  = "Source: ONS ASHE 2025"
  ) +
  theme_gpg()

print(p3)
ggsave("plot3_age_emptype.png", p3, width = 9, height = 5, dpi = 150)


# ── 5. Plot 4 — GPG by industry (sorted bar chart) ───────────────────────────

p4_data <- df_industry_clean %>%
  filter(emp_type == "All") %>%
  arrange(gpg_median) %>%
  mutate(short_label = factor(short_label, levels = short_label))

p4 <- ggplot(p4_data, aes(x = short_label, y = gpg_median,
                          fill = gpg_median)) +
  geom_col(show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_hline(yintercept = 12.8, linetype = "dotted",
             colour = COL_POS, linewidth = 0.8) +
  annotate("text", x = 2, y = 13.8,
           label = "UK average (12.8%)", colour = COL_POS, size = 3.2) +
  geom_text(aes(label = paste0(round(gpg_median, 1), "%"),
                hjust = ifelse(gpg_median >= 0, -0.15, 1.15)),
            size = 3.2) +
  scale_fill_gradient2(low = COL_NEG, mid = "grey90",
                       high = COL_POS, midpoint = 0) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(-5, 32)) +
  coord_flip() +
  labs(
    title    = "Gender pay gap by industry, 2025 (all employees, median)",
    subtitle = "Financial & insurance activities has the largest gap at 27.2% — more than double the UK average",
    x        = NULL,
    y        = "Median gender pay gap (%)",
    caption  = "Source: ONS ASHE 2025"
  ) +
  theme_gpg()

print(p4)
ggsave("plot4_industry.png", p4, width = 10, height = 7, dpi = 150)


# ── 6. Plot 5 — GPG by occupation ────────────────────────────────────────────

p5_data <- df_occupation_clean %>%
  filter(emp_type == "All") %>%
  arrange(gpg_median) %>%
  mutate(description = factor(description, levels = description))

p5 <- ggplot(p5_data, aes(x = description, y = gpg_median,
                          fill = gpg_median)) +
  geom_col(show.legend = FALSE) +
  geom_hline(yintercept = 12.8, linetype = "dotted",
             colour = COL_POS, linewidth = 0.8) +
  annotate("text", x = 2, y = 13.8,
           label = "UK average (12.8%)", colour = COL_POS, size = 3.2) +
  geom_text(aes(label = paste0(round(gpg_median, 1), "%"),
                hjust = ifelse(gpg_median >= 0, -0.15, 1.15)),
            size = 3.5) +
  scale_fill_gradient2(low = COL_NEG, mid = "grey90",
                       high = COL_POS, midpoint = 0) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 20)) +
  coord_flip() +
  labs(
    title    = "Gender pay gap by occupation group, 2025 (all employees, median)",
    subtitle = "Skilled trades and associate professionals show the largest gaps within occupation categories",
    x        = NULL,
    y        = "Median gender pay gap (%)",
    caption  = "Source: ONS ASHE 2025"
  ) +
  theme_gpg()

print(p5)
ggsave("plot5_occupation.png", p5, width = 10, height = 6, dpi = 150)


# ── 7. Plot 6 — GPG by region ─────────────────────────────────────────────────

p6_data <- df_region_clean %>%
  filter(emp_type == "All") %>%
  pivot_longer(cols = c(gpg_median, gpg_mean),
               names_to = "metric", values_to = "value") %>%
  mutate(metric = recode(metric,
                         gpg_median = "Median GPG",
                         gpg_mean   = "Mean GPG"))

p6 <- ggplot(p6_data, aes(x = description, y = value, fill = metric)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_hline(yintercept = 12.8, linetype = "dotted",
             colour = "grey40", linewidth = 0.8) +
  annotate("text", x = 0.6, y = 13.6,
           label = "UK median avg", colour = "grey40", size = 3, hjust = 0) +
  scale_fill_manual(values = c("Median GPG" = COL_MEDIAN,
                               "Mean GPG"   = COL_MEAN)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  coord_flip() +
  labs(
    title    = "Gender pay gap by UK region, 2025 (all employees)",
    subtitle = "London's mean gap (17.8%) far exceeds its median (11.8%) — a sign of high-earner skew",
    x        = NULL,
    y        = "Gender pay gap (%)",
    caption  = "Source: ONS ASHE 2025"
  ) +
  theme_gpg()

print(p6)
ggsave("plot6_region.png", p6, width = 10, height = 6, dpi = 150)


# ── 8. Plot 7 — Public vs private sector ──────────────────────────────────────

p7_data <- df_pubpriv_clean %>%
  pivot_longer(cols = c(gpg_median, gpg_mean),
               names_to = "metric", values_to = "value") %>%
  mutate(
    metric   = recode(metric, gpg_median = "Median GPG", gpg_mean = "Mean GPG"),
    emp_type = factor(emp_type, levels = c("All", "Full-Time", "Part-Time"))
  )

p7 <- ggplot(p7_data, aes(x = description, y = value,
                          fill = metric, alpha = emp_type)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  scale_fill_manual(values = c("Median GPG" = COL_MEDIAN,
                               "Mean GPG"   = COL_MEAN)) +
  scale_alpha_manual(values = c("All" = 1, "Full-Time" = 0.7,
                                "Part-Time" = 0.4)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  facet_wrap(~ emp_type, ncol = 3) +
  labs(
    title    = "Gender pay gap by sector and employment type, 2025",
    subtitle = "Private sector consistently shows a wider gap than public sector across all employment types",
    x        = NULL,
    y        = "Gender pay gap (%)",
    caption  = "Source: ONS ASHE 2025"
  ) +
  theme_gpg() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 20, hjust = 1))

print(p7)
ggsave("plot7_pubpriv.png", p7, width = 11, height = 6, dpi = 150)


# ── 9. Plot 8 — Summary panel: all four key dimensions ───────────────────────


p_summary <- (p2 + p4) / (p5 + p6) +
  plot_annotation(
    title   = "UK Gender Pay Gap 2025 — exploratory analysis summary",
    caption = "Source: ONS Annual Survey of Hours and Earnings (ASHE), 2025",
    theme   = theme(plot.title = element_text(size = 15, face = "bold"))
  )

print(p_summary)

ggsave("plot8_summary_panel.png", p_summary,
       width = 18, height = 14, dpi = 150)

message("Phase 3 complete — 8 plots saved to your project folder.")
