library(tidyverse)
library(broom)      # converts model outputs into clean data frames
library(car)        # for additional regression diagnostics
library(lmtest)

source("DataCleaning.R")
source("DataLoading.R")

# Prepare analytical datasets


age_midpoints <- c(
  "16-17"  = 16.5,
  "18-21"  = 19.5,
  "22-29"  = 25.5,
  "30-39"  = 34.5,
  "40-49"  = 44.5,
  "50-59"  = 54.5,
  "60+"    = 65.0
)

df_regression <- df_age_clean %>%
  filter(emp_type == "All") %>%
  filter(description != "All employees") %>%
  mutate(
    age_mid = age_midpoints[as.character(description)]
  ) %>%
  filter(!is.na(gpg_median))   # drop any suppressed rows

# Quick check
print(df_regression[, c("description", "age_mid", "gpg_median")])


# Analysis 1 — Simple linear regression: age vs GPG
#
# Research question: is there a statistically significant linear relationship
# between age (midpoint) and the gender pay gap?
#
# H0 (null hypothesis):      age has no effect on GPG (slope = 0)
# H1 (alternative hypothesis): age has a significant positive effect on GPG

model_age <- lm(gpg_median ~ age_mid, data = df_regression)

# Full model summary
summary(model_age)

tidy(model_age)       # coefficients, standard errors, t-stats, p-values
glance(model_age)     # R-squared, F-statistic, AIC, BIC


cat("\n── Model 1: Simple linear regression (age → GPG) ──\n")
cat("Slope (age effect):", round(coef(model_age)["age_mid"], 3), "pp per year\n")
cat("R-squared:         ", round(summary(model_age)$r.squared, 3), "\n")
cat("p-value (age):     ", round(tidy(model_age)$p.value[2], 4), "\n")

# Analysis 2 — Check for non-linearity (polynomial regression)
#
# The age-GPG relationship may not be perfectly linear — the gap rises
# steeply then levels off at 60+. We test whether adding a quadratic
# term (age²) significantly improves the model fit.

model_age_poly <- lm(gpg_median ~ age_mid + I(age_mid^2),
                     data = df_regression)

summary(model_age_poly)

# Compare linear vs polynomial using ANOVA

anova(model_age, model_age_poly)

cat("\n── Model 2: Polynomial regression ──\n")
cat("Linear R²:     ", round(summary(model_age)$r.squared, 3), "\n")
cat("Polynomial R²: ", round(summary(model_age_poly)$r.squared, 3), "\n")

# Analysis 3 — Regression diagnostics 
#
# Before trusting regression results we check four key assumptions:
# 1. Linearity         — residuals vs fitted values should show no pattern
# 2. Normality         — residuals should be roughly normally distributed
# 3. Homoscedasticity  — residual variance should be constant (not fan-shaped)
# 4. No influential outliers

# Save diagnostic plots to a file
png("plot_diagnostics.png", width = 900, height = 700, res = 120)
par(mfrow = c(2, 2))   # 2×2 grid of the four standard diagnostic plots
plot(model_age, main = "Model diagnostics — age → GPG")
dev.off()

# Breusch-Pagan test for heteroscedasticity
# H0: residual variance is constant (homoscedastic — good)
# H1: residual variance changes with fitted values (heteroscedastic — problem)
bp_test <- bptest(model_age)
cat("\n── Breusch-Pagan test (heteroscedasticity) ──\n")
print(bp_test)
cat("If p > 0.05: no evidence of heteroscedasticity — assumption holds\n")

# Analysis 4 — t-test: public vs private sector GPG 
# Research question: is the gender pay gap significantly different between the public and private sector?
# H0: mean GPG is the same in public and private sectors
# H1: mean GPG differs between public and private sectors

public_gpg  <- df_pubpriv_clean %>%
  filter(description == "Public sector") %>%
  pull(gpg_median)

private_gpg <- df_pubpriv_clean %>%
  filter(description == "Private sector") %>%
  pull(gpg_median)

cat("\n── Public sector GPG values (median, by employment type):\n")
print(public_gpg)
cat("── Private sector GPG values (median, by employment type):\n")
print(private_gpg)

# Welch two-sample t-test (does not assume equal variance — safer default)
t_result <- t.test(private_gpg, public_gpg,
                   alternative = "greater",   # H1: private > public
                   var.equal   = FALSE)

cat("\n── t-test: private vs public sector GPG ──\n")
print(t_result)

cat("\nPrivate sector mean GPG:", round(mean(private_gpg), 2), "%\n")
cat("Public sector mean GPG: ", round(mean(public_gpg),  2), "%\n")
cat("Difference:  ", round(mean(private_gpg) - mean(public_gpg), 2), "pp\n")

# Interpretation
if (t_result$p.value < 0.05) {
  cat("\nResult: p <", round(t_result$p.value, 4),
      "— reject H0. Private sector gap is significantly larger.\n")
} else {
  cat("\nResult: p =", round(t_result$p.value, 4),
      "— fail to reject H0. Difference is not statistically significant.\n")
}

# Analysis 5 — One-way ANOVA: GPG across industries

# Research question: is the mean GPG significantly different across industries?

# H0: all industries have the same mean GPG
# H1: at least one industry has a significantly different mean GPG

anova_data <- df_industry_clean %>%
  filter(emp_type == "All") %>%
  filter(!is.na(gpg_median)) %>%
  mutate(short_label = as.character(short_label))

anova_model <- aov(gpg_median ~ short_label, data = anova_data)

cat("\n── One-way ANOVA: GPG across industries ──\n")
summary(anova_model)

# If ANOVA is significant, run Tukey's HSD post-hoc test
# This tells us WHICH pairs of industries differ significantly
tukey_result <- TukeyHSD(anova_model)

# Show only the significant pairs (p.adj < 0.05)
tukey_df <- as.data.frame(tukey_result$short_label) %>%
  rownames_to_column("comparison") %>%
  filter(`p adj` < 0.05) %>%
  arrange(`p adj`)

cat("\nSignificant industry pair differences (Tukey HSD, p < 0.05):\n")
print(tukey_df)

# ── 8. Analysis 6 — Correlation matrix across dimensions ─────────────────────
#
# Research question: how correlated are median and mean GPG?
# A high correlation means either metric tells the same story.
# A low correlation means mean is picking up skew that median misses.

cor_data <- bind_rows(
  df_age_clean       %>% filter(emp_type == "All") %>%
    select(gpg_median, gpg_mean),
  df_industry_clean  %>% filter(emp_type == "All") %>%
    select(gpg_median, gpg_mean),
  df_occupation_clean %>% filter(emp_type == "All") %>%
    select(gpg_median, gpg_mean),
  df_region_clean    %>% filter(emp_type == "All") %>%
    select(gpg_median, gpg_mean)
) %>%
  filter(!is.na(gpg_median), !is.na(gpg_mean))

cor_result <- cor.test(cor_data$gpg_median, cor_data$gpg_mean,
                       method = "pearson")

cat("\n── Pearson correlation: median GPG vs mean GPG ──\n")
cat("r =", round(cor_result$estimate, 3), "\n")
cat("p =", round(cor_result$p.value,  4), "\n")

# Scatter plot of median vs mean GPG
p_cor <- ggplot(cor_data, aes(x = gpg_median, y = gpg_mean)) +
  geom_point(colour = "#378ADD", alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", colour = "#E24B4A",
              se = TRUE, linewidth = 1) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", colour = "grey50") +
  annotate("text", x = -3, y = 28,
           label = paste0("r = ", round(cor_result$estimate, 3)),
           hjust = 0, size = 4, colour = "#E24B4A") +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Median vs mean gender pay gap across all dimensions",
    subtitle = "Points above the dashed line = mean GPG exceeds median (right-skewed distribution)",
    x        = "Median GPG (%)",
    y        = "Mean GPG (%)",
    caption  = "Source: ONS ASHE 2025"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(colour = "grey40"))

print(p_cor)
ggsave("plot9_correlation.png", p_cor, width = 8, height = 6, dpi = 150)


# ── 9. Summary results table

cat("  PHASE 4 RESULTS SUMMARY\n")

cat("\n1. Linear regression (age → median GPG)\n")
cat("   Slope:     ", round(coef(model_age)["age_mid"], 3), "pp per year of age\n")
cat("   R²:        ", round(summary(model_age)$r.squared, 3), "\n")
cat("   p-value:   ", round(tidy(model_age)$p.value[2], 4), "\n")

cat("\n2. t-test (private vs public sector GPG)\n")
cat("   Private mean: ", round(mean(private_gpg), 2), "%\n")
cat("   Public mean:  ", round(mean(public_gpg),  2), "%\n")
cat("   p-value:      ", round(t_result$p.value, 4), "\n")

cat("\n3. ANOVA (GPG across industries)\n")
cat("   Significant industry differences: see Tukey HSD output above\n")

cat("\n4. Pearson correlation (median vs mean GPG)\n")
cat("   r =", round(cor_result$estimate, 3),
    "  p =", round(cor_result$p.value, 4), "\n")
