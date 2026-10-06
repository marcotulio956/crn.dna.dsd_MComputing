# ==============================================================================
# Setup & Libraries
# ==============================================================================
rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

source('R/analysis.R')

# ==============================================================================
# 2. Load Data and Apply Metrics
# ==============================================================================
cat("Loading timeseries data...\n")
ts_data <- read.csv("factorial_design/data/mlexactboth_timeseries_800ms_varyingI.csv")

cat("Calculating metrics per run...\n")
# Group by run_id and iapp, then apply the metric function
results_df <- ts_data %>%
  group_by(run_id, iapp) %>%
  summarise(
    metrics = list(calculate_spiking_metrics(time, V, tmax = 800)),
    .groups = "drop"
  ) %>%
  unnest(metrics)

# Calculate residuals (difference between realization value and group mean)
results_df <- results_df %>%
  group_by(iapp) %>%
  mutate(
    res_spikes = n_spikes - mean(n_spikes, na.rm = TRUE),
    res_fr = mean_fr - mean(mean_fr, na.rm = TRUE)
  ) %>%
  ungroup()

# ==============================================================================
# 3. Plot Set 1: Spike Counts and Normality
# ==============================================================================
cat("Generating Spike Count plots...\n")

# A. Spike Count Boxplot/Scatter
p_spikes <- ggplot(results_df, aes(x = factor(iapp), y = n_spikes)) +
  geom_boxplot(outlier.shape = NA, fill = "lightblue", alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "darkblue") +
  labs(title = "Spike Count across Input Currents (I_app) ",
       x = "Applied Current (I_app)", y = "Number of Spikes") +
  theme_minimal()

# B. Histogram & Density of Spike Residuals
p_spikes_hist <- ggplot(results_df, aes(x = res_spikes)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "gray", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(results_df$res_spikes), sd = sd(results_df$res_spikes)), color = "red", lwd = 1) +
  labs(title = "Distribution of Spike Count Residuals",
       x = "Residuals (Value - Mean)", y = "Density") +
  theme_minimal()

# C. QQ-Plot to prove Normality
p_spikes_qq <- ggplot(results_df, aes(sample = res_spikes)) +
  stat_qq(color = "darkblue") +
  stat_qq_line(color = "red", lwd = 1) +
  labs(title = "Q-Q Plot: Spike Count Residuals",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

# Combine Set 1
plot_set_1 <- p_spikes / (p_spikes_hist | p_spikes_qq)
ggsave("spike_count_analysis.png", plot_set_1, width = 10, height = 8)

# ==============================================================================
# 4. Plot Set 2: Mean Firing Rate, Linear Fit, and ISI
# ==============================================================================
cat("Generating Mean Firing Rate and ISI plots...\n")

# A. Mean Firing Rate with Linear Fit
p_fr_lm <- ggplot(results_df, aes(x = iapp, y = mean_fr)) +
  geom_jitter(width = 1.5, alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red", fill = "pink", alpha = 0.3) +
  labs(title = "Mean Firing Rate vs I_app (Linear Fit)",
       x = "Applied Current (I_app)", y = "Mean Firing Rate") +
  theme_minimal()

# B. Mean ISI (Diminishing curve)
p_isi <- ggplot(results_df, aes(x = factor(iapp), y = mean_isi)) +
  geom_boxplot(fill = "orange", alpha = 0.5) +
  labs(title = "Inter-Spike Interval Diminishes with I_app",
       x = "Applied Current (I_app)", y = "Mean ISI (ms)") +
  theme_minimal()

# C. QQ-Plot to prove Normality of Firing Rate Residuals
p_fr_qq <- ggplot(results_df, aes(sample = res_fr)) +
  stat_qq(color = "darkgreen") +
  stat_qq_line(color = "red", lwd = 1) +
  labs(title = "Q-Q Plot: Firing Rate Residuals",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

# Combine Set 2
plot_set_2 <- (p_fr_lm | p_isi) / p_fr_qq
ggsave("firing_rate_isi_analysis.png", plot_set_2, width = 10, height = 8)

# ==============================================================================
# 5. Statistical Outputs (Console)
# ==============================================================================
cat("\n--- Statistical Analysis ---\n")

# Linear Model Summary for Mean Firing Rate
cat("Linear Fit Summary (Mean Firing Rate ~ I_app):\n")
fit <- lm(mean_fr ~ iapp, data = results_df)
print(summary(fit))

# Shapiro-Wilk test for normality of residuals
# Note: p-value > 0.05 implies we cannot reject the null hypothesis of normality
cat("\nShapiro-Wilk Normality Test (Spike Count Residuals):\n")
print(shapiro.test(results_df$res_spikes))

cat("\nShapiro-Wilk Normality Test (Mean Firing Rate Residuals):\n")
print(shapiro.test(results_df$res_fr))

cat("\nLinear Fit Summary (Spike Count ~ I_app):\n")
fit_spikes <- lm(n_spikes ~ iapp, data = results_df)
print(summary(fit_spikes))
# ==============================================================================
# 6. One-Factor Factorial Analysis (ANOVA & Explainability)
# ==============================================================================
cat("\n--- One-Factor Factorial Design Analysis (I_app) ---\n")

# Target variable for analysis (can change to n_spikes)
response_var <- "mean_fr"

# Convert iapp to a categorical factor for ANOVA
results_df$iapp_factor <- as.factor(results_df$iapp)

# 1. Fit the ANOVA model
# Formula: Y ~ A
anova_model <- aov(as.formula(paste(response_var, "~ iapp_factor")), data = results_df)
anova_table <- summary(anova_model)[[1]]

# 2. Extract Sum of Squares
SSA <- anova_table$`Sum Sq`[1]  # Sum of Squares for the Factor (I_app)
SSE <- anova_table$`Sum Sq`[2]  # Sum of Squares for Error (Residuals)
SST <- SSA + SSE                # Total Sum of Squares

explainability_pct <- (SSA / SST) * 100

# Build the Sum of Squares Table
ss_output <- data.frame(
  Component = c("SSA (I_app Factor)", "SSE (Error/Residuals)", "SST (Total Variance)"),
  Degrees_of_Freedom = c(anova_table$Df[1], anova_table$Df[2], sum(anova_table$Df)),
  Sum_of_Squares = c(SSA, SSE, SST),
  Mean_Square = c(anova_table$`Mean Sq`[1], anova_table$`Mean Sq`[2], NA),
  Explainability_Pct = c(explainability_pct, 100 - explainability_pct, 100)
)

cat(sprintf("\n=== TABLE: Sum of Squares & Explainability for '%s' ===\n", response_var))
print(ss_output, row.names = FALSE)
cat(sprintf("\nConclusion: Changing I_app explains %.2f%% of the variance in %s.\n", 
            explainability_pct, response_var))

# 3. Calculate 95% Confidence Intervals for each level of I_app
# By fitting a linear model without an intercept (- 1), the coefficients 
# directly represent the mean of each group, making CI extraction easy.
lm_means <- lm(as.formula(paste(response_var, "~ iapp_factor - 1")), data = results_df)

group_means <- coef(lm_means)
ci_bounds <- confint(lm_means, level = 0.95)

# Clean up row names for the table (e.g., "iapp_factor80" -> "80")
level_names <- gsub("iapp_factor", "", rownames(ci_bounds))

ci_output <- data.frame(
  I_app_Level = level_names,
  Mean_Response = group_means,
  CI_Lower_95 = ci_bounds[, 1],
  CI_Upper_95 = ci_bounds[, 2]
)
rownames(ci_output) <- NULL

cat(sprintf("\n=== TABLE: 95%% Confidence Intervals for Mean '%s' by I_app ===\n", response_var))
print(ci_output, row.names = FALSE)

# 4. (Optional) Tukey's Honest Significant Difference (HSD)
# This shows the confidence intervals for the *difference* between specific levels.
cat("\n=== Pairwise Differences between I_app levels (Tukey HSD) ===\n")
tukey_results <- TukeyHSD(anova_model)
print(tukey_results)

# ==============================================================================
# 6. Robust Non-Parametric Factorial Analysis (Permutation & Bootstrapping)
# ==============================================================================
cat("\n--- Robust Analysis (Bootstrapping & Permutation) for I_app ---\n")
library(boot)

response_var <- "mean_fr"
results_df$iapp_factor <- as.factor(results_df$iapp)

# ------------------------------------------------------------------------------
# A. Permutation ANOVA (Exact p-value without Normality Assumption)
# ------------------------------------------------------------------------------
# Calculate the real F-statistic
real_anova <- anova(lm(as.formula(paste(response_var, "~ iapp_factor")), data = results_df))
real_F <- real_anova$`F value`[1]

# Permutation loop
n_perms <- 4999
permuted_Fs <- numeric(n_perms)

set.seed(42)
for (i in 1:n_perms) {
  # Shuffle the response variable randomly
  shuffled_Y <- sample(results_df[[response_var]])
  # Calculate F-statistic for shuffled data
  perm_anova <- anova(lm(shuffled_Y ~ results_df$iapp_factor))
  permuted_Fs[i] <- perm_anova$`F value`[1]
}

# The p-value is the proportion of permuted Fs greater than or equal to our real F
p_value_perm <- sum(permuted_Fs >= real_F) / (n_perms + 1)

cat(sprintf("\n=== Permutation ANOVA Results ===\n"))
cat(sprintf("Observed F-statistic: %.3f\n", real_F))
cat(sprintf("Permutation p-value (%d shuffles): %.5f\n", n_perms, p_value_perm))

# ------------------------------------------------------------------------------
# B. Bootstrapped Explainability (SSA / SST)
# ------------------------------------------------------------------------------
# Function to calculate explainability for the boot() function
explain_func <- function(data, indices) {
  d <- data[indices, ]
  fit <- anova(lm(as.formula(paste(response_var, "~ iapp_factor")), data = d))
  SSA <- fit$`Sum Sq`[1]
  SST <- SSA + fit$`Sum Sq`[2]
  return(SSA / SST)
}

# Run bootstrap for explainability
boot_explain <- boot(data = results_df, statistic = explain_func, R = 1999)
explain_ci <- boot.ci(boot_explain, type = "bca") # Bias-Corrected and Accelerated CI

cat("\n=== Bootstrapped Explainability (Sum of Squares) ===\n")
cat(sprintf("Estimated Explainability: %.2f%%\n", boot_explain$t0 * 100))
if (!is.null(explain_ci)) {
  cat(sprintf("95%% CI [BCa]: [%.2f%%,  %.2f%%]\n", 
              explain_ci$bca[4] * 100, explain_ci$bca[5] * 100))
}

# ------------------------------------------------------------------------------
# C. Bootstrapped Confidence Intervals for Group Means
# ------------------------------------------------------------------------------
cat("\n=== Bootstrapped 95% CIs for Mean Firing Rate by I_app ===\n")

# Function to extract mean for a specific group
mean_func <- function(data, indices) {
  return(mean(data[indices], na.rm = TRUE))
}

unique_iapp <- sort(unique(results_df$iapp))
boot_ci_results <- data.frame(I_app = numeric(), Mean = numeric(), CI_Lower = numeric(), CI_Upper = numeric())

for (lvl in unique_iapp) {
  group_data <- results_df[[response_var]][results_df$iapp == lvl]
  
  # Run bootstrap for this specific I_app level
  b_out <- boot(data = group_data, statistic = mean_func, R = 1999)
  b_ci <- boot.ci(b_out, type = "perc") # Percentile interval
  
  boot_ci_results <- rbind(boot_ci_results, data.frame(
    I_app = lvl,
    Mean = b_out$t0,
    CI_Lower = b_ci$percent[4],
    CI_Upper = b_ci$percent[5]
  ))
}

print(boot_ci_results, row.names = FALSE)