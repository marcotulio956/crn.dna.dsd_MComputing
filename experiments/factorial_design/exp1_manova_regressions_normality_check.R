library(dplyr)
library(tidyr)
library(ggplot2)

# Load your generated CRN data
crn_data <- read.csv("./crn_stochastic_metrics_800ms.csv")

# ------------------------------------------------------------------------------
# STEP 1: Normalize metrics and plot against a Standard Normal Distribution
# ------------------------------------------------------------------------------
plot_normality <- function(data) {
  
  # Standardize the regime strings to avoid typos/case mismatches
  df_filtered <- data %>%
    mutate(regime_clean = trimws(tolower(regime))) %>%
    filter(regime_clean == "tonic_spiking_hv")
  
  # SAFETY CHECK: Stop if no data matches the filter
  if (nrow(df_filtered) == 0) {
    stop(paste(
      "Error: No data found for 'tonic spiking lv'.\n",
      "Available regimes in your CSV are:\n", 
      paste(unique(data$regime), collapse = ", ")
    ))
  }
  
  # Select relevant columns and pivot to long format
  df_long <- df_filtered %>%
    select(regime, replicate, delay_first:snr) %>%
    pivot_longer(cols = delay_first:snr, names_to = "metric", values_to = "value") %>%
    # CRITICAL: Remove NAs (e.g., quiescent regimes won't have a 'delay_first' or 'mean_fr')
    filter(!is.na(value)) %>%
    # Group by both regime and metric to normalize within each specific context
    group_by(regime, metric) %>%
    mutate(
      mean_val = mean(value),
      sd_val = sd(value),
      # Calculate Z-score for normalization
      z_score = ifelse(sd_val > 0, (value - mean_val) / sd_val, 0) 
    ) %>%
    ungroup()
  
  # Create the plot
  p <- ggplot(df_long, aes(x = z_score)) +
    # FIXED: Replaced geom_histogram with a smooth density curve to avoid thick boxes
    geom_density(fill = "steelblue", color = "darkblue", alpha = 0.4) +
    # Overlay the theoretical standard normal bell curve
    stat_function(fun = dnorm, args = list(mean = 0, sd = 1), color = "red", linewidth = 1) +
    # Changed to facet_wrap since there is only one regime now
    facet_wrap(~ metric, ncol = 1, scales = "free_y") +
    theme_bw() +
    labs(
      title = "CRN Normality Check: Tonic Spiking LV Metrics vs. Standard Normal Curve",
      subtitle = "Red line ~N(mean=0, std=1), N=30",
      x = "Z-Score (Normalized Value)",
      y = "Density"
    ) +
    theme(strip.text.y = element_text(angle = 0))
  
  print(p)
  ggsave("crn_normality_tonic_spiking_lv.png", plot = p, width = 8, height = 14)
  
  
  # Bonus: Compute formal Shapiro-Wilk p-values for reference
  shapiro_results <- df_long %>%
    group_by(regime, metric) %>%
    # Shapiro-Wilk requires between 3 and 5000 observations
    summarise(
      n = n(),
      shapiro_p = ifelse(n >= 3 & sd_val > 0, shapiro.test(value)$p.value, NA),
      is_normal = ifelse(!is.na(shapiro_p) & shapiro_p > 0.05, "Yes", "No"),
      .groups = "drop"
    )
  
  write.csv(shapiro_results, "crn_shapiro_normality_results_tonic_spiking_lv.csv", row.names = FALSE)
  return(shapiro_results)
}

extract_reference_normality <- function(ref_timeseries_path, tmax = 800) {
  cat("\nLoading Reference Dataset for Normality Check...\n")
  
  # 1. Load and clean the time series dataset
  ref_ts <- read.csv(ref_timeseries_path)
  
  # Standardize regime names inside the raw time series to avoid typo mismatches
  ref_ts <- ref_ts %>%
    mutate(regime_clean = trimws(tolower(regime)))
  
  # 2. Filter strictly for the tonic spiking lv regime
  target_regime <- "tonic_spiking_hv"
  ref_ts_filtered <- ref_ts %>%
    filter(regime_clean == target_regime)
  
  # Safety Check: Stop if the regime doesn't exist in this dataset
  if (nrow(ref_ts_filtered) == 0) {
    stop(paste(
      "Error: No data found for 'tonic_spiking_lv' in the reference file.\n",
      "Available regimes in this CSV are:\n", 
      paste(unique(ref_ts$regime), collapse = ", ")
    ))
  }
  
  cat(paste("Extracting spiking metrics for:", target_regime, "\n"))
  
  # 3. Calculate spiking metrics per run_id/replicate
  ref_metrics <- ref_ts_filtered %>%
    group_by(run_id, regime) %>%
    summarise(
      metrics = list(calculate_spiking_metrics(time, V, tmax)),
      .groups = "drop"
    ) %>%
    unnest(metrics)
  
  # 4. Transform to long format and normalize data (Z-score)
  df_long <- ref_metrics %>%
    pivot_longer(cols = delay_first:snr, names_to = "metric", values_to = "value") %>%
    filter(!is.na(value)) %>%
    group_by(metric) %>%
    mutate(
      mean_val = mean(value),
      sd_val = sd(value),
      z_score = ifelse(sd_val > 0, (value - mean_val) / sd_val, 0)
    ) %>%
    ungroup()
  
  # 5. Plot the smooth density curve vs Standard Normal Curve
  cat("Generating smooth density normality plot...\n")
  p <- ggplot(df_long, aes(x = z_score)) +
    geom_density(fill = "steelblue", color = "darkblue", alpha = 0.4) +
    stat_function(fun = dnorm, args = list(mean = 0, sd = 1), color = "red", linewidth = 1) +
    facet_wrap(~ metric, ncol = 1, scales = "free_y") +
    theme_bw() +
    labs(
      title = "REF Normality Check: Tonic Spiking LV Metrics vs. Standard Normal Curve",
      subtitle = "Red line ~N(mean=0, std=1), N=30",
      x = "Z-Score (Normalized Value)",
      y = "Density"
    ) +
    theme(strip.text.x = element_text(face = "bold"))
  
  print(p)
  ggsave("ref_normality_tonic_spiking_lv.png", plot = p, width = 8, height = 14)
  
  # 6. Compute formal Shapiro-Wilk p-values
  shapiro_results <- df_long %>%
    group_by(metric) %>%
    summarise(
      n = n(),
      shapiro_p = ifelse(n >= 3 & sd_val[1] > 0, shapiro.test(value)$p.value, NA),
      is_normal = ifelse(!is.na(shapiro_p) & shapiro_p > 0.05, "Yes", "No"),
      .groups = "drop"
    ) %>%
    mutate(regime = target_regime) %>%
    select(regime, metric, n, shapiro_p, is_normal)
  
  write.csv(shapiro_results, "ref_shapiro_results_tonic_spiking_lv.csv", row.names = FALSE)
  cat("Reference normality check complete! Files saved.\n")
  
  return(shapiro_results)
}

# ------------------------------------------------------------------------------
# STEP 2: MANOVA and Linear Regression (Sensitivity Analysis)
# ------------------------------------------------------------------------------
run_sensitivity_analysis <- function(data) {
  
  # Filter for complete cases across all metrics (removes non-spiking regimes)
  # We also exclude sub_var and snr here if they are mostly NA during high spiking, 
  # but adjust the select() as needed for your specific complete cases.
  spiking_data <- data %>%
    select(Iapp, Mtot, Wtot, mean_fr, v_first_peak, delay_first, cv_isi) %>%
    drop_na() 
  
  cat("\n--- Running MANOVA ---\n")
  # Bind the dependent variables into a matrix
  Y <- cbind(spiking_data$mean_fr, spiking_data$v_first_peak, spiking_data$delay_first, spiking_data$cv_isi)
  
  # Fit the MANOVA model (testing main effects and their interactions)
  manova_model <- manova(Y ~ Iapp * Mtot * Wtot, data = spiking_data)
  print(summary(manova_model)) # Wilks' Lambda is default
  
  cat("\n--- Multivariable Linear Regressions (Sum of Squares) ---\n")
  
  # Fit individual linear models for sensitivity analysis
  # Example 1: Mean Firing Rate
  lm_mean_fr <- lm(mean_fr ~ Iapp * Mtot * Wtot, data = spiking_data)
  
  cat("\n--- Multivariable Linear Regressions (Sum of Squares) ---\n")
  
  # Fit individual linear models for sensitivity analysis
  lm_mean_fr <- lm(mean_fr ~ Iapp * Mtot * Wtot, data = spiking_data)
  
  # ---------------------------------------------------------
  # NOVO: Extração e Teste da Normalidade dos Erros (Resíduos)
  # ---------------------------------------------------------
  cat("\nDiagnosticando a Normalidade dos Erros para mean_fr:\n")
  
  # 1. Extrair os resíduos do modelo
  residuos_mean_fr <- residuals(lm_mean_fr)
  
  # 2. Teste formal de Shapiro-Wilk nos resíduos
  # (Verifica se a distribuição do erro difere significativamente de uma normal)
  shapiro_residuos <- shapiro.test(residuos_mean_fr)
  print(shapiro_residuos)
  
  if(shapiro_residuos$p.value < 0.05) {
    cat("Atenção: Os resíduos NÃO são normais (p < 0.05). Considere transformação ou testes não-paramétricos.\n")
  } else {
    cat("Sucesso: Os resíduos parecem normais (p >= 0.05). A ANOVA é válida.\n")
  }
  
  # 3. Gerar Q-Q Plot dos Resíduos Visualmente
  p_qq <- ggplot(data.frame(residuos = residuos_mean_fr), aes(sample = residuos)) +
    stat_qq(color = "darkblue", alpha = 0.6) +
    stat_qq_line(color = "red", linewidth = 1) +
    theme_bw() +
    labs(
      title = "Q-Q Plot: Residue Normality mean_fr",
      x = "Quantis Teóricos (Distribuição Normal)",
      y = "Quantis da Amostra (Resíduos do Modelo)"
    )
  print(p_qq)
  ggsave("qqplot_residuos_mean_fr.png", plot = p_qq, width = 6, height = 5)
  # ---------------------------------------------------------
  

  # anova() provides the Sum of Squares table to see which factor drives variance
  cat("\nANOVA Table for Mean Firing Rate:\n")
  print(anova(lm_mean_fr)) 
  
  # Example 2: Transform Data if not normal
  # If normality failed, you can apply a transformation directly in the formula
  # e.g., log transformation (adding a small constant if zeroes exist)
  cat("\nANOVA Table for log(Mean Firing Rate) [Transformation Example]:\n")
  lm_mean_fr_log <- lm(log(mean_fr + 1e-5) ~ Iapp * Mtot * Wtot, data = spiking_data)
  print(anova(lm_mean_fr_log))
  
  # You can repeat the lm() and anova() for v_first_peak, cv_isi, etc.
}


# Execute the plotting
normality_stats <- plot_normality(crn_data)
normality_reference <- extract_reference_normality("./factorial_design/data/mlexactboth_timeseries_800ms.csv")
run_sensitivity_analysis(crn_data)

# Here are the non-parametric tests you should use instead:
#   
# For the Multivariate Analysis (Non-Parametric MANOVA):
# You should use PERMANOVA (Permutational Multivariate Analysis of Variance). It uses distance matrices (like Euclidean or Bray-Curtis) and permutation testing rather than assuming multivariate normality.
# library(vegan)
# # Create a distance matrix of your metrics
# # Note: Metrics must be scaled because they have different units (Hz, Volts, ms)
# Y_scaled <- scale(spiking_data[, c("mean_fr", "v_first_peak", "delay_first", "cv_isi")])
# 
# # Run PERMANOVA
# permanova_model <- adonis2(Y_scaled ~ Iapp * Mtot * Wtot, data = crn_data, method = "euclidean", permutations = 999)
# print(permanova_model)
# # 
# # For the Individual Metric Sensitivity (Non-Parametric ANOVA):
# #   If you want to see the impact of factors on individual metrics without assuming normality, you use the Kruskal-Wallis test. However, Kruskal-Wallis only tests one factor at a time (it cannot handle interactions like Iapp * Mtot).
# # Test if Iapp significantly alters Mean Firing Rate
# kruskal.test(mean_fr ~ as.factor(Iapp), data = crn_data)
