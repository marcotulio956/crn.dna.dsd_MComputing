# ==============================================================================
# Setup & Libraries
# ==============================================================================
rm(list = ls())
library(dplyr) 
library(tidyr) 
library(ggplot2)

source("R/analysis.R")
cat("Carregando datasets...\n")

# Carrega dataset CRN (Métricas já extraídas)
crn_data <- read.csv("crn_stochastic_metrics_tonicspiking_hv_vol3.csv") %>%
  filter(grepl("tonic_spiking_hV", regime)) %>% # Filtro corrigido para o regime específico
  mutate(system = "CRN")

# Carrega dataset de Séries Temporais Exatas (ML Exact)
ts_exact <- read.csv("factorial_design/data/mlexactboth_timeseries_800ms_tonichv.csv")

cat("Extraindo métricas do dataset Exato...\n")
exact_metrics <- ts_exact %>%
  filter(grepl("tonic_spiking_hV", regime) | grepl("iapp_100", regime)) %>% # Garante compatibilidade de nomes
  group_by(run_id, regime) %>%
  summarise(
    metrics = list(calculate_spiking_metrics(time, V, tmax = 800)),
    .groups = "drop"
  ) %>%
  unnest(metrics) %>%
  mutate(system = "Exato (ML)")

# REMOVIDO n_spikes e mean_isi para parear perfeitamente com os dados CRN pré-extraídos
metric_names <- c("mean_fr", "delay_first", "v_first_peak", "cv_isi", "fano_factor", "sub_var", "snr")

# Combina os dois datasets para comparação
combined_data <- bind_rows(
  crn_data %>% select(system, all_of(metric_names)),
  exact_metrics %>% select(system, all_of(metric_names))
) %>%
  pivot_longer(cols = all_of(metric_names), names_to = "metric", values_to = "value") %>%
  filter(!is.na(value)) # Remove NAs gerados por simulações sem spikes

# ==============================================================================
# 2. Análise Visual (Plots)
# ==============================================================================
cat("Gerando gráficos de comparação...\n")

p_comparacao <- ggplot(combined_data, aes(x = system, y = value, fill = system)) +
  geom_violin(alpha = 0.4, trim = FALSE, color = NA) +
  geom_boxplot(width = 0.3, alpha = 0.8, color = "black", outlier.size = 0.5) +
  facet_wrap(~metric, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = c("CRN" = "#2c7fb8", "Exato (ML)" = "#f03b20")) +
  theme_bw() +
  labs(title = "Comparação de Algoritmos de Simulação: CRN vs Exato (ML)",
       subtitle = "Regime: Tonic Spiking hV (800ms) - Distribuições Não-Paramétricas",
       x = "", y = "Valor da Métrica") +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))

ggsave("experimento3_comparacao_nao_parametrica.png", p_comparacao, width = 12, height = 10)

# ==============================================================================
# 3. Análise Estatística Não-Paramétrica
# ==============================================================================
boot_ci_median <- function(data, n_boot = 2000) {
  medians <- numeric(n_boot)
  for(i in 1:n_boot) {
    medians[i] <- median(sample(data, replace = TRUE), na.rm = TRUE)
  }
  quantiles <- quantile(medians, probs = c(0.025, 0.975), na.rm = TRUE)
  return(c(lower = unname(quantiles[1]), upper = unname(quantiles[2]), median = median(data, na.rm = TRUE)))
  # return(c(lower = quantiles[1], upper = quantiles[2], median = median(data, na.rm = TRUE)))
}

cat("\n========================================================================================================================\n")
cat("RESULTADOS DA COMPARAÇÃO NÃO-PARAMÉTRICA (Regime: Tonic Spiking hV)\n")
cat("========================================================================================================================\n")
cat(sprintf("%-15s | %-12s | %-30s | %-30s | %-12s | %-15s\n", 
            "Métrica", "Normalidade", "Mediana CRN [IC 95%]", "Mediana Exato [IC 95%]", "p-Wilcoxon", "Maior Valor"))
cat(rep("-", 125), "\n", sep="")

for (m in metric_names) {
  df_m <- combined_data %>% filter(metric == m)
  if(nrow(df_m) == 0) next
  
  crn_vals <- df_m$value[df_m$system == "CRN"]
  exact_vals <- df_m$value[df_m$system == "Exato (ML)"]
  
  p_shap_crn <- tryCatch(shapiro.test(crn_vals)$p.value, error = function(e) NA)
  p_shap_exact <- tryCatch(shapiro.test(exact_vals)$p.value, error = function(e) NA)
  
  # Rigor: Apenas consideramos "Normal" se AMBOS os datasets passarem no teste com p > 0.05
  is_normal <- ifelse(!is.na(p_shap_crn) && !is.na(p_shap_exact) && p_shap_crn > 0.05 && p_shap_exact > 0.05, "Normal", "Não-Normal")
  
  boot_crn <- boot_ci_median(crn_vals)
  boot_exact <- boot_ci_median(exact_vals)
  
  wilcox_p <- tryCatch(wilcox.test(crn_vals, exact_vals, exact = FALSE)$p.value, error = function(e) NA)
  
  if (!is.na(wilcox_p) && wilcox_p < 0.05) {
    maior <- ifelse(boot_crn["median"] > boot_exact["median"], "CRN > Exato", "Exato > CRN")
  } else {
    maior <- "Equivalentes" 
  }
  
  str_crn <- sprintf("%.4f [%.4f, %.4f]", boot_crn["median"], boot_crn["lower"], boot_crn["upper"])
  str_exact <- sprintf("%.4f [%.4f, %.4f]", boot_exact["median"], boot_exact["lower"], boot_exact["upper"])
  str_p <- ifelse(is.na(wilcox_p), "NA", ifelse(wilcox_p < 0.0001, "<0.0001", sprintf("%.4f", wilcox_p)))
  
  cat(sprintf("%-15s | %-12s | %-30s | %-30s | %-12s | %-15s\n", 
              m, is_normal, str_crn, str_exact, str_p, maior))
}
cat(rep("-", 125), "\n", sep="")
cat("* Intervalos de Confiança (IC 95%) calculados via Bootstrapping da Mediana (2000 iterações).\n")
cat("* Teste de Normalidade: Shapiro-Wilk (p < 0.05 = Não-Normal).\n")
cat("* Comparação P-value: Teste U de Mann-Whitney (Wilcoxon Rank-Sum).\n")