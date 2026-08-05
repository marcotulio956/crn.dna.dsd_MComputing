# ==============================================================================
# Setup & Libraries
# ==============================================================================
rm(list = ls())
library(dplyr) 
library(tidyr) 
library(ggplot2)

# ==============================================================================
# 1. Função de Extração de Métricas Corrigida (Exige V > 0 para Spike)
# ==============================================================================
calculate_spiking_metrics <- function(time, V, tmax, bin_size_ms = 50) { 
  v_min <- min(V, na.rm = TRUE) 
  v_max <- max(V, na.rm = TRUE) 
  threshold <- v_min + 0.70 * (v_max - v_min) 
  
  # CORREÇÃO CRÍTICA: O limiar dinâmico deve ser cruzado E a voltagem deve ser > 0
  is_above <- (V >= threshold) & (V > 0) 
  spike_train <- c(0, diff(is_above) == 1) 
  spike_idx <- which(spike_train == 1) 
  spike_times <- time[spike_idx] 
  n_spikes <- length(spike_times) 
  
  mean_fr <- n_spikes / tmax 
  
  # Fano Factor 
  breaks <- seq(0, tmax, by = bin_size_ms) 
  if(length(breaks) > 1 && n_spikes > 0) { 
    counts <- hist(spike_times, breaks = breaks, plot = FALSE)$counts 
    mean_count <- mean(counts) 
    var_count <- var(counts) 
    fano_factor <- ifelse(mean_count > 0, var_count / mean_count, NA) 
  } else { 
    fano_factor <- NA 
  } 
  
  # Variância Sub-limiar e SNR
  sub_v <- V[!is_above] # Pega tudo que não é pico de spike
  sub_var <- ifelse(length(sub_v) > 0, var(sub_v), NA) 
  total_var <- var(V, na.rm = TRUE) 
  snr <- ifelse(!is.na(sub_var) && sub_var > 0, total_var / sub_var, NA) 
  
  return(data.frame( 
    n_spikes = n_spikes,
    fano_factor = fano_factor, 
    sub_var = sub_var, 
    snr = snr 
  )) 
}

cat("Carregando e processando datasets...\n")

# 1. Carrega dataset CRN
crn_data <- read.csv("crn_stochastic_metrics_nearthreshold_fewerVsExtraGates_vol2.csv") %>%
  mutate(
    system = "CRN",
    # Padroniza os nomes dos regimes para bater com o dataset exato
    regime = case_when(
      grepl("fewGates", regime) ~ "near_threshold_irregular_fewerGates",
      grepl("extraGates", regime) ~ "near_threshold_irregular_extraGates",
      TRUE ~ regime
    )
  )

# 2. Carrega e extrai dataset Exato
ts_exact <- read.csv("factorial_design/data/mlexactboth_timeseries_800ms_nearthreshold_fewerVsExtraGates.csv")

exact_metrics <- ts_exact %>%
  group_by(run_id, regime) %>%
  summarise(
    metrics = list(calculate_spiking_metrics(time, V, tmax = 800)),
    .groups = "drop"
  ) %>%
  unnest(metrics) %>%
  mutate(system = "Exato (ML)")

# 3. Combina Datasets
metrics_of_interest <- c("n_spikes", "fano_factor", "sub_var", "snr")

combined_data <- bind_rows(
  crn_data %>% select(system, regime, all_of(metrics_of_interest)),
  exact_metrics %>% select(system, regime, all_of(metrics_of_interest))
) %>%
  mutate(
    Gates = ifelse(grepl("fewer", regime), "N=20 (Alto Ruído)", "N=120 (Baixo Ruído)"),
    Gates = factor(Gates, levels = c("N=20 (Alto Ruído)", "N=120 (Baixo Ruído)"))
  )

# ==============================================================================
# 2. Análise da Taxa de Falhas (Simulações com Zero Spikes)
# ==============================================================================
cat("\n--- TAXA DE SILÊNCIO (Zero Spikes) ---\n")
zero_spikes_summary <- combined_data %>%
  group_by(system, Gates) %>%
  summarise(
    Total_Runs = n(),
    Zero_Spikes = sum(n_spikes == 0, na.rm = TRUE),
    Perc_Silencioso = (Zero_Spikes / Total_Runs) * 100,
    .groups = "drop"
  )
print(zero_spikes_summary)

# ==============================================================================
# 3. Gráficos de Interação (Mediana e IC)
# ==============================================================================
cat("Gerando gráficos de interação...\n")

plot_data <- combined_data %>%
  pivot_longer(cols = c("fano_factor", "sub_var", "snr"), names_to = "metric", values_to = "value") %>%
  filter(!is.na(value)) # Remove NAs (simulações sem spikes não tem FF/SNR)

p_interacao <- ggplot(plot_data, aes(x = Gates, y = value, color = system, group = system)) +
  stat_summary(fun = median, geom = "point", size = 4) +
  stat_summary(fun = median, geom = "line", linewidth = 1.2) +
  stat_summary(fun.data = median_hilow, fun.args = list(conf.int = 0.95), geom = "errorbar", width = 0.2) +
  facet_wrap(~metric, scales = "free_y", ncol = 3, strip.position = "top") +
  scale_color_manual(values = c("CRN" = "#2c7fb8", "Exato (ML)" = "#f03b20")) +
  theme_bw() +
  labs(title = "Efeito do Escalonamento de Ruído (N=20 vs N=120)",
       subtitle = "Regime Near-Threshold Irregular (Iapp = 80) - Pontos representam Medianas",
       x = "Nível de Ruído (Canais/Volume)", y = "Valor da Métrica", color = "Algoritmo") +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold", size=12))

ggsave("experimento4_ruido_sublimiar.png", p_interacao, width = 12, height = 6)

# ==============================================================================
# 4. Estatística: Testes de Wilcoxon (Comparação de Medianas)
# ==============================================================================
cat("\n--- ESTATÍSTICA: COMPARAÇÃO ENTRE ALGORITMOS (CRN vs Exato) ---\n")
for(g in levels(combined_data$Gates)) {
  cat(sprintf("\nCondição: %s\n", g))
  cat(sprintf("%-15s | %-10s | %-15s\n", "Métrica", "P-Value", "Conclusão"))
  cat(rep("-", 45), "\n", sep="")
  
  df_gate <- combined_data %>% filter(Gates == g)
  
  for(m in c("fano_factor", "sub_var", "snr")) {
    vals_crn <- df_gate[[m]][df_gate$system == "CRN"]
    vals_exato <- df_gate[[m]][df_gate$system == "Exato (ML)"]
    
    # Remove NAs
    vals_crn <- vals_crn[!is.na(vals_crn)]
    vals_exato <- vals_exato[!is.na(vals_exato)]
    
    if(length(vals_crn) > 3 && length(vals_exato) > 3) {
      w_test <- wilcox.test(vals_crn, vals_exato, exact = FALSE)
      p_val <- w_test$p.value
      conc <- ifelse(p_val > 0.05, "Equivalentes (OK)", "Diferentes (Alerta)")
      cat(sprintf("%-15s | %.4f     | %-15s\n", m, p_val, conc))
    } else {
      cat(sprintf("%-15s | %-10s | %-15s\n", m, "N/A", "Sem spikes suficientes"))
    }
  }
}