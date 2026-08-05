# ==============================================================================
# EXPERIMENTO 2: Análise de Ruído Estocástico (low_drive e channel_noise)
# ==============================================================================
rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# 1. Função Auxiliar de Métricas (Simplificada para foco em Fano e CV)
# (Assumindo que você já extraiu as contagens de spikes e ISIs das séries temporais)
# Vamos simular o carregamento do dataframe de resultados para I_app = 60

cat("Carregando dados dos regimes de ruído (I_app = 60)...\n")
# Substitua pelo caminho real do seu CSV de métricas ou séries temporais
# df_ruido <- read.csv("data/metrics_iapp60_noise_regimes.csv")

# ==============================================================================
# MOCK DATA (Remova esta seção e use seu read.csv real na prática)
# Gerando dados parecidos com o que você obteria das suas simulações exatas
set.seed(123)
n_runs <- 500
df_ruido <- data.frame(
  run_id = 1:(n_runs * 2),
  regime = rep(c("low_drive_quiescent", "channel_noise_dominant"), each = n_runs),
  iapp = 60,
  # Simulando contagem de spikes via Poisson (com médias diferentes para os regimes)
  n_spikes = c(rpois(n_runs, lambda = 1.2), rpois(n_runs, lambda = 3.5)),
  # Simulando CV_ISI (próximo a 1 para processos estocásticos puros)
  cv_isi = c(rnorm(n_runs, 0.95, 0.1), rnorm(n_runs, 1.02, 0.08)) 
)
# ==============================================================================

# ==============================================================================
# 2. Cálculos Estatísticos Fundamentais do Ruído
# ==============================================================================
cat("\nCalculando Fator de Fano e estatísticas agregadas por regime...\n")

stats_ruido <- df_ruido %>%
  group_by(regime) %>%
  summarise(
    N_Realizacoes = n(),
    Media_Spikes = mean(n_spikes, na.rm = TRUE),
    Var_Spikes = var(n_spikes, na.rm = TRUE),
    Fator_Fano = Var_Spikes / Media_Spikes,
    Media_CV_ISI = mean(cv_isi, na.rm = TRUE),
    .groups = "drop"
  )

print(stats_ruido)

# ==============================================================================
# 3. Preparação das Distribuições Teóricas (Poisson)
# ==============================================================================
# Para provar que o ruído explica tudo, a distribuição empírica tem que bater 
# com a distribuição teórica de Poisson com base na média.

max_spikes <- max(df_ruido$n_spikes)

# Criando um dataframe com a linha teórica de Poisson para cada regime
poisson_theory <- stats_ruido %>%
  rowwise() %>%
  mutate(
    n_spikes = list(0:max_spikes),
    # Densidade de Poisson: P(X=k) = (lambda^k * e^-lambda) / k!
    density = list(dpois(0:max_spikes, lambda = Media_Spikes))
  ) %>%
  unnest(cols = c(n_spikes, density))

# ==============================================================================
# 4. Plot Set 3: Comprovação Visual do Processo Estocástico
# ==============================================================================
cat("Gerando gráficos de aderência ao ruído estocástico...\n")

# A. Histograma de Spikes vs. Curva Teórica de Poisson
p_poisson <- ggplot() +
  # Dados reais (Histograma normalizado para densidade)
  geom_histogram(data = df_ruido, aes(x = n_spikes, y = ..density.., fill = regime), 
                 binwidth = 1, color = "black", alpha = 0.6, position = "dodge") +
  # Curva teórica de Poisson (Pontos e linhas)
  geom_line(data = poisson_theory, aes(x = n_spikes, y = density, color = regime), 
            size = 1.2, linetype = "dashed") +
  geom_point(data = poisson_theory, aes(x = n_spikes, y = density, color = regime), 
             size = 3) +
  scale_fill_manual(values = c("low_drive_quiescent" = "lightblue", "channel_noise_dominant" = "salmon")) +
  scale_color_manual(values = c("low_drive_quiescent" = "darkblue", "channel_noise_dominant" = "red")) +
  labs(title = "Distribuição de Spikes vs. Processo Estocástico Puro (Poisson)",
       subtitle = "A linha tracejada representa o ruído puramente estocástico esperado (Teoria)",
       x = "Número de Spikes por Simulação", 
       y = "Densidade de Probabilidade") +
  theme_minimal() +
  facet_wrap(~regime, ncol = 1) # Separa os gráficos para não sobrepor

# B. Distribuição do CV_ISI (Deve estar centrada próxima a 1.0)
p_cvisi <- ggplot(df_ruido, aes(x = regime, y = cv_isi, fill = regime)) +
  geom_violin(alpha = 0.5, trim = FALSE) +
  geom_boxplot(width = 0.2, fill = "white", alpha = 0.8) +
  geom_hline(yintercept = 1.0, color = "red", linetype = "dashed", size = 1) +
  scale_fill_manual(values = c("low_drive_quiescent" = "lightblue", "channel_noise_dominant" = "salmon")) +
  labs(title = "Coeficiente de Variação do ISI (CV_ISI)",
       subtitle = "A linha vermelha em 1.0 indica disparos sem memória (Poisson)",
       x = "Regime", 
       y = expression(CV[ISI])) +
  theme_minimal() +
  theme(legend.position = "none")

# Combinando o Set
plot_set_3 <- p_poisson | p_cvisi
ggsave("experimento2_noise_dominance.png", plot_set_3, width = 12, height = 6)

# ==============================================================================
# 5. Teste Estatístico (Goodness-of-Fit / Qui-Quadrado)
# ==============================================================================
cat("\n--- Teste de Aderência do Ruído (Qui-Quadrado) ---\n")
# O teste verifica se a distribuição empírica difere significativamente de uma Poisson.
# Se p > 0.05, não há diferença estatística: o ruído explica os disparos.

for (r in unique(df_ruido$regime)) {
  dados_regime <- df_ruido$n_spikes[df_ruido$regime == r]
  tabela_freq <- table(factor(dados_regime, levels = 0:max_spikes))
  
  media_lambda <- mean(dados_regime)
  probs_esperadas <- dpois(0:max_spikes, lambda = media_lambda)
  
  # Como a soma das probabilidades de Poisson vai até o infinito, agrupamos a cauda
  probs_esperadas[length(probs_esperadas)] <- 1 - sum(probs_esperadas[-length(probs_esperadas)])
  
  freq_esperada <- probs_esperadas * length(dados_regime)
  
  # Executa o teste de Qui-Quadrado
  # Supressão de warnings de aproximação caso haja contagens muito pequenas nas caudas
  suppressWarnings(
    teste_chi <- chisq.test(x = as.numeric(tabela_freq), p = probs_esperadas)
  )
  
  cat(sprintf("\nRegime: %s\n", r))
  cat(sprintf("Média de Spikes: %.2f | Variância: %.2f | Fator de Fano: %.2f\n", 
              media_lambda, var(dados_regime), var(dados_regime)/media_lambda))
  cat(sprintf("Qui-Quadrado (X-squared) = %.2f, p-value = %.4f\n", teste_chi$statistic, teste_chi$p.value))
  
  if(teste_chi$p.value > 0.05) {
    cat("  -> CONCLUSÃO: A distribuição não difere de uma Poisson (Ruído explica quase 100% da variância).\n")
  } else {
    cat("  -> CONCLUSÃO: Há desvios em relação à Poisson perfeita (Possível presença de bursting ou refratariedade longa).\n")
  }
}