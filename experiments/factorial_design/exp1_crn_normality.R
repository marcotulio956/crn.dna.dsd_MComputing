cat("\n--- Multivariable Linear Regressions (Sensitivity Analysis) ---\n")

ref_ts <- read.csv("./crn_stochastic_metrics_800ms.csv")
# 1. Ajustar o modelo linear
lm_mean_fr <- lm(mean_fr ~ Iapp * Mtot * Wtot, data = ref_ts)

# 2. Criar um dataframe com os res??duos do modelo
df_diagnostico <- data.frame(residuos = residuals(lm_mean_fr))

# Calcular o desvio padr??o dos res??duos para ajustar a curva normal te??rica correspondente
sd_residuos <- sd(df_diagnostico$residuos, na.rm = TRUE)

cat("\nGerando gr??ficos de diagn??stico de normalidade para os erros...\n")

# ----------------------------------------------------------------------------
# GR??FICO 1: Densidade dos Res??duos vs. Curva Normal Te??rica
# ----------------------------------------------------------------------------
p_densidade <- ggplot(df_diagnostico, aes(x = residuos)) +
  # Curva de densidade real dos seus erros
  geom_density(fill = "steelblue", color = "darkblue", alpha = 0.4) +
  # Curva normal te??rica com m??dia 0 (propriedade dos res??duos) e o mesmo desvio padr??o
  stat_function(
    fun = dnorm, 
    args = list(mean = 0, sd = sd_residuos), 
    color = "red", 
    linewidth = 1, 
    linetype = "dashed"
  ) +
  theme_bw() +
  labs(
    title = "A: Densidade dos Erros vs. Curva Normal",
    subtitle = "Linha vermelha tracejada representa a Normal Te??rica",
    x = "Valor do Res??duo (Erro)",
    y = "Densidade"
  )

# ----------------------------------------------------------------------------
# GR??FICO 2: Q-Q Plot dos Res??duos
# ----------------------------------------------------------------------------
p_qq <- ggplot(df_diagnostico, aes(sample = residuos)) +
  stat_qq(color = "darkblue", alpha = 0.6) +
  stat_qq_line(color = "red", linewidth = 1) +
  theme_bw() +
  labs(
    title = "B: Q-Q Plot dos Erros",
    subtitle = "Pontos fora da linha vermelha indicam desvio de normalidade",
    x = "Quantis Te??ricos",
    y = "Quantis da Amostra (Res??duos)"
  )

# ----------------------------------------------------------------------------
# COMBINANDO OS GR??FICOS LADO A LADO (Uso do Patchwork)
# ----------------------------------------------------------------------------
# Garanta que a library(patchwork) esteja carregada no in??cio do seu script geral
grafico_combinado <- p_densidade + p_qq + 
  plot_annotation(
    title = "Diagn??stico Estat??stico Completo dos Res??duos (mean_fr)",
    subtitle = "An??lise visual obrigat??ria para valida????o da ANOVA"
  )

# Exibe no RStudio / Console
print(grafico_combinado)

# Salva em arquivo
ggsave("diagnostico_normalidade_erros_mean_fr.png", plot = grafico_combinado, width = 12, height = 5)

# ----------------------------------------------------------------------------
# TESTE FORMAL E TABELA ANOVA
# ----------------------------------------------------------------------------
cat("\nTeste de Shapiro-Wilk para os Res??duos:\n")
print(shapiro.test(df_diagnostico$residuos))

cat("\nTabela ANOVA para Firing Rate:\n")
print(anova(lm_mean_fr))