# ==============================================================================
# Setup & Libraries
# ==============================================================================
rm(list = ls())
library(dplyr)
library(ggplot2)

# ==============================================================================
# 1. Criação Manual dos Dados (Mock Data)
# ==============================================================================
# Construindo um dataframe com as medianas e limites (lower/upper) manuais
# Lógica aplicada conforme solicitado:
# - fano_factor: varia de N=20 para N=120 (queda)
# - snr: maior quando se tem menos gates (N=20)
# - sub_var: aumenta levemente quando se tem mais gates (N=120)

mock_data <- data.frame(
  Gates = rep(c("N=20 (Alto Ruído)", "N=120 (Baixo Ruído)"), each = 6),
  system = rep(rep(c("CRN", "Exato (ML)"), each = 3), times = 2),
  metric = rep(c("fano_factor", "snr", "sub_var"), times = 4),
  
  # Valores inseridos manualmente
  median = c(
    # N=20: CRN (FF, SNR, Sub_var)
    1.82, 5.10, 105.0,
    # N=20: Exato (FF, SNR, Sub_var)
    1.93, 4.80, 100.0,
    
    # N=120: CRN (FF, SNR, Sub_var)
    0.90, 2.20, 331.0,
    # N=120: Exato (FF, SNR, Sub_var)
    0.85, 1.95, 130.0
  ),
  
  # Limite inferior do Desvio/IC
  lower = c(
    1.50, 4.20, 95.0,  # N=20 CRN
    1.45, 4.50, 92.0,  # N=20 Exato
    0.70, 1.50, 296.0, # N=120 CRN
    0.65, 1.60, 115.0  # N=120 Exato
  ),
  
  # Limite superior do Desvio/IC
  upper = c(
    2.10, 5.80, 115.0, # N=20 CRN
    2.05, 5.90, 108.0, # N=20 Exato
    1.10, 2.10, 352.0, # N=120 CRN
    1.05, 2.30, 135.0  # N=120 Exato
  )
)

# Garantindo a ordem correta no eixo X
mock_data$Gates <- factor(mock_data$Gates, levels = c("N=20 (Alto Ruído)", "N=120 (Baixo Ruído)"))

# ==============================================================================
# 2. Geração do Gráfico de Interação
# ==============================================================================
cat("Gerando gráfico de exemplo com dados manuais...\n")

p_falso <- ggplot(mock_data, aes(x = Gates, y = median, color = system, group = system)) +
  # Adiciona os pontos das medianas
  geom_point(size = 4) +
  # Conecta as linhas
  geom_line(linewidth = 1.2) +
  # Adiciona as barras de erro baseadas no lower/upper definidos
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, linewidth = 0.8) +
  # Cria os painéis separados para cada métrica
  facet_wrap(~metric, scales = "free_y", ncol = 3, strip.position = "top") +
  # Cores consistentes com o seu padrão
  scale_color_manual(values = c("CRN" = "#2c7fb8", "Exato (ML)" = "#f03b20")) +
  theme_bw() +
  labs(title = "Efeito do Escalonamento de Ruído - Canais de Íons 20 vs 120",
       subtitle = "Regime Near-Threshold Irregular (Iapp = 80) - Pontos Representam Medianas",
       x = "Nível de Ruído (Canais/Volume)", 
       y = "Valor da Métrica", 
       color = "Algoritmo") +
  theme(legend.position = "bottom", 
        strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(color = "black", face = "bold"))

# Salva o gráfico
ggsave("experimento4_exemplo_falso.png", p_falso, width = 12, height = 6)

cat("Pronto! Gráfico salvo como 'experimento4_exemplo_falso.png'.\n")