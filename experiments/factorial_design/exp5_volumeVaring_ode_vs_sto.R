# ==============================================================================
# Setup & Libraries
# ==============================================================================
rm(list = ls())
library(dplyr)
library(purrr)
library(ggplot2)
library(patchwork) # Necessário para colocar os gráficos lado a lado

source('R/crn_reactor.R')
source('R/parser.R')
source('R/io.R')
source('R/NEURON_LIB.R')
source('R/util_functions.R')
source('R/forced_concentrations.R')
# Parâmetros Base
t_max <- 200
timing <- seq(0, t_max, length.out = 600)
fixed_amplitude <- 60 # Corrente garantindo um disparo bem definido

# Vetor de volumes solicitado (1 a 10 passo 1, depois saltos maiores)
volumes_to_test <- c(seq(1, 10, by = 1), 50, 100, 500, 750, 1000)

# Inicializa o circuito
circuit <- create_ml_crn_varyingRates()
circuit$t <- timing

# Função de Corrente Dinâmica Fixa
Ip_fixed_input <- function(t) {
  pulse_input(t, time = 0, width = 150, amplitude = fixed_amplitude)
}
# ==============================================================================
# 1. Padrão Ouro Determinístico (ODE Baseline)
# ==============================================================================
cat("Rodando simulação ODE (Baseline)...\n")
res_ode <- react2(
  species   = circuit$species,
  ci        = circuit$ci,
  reactions = circuit$reactions,
  ki        = circuit$ki,
  t         = circuit$t,
  verbose   = FALSE,
  forced_concentrations = list(Ip = Ip_fixed_input)
)

# CORREÇÃO: Usando o $ e as.numeric() para garantir que sejam vetores puros
V_ode <- as.numeric(res_ode$Vp - res_ode$Vm)
time_ode <- as.numeric(res_ode$time)

if (!dir.exists("plots_comparacao")) {
  dir.create("plots_comparacao")
}
# ==============================================================================
# 2. Varredura Estocástica e Cálculo de Métricas (Loop)
# ==============================================================================
cat("Iniciando varredura de Volumes Estocásticos...\n")

experiment_results <- map_dfr(volumes_to_test, function(vol) {
  cat(sprintf("Simulando Volume: %d...\n", vol))
  
  # Inicia o cronômetro
  t_start <- Sys.time()
  
  # Executa a simulação estocástica
  res_sto <- react_stochastic_frates(
    species = circuit$species,
    ci = circuit$ci,
    reactions = circuit$reactions,
    ki = circuit$ki,
    t = circuit$t,
    forced_concentrations =  
      list(
        Ip = Ip_fixed_input
      ),
    verbose = FALSE,
    volume = vol
  )
  
  # Para o cronômetro
  t_end <- Sys.time()
  sim_time <- as.numeric(difftime(t_end, t_start, units = "secs"))
  
  # Variáveis estocásticas brutas
  time_sto <- res_sto$time
  
  # ATENÇÃO: Dependendo da implementação do React_stochastic, 
  # a saída pode estar em número de moléculas em vez de concentração. 
  # Caso o erro seja massivo, altere a linha abaixo para: (res_sto$Vp - res_sto$Vm) / vol
  # CORREÇÃO PREVENTIVA: Extração segura do vetor estocástico
  V_sto <- as.numeric(res_sto$Vp - res_sto$Vm)
  
  # ALINHAMENTO: Interpolação linear da série temporal estocástica 
  # para corresponder aos instantes exatos da grade ODE
  V_sto_aligned <- approx(
    x = time_sto, 
    y = V_sto, 
    xout = time_ode, 
    method = "linear", 
    rule = 2 # rule=2 evita NAs repetindo os valores extremos caso xout passe de x
  )$y
  
  
  
  # Cálculo do Root Mean Squared Error (RMSE)
  rmse_val <- sqrt(mean((V_ode - V_sto_aligned)^2, na.rm = TRUE))
  
  # CRIAÇÃO DO PLOT INDIVIDUAL (Frame a Frame)
  df_plot <- data.frame(time = time_ode, ODE = V_ode, Stochastic = V_sto_aligned)
  
  p_frame <- ggplot(df_plot, aes(x = time)) +
    geom_line(aes(y = ODE), color = "black", linewidth = 1, alpha = 0.6) +
    geom_line(aes(y = Stochastic), color = "#d95f02", linewidth = 0.5) +
    theme_minimal() +
    labs(title = sprintf("Volume = %d (RMSE: %.2f)", vol, rmse_val),
         y = "Voltagem (mV)", x = "Tempo (ms)")
  
  # Salva o frame atual
  ggsave(sprintf("plots_comparacao/plot_vol_%04d.png", vol), p_frame, width = 8, height = 4)
  
  return(data.frame(
    volume = vol,
    rmse = rmse_val,
    computational_time_s = sim_time
  ))
})

cat("\nResumo dos Resultados:\n")
print(experiment_results)

# ==============================================================================
# 3. Visualização Dupla (Trade-off: Erro vs Custo)
# ==============================================================================
cat("\nGerando gráficos de visualização...\n")

# Gráfico 1: Decaimento do Erro (RMSE) em função do Volume
p_error <- ggplot(experiment_results, aes(x = volume, y = rmse)) +
  geom_point(color = "#d95f02", size = 4, alpha = 0.8) +
  geom_line(color = "#d95f02", linewidth = 1.2) +
  scale_x_log10(breaks = c(1, 5, 10, 50, 100, 500, 1000)) + 
  theme_minimal(base_size = 14) +
  labs(
    title = "Convergência ao Limite Termodinâmico",
    subtitle = "Root Mean Squared Error (Estocástico vs ODE)",
    x = "Volume (Escala Log)",
    y = "Erro RMSE (mV)"
  ) +
  theme(panel.grid.minor = element_blank())

# Gráfico 2: Aumento do Custo Computacional em função do Volume
p_time <- ggplot(experiment_results, aes(x = volume, y = computational_time_s)) +
  geom_point(color = "#1b9e77", size = 4, alpha = 0.8) +
  geom_line(color = "#1b9e77", linewidth = 1.2) +
  scale_x_log10(breaks = c(1, 5, 10, 50, 100, 500, 1000)) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Custo Computacional",
    subtitle = "Tempo de execução do algoritmo de Gillespie",
    x = "Volume (Escala Log)",
    y = "Tempo de Simulação (Segundos)"
  ) +
  theme(panel.grid.minor = element_blank())

# Combina os dois gráficos lado a lado e salva a imagem
final_plot <- p_error + p_time
ggsave("experimento5_tradeoff_volume.png", final_plot, width = 14, height = 6)

cat("Tudo pronto! Gráfico salvo como 'experimento5_tradeoff_volume.png'.\n")