rm(list = ls())
library(dplyr)
library(purrr)
library(ggplot2)
library(gganimate)

source('R/4domain_reactor.R')
source('R/analysis.R')
source('R/crn_reactor.R')
source('R/dsd.R')
source('R/GATE_LIB.R')
source('R/io.R')
source('R/parser.R')

source('R/util_functions.R')

source('R/forced_concentrations.R')
source('R/NEURON_LIB.R')
source('R/NEURON_SIM.R')
source('R/neuron_hjelmfelt.R')

n_runs <- 100
timing <-  seq(0, 200, length.out = 600)
input_values <- seq(57, 61, length.out = n_runs)

circuit <-create_morris_lecar_crn()
circuit$t <- timing

React_circuit <- function(circuit) {
  return(react2(
    species   = circuit$species,
    ci        = circuit$ci,
    reactions = circuit$reactions,
    ki        = circuit$ki,
    t         = circuit$t,
    # engine = 'diffeqr',
    verbose = FALSE,
    forced_concentrations =  
      list(
        #Ip = Ip_common_input
      )
  )
  )
}

# 1. Loop para rodar o circuito n vezes com a amplitude variando de 57 a 61
all_results <- map_dfr(1:n_runs, function(i) {
  message(paste("Executando simulação", i, "de", n_runs, "| Amplitude:", round(input_values[i], 2)))
  
  # Captura a amplitude específica para esta iteração atual (vai de 57 a 61)
  current_amplitude <- input_values[i]
  
  # Define a função de entrada customizada para esta rodada específica
  Ip_dynamic_input <- function(t) {
    pulse_input(t, time = 0, width = 150, amplitude = current_amplitude)
  }
  
  # Executa o circuito injetando a função com a nova amplitude
  res_crn <- react2(
    species   = circuit$species,
    ci        = circuit$ci,
    reactions = circuit$reactions,
    ki        = circuit$ki,
    t         = circuit$t,
    verbose   = FALSE,
    forced_concentrations = list(
      Ip = Ip_dynamic_input
    )
  )
  
  # Processamento da variável de voltagem diferencial
  res_crn['V'] <- res_crn['Vp'] - res_crn['Vm']
  
  # Retorna os dados identificados pelo ID da rodada
  return(data.frame(
    time       = res_crn$time, # Verifique se o pacote retorna 'time' ou 't'
    V          = res_crn$V,
    run_id     = i
  ))
})

# 2. Calcular estatísticas cumulativas para construir o efeito estatístico do GIF
cumulative_summary <- map_dfr(1:n_runs, function(current_frame) {
  all_results %>%
    filter(run_id <= current_frame) %>%
    group_by(time) %>%
    summarise(
      mean_V = mean(V, na.rm = TRUE),
      sd_V   = sd(V, na.rm = TRUE),
      max_V  = max(V, na.rm = TRUE),
      min_V  = min(V, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(frame_id = current_frame)
})

# 3. Construir o gráfico base com fita de desvio padrão e intervalos
anim_plot <- ggplot() +
  # Faixa (Ribbon) indicando 1 Desvio Padrão (±1 DP) da média acumulada
  geom_ribbon(
    data = cumulative_summary, 
    aes(x = time, ymin = mean_V - sd_V, ymax = mean_V + sd_V), 
    fill = "steelblue", alpha = 0.25
  ) +
  # Linhas pontilhadas indicando o intervalo total (máximo e mínimo observados)
  geom_line(data = cumulative_summary, aes(x = time, y = max_V), color = "red", linetype = "dotted", linewidth = 0.8) +
  geom_line(data = cumulative_summary, aes(x = time, y = min_V), color = "red", linetype = "dotted", linewidth = 0.8) +
  # Linha sólida mostrando o valor médio
  geom_line(data = cumulative_summary, aes(x = time, y = mean_V), color = "black", linewidth = 1.2) +
  # Linhas finas ao fundo mostrando o histórico das trajetórias individuais já calculadas
  geom_line(
    data = all_results, 
    aes(x = time, y = V, group = run_id), 
    color = "gray60", alpha = 0.15, linewidth = 0.5
  ) +
  # Customização do layout e títulos dinâmicos
  labs(
    title = "Morris-Lecar CRN Current Responses",
    x = "Time",
    y = "Tension (Vp - Vm)"
  ) +
  theme_minimal(base_size = 14) +
  # Transição do gganimate usando os frames acumulados
  transition_states(frame_id, transition_length = 1, state_length = 2)

# result_sto <- React_stochastic(crn1, volume = volume)


# 4. Renderizar e salvar a animação final em formato GIF
# Certifique-se de que o pacote 'gifski' está instalado: install.packages("gifski")
animate(
  anim_plot, 
  nframes  = n_runs * 2, # Quantidade de quadros para a suavidade da transição
  fps      = 10,         # Velocidade dos quadros por segundo
  width    = 900, 
  height   = 600, 
  renderer = gifski_renderer("crn_varying_amplitude.gif")
)
