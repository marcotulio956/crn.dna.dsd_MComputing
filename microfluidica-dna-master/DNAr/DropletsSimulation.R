library(DNAr)  # Loading the library
library(ggplot2)
library(dplyr)
library(jsonlite)


simulate_CRNs <- function(times_of_merge, final_simulation_time, merged_droplet_ids, id_of_the_new_droplet, data) {
  print(times_of_merge)
  print(final_simulation_time)
  print(merged_droplet_ids)
  print(id_of_the_new_droplet)

  createGoticula <- function(ID, especies, concentracoesIniciais, reacoes, tempo_inicial, tempo_final) {
    goticula <- list(
      ID = ID,
      especies = especies,
      concentracoesIniciais = concentracoesIniciais,
      reacoes = reacoes,
      tempo_inicial = tempo_inicial,
      tempo_final = tempo_final
    )
    class(goticula) <- "Goticula"
    return(goticula)
  }

  change_tempo_final <- function(goticula_list, ID, new_tempo_final) {
    for (i in seq_along(goticula_list)) {
      if (goticula_list[[i]]$ID == ID) {
        goticula_list[[i]]$tempo_final <- new_tempo_final
        break  # Exit loop once the ID is found
      }
    }
    return(goticula_list)
  }

  goticulas_iniciais <- data[[1]]
  for (i in 1:length(goticulas_iniciais$especies)) {
    especie <- goticulas_iniciais$especies[[i]]
    ID <- goticulas_iniciais$id[i]
    cat("Nome:", toString(especie$nome), "\n")
    cat("Concentracao Inicial:", toString(especie$concentracaoInicial), "\n\n")
  }
  for (i in 1:length(goticulas_iniciais$especies)) {
    especie <- goticulas_iniciais$especies[[i]]
    ID <- goticulas_iniciais$id[i]
    especies <- c()
    concentracoesIniciais <- c()
    for (i in 1:length(especie$nome)) { 
      especie <- c(toString(especie$nome[[i]]))
      especies <- c(especies, especie)
    }
    for (i in 1:length(especie$concentracaoInicial)) { 
      concentracaoInicial <- c(especie$concentracaoInicial[[i]])
      concentracoesIniciais <- c(concentracoesIniciais, concentracaoInicial)
    }
  }
  #Na Goticula 0 acontecerá a Subtração (InputA + InputB -> Waste)'
  #Ela também carrega todas as entradas

  InputA <- 4
  InputB <- 4
  InputC <- 4
  ID <- 0
  concentracoesIniciais <- c(InputA, InputB, InputC, 0, 0, 0)
  especies <- c('InputA','InputB','InputC','Fuel', 'Waste', 'Output')
  reacoes <- c('InputA + InputB -> Waste', 
              'InputA + Fuel -> Output',
              'InputC + Fuel -> Output')

  goticula_0 <- createGoticula(ID, especies, concentracoesIniciais, reacoes, 0, final_simulation_time)

  #A Goticula 1 carregará o Fuel, que é necessário para realizar a soma, ou seja
  #a soma só será iniciada quando a gotícula 1 e 0 se misturarem.
  Fuel <- 10
  ID <- 1
  concentracoesIniciais <- c(0, 0, 0, Fuel, 0, 0)
  goticula_1 <- createGoticula(ID, especies, concentracoesIniciais, reacoes, 0, final_simulation_time)

  #Criar goticulas misturadas
  goticula_list <- list(goticula_0, goticula_1)

  #Para cada Gota Nova (misturada)
  for (i in seq_along(id_of_the_new_droplet)) {
    # Obtenha o ID dessa gota
    ID <- id_of_the_new_droplet[i]
    # Obtenha o ID das gotas que a formaram
    merged_droplet_id <- merged_droplet_ids[[i]]
    time_of_merge <- times_of_merge[[i]]
    
    for (j in seq_along(merged_droplet_id)) {
      # Atualizar Tempo final de simulação das goticulas que se misturaram
      goticula_list <- change_tempo_final(goticula_list, merged_droplet_id[j], time_of_merge)
    }
  }

  SimularReacoesEmGoticula  <- function(goticula) {
    taxas_reacao <- c()
    for(i in seq_along(goticula$reacoes)){
      taxas_reacao <- append(taxas_reacao, 2.8e-3) 
    }
    return(react(
      species   = c(goticula$especies),
      ci        = c(goticula$concentracoesIniciais),
      reactions = c(goticula$reacoes),
      ki        = taxas_reacao,
      t         = seq(goticula$tempo_inicial, goticula$tempo_final, length.out = 50)
    ))
  }

  obter_concentracoes_finais <- function(resultado_simulacao_antiga, especies) {
    concentracoes_simulacao <- c()
    for(especie in especies){
      array_result <- tail(resultado_simulacao_antiga[[especie]])
      concentracoes_simulacao <- append(concentracoes_simulacao, array_result[length(array_result)])
    }
    return (concentracoes_simulacao)
  }

  concentracoes_simulacao <- c()
  print("Simulando goticulas iniciais")
  for (i in seq_along(goticula_list)) {
    goticula <- goticula_list[[i]]
    b <- SimularReacoesEmGoticula(goticula)
    # Plot
    p <- plot_behavior(
      b,
      x_label     = 'Time (s)',
      y_label     = 'Concentration (M)',
      legend_name = 'Species',
      geom_list   = c('line', 'point'),
      species = c('Output','Fuel','InputA','InputB','InputC')
    )
    print(p)
    concentracoes_simulacao <- c (concentracoes_simulacao, list(obter_concentracoes_finais(b, goticula$especies)))
    print("Pressione enter para continuar a simulação")
    
    input_wait <- readline()
  }
  print("Simulando goticulas misturadas")
  for (i in seq_along(id_of_the_new_droplet)) {
    ID <- id_of_the_new_droplet[[i]]
    merged_droplet_id <- merged_droplet_ids
    resultados_gotas_origem <- c()
    concentracoes_nova_gota <- Map("+", concentracoes_simulacao[[1]],concentracoes_simulacao[[2]])
    concentracoes_nova_gota <- unlist(concentracoes_nova_gota)
    goticula_misturada <- createGoticula(ID, especies, concentracoes_nova_gota, reacoes, times_of_merge, final_simulation_time)
    
    
    #Modularizar
    b <- SimularReacoesEmGoticula(goticula_misturada)
    # Plot
    p <- plot_behavior(
      b,
      x_label     = 'Time (s)',
      y_label     = 'Concentration (M)',
      legend_name = 'Species',
      geom_list   = c('line', 'point'),
      species = c('Output','Fuel','InputA','InputB','InputC')
    )
    print(p)
    concentracoes_simulacao <- c (concentracoes_simulacao, list(obter_concentracoes_finais(b, goticula_misturada$especies)))
    print("Pressione enter para continuar a simulação")
    input_wait <- readline()
    
    goticula_list <- append(goticula_list, goticula_misturada)
  }
  print("FIM")
}


