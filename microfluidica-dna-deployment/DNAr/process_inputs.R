library(jsonlite)
library(DNAr)  # Loading the library
library(ggplot2)
library(dplyr)

obter_concentracoes_iniciais <- function(concentracoes_simulacao, merged_droplet_id, i) {
  # Initialize the sum for the first two elements
  print("IDS Gotículas a misturar")
  print(merged_droplet_id[[i]][[1]])
  print(merged_droplet_id[[i]][[2]])
  mapped <- Map("+", concentracoes_simulacao[[as.character(merged_droplet_id[[i]][[1]])]], 
                    concentracoes_simulacao[[as.character(merged_droplet_id[[i]][[2]])]])
  
  # Check if there are more than two elements in merged_droplet_id[[i]]
  if (length(merged_droplet_id[[i]]) >= 3) {
    # Iterate over the rest of the elements starting from the third position
    for (j in 3:length(merged_droplet_id[[i]])) {
      print(merged_droplet_id[[i]][[j]])
      # Add the corresponding concentration from concentracoes_simulacao
      mapped <- Map("+", mapped, concentracoes_simulacao[[as.character(merged_droplet_id[[i]][[j]])]])
    }
  }
  
  return(mapped)
}


combine_and_sum <- function(dfs) {
  # Combine all data frames into one
  combined_df <- do.call(rbind, dfs)
  
  # Aggregate the data by 'time' and sum other columns
  summarized_df <- aggregate(. ~ time, data = combined_df, sum, na.rm = TRUE)
  
  return(summarized_df)
}

createGoticula <- function(ID, especies, concentracoesIniciais, reacoes, taxasReacao, tempo_inicial, tempo_final) {
  goticula <- list(
    ID = ID,
    especies = especies,
    concentracoesIniciais = concentracoesIniciais,
    reacoes = reacoes,
    taxasReacao = taxasReacao,
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

SimularReacoesEmGoticula  <- function(goticula) {
  #taxas_reacao <- c()
  #print(goticula)
  #for(i in seq_along(goticula$reacoes)){
  #  taxas_reacao <- append(taxas_reacao, 2.8e-3) 
  #}
  return(react(
    species   = c(goticula$especies),
    ci        = c(goticula$concentracoesIniciais),
    reactions = c(goticula$reacoes),
    ki        = c(goticula$taxasReacao),
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

simulate_CRNs <- function(times_of_merge, final_simulation_time, merged_droplet_ids, id_of_the_new_droplet, data) {
  print("start simulation")
  goticulas_iniciais <- data[[1]]
  reacoes <- data[[3]]$reacao
  taxas_reacao <- data[[3]]$rate
  goticula_list <- list()
  for (i in 1:length(goticulas_iniciais$especies)) {
    especie_data <- goticulas_iniciais$especies[[i]]
    ID <- goticulas_iniciais$id[i]
    especies <- c()
    concentracoesIniciais <- c()
    
    for (j in 1:length(especie_data$nome)) { 
      nome <- toString(especie_data$nome[[j]])
      especies <- c(especies, nome)
    }
    if(length(especie_data$nome) <= 1){
      next
    }
    for (j in 1:length(especie_data$concentracaoInicial)) { 
      concentracaoInicial <- as.numeric(especie_data$concentracaoInicial[[j]])
      concentracoesIniciais <- c(concentracoesIniciais, concentracaoInicial)
    }

    goticula <- createGoticula(ID, especies, concentracoesIniciais, reacoes, taxas_reacao, 0, final_simulation_time)
    goticula_list <- append(goticula_list, list(goticula))
  }
  
  time_of_merge_list <- list()
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
      time_of_merge_list[[as.character(merged_droplet_id[j])]] <- time_of_merge
    }
  }
  print("hey")
  print(time_of_merge_list)

  concentracoes_simulacao <- list()
  b_list <- list()
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
      species = especies
    )
    #print(p)
    b_list <- append(b_list, list(b))
    concentracoes_simulacao[[as.character(goticula$ID)]] <- obter_concentracoes_finais(b, especies)
    print("Criada a gotícula")
    print(goticula$ID)
    
    input_wait <- readline()
  }

  print("Simulando goticulas misturadas")
  for (i in seq_along(id_of_the_new_droplet)) {
    ID <- id_of_the_new_droplet[[i]]
    last_id<-ID
    merged_droplet_id <- merged_droplet_ids
    resultados_gotas_origem <- c()
    #print(concentracoes_simulacao[[merged_droplet_id[[i]][[1]]+1]])
    endTime <- time_of_merge_list[[as.character(ID)]]
    if(is.null(endTime)){
      endTime <- final_simulation_time
    }
    #print(concentracoes_simulacao)
    concentracoes_nova_gota <- obter_concentracoes_iniciais(concentracoes_simulacao, merged_droplet_id, i)
    #print("nova")
    concentracoes_nova_gota <- unlist(concentracoes_nova_gota)
    #print(concentracoes_nova_gota)
    #print("/nova")
    

    goticula_misturada <- createGoticula(ID, especies, concentracoes_nova_gota, reacoes, taxas_reacao, times_of_merge[[i]], endTime)
   
    #Modularizar
    print("Simula")
    b <- SimularReacoesEmGoticula(goticula_misturada)

    # Plot
    p <- plot_behavior(
      b,
      x_label     = 'Time (s)',
      y_label     = 'Concentration (M)',
      legend_name = 'Species',
      geom_list   = c('line', 'point'),
      species = especies
    )
    #print(p)
    b_list <- append(b_list, list(b))
    concentracoes_simulacao[[as.character(ID)]] <- obter_concentracoes_finais(b, goticula_misturada$especies)
    print("Criada a gotícula misturada")
    print(as.character(ID))
    input_wait <- readline()
    
    goticula_list <- append(goticula_list, goticula_misturada)
  }
  
  #combined_b <- combine_and_sum(b_list)
  #saveRDS(b_list, file = "output.rds")
  #print(combined_b)
  #p <- plot_behavior(
  #  combined_b,
  #  x_label     = 'Time (s)',
  #  y_label     = 'Concentration (M)',
  #  legend_name = 'Species',
  #  geom_list   = c('line', 'point'),
  #  species = c(especies)
  #)
  #print(p)
  # Assuming b_list is your list of objects
  json_data <- toJSON(b_list, pretty = TRUE)
  # Save to a file
  write(json_data, file = "tabelas.json")
  print("FIM")
}

args <- commandArgs(trailingOnly = TRUE)

# Extract the arguments
simulation_result <- args[1]
droplet_injection_times <- fromJSON(args[2])
data <- fromJSON(args[3])
final_simulation_time <- fromJSON(args[4])

# Check if droplet_injection_times length is 0
if (length(droplet_injection_times) == 0) {
  times_of_merge <- list()
  id_of_the_new_droplet <- list()
  merged_droplet_ids <- list()
  goticulas_iniciais <- list()
} else {
  # Extract the values from droplet_injection_times and data
  times_of_merge <- droplet_injection_times[[1]]
  id_of_the_new_droplet <- droplet_injection_times[[2]]
  merged_droplet_ids <- droplet_injection_times[[3]]
  goticulas_iniciais <- data[[1]]
}
#times_of_merge, final_simulation_time, merged_droplet_ids, id_of_the_new_droplet, data
simulate_CRNs(times_of_merge, final_simulation_time, merged_droplet_ids, id_of_the_new_droplet, data)

