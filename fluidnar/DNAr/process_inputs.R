library(jsonlite)

script_arguments <- commandArgs(trailingOnly = FALSE)
script_file <- script_arguments[grepl("^--file=", script_arguments)][1]
script_directory <- if (length(script_file) == 0 || is.na(script_file)) {
  getwd()
} else {
  dirname(normalizePath(sub("^--file=", "", script_file)))
}
source(file.path(script_directory, "source_dnar.R"))

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

resolve_forcing <- function(forcing_config) {
  if (is.null(forcing_config) || isFALSE(forcing_config$enabled %||% TRUE)) {
    return(NULL)
  }
  forcing_name <- forcing_config$name %||% forcing_config[["function"]]
  allowed <- c("saw_wave_input", "sinusoidal_input", "step_input", "pulse_input", "square_input")
  if (is.null(forcing_name) || !forcing_name %in% allowed) {
    stop("Unsupported forcing function")
  }
  forcing_function <- get(forcing_name, envir = .GlobalEnv)
  parameters <- forcing_config$params %||% list()
  forcing_species <- forcing_config$species %||% ""
  if (!nzchar(forcing_species)) stop("A forcing species is required")
  setNames(list(function(t) do.call(forcing_function, c(list(t = t), parameters))), forcing_species)
}

`%||%` <- function(value, fallback) {
  if (is.null(value)) fallback else value
}

simulate_circuit <- function(goticula, simulation_config, forcing) {
  circuit <- list(
    species = goticula$especies,
    ci = as.numeric(goticula$concentracoesIniciais),
    reactions = goticula$reacoes,
    ki = as.numeric(goticula$taxasReacao),
    t = seq(goticula$tempo_inicial, goticula$tempo_final, length.out = 50)
  )
  stochastic <- isTRUE(simulation_config$stochastic)
  dna <- isTRUE(simulation_config$dna)
  volume <- as.numeric(simulation_config$volume %||% 10)
  seed <- simulation_config$seed %||% NULL
  engine <- simulation_config$engine %||% "desolve"

  if (dna && stochastic) {
    return(React_4domain_stochastic(circuit, volume = volume, seed = seed,
                                    forced_concentrations = forcing))
  }
  if (dna) {
    return(React_4domain(circuit, forced_concentrations = forcing, engine = engine))
  }
  if (stochastic) {
    return(React_stochastic(circuit, volume = volume, seed = seed,
                            forced_concentrations = forcing))
  }
  React_circuit(circuit, forced_concentrations = forcing, engine = engine)
}

extract_behavior <- function(result) {
  if (is.list(result) && !is.null(result$behavior)) result$behavior else result
}

weighted_concentrations <- function(concentration_lists, volumes) {
  all_species <- unique(unlist(lapply(concentration_lists, names)))
  total_volume <- sum(volumes)
  result <- setNames(numeric(length(all_species)), all_species)
  for (i in seq_along(concentration_lists)) {
    values <- concentration_lists[[i]]
    present <- intersect(names(values), all_species)
    result[present] <- result[present] + as.numeric(values[present]) * volumes[i]
  }
  result / total_volume
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
  simulate_circuit(goticula, simulation_config, forcing)
}

obter_concentracoes_finais <- function(resultado_simulacao_antiga, especies) {
  concentracoes_simulacao <- c()
  for(especie in especies){
    array_result <- tail(resultado_simulacao_antiga[[especie]])
    concentracoes_simulacao <- append(concentracoes_simulacao, array_result[length(array_result)])
  }
  setNames(concentracoes_simulacao, especies)
}

simulate_CRNs <- function(times_of_merge, final_simulation_time, merged_droplet_ids, id_of_the_new_droplet, data) {
  print("start simulation")
  goticulas_iniciais <- data[[1]]
  reacoes_data <- data$reacoes %||% data[[3]]
  reacoes <- as.character(reacoes_data$reacao %||% vapply(reacoes_data, function(reaction) reaction$reacao, character(1)))
  taxas_reacao <- as.numeric(reacoes_data$rate %||% vapply(reacoes_data, function(reaction) reaction$rate, numeric(1)))
  simulation_config <<- data$simulation %||% list(engine = "desolve", stochastic = FALSE, dna = FALSE, volume = 10)
  forcing <<- resolve_forcing(data$forcing)
  goticula_list <- list()
  droplet_volumes <- list()
  for (i in 1:length(goticulas_iniciais$especies)) {
    goticula_data <- list(volume = goticulas_iniciais$volume[[i]] %||% 2.25e-13)
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
    goticula$volume <- as.numeric(goticula_data$volume %||% 2.25e-13)
    droplet_volumes[[as.character(ID)]] <- goticula$volume
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
    b <- extract_behavior(SimularReacoesEmGoticula(goticula))
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
  }

  print("Simulando goticulas misturadas")
  for (i in seq_along(id_of_the_new_droplet)) {
    ID <- id_of_the_new_droplet[[i]]
    last_id<-ID
    merged_droplet_id <- merged_droplet_ids[[i]]
    endTime <- time_of_merge_list[[as.character(ID)]]
    if(is.null(endTime)){
      endTime <- final_simulation_time
    }
    #print(concentracoes_simulacao)
    origin_concentrations <- lapply(merged_droplet_id, function(origin_id) {
      concentracoes_simulacao[[as.character(origin_id)]]
    })
    origin_volumes <- vapply(merged_droplet_id, function(origin_id) {
      as.numeric(droplet_volumes[[as.character(origin_id)]])
    }, numeric(1))
    concentracoes_nova_gota <- weighted_concentrations(origin_concentrations, origin_volumes)
    

    goticula_misturada <- createGoticula(ID, especies, concentracoes_nova_gota, reacoes, taxas_reacao, times_of_merge[[i]], endTime)
    goticula_misturada$volume <- sum(origin_volumes)
    droplet_volumes[[as.character(ID)]] <- goticula_misturada$volume
   
    #Modularizar
    print("Simula")
    b <- extract_behavior(SimularReacoesEmGoticula(goticula_misturada))

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

