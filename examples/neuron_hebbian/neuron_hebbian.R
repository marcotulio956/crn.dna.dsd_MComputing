rm(list = ls())

# ============================================================
# LOAD LIBRARIES
# ============================================================

source('R/parser.R')
source('R/util_functions.R')
source('R/crn_reactor.R')
source('R/NEURON_SIM.R')
source('R/NEURON_LIB.R')
source('R/dsd.R')
source('R/4domain_reactor.R')
source('R/io.R')

library(GillespieSSA2)
library(ggplot2)
library(tidyr)

# ============================================================
# UTILITIES
# ============================================================

USE_REACT_4DOMAIN <- FALSE

jn <- function(...) { paste(..., sep = '') }

flatten_neuron_crn <- function(neuron) {
  
  species_map <- unlist(neuron$species)
  
  list(
    species = unname(species_map),
    species_map = species_map,
    ci = as.numeric(neuron$ci),
    reactions = as.character(neuron$reactions),
    ki = neuron$ki
  )
}

make_neuron_case <- function(name, timing, neuron_args) {
  
  neuron <- do.call(
    Make_Spiking_CRN_Neuron,
    c(list(name = name), neuron_args)
  )
  
  neuron$t <- timing
  
  neuron
}

simulate_neuron_case <- function(
    neuron,
    qmax = 1e6,
    cmax = 1e7,
    alpha = 1,
    beta = 1
) {
  
  flat <- flatten_neuron_crn(neuron)
  
  cat("\n====================================================\n")
  cat("SIMULATING:", neuron$name, "\n")
  cat("====================================================\n")
  
  print(flat$reactions)
  
  # --------------------------------------------------------
  # Deterministic CRN
  # --------------------------------------------------------
  
  result_crn <- react(
    species = flat$species,
    ci = flat$ci,
    reactions = flat$reactions,
    ki = flat$ki,
    t = neuron$t
  )
  
  # --------------------------------------------------------
  # DSD (optional)
  # --------------------------------------------------------
  
  result_dsd <- NULL
  
  if (USE_REACT_4DOMAIN) {
    
    result_dsd <- react_4domain(
      species = flat$species,
      ci = flat$ci,
      reactions = flat$reactions,
      ki = flat$ki,
      qmax = qmax,
      cmax = cmax,
      alpha = alpha,
      beta = beta,
      t = neuron$t
    )
    
  }
  
  list(
    neuron = neuron,
    flat = flat,
    crn = result_crn,
    dsd = result_dsd
  )
}

# ============================================================
# BUILD COMPARISON DATAFRAME
# ============================================================

build_comparison_frame <- function(
    result_crn,
    result_dsd,
    species_roles,
    species_names
) {
  
  comparison <- data.frame(
    time = result_crn$time
  )
  
  behavior_dsd <- as.data.frame(result_dsd$behavior)
  
  if (length(species_roles) != length(species_names)) {
    stop("species_roles and species_names must have same size")
  }
  
  for (i in seq_along(species_names)) {
    
    role_name <- species_roles[[i]]
    species_name <- species_names[[i]]
    
    if (!species_name %in% names(result_crn)) {
      stop(paste("Species missing in CRN:", species_name))
    }
    
    if (!species_name %in% names(result_dsd$behavior)) {
      stop(paste("Species missing in DSD:", species_name))
    }
    
    comparison[[role_name]] <- result_crn[[species_name]]
    
    comparison[[jn(role_name, "_DSD")]] <- approx(
      x = behavior_dsd$time,
      y = behavior_dsd[[species_name]],
      xout = result_crn$time,
      rule = 2,
      ties = mean
    )$y
  }
  
  comparison
}

# ============================================================
# PLOTTING
# ============================================================

Plot_behavior <- function(
    resultado,
    species,
    chart_title,
    species_dotted = NULL
) {
  
  g <- plot_behavior(
    behavior = resultado,
    species = species,
    species_dotted = species_dotted,
    chart_title = chart_title,
    x_label = "Time",
    y_label = "Concentration",
    legend_name = "Species",
    geom_list = c("line")
  )
  
  print(g)
  
  invisible(g)
}

plot_neuron_case <- function(simulation, case_name) {
  
  has_dsd <- !is.null(simulation$dsd)
  
  # --------------------------------------------------------
  # Groups
  # --------------------------------------------------------
  
  input_species <- c("A1", "A2", "A3")
  
  weight_species <- c("W1", "W2", "W3")
  
  membrane_species <- c("V", "S", "R")
  
  # --------------------------------------------------------
  # Resolve names
  # --------------------------------------------------------
  
  input_names <- unname(
    simulation$flat$species_map[input_species]
  )
  
  weight_names <- unname(
    simulation$flat$species_map[weight_species]
  )
  
  membrane_names <- unname(
    simulation$flat$species_map[membrane_species]
  )
  
  # --------------------------------------------------------
  # Build frames
  # --------------------------------------------------------
  
  if (has_dsd) {
    
    input_frame <- build_comparison_frame(
      result_crn = simulation$crn,
      result_dsd = simulation$dsd,
      species_roles = input_species,
      species_names = input_names
    )
    
    weight_frame <- build_comparison_frame(
      result_crn = simulation$crn,
      result_dsd = simulation$dsd,
      species_roles = weight_species,
      species_names = weight_names
    )
    
    membrane_frame <- build_comparison_frame(
      result_crn = simulation$crn,
      result_dsd = simulation$dsd,
      species_roles = membrane_species,
      species_names = membrane_names
    )
    
  } else {
    
    input_frame <- data.frame(time = simulation$crn$time)
    weight_frame <- data.frame(time = simulation$crn$time)
    membrane_frame <- data.frame(time = simulation$crn$time)
    
    for (i in seq_along(input_species)) {
      input_frame[[input_species[i]]] <-
        simulation$crn[[input_names[i]]]
    }
    
    for (i in seq_along(weight_species)) {
      weight_frame[[weight_species[i]]] <-
        simulation$crn[[weight_names[i]]]
    }
    
    for (i in seq_along(membrane_species)) {
      membrane_frame[[membrane_species[i]]] <-
        simulation$crn[[membrane_names[i]]]
    }
    
  }
  
  # --------------------------------------------------------
  # Plots
  # --------------------------------------------------------
  
  Plot_behavior(
    resultado = input_frame,
    species = input_species,
    species_dotted =
      if (has_dsd) jn(input_species, "_DSD") else c(),
    chart_title = jn(
      "Neuron Testbench - ",
      case_name,
      " - Inputs"
    )
  )
  
  Plot_behavior(
    resultado = membrane_frame,
    species = membrane_species,
    species_dotted =
      if (has_dsd) jn(membrane_species, "_DSD") else c(),
    chart_title = jn(
      "Neuron Testbench - ",
      case_name,
      " - Membrane / Spike / Refractory"
    )
  )
  
  Plot_behavior(
    resultado = weight_frame,
    species = weight_species,
    species_dotted =
      if (has_dsd) jn(weight_species, "_DSD") else c(),
    chart_title = jn(
      "Neuron Testbench - ",
      case_name,
      " - Synaptic Weights"
    )
  )
  
  invisible(
    list(
      inputs = input_frame,
      membrane = membrane_frame,
      weights = weight_frame
    )
  )
}

# ============================================================
# TEST CASES
# ============================================================

timing <- seq(0, 1000, by = 0.25)

neuron_cases <- list(
  
  # ----------------------------------------------------------
  # QUIESCENT
  # ----------------------------------------------------------
  
  quiescent = list(
    
    name = "quiescent",
    
    neuron_args = list(
      rate = 10,
      
      cA1 = 1000,
      cA2 = 0,
      cA3 = 0,
      
      cW1 = 1,
      cW2 = 1,
      cW3 = 1,
      
      cV = 0,
      cS = 0,
      cR = 0,
      
      kIn1 = 1,
      kIn2 = 1,
      kIn3 = 1,
      
      kFire = 0.5,
      
      kLeakV = 0.15,
      kLeakS = 0.1,
      
      kRefCreate = 0.4,
      kRefDecay = 0.1,
      kReset = 1,
      
      kLearn1 = 0.01,
      kLearn2 = 0.01,
      kLearn3 = 0.01,
      
      kWDecay1 = 0.005,
      kWDecay2 = 0.005,
      kWDecay3 = 0.005
    )
  ),
  
  # ----------------------------------------------------------
  # SINGLE INPUT
  # ----------------------------------------------------------
  
  single_input = list(
    
    name = "single_input",
    
    neuron_args = list(
      
      cA1 = 4,
      cA2 = 0,
      cA3 = 0,
      
      cW1 = 1,
      cW2 = 1,
      cW3 = 1,
      
      cV = 0,
      cS = 0,
      cR = 0,
      
      kIn1 = 1.5,
      kIn2 = 1,
      kIn3 = 1,
      
      kFire = 0.7,
      
      kLeakV = 0.12,
      kLeakS = 0.08,
      
      kRefCreate = 0.35,
      kRefDecay = 0.08,
      kReset = 1,
      
      kLearn1 = 0.02,
      kLearn2 = 0.01,
      kLearn3 = 0.01,
      
      kWDecay1 = 0.004,
      kWDecay2 = 0.004,
      kWDecay3 = 0.004
    )
  ),
  
  # ----------------------------------------------------------
  # COMPETITIVE INPUTS
  # ----------------------------------------------------------
  
  competitive = list(
    
    name = "competitive",
    
    neuron_args = list(
      
      cA1 = 6,
      cA2 = 2,
      cA3 = 1,
      
      cW1 = 1,
      cW2 = 1,
      cW3 = 1,
      
      cV = 0,
      cS = 0,
      cR = 0,
      
      kIn1 = 1.4,
      kIn2 = 1.1,
      kIn3 = 0.8,
      
      kFire = 0.9,
      
      kLeakV = 0.1,
      kLeakS = 0.06,
      
      kRefCreate = 0.45,
      kRefDecay = 0.1,
      kReset = 1.2,
      
      kLearn1 = 0.03,
      kLearn2 = 0.015,
      kLearn3 = 0.01,
      
      kWDecay1 = 0.005,
      kWDecay2 = 0.005,
      kWDecay3 = 0.005
    )
  ),
  
  # ----------------------------------------------------------
  # BURSTING
  # ----------------------------------------------------------
  
  bursting = list(
    
    name = "bursting",
    
    neuron_args = list(
      
      cA1 = 7,
      cA2 = 7,
      cA3 = 7,
      
      cW1 = 1.5,
      cW2 = 1.5,
      cW3 = 1.5,
      
      cV = 0,
      cS = 0,
      cR = 0,
      
      kIn1 = 1.8,
      kIn2 = 1.8,
      kIn3 = 1.8,
      
      kFire = 1.3,
      
      kLeakV = 0.08,
      kLeakS = 0.04,
      
      kRefCreate = 0.8,
      kRefDecay = 0.15,
      kReset = 1.5,
      
      kLearn1 = 0.02,
      kLearn2 = 0.02,
      kLearn3 = 0.02,
      
      kWDecay1 = 0.003,
      kWDecay2 = 0.003,
      kWDecay3 = 0.003
    )
  )
)

# ============================================================
# RUN TESTBENCH
# ============================================================

run_neuron_case <- function(case_name, case_spec) {
  
  neuron <- make_neuron_case(
    name = case_spec$name,
    timing = timing,
    neuron_args = case_spec$neuron_args
  )
  
  simulation <- simulate_neuron_case(neuron)
  
  plot_neuron_case(
    simulation = simulation,
    case_name = case_name
  )
  
  invisible(simulation)
}

# ============================================================
# EXECUTION
# ============================================================

neuron_results <- lapply(
  
  names(neuron_cases),
  
  function(case_name) {
    
    run_neuron_case(
      case_name,
      neuron_cases[[case_name]]
    )
  }
)

names(neuron_results) <- names(neuron_cases)

cat("\n====================================================\n")
cat("ALL TESTS COMPLETED\n")
cat("====================================================\n")

invisible(neuron_results)