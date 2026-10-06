rm(list = ls()) 
library(dplyr) 
library(tidyr) 

setwd("~/MEGAsync/_CEFET/tcc/dnar") 
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


jn <- function(...) { paste(..., sep = '') } 


# ============================================================================== 
# 2. CRN Stochastic Generation Loop 
# ============================================================================== 
generate_crn_dataset <- function(tmax = NULL, n_replicates = NULL, volume = NULL) { 
  # Define regimes exactly as in MATLAB 
  regimes <- list( 
    # list(name="low_drive_quiescent", Iapp=60, Mtot=40, Wtot=40, volume=10), 
    list(name="tonic_spiking_hV", Iapp=100, Mtot=60, Wtot=60, volume=3)
    #list(name="near_threshold_irregular_extraGates", Iapp=80, Mtot=120, Wtot=120, volume=2)
    # list(name="tonic_spiking", Iapp=100, Mtot=40, Wtot=40, volume=10), 
    # list(name="high_drive_fast_spiking", Iapp=130, Mtot=40, Wtot=40, volume=10), 
    # list(name="channel_noise_dominant", Iapp=100, Mtot=20, Wtot=20, volume=10),
    # list(name="tonic_spiking_lV", Iapp=100, Mtot=40, Wtot=40, volume=5),
    # list(name="tonic_spiking_hV", Iapp=100, Mtot=40, Wtot=40, volume=100)
  ) 
  
  timing <- seq(0, tmax, length.out = tmax+1) 
  all_metrics <- list() 
  simulation_times <- c() # Vector to store time taken for each simulation
  
  for (r in seq_along(regimes)) { 
    reg <- regimes[[r]] 
    for (rep in 1:n_replicates) { 
      cat(sprintf("Running CRN Regime: %s, replication - %d\n", reg$name, rep)) 
      seed <- r * 10000 + rep 
      set.seed(seed) 
      
      # Initialize CRN (Assuming your wrapper functions are loaded) 
      crn <- create_ml_crn_varyingRates(rate=1, Iapp=reg$Iapp, Mtot=reg$Mtot, Wtot=reg$Wtot, use_3d_m = TRUE, volume=reg$volume) 
      crn$params$Mtot <- reg$Mtot 
      crn$params$Wtot <- reg$Wtot 
      crn$t <- timing 
      
      # Run Stochastic Simulation and track elapsed execution time
      sim_elapsed <- system.time({
        result_sto <- React_stochastic(crn, volume = reg$volume, seed=seed) 
      })[["elapsed"]]
      
      # Store the time taken for later calculation
      simulation_times <- c(simulation_times, sim_elapsed)
      
      V_sto <- result_sto[,'Vp'] - result_sto[,'Vm'] 
      
      # Extract Metrics 
      metrics_df <- calculate_spiking_metrics(timing, V_sto, tmax) 
      
      # Append Metadata (Including individual simulation time)
      metrics_df <- cbind( 
        run_id = length(all_metrics) + 1, 
        regime = reg$name, 
        replicate = rep, 
        seed = seed, 
        Iapp = reg$Iapp, 
        Mtot = reg$Mtot, 
        Wtot = reg$Wtot, 
        sim_time_seconds = sim_elapsed,
        metrics_df 
      ) 
      
      all_metrics[[length(all_metrics) + 1]] <- metrics_df 
    } 
  } 
  
  final_df <- do.call(rbind, all_metrics) 
  write.csv(final_df, "crn_stochastic_metrics_tonicspiking_hv_vol3.csv", row.names = FALSE) 
  
  # Calculate and display the final average time
  avg_sim_time <- mean(simulation_times)
  
  return(final_df) 
} 

# Execute Generation 
crn_data <- generate_crn_dataset(tmax = 800, n_replicates = 30)
# crn_data <- read.csv("./crn_stochastic_metrics_800ms.csv")

