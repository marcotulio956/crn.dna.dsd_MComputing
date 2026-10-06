rm(list = ls())

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

jn <- function(...) { paste(..., sep = '') }

tune_crn_dynamic_range <- function(base_pars, base_state, target_time_seq) {
  
  # Define the spread of currents we want the CRN to respond well to
  # e.g., Quiescent (40), Onset (60), Mid-fire (90), Max-fire (120)
  current_steps <- c(40, 60, 90, 120) 
  
  # 1. Pre-calculate Ground Truth ODE traces for ALL current steps
  target_traces <- list()
  for (I_test in current_steps) {
    temp_pars <- base_pars
    temp_pars$input_func <- function(t) I_test
    
    ode_out <- ode(y = base_state, times = target_time_seq, 
                   func = morris_lecar, parms = temp_pars)
    target_traces[[as.character(I_test)]] <- ode_out[, "v"]
  }
  
  # 2. Objective Function evaluating the whole f-I curve
  objective_function <- function(opt_params) {
    
    total_sse <- 0
    
    # Loop through each current step and test the CRN
    for (I_test in current_steps) {
      
      ml <- create_morris_lecar_crn(
        # --- OPTIMIZED KINETICS ---
        gCa = opt_params["gCa"], gK = opt_params["gK"], gL = opt_params["gL"],
        k_m_open = opt_params["k_m_open"], k_m_close = opt_params["k_m_close"],
        k_w_open = opt_params["k_w_open"], k_w_close = opt_params["k_w_close"],
        k_m_spont_open = opt_params["k_m_spont_open"], k_m_spont_close = opt_params["k_m_spont_close"],
        k_w_spont_open = opt_params["k_w_spont_open"], k_w_spont_close = opt_params["k_w_spont_close"],
        k_ann = opt_params["k_ann"],
        
        # --- FIXED PHYSICS ---
        C = 20, # KEEP THIS AT 20!
        VCa = 120, VK = -84, VL = -60, Mtot = 40, Wtot = 40,
        V0 = base_state["v"], M0 = 0, W0 = 20, 
        
        # --- CURRENT STEP INJECTION ---
        Ip0 = I_test, Im0 = 0
      )
      
      # Simulate Deterministic CRN
      crn_out <- react2(
        species = ml$species, ci = ml$ci, reactions = ml$reactions, ki = ml$ki,
        t = target_time_seq, verbose = FALSE,
        forced_concentrations = list() # Remove forced input; using Ip0 initial condition instead
      )
      
      v_crn <- crn_out[, "Vp"] - crn_out[, "Vm"]
      v_target <- target_traces[[as.character(I_test)]]
      
      # Accumulate error across all tested currents
      step_sse <- sum((v_target - v_crn)^2, na.rm = TRUE)
      total_sse <- total_sse + step_sse
    }
    
    if (is.na(total_sse) || is.infinite(total_sse)) return(1e12)
    return(total_sse)
  }
  
  # 3. Run Optimization
  opt_results <- optim(
    par = initial_guess, 
    fn = objective_function, 
    method = "L-BFGS-B", 
    lower = lower_bounds, upper = upper_bounds,
    control = list(trace = 1, maxit = 5)
  )
  
  return(opt_results)
}