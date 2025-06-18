
rm(list = ls())
source('R/4domain_reactor.R')
source('R/ANALOG_GATE_LIB.R')
source('R/analysis.R')
source('R/crn_reactor.R')
source('R/dsd.R')
source('R/ELECTRO_LIB.R')
source('R/ELECTRO_SIM.R')
source('R/GATE_LIB.R')
source('R/io.R')
source('R/neuron_hjelmfelt.R')
source('R/parser.R')
source('R/util_functions.R')



library(ggplot2)
library(dplyr)

R_val <- 2        # [Ω]
L_val <- 1        # [H]
C_val <- 1        # [F]

t0 <- 0
t1 <- 35
npoints <- (t1 - t0) * 50
time_grid <- seq(t0, t1, length.out = npoints)

Make_Generic_with_params <- function(timing, p) {
  circuit <- DNArLogic::make_circuit(timing)
  
  # – Dig and Analog gates that you had hard‐coded:
  g_dalchau <- Make_Oscillator_Dalchau(
    'sin', 'x', 'v1p', 'z', 
    1e-3, 1e-3, 15, 4e-1
  )
  c_comparator <- Make_Mux2_balanced(
    'mux1',
    'x', 'v1p',
    'low', 'high',
    'comp_out',
    0, 0,
    3, 8,
    0, 7.0e-1
  )
  circuit <- circuit_add_gate(circuit, g_dalchau)
  circuit <- circuit_add_gate(circuit, c_comparator)
  
  rlc <- Make_RLC_Component(R_val, L_val, C_val)
  rlc$il$voltage_positive <- 'v1p'
  
    
  rlc_gate <- Make_Circuit_RLC(
    rlc$name,
    rlc$il,
    rlc$ol,
    rlc$ic,
    p
  )
  
  circuit <- circuit_add_electro_gates(circuit, rlc_gate)
  return(circuit)
}


run_one_sim <- function(p) {
  circuit_p <- Make_Generic_with_params(time_grid, p)
  
  result_crn <- React_circuit(circuit_p)
  result_crn <- result_crn[, order(names(result_crn)), drop = FALSE]
  
  vc_scale <- 0.1
  i_scale  <- 0.1
  
  crn_vc <- vc_scale * ( result_crn[['rlcol_vcp']] - result_crn[['rlcol_vcn']] )
  crn_il <- i_scale  * ( result_crn[['rlcol_ip']]  - result_crn[['rlcol_in']]  )
  
  simRLC <- simulate_sRLC_voltage_source(
    time_grid,
    result_crn[['v1p']],
    R_val, L_val, C_val
  )
  true_vc <- simRLC$capacitor_voltage
  true_il <- simRLC$inductor_current
  SSE_vc <- sum( (crn_vc - true_vc)^2 )
  SSE_il <- sum( (crn_il - true_il)^2 )
  total_error <- SSE_vc + SSE_il
  return(total_error)
}


calibrate_all_methods <- function(init_p, run_one_sim,
                                  lower = rep(0, length(init_p)),
                                  upper = rep(10, length(init_p))) {
  results <- list()
  
  # 1. Nelder–Mead
  results[["Nelder-Mead"]] <- tryCatch({
    res <- optim(par = init_p,
                 fn = run_one_sim,
                 method = "Nelder-Mead",
                 control = list(maxit = 200, trace = 0))
    list(par = res$par, error = res$value)
  }, error = function(e) NULL)
  
  # 2. L-BFGS-B
  results[["L-BFGS-B"]] <- tryCatch({
    res <- optim(par = init_p,
                 fn = run_one_sim,
                 method = "L-BFGS-B",
                 lower = lower,
                 upper = upper,
                 control = list(maxit = 200, trace = 0))
    list(par = res$par, error = res$value)
  }, error = function(e) NULL)
  
  # 3. Simulated Annealing (SANN)
  results[["SANN"]] <- tryCatch({
    res <- optim(par = init_p,
                 fn = run_one_sim,
                 method = "SANN",
                 control = list(maxit = 5000, temp = 10, trace = 0))
    list(par = res$par, error = res$value)
  }, error = function(e) NULL)
  
  # 4. Differential Evolution
  results[["DEoptim"]] <- tryCatch({
    de <- DEoptim(fn = run_one_sim,
                  lower = lower,
                  upper = upper,
                  control = DEoptim.control(NP = 50, itermax = 200, trace = FALSE))
    list(par = de$optim$bestmem, error = de$optim$bestval)
  }, error = function(e) NULL)
  
  # 5. Genetic Algorithm
  results[["GA"]] <- tryCatch({
    ga_res <- ga(type = "real-valued",
                 fitness = function(x) -run_one_sim(x),
                 lower = lower,
                 upper = upper,
                 popSize = 50,
                 maxiter = 200,
                 run = 50,
                 parallel = FALSE,
                 seed = 123)
    sol <- if (is.matrix(ga_res@solution)) ga_res@solution[1,] else ga_res@solution
    list(par = sol, error = run_one_sim(sol))
  }, error = function(e) NULL)
  
  # 6. Bayesian Optimization
  results[["Bayes"]] <- tryCatch({
    bounds <- setNames(as.list(rbind(lower, upper)), paste0("p", seq_along(init_p)))
    obj_fun <- function(...) {
      pars <- unlist(list(...))
      list(Score = -run_one_sim(pars))
    }
    bo <- bayesOpt(FUN = obj_fun,
                   bounds = bounds,
                   initPoints = 10,
                   iters.n = 30,
                   acq = "ucb",
                   kappa = 2.576,
                   verbose = 0)
    best <- as.numeric(getBestPars(bo))
    list(par = best, error = run_one_sim(best))
  }, error = function(e) NULL)
  
  # 7. Particle Swarm Optimization
  results[["PSO"]] <- tryCatch({
    pso_res <- psoptim(par = init_p,
                       fn = run_one_sim,
                       lower = lower,
                       upper = upper,
                       control = list(maxit = 200
                                      , trace = 0))
    list(par = pso_res$par, error = pso_res$value)
  }, error = function(e) NULL)
  
  # Assemble and rank
  df <- do.call(rbind, lapply(names(results), function(m) {
    r <- results[[m]]
    if (is.null(r)) return(NULL)
    data.frame(
      method = m,
      error  = r$error,
      par    = I(list(r$par)),
      stringsAsFactors = FALSE
    )
  }))
  
  df[order(df$error), ]
}

# ──────────────────────────────────────────────────────────────────────────
# Example usage:
# Assume `init_p` is defined and `run_one_sim` is in your environment.
  init_p <- c(
    a1 = 2, # rate base
    a2 = 100, # fuel base
    a3 = 10, # range base add3
    a4 = 2.32,    # rate_mul1 
    a5 = 0.059,    # rate_mul2 
    a6 = 1.181,    # rate_mul3 
    a7 = 0.2360,    # rate_mul4 
    a8 = 0.7535,    # rate_int1 
    a9 = 2.699,    # rate_int2 
    a10 = 1.2330,    # rate_add3 
    a11 = 1.6467,    # range_add3
    a12 = 10,    # fuel_states
    a13 = 1.61649,  # rate_states1
    a14 = 0.1218   # rate_states2
  )
ranked <- calibrate_all_methods(init_p, run_one_sim)
print(ranked)
# ──────────────────────────────────────────────────────────────────────────
