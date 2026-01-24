rm(list = ls())

source('R/4domain_reactor.R')
source('R/ANALOG_GATE_LIB.R')
source('R/analysis.R')
source('R/crn_reactor.R')
source('R/dsd.R')
source('R/NEURON_SIM.R')
source('R/GATE_LIB.R')
source('R/ELECTRO_LIB.R')
source('R/io.R')
source('R/parser.R')
source('R/util_functions.R')
source('R/metric_functions.R')
source('examples/izhikevich_neuron/plotspikes.R')

jn <- function(...) { paste(..., sep = '') }

library(ggplot2) # plot()
library(dplyr) # mutate()


# # --- presets
# izh_presets <- list(
#   "Regular Spiking (RS)" = list(a=0.02, b=0.2, c=-65, d=8, v0=-65),
#   "Intrinsically Bursting (IB)" = list(a=0.02, b=0.2, c=-55, d=4, v0=-65),
#   "Chattering (CH)" = list(a=0.02, b=0.2, c=-50, d=2, v0=-60),
#   "Fast Spiking (FS)" = list(a=0.1, b=0.2, c=-65, d=2, v0=-65),
#   "Low-threshold spiking (LTS)" = list(a=0.02, b=0.25, c=-65, d=2, v0=-65),
#   "Phasic Spiking (PS)" = list(a=0.02, b=0.25, c=-65, d=6, v0=-64),
#   "Tonic Spiking (TS)" = list(a=0.02, b=0.2, c=-65, d=6, v0=-65)
# )

izh_behaviours <- list(
  "RS" = list(a=0.02, b=0.2, c=-65, d=8, v_thresh = 30),
  "IB" = list(a=0.02, b=0.2, c=-55, d=4, v_thresh = 30),
  "CH" = list(a=0.02, b=0.2, c=-50, d=2, v_thresh = 30)
  # add others as needed
)

# ODE simulator wrapper already provided by you: simulate_izhikevich(timing, I, params)
# We will assume a driver gate `Make_Oscillator_Dalchau` or any generator that outputs to comp$il$I_p.

# Example objective function: p_vec is the list of tuning rates (named vector)
obj_fun_izh <- function(p_vec, timing, behaviour_name, izh_comp, driver_gate = NULL) {
  # p_vec: named vector of rates to tune (we map them into p used by Make_Circuit_Izhikevich)
  p_list <- list(
    rate_base    = p_vec['rate_base'],
    rate_mul_sq  = p_vec['rate_mul_sq'],
    rate_mul_lin = p_vec['rate_mul_lin'],
    rate_add     = p_vec['rate_add'],
    rate_int_v   = p_vec['rate_int_v'],
    rate_int_u   = p_vec['rate_int_u'],
    rate_mul_b   = p_vec['rate_mul_b'],
    rate_a_scale = p_vec['rate_a_scale'],
    mux_rate     = p_vec['mux_rate'],
    comp_rate    = p_vec['comp_rate']
  )

  circuit <- DNArLogic::make_circuit(timing)

  # --- Insert driver (input current) gate
  # If driver_gate is NULL, create a default tonic pulse into izh_comp$il$I_p
  if (is.null(driver_gate)) {
    # use your existing oscillator to feed I_p (a simple pulse)
    # Example: Make_Oscillator_Dalchau('sin', 'x', izh_comp$il$I_p, 'z', 1e-3, 1e-3, 15, 4e-1)
    # If you have a pulse maker, use it; here we create a minimal placeholder gate:
    if (exists("Make_Oscillator_Dalchau", mode = "function")) {
      g_drv <- Make_Oscillator_Dalchau('sin', 'x', izh_comp$il$I_p, 'z', 1e-3, 1e-3, 15, 4e-1)
      circuit <- circuit_add_gate(circuit, g_drv)
    } else {
      # user can supply a proper driver; otherwise the input stays zero (I_p/I_n zero)
      warning("No driver provided and Make_Oscillator_Dalchau not found; input I will be zero.")
    }
  } else {
    circuit <- circuit_add_gate(circuit, driver_gate)
  }

  # --- Build neuron gates and add to circuit
  # izh_gate <- Make_Circuit_Izhikevich_tuning(izh_comp$name, izh_comp$il, izh_comp$ol, izh_comp$ic, p_list)
  izh_gate <- Make_Circuit_Izhikevich_Full(izh_comp$name, izh_comp$il, izh_comp$ol, izh_comp$ic, p_list)
  circuit <- circuit_add_electro_gates(circuit, izh_gate)

  # --- React the circuit (simulate CRN)
  res_crn <- React_circuit(circuit)
  res_crn <- res_crn[, order(names(res_crn))]

  # --- Extract CRN voltage trace v_crn = v_p - v_n
  v_p_name <- izh_comp$ol$v_p
  v_n_name <- izh_comp$ol$v_n
  if (!(v_p_name %in% names(res_crn))) stop("v_p species not found in CRN results.")
  v_p_tr  <- res_crn[[v_p_name]]
  v_n_tr  <- if (v_n_name %in% names(res_crn)) res_crn[[v_n_name]] else rep(0, nrow(res_crn))
  v_crn   <- v_p_tr - v_n_tr

  # --- Extract CRN input current (I_p - I_n) to feed ODE sim
  I_p_name <- izh_comp$il$I_p
  I_n_name <- izh_comp$il$I_n
  I_p_tr <- if (I_p_name %in% names(res_crn)) res_crn[[I_p_name]] else rep(0, nrow(res_crn))
  I_n_tr <- if (I_n_name %in% names(res_crn)) res_crn[[I_n_name]] else rep(0, nrow(res_crn))
  I_crn  <- I_p_tr - I_n_tr

  # --- Run ODE sim with the same input I_crn
  # If your simulate_izhikevich expects I to be same length as timing, pass I_crn
  sim <- simulate_izhikevich(timing = timing, I = I_crn, params = izh_comp$ic)

  # --- Align and compute SSE on membrane potential (v)
  # Ensure lengths match (React_circuit and timing must match)
  # Convert sim$v (numeric) and v_crn to same length
  if (length(sim$v) != length(v_crn)) {
    # attempt simple interpolation of sim$v onto CRN times if length differs
    sim_v_interp <- approx(sim$time, sim$v, xout = timing)$y
  } else sim_v_interp <- sim$v

  sse_v <- sum((v_crn - sim_v_interp)^2, na.rm = TRUE)

  # Optionally include u trace or spike time penalty; for now return sse_v
  cat(sprintf("[obj_fun_izh] SSE_v = %g\n", sse_v))
  return(sse_v)
}

# -------------------------
# 4) Example: run optimization for one behaviour
# -------------------------
# timing (choose a resolution high enough for spikes)
t0 <- 0; t1 <- 200; dt <- 0.1
timing <- seq(t0, t1, by = dt)

# prepare component for behaviour 'RS' as example
izh_comp <- Make_Izhikevich_Component(a = 0.02, b = 0.2, c = -65, d = 8, v_thresh = 30)

# initial guess for p_vec (named vector)
init_p <- c(rate_base = 1, rate_mul_sq = 1, rate_mul_lin = 1, rate_add = 1, rate_int_v = 1, rate_int_u = 1, rate_mul_b = 1, rate_a_scale = 1, mux_rate = 1, comp_rate = 1)
lower_bnd <- init_p * 0.1
upper_bnd <- init_p * 10

# Run a short optimization (L-BFGS-B). This will be expensive: React_circuit each evaluation.
opt_res <- optim(par = init_p,
                 fn = function(p) obj_fun_izh(p, timing, 'RS', izh_comp, driver_gate = NULL),
                 method = "L-BFGS-B",
                 lower = lower_bnd,
                 upper = upper_bnd,
                 control = list(trace = 1, maxit = 20)
)

best_p <- opt_res$par
print(best_p)