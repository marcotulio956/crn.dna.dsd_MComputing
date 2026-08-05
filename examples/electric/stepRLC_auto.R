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

jn <- function(...) { paste(..., sep = '') }

library(ggplot2) # plot()
library(dplyr) # mutate()

behaviours <- c(
  overdamped = c(R = 2.5, L = 1, C = 1), 
  critically_damped = c(R = 2, L = 1, C = 1),
  underdamped = c(R = 0.5, L = 1, C = 1)
  
)


R <- 1#1e3         # 1 kOhm
L <- 1.5#15e-3       # 15 mH
C <- 1.5#15e-4       # 150 uF


t0 = 0
t1 = 40
points = (t1 - t0) * 100 # * 80 # Using 50 time points
timing  <- seq(t0, t1, length.out = points) 

# --- 1. Define objective: sum of squared errors between CRN and analog sim
obj_fun <- function(p_vec) {
  circuit <- DNArLogic::make_circuit(timing)
  
  # - Dig and Analog
  g_dalchau <- Make_Oscillator_Dalchau('sin', 'x', 'v1p', 'z', 1e-3, 1e-3, 15, 4e-1)
  c_comparator <- Make_Mux2_balanced(
    'mux1',
    'x', 'v1p',
    'low', 'high',
    'comp_out',
    0, 0,
    3, 8,
    0, 7.0e-1
  )
  
  # add2circuit
  circuit <- circuit_add_gate(circuit, g_dalchau)
  circuit <- circuit_add_gate(circuit, c_comparator)
  
  rlc <- Make_RLC_Component(R,L,C)
  
  rlc$il$voltage_positive <- 'v1p'
  
  init_p <- p_vec
  
  rlc_gate <- Make_Circuit_RLC(
    rlc$name,
    rlc$il,
    rlc$ol,
    rlc$ic,
    # Pass in exactly the six values we just computed:
    init_p
  )
  
  circuit <- circuit_add_compile_gates(circuit, rlc_gate)
  
  res_crn <- React_circuit(circuit)
#  print('react')
  res_crn <- res_crn[, order(names(res_crn))]
  res_crn$vc_crn <- res_crn$rlcol_vcp - res_crn$rlcol_vcn
  res_crn$il_crn <- res_crn$rlcol_ip  - res_crn$rlcol_in
  
  # 1.3 simulate analog RLC
  sim <- simulate_sRLC_voltage_source(timing, res_crn$v1p, R, L, C)
  
  # 1.4 compute SSE
  sse_vc <- sum((res_crn$vc_crn - sim$capacitor_voltage)^2)
  sse_il <- sum((res_crn$il_crn - sim$inductor_current)^2)
  
  total <- sse_vc + sse_il
  
  str = sprintf("sse_vc: %s sse_il: %s", sse_vc, sse_il)
  print(str)
  
  return(sse_vc + sse_il)
}

# --- 2. Choose initial guess and bounds
init_guess <- c(
  a1  = 1, # rate base
  a2  = 1, # rate_mul1
  a3  = 1, # rate_mul2
  a4  = 2, # rate_mul3 
  a5  = 1, # rate_mul4 
  a6  = 0.5, # rate_int1 
  a7  = 1, # rate_int2 
  a8  = 100  # rate_add3 
)
lower_bnd <- init_guess * 0.1
upper_bnd <- init_guess * 10

# --- 3. Run optimization
opt_res <- optim(
  par    = init_guess,
  fn     = obj_fun,
  method = "L-BFGS-B",
  lower  = lower_bnd,
  upper  = upper_bnd,
  control= list(trace=1, maxit=10)
)

# --- 4. Extract optimized parameters
best_p <- opt_res$par
print(best_p)
