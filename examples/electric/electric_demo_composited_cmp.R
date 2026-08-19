source('R/parser.R')
source('R/util_functions.R')
source('R/crn_reactor.R')
source('R/4domain_reactor.R')
source('R/io.R')

source('R/GATE_LIB.R')
source('R/ANALOG_GATE_LIB.R')
source('R/ELECTRO_LIB.R')
source('R/ELECTRO_SIM.R')

source('R/forced_concentrations.R')

jn <- function(...) { paste(..., sep = '') }

library(ggplot2)
library(dplyr)

behaviours <- list(
  'O' = c(R = 4, L = 1, C = 1),
  'C' = c(R = 2, L = 1, C = 1),
  'U' = c(R = 1, L = 1, C = 1)
)

requireNamespace('diffeqr', quietly = TRUE)
solver <- 'diffeqr'

# ============================================================
# 1. PURE COMPONENTS
# ============================================================

Make_Circuit_Pure_Inductor <- function(name, species_input, species_output, ic, rate) {
  gates <- list()
  
  # Pure Inductor Model (Admittance causality):
  # di/dt = V_L / L
  # i = Integral(di/dt)
  
  di_p <- jn(name, '_di_p')
  di_n <- jn(name, '_di_n')
  
  g_dip <- Make_Mul2In_Wang(
    jn(name, '_mul_dip'),
    species_input$voltage_positive, jn(name, '_1oL'), di_p,
    0, 1 / ic$inductance, rate
  )
  g_din <- Make_Mul2In_Wang(
    jn(name, '_mul_din'),
    species_input$voltage_negative, jn(name, '_1oL'), di_n,
    0, 1 / ic$inductance, rate
  )
  
  g_int_i <- Make_Integrator_OishiYordanov(
    jn(name, '_int_i'),
    di_p, di_n,
    species_output$current_positive, species_output$current_negative,
    0, 0, rate
  )
  
  gates <- append(gates, list(g_dip, g_din, g_int_i))
  return(gates)
}

Make_Circuit_Pure_Capacitor_Series <- function(name, species_input, species_output, ic, rate) {
  gates <- list()
  
  # Pure Capacitor Model (Impedance causality for Series):
  # dV_c/dt = i / C
  # V_c = Integral(dV_c/dt)
  
  dvc_p <- jn(name, '_dvc_p')
  dvc_n <- jn(name, '_dvc_n')
  
  g_dvcp <- Make_Mul2In_Wang(
    jn(name, '_mul_dvcp'),
    species_input$current_positive, jn(name, '_1oC'), dvc_p,
    0, 1 / ic$capacitance, rate
  )
  g_dvcn <- Make_Mul2In_Wang(
    jn(name, '_mul_dvcn'),
    species_input$current_negative, jn(name, '_1oC'), dvc_n,
    0, 1 / ic$capacitance, rate
  )
  
  g_int_vc <- Make_Integrator_OishiYordanov(
    jn(name, '_int_vc'),
    dvc_p, dvc_n,
    species_output$voltage_positive, species_output$voltage_negative,
    0, 0, rate
  )
  
  gates <- append(gates, list(g_dvcp, g_dvcn, g_int_vc))
  return(gates)
}

# ============================================================
# 2. STATE SPACE RLC (Without Composability - Swapped Add/Mul)
# ============================================================
Make_Circuit_RLC_StateSpace <- function(name, input_species, R, L, C, rate) {
  gates <- list()
  
  i_p <- jn(name, '_i_p')
  i_n <- jn(name, '_i_n')
  vc_p <- jn(name, '_vc_p')
  vc_n <- jn(name, '_vc_n')
  
  vr_p <- jn(name, '_vr_p')
  vr_n <- jn(name, '_vr_n')
  
  # V_R = i * R
  gates[[1]] <- Make_Mul2In_Wang(jn(name, '_vrp'), i_p, jn(name, '_R'), vr_p, 0, R, rate)
  gates[[2]] <- Make_Mul2In_Wang(jn(name, '_vrn'), i_n, jn(name, '_R'), vr_n, 0, R, rate)
  
  # Add before Multiply (Subtract V_R and V_C from V_in)
  vl_p <- jn(name, '_vl_p')
  vl_n <- jn(name, '_vl_n')
  
  gates[[3]] <- Make_Add3In(jn(name, '_vlp'), input_species$vp, vr_n, vc_n, vl_p, 0, 0, 0, rate)
  gates[[4]] <- Make_Add3In(jn(name, '_vln'), input_species$vn, vr_p, vc_p, vl_n, 0, 0, 0, rate)
  
  # Multiply by 1/L leading to integration
  di_p <- jn(name, '_di_p')
  di_n <- jn(name, '_di_n')
  gates[[5]] <- Make_Mul2In_Wang(jn(name, '_dip'), vl_p, jn(name, '_1oL'), di_p, 0, 1/L, rate)
  gates[[6]] <- Make_Mul2In_Wang(jn(name, '_din'), vl_n, jn(name, '_1oL'), di_n, 0, 1/L, rate)
  
  # Integrate to get i
  gates[[7]] <- Make_Integrator_OishiYordanov(jn(name, '_inti'), di_p, di_n, i_p, i_n, 0, 0, rate)
  
  # Capacitor path: dV_c/dt = i / C
  dvc_p <- jn(name, '_dvc_p')
  dvc_n <- jn(name, '_dvc_n')
  gates[[8]] <- Make_Mul2In_Wang(jn(name, '_dvcp'), i_p, jn(name, '_1oC'), dvc_p, 0, 1/C, rate)
  gates[[9]] <- Make_Mul2In_Wang(jn(name, '_dvcn'), i_n, jn(name, '_1oC'), dvc_n, 0, 1/C, rate)
  
  # Integrate to get V_C
  gates[[10]] <- Make_Integrator_OishiYordanov(jn(name, '_intvc'), dvc_p, dvc_n, vc_p, vc_n, 0, 0, rate)
  
  return(gates)
}

# ============================================================
# 3. COMPOSITED RLC TOPOLOGY
# ============================================================
Make_Composited_RLC <- function(name, input_species, R, L, C, rate) {
  gates <- list()
  
  # Shared variables
  i_p <- jn(name, '_i_p')
  i_n <- jn(name, '_i_n')
  vc_p <- jn(name, '_vc_p')
  vc_n <- jn(name, '_vc_n')
  vl_p <- jn(name, '_vl_p')
  vl_n <- jn(name, '_vl_n')
  vr_p <- jn(name, '_vr_p')
  vr_n <- jn(name, '_vr_n')
  
  # KVL: V_L = V_in - V_R - V_C
  g_vlp <- Make_Add3In(jn(name, '_add_vlp'), input_species$vp, vr_n, vc_n, vl_p, 0, 0, 0, rate)
  g_vln <- Make_Add3In(jn(name, '_add_vln'), input_species$vn, vr_p, vc_p, vl_n, 0, 0, 0, rate)
  gates <- append(gates, list(g_vlp, g_vln))
  
  # Pure Inductor Instantiation
  ind_in <- list(voltage_positive = vl_p, voltage_negative = vl_n)
  ind_out <- list(current_positive = i_p, current_negative = i_n)
  ind_ic <- list(inductance = L)
  gates <- append(gates, Make_Circuit_Pure_Inductor(jn(name, '_L'), ind_in, ind_out, ind_ic, rate))
  
  # Pure Resistor (Simple Multiplier)
  g_vrp <- Make_Mul2In_Wang(jn(name, '_R_vp'), i_p, jn(name, '_R_val'), vr_p, 0, R, rate)
  g_vrn <- Make_Mul2In_Wang(jn(name, '_R_vn'), i_n, jn(name, '_R_val'), vr_n, 0, R, rate)
  gates <- append(gates, list(g_vrp, g_vrn))
  
  # Pure Capacitor Instantiation
  cap_in <- list(current_positive = i_p, current_negative = i_n)
  cap_out <- list(voltage_positive = vc_p, voltage_negative = vc_n)
  cap_ic <- list(capacitance = C)
  gates <- append(gates, Make_Circuit_Pure_Capacitor_Series(jn(name, '_C'), cap_in, cap_out, cap_ic, rate))
  
  return(gates)
}

# ============================================================
# 4. TESTBENCH EXECUTION
# ============================================================
Make_Circuit_Testbench <- function(timing, regime) {
  params <- behaviours[[regime]]
  R <- params["R"]
  L <- params["L"]
  C <- params["C"]
  rate <- 1e-3
  
  circuit <- make_circuit(timing)
  
  # Input Stimulus (Step response mapped as standard oscillator input block)
  g_dalchau <- Make_Oscillator_Dalchau('sin', 'x', 'v1p', 'z', 1e-3, 1e-3, 15, 4e-1)
  c_comparator <- Make_Mux2_balanced('mux1', 'x', 'v1p', 'low', 'high', 'comp_out', 0, 0, 3, 8, 0, 7.0e-1)
  
  circuit <- circuit_add_gate(circuit, g_dalchau)
  circuit <- circuit_add_gate(circuit, c_comparator)
  
  vin_signals <- list(vp = 'v1p', vn = 'dummy_v1n') # dummy_v1n starts at 0
  
  # Add State Space RLC
  ss_gates <- Make_Circuit_RLC_StateSpace('ssRLC', vin_signals, R, L, C, rate)
  circuit <- circuit_add_compile_gates(circuit, ss_gates)
  
  # Add Composited RLC
  comp_gates <- Make_Composited_RLC('compRLC', vin_signals, R, L, C, rate)
  circuit <- circuit_add_compile_gates(circuit, comp_gates)
  
  return(circuit)
}
# ============================================================
# 4. TESTBENCH EXECUTION
# ============================================================
t0 = 0
t1 = 40
points = (t1 - t0) * 400
timing  <- seq(t0, t1, length.out = points)
behavior <- data.frame(time = timing)

for (regime in names(behaviours)) {
  params <- behaviours[[regime]]
  
  circuit <- Make_Circuit_Testbench(timing, regime)
  
  cat(sprintf("Simulating %s RLC circuit (State-Space vs Composited)...\n", regime))
  result <- react2(
    species = circuit$species,
    ci = circuit$ci,
    reactions = circuit$reactions,
    ki = circuit$ki,
    t = circuit$t,
    engine = solver,
    verbose = FALSE
  )
  
  if (!"v_in" %in% names(behavior)) {
    behavior[["v_in"]] <- result[, "v1p"]
  }
  
  # Capture State-Space outputs
  behavior[[jn("vc_SS_", regime)]] <- result[, "ssRLC_vc_p"] - result[, "ssRLC_vc_n"]
  
  # Capture Composited outputs (Corrected column names)
  behavior[[jn("vc_COMP_", regime)]] <- result[, "compRLC_vc_p"] - result[, "compRLC_vc_n"]
  
  # Reference Model Simulation
  simRLC <- simulate_sRLC_voltage_source(timing, result[['v1p']], params["R"], params["L"], params["C"])
  behavior[[jn("V(C)_REF_", regime)]] <- simRLC$capacitor_voltage
}

# ============================================================
# 5. GENERATE PLOTS (One for each regime)
# ============================================================

# Plot 1: Overdamped (O)
Plot_behavior(
  behavior, 
  title = "Overdamped (O) RLC Response: Composited vs State-Space vs Reference\n",
  species = c('v_in', 'vc_COMP_O', 'vc_SS_O'),
  species_dotted = c('V(C)_REF_O'),
  normalize = TRUE
)

# Plot 2: Critically Damped (C)
Plot_behavior(
  behavior, 
  title = "Critically Damped (C) RLC Response: Composited vs State-Space vs Reference\n",
  species = c('v_in', 'vc_COMP_C', 'vc_SS_C'),
  species_dotted = c('V(C)_REF_C'),
  normalize = TRUE
)

# Plot 3: Underdamped (U)
Plot_behavior(
  behavior, 
  title = "Underdamped (U) RLC Response: Composited vs State-Space vs Reference\n",
  species = c('v_in', 'vc_COMP_U', 'vc_SS_U'),
  species_dotted = c('V(C)_REF_U'),
  normalize = TRUE
)