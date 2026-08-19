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

behaviours <- list(
  'O' = c(R = 2.2, L = 1, C = 1),# 2
  'C' = c(R = 1.2, L = 1, C = 1),# 1 
  'U' = c(R = 0.25, L = 1, C = 1) # 0.5
)


requireNamespace('diffeqr',quietly = TRUE)
solver <- 'diffeqr'
  

# ---------------------------------------------------------
# Composited RLC Circuit Implementation
# ---------------------------------------------------------
Make_Composited_RLC <- function(timing, regime) {
  # Load parameters for the requested regime
  params <- behaviours[[regime]]
  R <- params["R"]
  L <- params["L"]
  C <- params["C"]

  # Base rate for the components
  rate <- 1e-3
  
  circuit <- make_circuit(timing)
  
  # 1. Input Signal Generation (Digital/Analog Interface)
  g_dalchau <- Make_Oscillator_Dalchau('sin', 'x', 'v1p', 'z', 1e-3, 1e-3, 15, 4e-1)
  c_comparator <- Make_Mux2_balanced(
    'mux1', 'x', 'v1p', 'low', 'high', 'comp_out',
    0, 0, 3, 8, 0, 7.0e-1
  )
  circuit <- circuit_add_gate(circuit, g_dalchau)
  circuit <- circuit_add_gate(circuit, c_comparator)
  
  # 2. Define Shared Species for the Series RLC Connection
  # V_RL = V_in - V_C
  v_rl_p <- jn('comp_', regime, '_vrl_p')
  v_rl_n <- jn('comp_', regime, '_vrl_n')
  
  i_series_p <- jn('comp_', regime, '_i_p')
  i_series_n <- jn('comp_', regime, '_i_n')
  
  vc_p <- jn('comp_', regime, '_vc_p')
  vc_n <- jn('comp_', regime, '_vc_n')
  
  dummy_0 <- 'dummy_0'

  # 3. Voltage Subtraction (Feedback Loop): V_RL = V_in - V_C
  g_add_vrl_p <- Make_Add3In(
    jn('add_vrl_p_', regime),
    'v1p', vc_n, dummy_0, # V_inp + V_Cn (dual rail subtraction)
    v_rl_p,
    0, 0, 0, rate
  )
  g_add_vrl_n <- Make_Add3In(
    jn('add_vrl_n_', regime),
    'v1n', vc_p, dummy_0, # V_inn + V_Cp
    v_rl_n,
    0, 0, 0, rate
  )
  circuit <- circuit_add_compile_gates(circuit, list(g_add_vrl_p, g_add_vrl_n))

  # 4. Instantiate the RL Block (Resistor + Inductor)
  rl_ic <- list(resistance = R, inductance = L)
  rl_input <- list(voltage_positive = v_rl_p, voltage_negative = v_rl_n)
  rl_output <- list(current_positive = i_series_p, current_negative = i_series_n, 
                    voltage_positive = 'vl_p_dummy', voltage_negative = 'vl_n_dummy')
  
  rl_gates <- Make_Circuit_RL2(jn('RL_', regime), rl_input, rl_output, rl_ic, rate)
  circuit <- circuit_add_compile_gates(circuit, rl_gates)

  # 5. Instantiate the C Block (Capacitor)
  # Since the RC model expects a voltage to calculate its own current, we bypass 
  # the RC's internal V->I conversion and directly integrate the series current.
  dvc_p <- jn('C_', regime, '_dvc_p')
  dvc_n <- jn('C_', regime, '_dvc_n')
  
  g_mul_dvcp <- Make_Mul2In_Wang(
    jn('C_mul_dvcp_', regime),
    i_series_p, jn('C_1oC_', regime), dvc_p,
    0, 1 / C, rate
  )
  g_mul_dvcn <- Make_Mul2In_Wang(
    jn('C_mul_dvcn_', regime),
    i_series_n, jn('C_1oC_', regime), dvc_n,
    0, 1 / C, rate
  )
  
  g_int_vc <- Make_Integrator_OishiYordanov(
    jn('C_int_vc_', regime),
    dvc_p, dvc_n,
    vc_p, vc_n,
    0, 0, rate
  )
  
  circuit <- circuit_add_compile_gates(circuit, list(g_mul_dvcp, g_mul_dvcn, g_int_vc))

  return(circuit)
}


t0 = 0
t1 = 80
points = (t1 - t0) * 400 # * 80 # Using 50 time points
timing  <- seq(t0, t1, length.out = points) # Using 50 time points

behavior <- data.frame(time = timing)

for (regime in names(behaviours)) {
  params <- behaviours[[regime]]
  R <- params["R"]
  L <- params["L"]
  C <- params["C"]

  circuit <- Make_RLC(timing, regime)


  cat(sprintf("Simulating %s RLC circuit...\n", regime))
  result <- react2(
    species = circuit$species,
    ci = circuit$ci,
    reactions = circuit$reactions,
    ki = circuit$ki,
    t = circuit$t,
    engine = solver,
    verbose = FALSE
  )


  if (!"vc_in" %in% names(behavior)) {
    behavior[["v_in"]] <- result[, "v1p"]
  }
  # all_result[[jn("i_",regime)]] <- result[, "rlcol_ip"] - result[, "rlcol_in"]
  behavior[[jn("vc_",regime)]] <- result[, "rlcol_vcp"] - result[, "rlcol_vcn"] 

  simRLC <- simulate_sRLC_voltage_source(
    timing, result[['v1p']], R, L, C
  )

  behavior[[jn("V(C)_",regime)]] <- simRLC$capacitor_voltage
  # all_result[[jn("I(L)_",regime)]] <- simRLC$inductor_current

  # assign(paste0("result_", gsub(" ", "_", regime)), result)

  metrics <- analyze_transient_metrics(
    timing = timing,
    v_in = behavior[["v_in"]],
    vc_model = result[['rlcol_vcp']] - result[['rlcol_vcn']],
    vc_sim = simRLC$capacitor_voltage,
    t0 = 10,
    t1 = 25,
    resistance = R,
    inductance = L,
    capacitance = C
  )

    print(metrics)
}


plot_behavior(
  behavior, 
  title = sprintf("RLC Response DSD Vin=10[V]\n"), # "RLC Step Response 
  species = c('v_in', 'vc_O', 'vc_C', 'vc_U'),
  species_dotted= c('V(C)_O', 'V(C)_C', 'V(C)_U'),
)

