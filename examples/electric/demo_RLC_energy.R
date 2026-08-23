rm(list = ls())

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

# Underdamped regime parameters
params <- c(R = 0.5, L = 1, C = 1)
regime <- 'U'

requireNamespace('diffeqr', quietly = TRUE)
solver <- 'diffeqr'

Make_Circuit_RLC <- function(name, species_input, species_output, ic, p) { 
  rate_base <- p[1]
  rate_mul1   <- rate_base * p[2]
  rate_mul2   <- rate_base * p[3]
  rate_mul3   <- rate_base * p[4]
  rate_mul4   <- rate_base * p[5]
  rate_int1   <- rate_base * p[6]
  rate_int2   <- rate_base * p[7]
  rate_add3   <- rate_base * p[8]

  gates <- list()

  l_1ol <- jn(name, 'l_1ol')
  l_mul1_p <- jn(name, 'l_mul1_p')
  l_mul1_n <- jn(name, 'l_mul1_n')
  l_mul2_p <- jn(name, 'l_mul2_p')
  l_mul2_n <- jn(name, 'l_mul2_n')
  l_mul3_p <- jn(name, 'l_mul3_p')
  l_mul3_n <- jn(name, 'l_mul3_n')
  l_mul4_p <- jn(name, 'l_mul4_p')
  l_mul4_n <- jn(name, 'l_mul4_n')
  l_add3_1_p_carry <- jn(name, 'l_add3_1_p_carry')
  l_add3_1_n_carry <- jn(name, 'l_add3_1_n_carry')

  g_mul1_p <- Make_Mul2In_Wang(jn(name, 'mul1_p'), species_input$voltage_positive, l_1ol, l_mul1_p, 0, 1/ic$inductance, rate_mul1)
  gates[[length(gates)+1]] <- g_mul1_p
  g_mul1_n <- Make_Mul2In_Wang(jn(name, 'mul1_n'), species_input$voltage_negative, l_1ol, l_mul1_n, 0, 1/ic$inductance, rate_mul1)
  gates[[length(gates)+1]] <-  g_mul1_n

  g_add3_1_p <- Make_Add3In(jn(name, 'g_add3_1_p'), l_mul1_p, l_mul2_n, l_mul4_n, l_add3_1_p_carry, 0, 0, 0, rate_add3) 
  gates[[length(gates)+1]] <- g_add3_1_p
  g_add3_1_n <- Make_Add3In(jn(name, 'g_add3_1_n'), l_mul1_n, l_mul2_p, l_mul4_p, l_add3_1_n_carry, 0, 0, 0, rate_add3)
  gates[[length(gates)+1]] <- g_add3_1_n

  g_int1 <- Make_Integrator_OishiYordanov(jn(name, 'g_int1'), l_add3_1_p_carry, l_add3_1_n_carry, species_output$current_positive, species_output$current_negative, 0, 0, rate_int1)
  gates[[length(gates)+1]] <- g_int1

  g_mul2_p <- Make_Mul2In_Wang(jn(name, 'mul2_p'), species_output$current_positive, jn(name, 'l_rol'), l_mul2_p, 0, ic$resistance/ic$inductance, rate_mul2)
  gates[[length(gates)+1]] <- g_mul2_p
  g_mul2_n <- Make_Mul2In_Wang(jn(name, 'mul2_n'), species_output$current_negative, jn(name, 'l_rol'), l_mul2_n, 0, ic$resistance/ic$inductance, rate_mul2)
  gates[[length(gates)+1]] <- g_mul2_n

  g_mul3_p <- Make_Mul2In_Wang(jn(name, 'mul3_p'), species_output$current_positive, jn(name, '_1oc'), l_mul3_p, 0, 1/ic$capacitance, rate_mul3)
  gates[[length(gates)+1]] <- g_mul3_p
  g_mul3_n <- Make_Mul2In_Wang(jn(name, 'mul3_n'), species_output$current_negative, jn(name, '_1oc'), l_mul3_n, 0, 1/ic$capacitance, rate_mul3)
  gates[[length(gates)+1]] <- g_mul3_n

  g_int2 <- Make_Integrator_OishiYordanov(jn(name, 'g_int2'), l_mul3_p, l_mul3_n, species_output$voltage_positive, species_output$voltage_negative, 0, 0, rate_int2)
  gates[[length(gates)+1]] <- g_int2

  g_mul4_p <- Make_Mul2In_Wang(jn(name, 'mul4_p'), species_output$voltage_positive, jn(name, '_m1ol'), l_mul4_p, 0, 1/ic$inductance, rate_mul4)
  gates[[length(gates)+1]] <- g_mul4_p
  g_mul4_n <- Make_Mul2In_Wang(jn(name, 'mul4_n'), species_output$voltage_negative, jn(name, '_m1ol'), l_mul4_n, 0, 1/ic$inductance, rate_mul4)
  gates[[length(gates)+1]] <- g_mul4_n

  return (gates)
}

# ---------------------------------------------------------
# Simulation Setup
# ---------------------------------------------------------
t0 <- 0
t1 <- 15 
# The timing vector for make_circuit
timing  <- seq(t0, t1, length.out = 1000) 

circuit <- make_circuit(timing)

init_p_values <- list(
  'O' = c(
    a1 = 9.5645737, a2 = 0.2892851, a3 = 9.5628290, a4 = 19.1258191,
    a5 = 9.5627359, a6 = 4.7823812, a7 = 0.1173948, a8 = 101.7993858
  ),
  'C' = c(
    a1 = 9.9977977, a2 = 0.2470188, a3 = 10.0000000, a4 = 19.9945977,
    a5 = 9.9972182, a6 = 4.9984647, a7 = 0.1158029, a8 = 106.1014152
  ),
  'U' = c(
    a1 = 1.9298698, a2 = 1.5563071, a3 = 4.7412489, a4 = 9.3631423,
    a5 = 4.6948411, a6 = 0.6383476, a7 = 0.5079602, a8 = 108.4715251
  )
)

init_p <- init_p_values[[regime]]

# Safely extract numeric parameters without names
rlc_ic <- list(
  resistance = params[["R"]], 
  inductance = params[["L"]], 
  capacitance = params[["C"]]
)

rlc_input <- list(voltage_positive = 'v1p', voltage_negative = 'v1n')
rlc_output <- list(
  current_positive = jn('i_p_', regime), current_negative = jn('i_n_', regime),
  voltage_positive = jn('vc_p_', regime), voltage_negative = jn('vc_n_', regime)
)

# circuit <- circuit_add_compile_gates(circuit, Make_Circuit_RLC(jn('RLC_', regime), rlc_input, rlc_output, rlc_ic, init_p))
circuit <- Make_Circuit_RLC_Composited_RLC(timing, 'U')

# Ground the external voltage source completely
forced_concentrations <- list(
  v1p = function(t) 0,
  v1n = function(t) 0
)

# Safely inject the initial condition into the exact numeric index of the array
target_species <- jn('vc_p_', regime)
species_idx <- which(circuit$species == target_species)
circuit$ci[species_idx] <- 1.0

# ---------------------------------------------------------
# Execution & Native Plotting
# ---------------------------------------------------------
result <- react4(
  species = circuit$species, ci = circuit$ci, reactions = circuit$reactions,
  ki = circuit$ki, t = circuit$t, engine = solver, verbose = FALSE,
  forced_concentrations = forced_concentrations
)

# Dynamically construct behavior from the solver's actual returned timesteps
# This prevents the row length mismatch error!
behavior <- data.frame(time = result[, "time"])

# Extract internal states (Dual-rail subtraction)
I_crn <- result[, jn('i_p_', regime)] - result[, jn('i_n_', regime)]
Vc_crn <- result[, jn('vc_p_', regime)] - result[, jn('vc_n_', regime)]

# Write the actual energies to the behavior table
behavior[["Charge_q"]] <- params[["C"]] * Vc_crn
behavior[["Flux_Phi"]] <- params[["L"]] * I_crn

# Write the exponential envelopes to the behavior table
alpha <- params[["R"]] / (2 * params[["L"]])
behavior[["Upper_Env"]] <- exp(-alpha * behavior$time)
behavior[["Lower_Env"]] <- -exp(-alpha * behavior$time)

# Render using your native function
Plot_behavior(
  title = "Underdamped Dissipation: Charge q(t) vs Flux \u03a6(t) (q(0)=1)", 
  behavior, 
  circuit, 
  species = c("Charge_q", "Flux_Phi"), 
  species_dotted = c("Upper_Env", "Lower_Env"), 
  normalize = FALSE
)

dsd <- Translate_4domain(circuit)