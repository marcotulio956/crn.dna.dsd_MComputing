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

# Extended time to observe a few clear oscillation cycles
timing <- seq(0, 30, by = 0.05)

# Increased rate to ensure the CRN analog gates compute fast enough 
# to track the sine wave without introducing artificial CRN-delay.
rate <- 100  

inductance <- 10
resistance <- 0

# 1. Initialize purely inductive component
ind0 <- Make_Inductor_Component(0, inductance, resistance)

# 2. Map dual-rail voltage inputs explicitly
ind0$il$voltage_positive <- 'v_in_p'
ind0$il$voltage_negative <- 'v_in_n'

# 3. Build CRN
l0 <- Make_Circuit_Pure_Inductor_Integrator(ind0$name, ind0$il, ind0$ol, ind0$ic, rate)

circuit <- make_circuit(timing)
circuit <- circuit_add_compile_gates(circuit, l0)

# ==============================================================================
# CRITICAL FIX: Zero-Mean Differential Voltage
# We apply a +5 offset to BOTH rails. 
# The CRN computes Vin = (v_in_p - v_in_n), resulting in a pure sine wave 
# oscillating around 0, preventing the inductor current from ramping to infinity.
# ==============================================================================
omega <- 2 * pi * 0.1
forced_concentrations <- list(
  v_in_p = function(t) 5 + 1 * sin(omega * t),
  v_in_n = function(t) 5
)
# hertz = 0.1 
behavior <- React_circuit(circuit, forced_concentrations = forced_concentrations, engine = 'desolve')

# 4. Extract Real (Differential) Signals for Plotting
behavior['v_in'] <- behavior['v_in_p'] - behavior['v_in_n']
behavior['il'] <- behavior['l0ol_ip'] - behavior['l0ol_in']

# 5. Generate Theoretical Target for Comparison
# Integral of sin(wt) is -cos(wt)/w. We add +1/(w*L) so initial current is 0.
behavior['I(L)'] <- -(1 / (omega * inductance)) * cos(omega * timing) + (1 / (omega * inductance))

# 6. Plot the Result
title <- "Inductor 90 Degree Phase Lag Vin=sin(0.1hz)"

# Because normalize=TRUE is active, the amplitude differences are scaled away,
# making the phase shift visually obvious.
Plot_behavior(
  title = title, 
  behavior, 
  circuit, 
  species = c('v_in', 'il'), 
  species_dotted = c('I(L)'), 
  normalize = TRUE
)