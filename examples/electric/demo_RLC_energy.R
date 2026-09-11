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

t0 <- 0
t1 <- 30 
timing  <- seq(t0, t1, length.out = 1000) 
circuit <- make_circuit(timing)

# Underdamped regime selection
regime <- 'Underdamped'
behaviours <- list(
  'Overdamped' = c(R = 4, L = 1, C = 1),
  'Critically' = c(R = 2, L = 1, C = 1),
  'Underdamped' = c(R = 1, L = 1, C = 1)
)
params <- behaviours[[regime]]
init_p_values <- list(
  'Overdamped' = c(
    a1 = 9.5645737, a2 = 0.2892851, a3 = 9.5628290, a4 = 19.1258191,
    a5 = 9.5627359, a6 = 4.7823812, a7 = 0.1173948, a8 = 101.7993858
  ),
  'Critically' = c(
    a1 = 9.9977977, a2 = 0.2470188, a3 = 10.0000000, a4 = 19.9945977,
    
    a5 = 9.9972182, a6 = 4.9984647, a7 = 0.1158029, a8 = 106.1014152
  ),
  'Underdamped' = c(
    a1 = 1.9298698, a2 = 1.5563071, a3 = 4.7412489, a4 = 9.3631423,
    a5 = 4.6948411, a6 = 0.6383476, a7 = 0.5079602, a8 = 108.4715251
  )
)

init_p <- init_p_values[[regime]]

# 1. Create the structured component
rlc_comp <- Make_RLC_Component(
  resistance = params[["R"]], 
  inductance = params[["L"]], 
  capacitance = params[["C"]]
)

### --------
##   HERE
### --------
# RLC SS 

# circuit <- circuit_add_compile_gates(circuit, Make_Circuit_RLC(jn('RLC_', regime), rlc_comp$il, rlc_comp$ol, rlc_comp$ic, init_p))
# 2. Add the composited circuit to the global circuit
# RLC Composited
circuit <- Make_Circuit_RLC_Composited2(circuit, rlc_comp, rate = 1)
# circuit <- Make_Circuit_RLC_Composited(timing, 'U')
##
### --------


# 3. Ground the external voltage source dynamically based on component IO
forced_concentrations <- list()
forced_concentrations[[rlc_comp$il$voltage_positive]] <- function(t) 0
forced_concentrations[[rlc_comp$il$voltage_negative]] <- function(t) 0

# 4. Safely inject the initial condition: V_c(0) = 1
target_species <- rlc_comp$ol$voltage_positive
species_idx <- which(circuit$species == target_species)
if (length(species_idx) > 0) {
  circuit$ci[species_idx] <- 1.0
}

# ---------------------------------------------------------
# Execution & Native Plotting
# ---------------------------------------------------------
result <- react4(
  species = circuit$species, ci = circuit$ci, reactions = circuit$reactions,
  ki = circuit$ki, t = circuit$t, engine = 'desolve', verbose = FALSE,
  forced_concentrations = forced_concentrations
)

# Dynamically construct behavior from the solver's returned timesteps
behavior <- data.frame(time = result[, "time"])

# Extract internal states dynamically (Dual-rail subtraction)
I_crn  <- result[, rlc_comp$ol$current_positive] - result[, rlc_comp$ol$current_negative]
Vc_crn <- result[, rlc_comp$ol$voltage_positive] - result[, rlc_comp$ol$voltage_negative]

# Write the actual energies to the behavior table
behavior[["Charge_q"]] <- params[["C"]] * Vc_crn
behavior[["Flux_Phi"]] <- params[["L"]] * I_crn

# Write the exponential envelopes
alpha <- params[["R"]] / (2 * params[["L"]])
behavior[["Upper_Env"]] <- exp(-alpha * behavior$time)
behavior[["Lower_Env"]] <- -exp(-alpha * behavior$time)

# Render
Plot_behavior(
  title = paste(regime, ": Charge q(t) vs Flux \u03a6(t) (q(0)=1)"), 
  behavior, 
  circuit, 
  species = c("Charge_q", "Flux_Phi"), 
  species_dotted = c("Upper_Env", "Lower_Env"), 
  normalize = FALSE
)