rm(list = ls())

# -----------------------------------------------------------------------------
# 1. Source Dependencies
# -----------------------------------------------------------------------------
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
# -----------------------------------------------------------------------------
# 2. SPICE Netlist Parser
# -----------------------------------------------------------------------------
#' Parses a simple text-based SPICE netlist into a named list of components
parse_spice_netlist <- function(netlist_text) {
  lines <- strsplit(trimws(netlist_text), "\n")[[1]]
  components <- list(R = 0, L = 0, C = 0, has_R = FALSE, has_L = FALSE, has_C = FALSE)
  
  for (line in lines) {
    line <- trimws(line)
    if (nchar(line) == 0 || startsWith(line, "*")) next # Skip empties and comments
    
    parts <- strsplit(line, "\\s+")[[1]]
    type <- toupper(substr(parts[1], 1, 1))
    
    # Format expected: [Name] [Node1] [Node2] [Value]
    if (type == "R") {
      components$R <- as.numeric(parts[4])
      components$has_R <- TRUE
    } else if (type == "L") {
      components$L <- as.numeric(parts[4])
      components$has_L <- TRUE
    } else if (type == "C") {
      components$C <- as.numeric(parts[4])
      components$has_C <- TRUE
    } else if (type == "V") {
      components$V_type <- parts[4] # e.g., "SIN"
    }
  }
  return(components)
}

# -----------------------------------------------------------------------------
# 3. CRN Netlist Compiler
# -----------------------------------------------------------------------------
#' Builds the appropriate CRN block based on detected components
build_crn_from_netlist <- function(parsed_netlist, circuit_name, rate) {
  
  # Setup initial conditions and parameters
  ic <- list(
    resistance = parsed_netlist$R,
    inductance = parsed_netlist$L,
    capacitance = parsed_netlist$C
  )
  
  # Define dual-rail I/O species definitions (Standardized for composite blocks)
  il <- list(voltage_positive = 'v_in_p', voltage_negative = 'v_in_n')
  ol <- list(
    current_positive = jn(circuit_name, '_i_p'),
    current_negative = jn(circuit_name, '_i_n'),
    voltage_positive = jn(circuit_name, '_v_p'),
    voltage_negative = jn(circuit_name, '_v_n')
  )
  
  # Topology Selection
  gates <- list()
  cat("Compiling CRN for topology: ")
  
  if (parsed_netlist$has_R && parsed_netlist$has_L && parsed_netlist$has_C) {
    cat("Series RLC\n")
    gates <- Make_Circuit_RLC(circuit_name, il, ol, ic, rate)
    
  } else if (parsed_netlist$has_R && parsed_netlist$has_L && !parsed_netlist$has_C) {
    cat("Series RL\n")
    gates <- Make_Circuit_RL(circuit_name, il, ol, ic, rate)
    
  } else if (parsed_netlist$has_R && !parsed_netlist$has_L && parsed_netlist$has_C) {
    cat("Series RC\n")
    gates <- Make_Circuit_RC(circuit_name, il, ol, ic, rate)
    
  } else if (!parsed_netlist$has_R && parsed_netlist$has_L && !parsed_netlist$has_C) {
    cat("Pure Inductor\n")
    gates <- Make_Circuit_Pure_Inductor(circuit_name, il, ol, ic, rate)
    
  } else if (!parsed_netlist$has_R && !parsed_netlist$has_L && parsed_netlist$has_C) {
    cat("Pure Capacitor\n")
    gates <- Make_Circuit_Pure_Capacitor(circuit_name, il, ol, ic, rate)
    
  } else {
    stop("Unsupported component combination or missing reactive element.")
  }
  
  return(list(gates = gates, input_nodes = il, output_nodes = ol))
}

# -----------------------------------------------------------------------------
# 4. Master Execution Demo
# -----------------------------------------------------------------------------
# Define a SPICE netlist (Series RLC)
spice_string <- "
* Demo Series RLC Circuit
V1 1 0 SIN
R1 1 2 12
L1 2 3 7
C1 3 0 2.5
"

rate <- 1
timing <- seq(0, 60, by = 0.001)

# 1. Parse
netlist <- parse_spice_netlist(spice_string)

# 2. Build CRN architecture
crn_model <- build_crn_from_netlist(netlist, "sys1", rate)

# 3. Initialize blank circuit and compile
circuit <- make_circuit(timing)
circuit <- circuit_add_compile_gates(circuit, crn_model$gates)

# 4. Setup Boundary Conditions (Dual-Rail Voltage Source)
# We map v_in to v_in_p and bind v_in_n to 0 for a purely positive input signal
forced_concentrations <- list(
  v_in_p = function(t) sinusoidal_input(t, offset=5),
  v_in_n = function(t) 0
)

# 5. Simulate via DeSolve
cat("Simulating CRN...\n")
behavior <- React_circuit(circuit, forced_concentrations = forced_concentrations, engine = 'desolve')

# 6. Post-Process Dual-Rail Signals to Real Values
v_in_real <- behavior[['v_in_p']] - behavior[['v_in_n']]
i_out_real <- behavior[[crn_model$output_nodes$current_positive]] - behavior[[crn_model$output_nodes$current_negative]]
behavior[['Real_Vin']] <- v_in_real
behavior[['Real_Iout']] <- i_out_real

# 7. Plot Results
title <- paste("SPICE to CRN Analog Simulation:", 
               "R=", netlist$R, "L=", netlist$L, "C=", netlist$C)
               
Plot_behavior(title = title, 
              behavior, 
              circuit, 
              species=c('Real_Vin', 'Real_Iout'), 
              normalize = FALSE)