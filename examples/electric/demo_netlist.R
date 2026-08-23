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
# ==============================================================================
# COMPOSITE ANALOG CRN CIRCUITS (STATE-SPACE FORMULATION)
# Append these to your R/ELECTRO_LIB.R file
# ==============================================================================

#' @title Make_Circuit_Series_RC
#' @description Implements a Series RC circuit using integrators.
#' Topology: V_R = V_in - V_C; i = V_R / R; dVc/dt = i / C; V_C = Integral(dVc/dt)
Make_Circuit_Series_RC <- function(name, species_input, species_output, ic, rate) {
  gates <- list()
  dummy_0 <- jn(name, '_dummy_0')
  
  vr_p <- jn(name, '_vr_p')
  vr_n <- jn(name, '_vr_n')
  
  # 1. Resistor Voltage Subtraction: V_R = V_in - V_C
  # Dual-rail subtraction: V_Rp = V_inp + V_Cn; V_Rn = V_inn + V_Cp
  gates[[length(gates) + 1]] <- Make_Add3In(
    jn(name, '_sub_vrp'), species_input$voltage_positive, species_output$voltage_negative, dummy_0, vr_p, 0, 0, 0, rate
  )
  gates[[length(gates) + 1]] <- Make_Add3In(
    jn(name, '_sub_vrn'), species_input$voltage_negative, species_output$voltage_positive, dummy_0, vr_n, 0, 0, 0, rate
  )
  
  # 2. Current Extraction: i = V_R * (1/R)
  gates[[length(gates) + 1]] <- Make_Mul2In_Wang(
    jn(name, '_mul_ip'), vr_p, jn(name, '_1oR'), species_output$current_positive, 0, 1 / ic$resistance, rate
  )
  gates[[length(gates) + 1]] <- Make_Mul2In_Wang(
    jn(name, '_mul_in'), vr_n, jn(name, '_1oR'), species_output$current_negative, 0, 1 / ic$resistance, rate
  )
  
  # 3. Capacitor Derivative: dVc/dt = i * (1/C)
  dvc_p <- jn(name, '_dvc_p')
  dvc_n <- jn(name, '_dvc_n')
  gates[[length(gates) + 1]] <- Make_Mul2In_Wang(
    jn(name, '_mul_dvcp'), species_output$current_positive, jn(name, '_1oC'), dvc_p, 0, 1 / ic$capacitance, rate
  )
  gates[[length(gates) + 1]] <- Make_Mul2In_Wang(
    jn(name, '_mul_dvcn'), species_output$current_negative, jn(name, '_1oC'), dvc_n, 0, 1 / ic$capacitance, rate
  )
  
  # 4. Capacitor Voltage Integration: V_C = Integral(dVc/dt)
  gates[[length(gates) + 1]] <- Make_Integrator_OishiYordanov(
    jn(name, '_int_vc'), dvc_p, dvc_n, species_output$voltage_positive, species_output$voltage_negative, 0, 0, rate
  )
  
  return(gates)
}


#' @title Make_Circuit_Series_RL
#' @description Implements a Series RL circuit using integrators.
#' Topology: V_R = i * R; V_L = V_in - V_R; di/dt = V_L / L; i = Integral(di/dt)
Make_Circuit_Series_RL <- function(name, species_input, species_output, ic, rate) {
  gates <- list()
  dummy_0 <- jn(name, '_dummy_0')
  
  vr_p <- jn(name, '_vr_p')
  vr_n <- jn(name, '_vr_n')
  
  # 1. Resistor Voltage: V_R = i * R
  gates[[length(gates) + 1]] <- Make_Mul2In_Wang(
    jn(name, '_mul_vrp'), species_output$current_positive, jn(name, '_R'), vr_p, 0, ic$resistance, rate
  )
  gates[[length(gates) + 1]] <- Make_Mul2In_Wang(
    jn(name, '_mul_vrn'), species_output$current_negative, jn(name, '_R'), vr_n, 0, ic$resistance, rate
  )
  
  # 2. Inductor Voltage Subtraction: V_L = V_in - V_R
  gates[[length(gates) + 1]] <- Make_Add3In(
    jn(name, '_add_vlp'), species_input$voltage_positive, vr_n, dummy_0, species_output$voltage_positive, 0, 0, 0, rate
  )
  gates[[length(gates) + 1]] <- Make_Add3In(
    jn(name, '_add_vln'), species_input$voltage_negative, vr_p, dummy_0, species_output$voltage_negative, 0, 0, 0, rate
  )
  
  # 3. Inductor Derivative: di/dt = V_L * (1/L)
  di_p <- jn(name, '_di_p')
  di_n <- jn(name, '_di_n')
  gates[[length(gates) + 1]] <- Make_Mul2In_Wang(
    jn(name, '_mul_dip'), species_output$voltage_positive, jn(name, '_1oL'), di_p, 0, 1 / ic$inductance, rate
  )
  gates[[length(gates) + 1]] <- Make_Mul2In_Wang(
    jn(name, '_mul_din'), species_output$voltage_negative, jn(name, '_1oL'), di_n, 0, 1 / ic$inductance, rate
  )
  
  # 4. Inductor Current Integration: i = Integral(di/dt)
  gates[[length(gates) + 1]] <- Make_Integrator_OishiYordanov(
    jn(name, '_int_i'), di_p, di_n, species_output$current_positive, species_output$current_negative, 0, 0, rate
  )
  
  return(gates)
}


#' @title Make_Circuit_Series_RLC
#' @description Implements a Series RLC circuit using multi-variable state-space integration.
#' States: Inductor Current (i) and Capacitor Voltage (V_C)
Make_Circuit_Series_RLC <- function(name, species_input, species_output, ic, rate) {
  gates <- list()
  dummy_0 <- jn(name, '_dummy_0')
  
  vr_p <- jn(name, '_vr_p')
  vr_n <- jn(name, '_vr_n')
  
  # 1. Resistor Voltage: V_R = i * R
  gates[[length(gates) + 1]] <- Make_Mul2In_Wang(
    jn(name, '_mul_vrp'), species_output$current_positive, jn(name, '_R'), vr_p, 0, ic$resistance, rate
  )
  gates[[length(gates) + 1]] <- Make_Mul2In_Wang(
    jn(name, '_mul_vrn'), species_output$current_negative, jn(name, '_R'), vr_n, 0, ic$resistance, rate
  )
  
  # 2. Intermediate Subtraction: (V_in - V_C)
  # In this block, species_output$voltage is mapped to Capacitor Voltage (V_C)
  vin_m_vc_p <- jn(name, '_vin_m_vc_p')
  vin_m_vc_n <- jn(name, '_vin_m_vc_n')
  
  gates[[length(gates) + 1]] <- Make_Add3In(
    jn(name, '_sub1_p'), species_input$voltage_positive, species_output$voltage_negative, dummy_0, vin_m_vc_p, 0, 0, 0, rate
  )
  gates[[length(gates) + 1]] <- Make_Add3In(
    jn(name, '_sub1_n'), species_input$voltage_negative, species_output$voltage_positive, dummy_0, vin_m_vc_n, 0, 0, 0, rate
  )
  
  # 3. Inductor Voltage: V_L = (V_in - V_C) - V_R
  vl_p <- jn(name, '_vl_p')
  vl_n <- jn(name, '_vl_n')
  
  gates[[length(gates) + 1]] <- Make_Add3In(
    jn(name, '_sub2_p'), vin_m_vc_p, vr_n, dummy_0, vl_p, 0, 0, 0, rate
  )
  gates[[length(gates) + 1]] <- Make_Add3In(
    jn(name, '_sub2_n'), vin_m_vc_n, vr_p, dummy_0, vl_n, 0, 0, 0, rate
  )
  
  # 4. Inductor Derivative: di/dt = V_L * (1/L)
  di_p <- jn(name, '_di_p')
  di_n <- jn(name, '_di_n')
  gates[[length(gates) + 1]] <- Make_Mul2In_Wang(
    jn(name, '_mul_dip'), vl_p, jn(name, '_1oL'), di_p, 0, 1 / ic$inductance, rate
  )
  gates[[length(gates) + 1]] <- Make_Mul2In_Wang(
    jn(name, '_mul_din'), vl_n, jn(name, '_1oL'), di_n, 0, 1 / ic$inductance, rate
  )
  
  # 5. Inductor Current Integration: i = Integral(di/dt)
  gates[[length(gates) + 1]] <- Make_Integrator_OishiYordanov(
    jn(name, '_int_i'), di_p, di_n, species_output$current_positive, species_output$current_negative, 0, 0, rate
  )
  
  # 6. Capacitor Derivative: dVc/dt = i * (1/C)
  dvc_p <- jn(name, '_dvc_p')
  dvc_n <- jn(name, '_dvc_n')
  gates[[length(gates) + 1]] <- Make_Mul2In_Wang(
    jn(name, '_mul_dvcp'), species_output$current_positive, jn(name, '_1oC'), dvc_p, 0, 1 / ic$capacitance, rate
  )
  gates[[length(gates) + 1]] <- Make_Mul2In_Wang(
    jn(name, '_mul_dvcn'), species_output$current_negative, jn(name, '_1oC'), dvc_n, 0, 1 / ic$capacitance, rate
  )
  
  # 7. Capacitor Voltage Integration: V_C = Integral(dVc/dt)
  gates[[length(gates) + 1]] <- Make_Integrator_OishiYordanov(
    jn(name, '_int_vc'), dvc_p, dvc_n, species_output$voltage_positive, species_output$voltage_negative, 0, 0, rate
  )
  
  return(gates)
}
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
    gates <- Make_Circuit_Series_RLC(circuit_name, il, ol, ic, rate)
    
  } else if (parsed_netlist$has_R && parsed_netlist$has_L && !parsed_netlist$has_C) {
    cat("Series RL\n")
    gates <- Make_Circuit_Series_RL(circuit_name, il, ol, ic, rate)
    
  } else if (parsed_netlist$has_R && !parsed_netlist$has_L && parsed_netlist$has_C) {
    cat("Series RC\n")
    gates <- Make_Circuit_Series_RC(circuit_name, il, ol, ic, rate)
    
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