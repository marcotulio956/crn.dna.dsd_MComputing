# Load the libraries
# DO NOT USE library(DNAr)
# DO NOT USE library(DNArLogic)
# DO NOT USE library(DNArAnalog)

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
source('R/forced_concentrations.R')
source('R/parser.R')
source('R/util_functions.R')
source('R/metric_functions.R')

jn <- function(...) { paste(..., sep = '') }

source("examples/pipeline/PIPELINE_LIB.R")


Make_SquareWave_Clock <- function(name, amplitude = 10, period = 20, start_high = FALSE) {
  
  # Fast reaction rate for sharp vertical edges
  k_edge <- 100 
  
  out_sp <- jn(name, "_out")
  i_sp   <- jn(name, "_I")
  
  # Set initial conditions based on whether we start High or Low
  # If High: Out = amplitude, I = 0
  # If Low:  Out = 0, I = amplitude
  initial_cond <- if (start_high) c(amplitude, 0) else c(0, amplitude)
  
  # 1. Define the LOW-driving stage
  Stage_DriveLow <- list(
    species   = c(out_sp, i_sp),
    reactions = c(jn(out_sp, ' -> ', i_sp)),
    ci        = initial_cond, 
    ki        = c(k_edge)
  )
  
  # 2. Define the HIGH-driving stage
  Stage_DriveHigh <- list(
    species   = c(out_sp, i_sp),
    reactions = c(jn(i_sp, ' -> ', out_sp)),
    ci        = initial_cond, 
    ki        = c(k_edge)
  )
  
  # 3. Map the two logical states to the 4 underlying clock phases
  # This guarantees a perfect 50% duty cycle while satisfying the N >= 3 rule.
  if (start_high) {
    stages <- list(Stage_DriveHigh, Stage_DriveHigh, Stage_DriveLow, Stage_DriveLow)
  } else {
    stages <- list(Stage_DriveLow, Stage_DriveLow, Stage_DriveHigh, Stage_DriveHigh)
  }
  
  # Tune oscillator speed
  estimated_rate <- 5 / period 
  
  # 4. Build the pipeline
  circuit <- Make_Pipeline(
    stages                   = stages,
    phase_names              = c(jn(name,"_P1"), jn(name,"_P2"), jn(name,"_P3"), jn(name,"_P4")),
    oscillator_rate          = estimated_rate,
    oscillator_total         = 150,
    oscillator_dominant_frac = 0.9,
    clock_name               = jn(name, "_Clk")
  )
  
  circuit$name <- name
  
  return(circuit)
}

Make_SquareWave_Clock <- function(name, amplitude = 10, period = 20, start_high = FALSE) {
  
  # Fast reaction rate for sharp vertical edges
  k_edge <- 100 
  
  out_sp <- jn(name, "_out")
  i_sp   <- jn(name, "_I")
  
  # Set initial conditions based on whether we start High or Low
  # If High: Out = amplitude, I = 0
  # If Low:  Out = 0, I = amplitude
  initial_cond <- if (start_high) c(amplitude, 0) else c(0, amplitude)
  
  # 1. Define the LOW-driving stage
  Stage_DriveLow <- list(
    species   = c(out_sp, i_sp),
    reactions = c(jn(out_sp, ' -> ', i_sp)),
    ci        = initial_cond, 
    ki        = c(k_edge)
  )
  
  # 2. Define the HIGH-driving stage
  Stage_DriveHigh <- list(
    species   = c(out_sp, i_sp),
    reactions = c(jn(i_sp, ' -> ', out_sp)),
    ci        = initial_cond, 
    ki        = c(k_edge)
  )
  
  # 3. Map the two logical states to the 4 underlying clock phases
  # This guarantees a perfect 50% duty cycle while satisfying the N >= 3 rule.
  if (start_high) {
    stages <- list(Stage_DriveHigh, Stage_DriveHigh, Stage_DriveLow, Stage_DriveLow)
  } else {
    stages <- list(Stage_DriveLow, Stage_DriveLow, Stage_DriveHigh, Stage_DriveHigh)
  }
  
  # Tune oscillator speed
  estimated_rate <- 5 / period 
  
  # 4. Build the pipeline
  circuit <- Make_Pipeline(
    stages                   = stages,
    phase_names              = c(jn(name,"_P1"), jn(name,"_P2"), jn(name,"_P3"), jn(name,"_P4")),
    oscillator_rate          = estimated_rate,
    oscillator_total         = 150,
    oscillator_dominant_frac = 0.9,
    clock_name               = jn(name, "_Clk")
  )
  
  circuit$name <- name
  
  return(circuit)
}


Make_Generic <- function(timing) {
  circuit <- make_circuit(timing)
  
  g_dalchau_sin <- Make_Oscillator_Dalchau('osc', 'y', 'z', 'sin', 9, 8, 5, 10e-2)  
  
  # FIXED: Match the period of 10 from 'ref_step' and set an appropriate amplitude. 
  # Assuming 'square_input' generates a 0-to-1 signal, we use amplitude=1. 
  # If square_input generates 0-to-10, change amplitude to 10.
  g_step <- Make_SquareWave_Clock('step', amplitude = 10, period = 200)
  
  # add2circuit
  circuit <- circuit_add_gate(circuit, g_dalchau_sin)
  circuit <- circuit_add_gate(circuit, g_step)
  
  return(circuit)
}


t0 = 0
t1 = 40
points = (t1 - t0) * t1 
timing  <- seq(t0, t1, length.out = points) 
circuit <- Make_Generic(timing)

behavior <- React_circuit(circuit,engine="desolve")

behavior[['ref_sin']] <- simulate_sin(timing)
behavior[['ref_step']] <- square_input(timing, period = 10, pulse_width = 10/2, delay = 0)

Plot_behavior_(behavior, circuit, title = "Stimuli in DSD", 
               species = c('step_out', 'sin'), 
               species_dotted = c('ref_sin', 'ref_step'))
