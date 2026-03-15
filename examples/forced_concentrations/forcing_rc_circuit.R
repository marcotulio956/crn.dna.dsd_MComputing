# =============================================================================
# Practical Example: RC Circuit with Square Wave Input
# =============================================================================
# This example demonstrates using forced concentrations to simulate an RC circuit
# responding to a square wave voltage input
#
# Circuit: Voltage Source -> Resistor -> Capacitor
# The capacitor voltage responds to the square wave input

rm(list = ls())

# Load required libraries
source('R/crn_reactor.R')
source('R/parser.R')
source('R/io.R')

library(ggplot2)

# =============================================================================
# Define Square Wave Input Function
# =============================================================================
# This function creates a square wave that alternates between 0 and V_high

square_wave_voltage <- function(t, V_low = 0, V_high = 5e-6, period = 20) {
  # Calculate the phase within the period
  phase <- (t %% period) / period
  
  # During first half of period, ramp up; during second half, ramp down
  # This creates a square wave in the concentration
  if (phase < 0.5) {
    # Rising edge - positive rate to increase voltage
    return((V_high - V_low) / (period * 0.1))  # Fast rise (10% of half-period)
  } else if (phase < 0.5 + 0.1) {
    # At high level - no change
    return(0)
  } else if (phase < 0.5 + 0.1 + 0.1) {
    # Falling edge - negative rate to decrease voltage
    return(-(V_high - V_low) / (period * 0.1))  # Fast fall
  } else {
    # At low level - no change
    return(0)
  }
}

# =============================================================================
# Define RC Circuit CRN
# =============================================================================
# Simple model: Input voltage drives current through resistor,
# which charges/discharges capacitor

species <- c(
  'V_input',     # Input voltage (forced by square wave)
  'V_capacitor', # Voltage across capacitor
  'I_circuit'    # Current through circuit
)

ci <- c(0, 0, 0)  # Initial conditions

# Simplified reactions modeling RC circuit behavior:
# 1. V_input drives current: V_input -> I_circuit
# 2. Current charges capacitor: I_circuit -> V_capacitor
# 3. Capacitor discharges through resistor: V_capacitor -> 0
reactions <- c(
  'V_input -> I_circuit',
  'I_circuit -> V_capacitor',
  'V_capacitor -> 0'
)

# Rate constants (simplified RC time constants)
R <- 1000  # Resistance (arbitrary units for CRN)
C <- 0.01  # Capacitance (arbitrary units for CRN)
ki <- c(
  1/R,        # Rate: V_input -> I_circuit (1/R for Ohm's law approximation)
  1/C,        # Rate: I_circuit -> V_capacitor (1/C for capacitor charging)
  1/(R*C)     # Rate: V_capacitor -> 0 (discharge with RC time constant)
)

# Time range
t_end <- 100
t <- seq(0, t_end, length.out = 1000)

# =============================================================================
# Apply Forced Square Wave Input
# =============================================================================
forced_conc <- list(
  V_input = function(t) square_wave_voltage(t, V_low = 0, V_high = 5e-6, period = 30)
)

# =============================================================================
# Run Simulation
# =============================================================================
cat("Running RC circuit simulation with square wave input...\n")

behavior <- react(
  species = species,
  ci = ci,
  reactions = reactions,
  ki = ki,
  t = t,
  forced_concentrations = forced_conc
)

# =============================================================================
# Plot Results
# =============================================================================
cat("Plotting results...\n")

# Plot all signals
plot_behavior(
  behavior,
  x_label = 'Time (arbitrary units)',
  y_label = 'Voltage/Current (arbitrary units)',
  legend_name = 'Signal',
  species = c('V_input', 'V_capacitor', 'I_circuit')
)

# =============================================================================
# Analysis
# =============================================================================
cat("\n=============================================================================\n")
cat("RC Circuit Simulation with Square Wave Input\n")
cat("=============================================================================\n")
cat("\n")
cat("Expected behavior:\n")
cat("1. V_input: Square wave oscillating between 0 and V_high\n")
cat("2. V_capacitor: Exponential charging/discharging curves following the input\n")
cat("   - Charges when V_input is high\n")
cat("   - Discharges when V_input is low\n")
cat("   - Cannot reach full V_high before next cycle (RC time constant effect)\n")
cat("3. I_circuit: Spikes at transitions, representing charging/discharging currents\n")
cat("\n")
cat("This demonstrates how forced concentrations can model external inputs\n")
cat("like voltage sources in analog chemical computing circuits.\n")
cat("\n")

# Print sample values
cat("Sample values at different time points:\n")
cat("----------------------------------------\n")
idx_25 <- which.min(abs(behavior$time - 25))
cat(sprintf("At t=%.1f:\n", behavior$time[idx_25]))
cat(sprintf("  V_input: %.6e\n", behavior$V_input[idx_25]))
cat(sprintf("  V_capacitor: %.6e\n", behavior$V_capacitor[idx_25]))
cat(sprintf("  I_circuit: %.6e\n", behavior$I_circuit[idx_25]))
cat("\n")

idx_50 <- which.min(abs(behavior$time - 50))
cat(sprintf("At t=%.1f:\n", behavior$time[idx_50]))
cat(sprintf("  V_input: %.6e\n", behavior$V_input[idx_50]))
cat(sprintf("  V_capacitor: %.6e\n", behavior$V_capacitor[idx_50]))
cat(sprintf("  I_circuit: %.6e\n", behavior$I_circuit[idx_50]))
cat("\n")