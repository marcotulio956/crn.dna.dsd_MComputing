# =============================================================================
# Example: CRN with Forced Square Wave Input
# =============================================================================
# This example demonstrates using forced concentrations with a square wave input

# Load libraries (adjust path as needed)
rm(list = ls())

# Load only the necessary files for this example
source('R/crn_reactor.R')
source('R/parser.R')
source('R/io.R')

library(ggplot2)

# =============================================================================
# Define a square wave forcing function
# =============================================================================
square_wave_forcing <- function(t, amplitude = 1e-6, period = 10) {
  # This function returns the derivative (rate) needed to create a square wave
  # We'll use a simple approach: positive rate in first half, negative in second
  phase <- (t %% period) / period
  
  if (phase < 0.5) {
    # Rising phase - add concentration
    return(amplitude * 0.5 / (period * 0.25))
  } else {
    # Falling phase - remove concentration  
    return(-amplitude * 0.5 / (period * 0.25))
  }
}

# =============================================================================
# Define CRN: Input A drives cascading reactions
# =============================================================================
species <- c('Input', 'Output1', 'Output2')
ci <- c(0, 0, 0)  # Start with zero concentrations

# Reactions: Input drives Output1, which drives Output2
reactions <- c(
  'Input -> Output1',
  'Output1 -> Output2',
  'Output2 -> 0'  # Output2 degrades
)

ki <- c(
  0.5,    # Input -> Output1 rate
  0.3,    # Output1 -> Output2 rate
  0.1     # Output2 degradation rate
)

# Time range
t <- seq(0, 100, length.out = 500)

# =============================================================================
# Simulate with forced square wave input
# =============================================================================
forced_conc <- list(
  Input = function(t) square_wave_forcing(t, amplitude = 2e-6, period = 20)
)

behavior <- react(
  species = species,
  ci = ci,
  reactions = reactions,
  ki = ki,
  t = t,
  forced_concentrations = forced_conc
)

# =============================================================================
# Plot results
# =============================================================================
cat("Plotting results...\n")
plot_behavior(
  behavior,
  x_label = 'Time (s)',
  y_label = 'Concentration (M)',
  legend_name = 'Species',
  species = c('Input', 'Output1', 'Output2')
)
