# =============================================================================
# Example: CRN with Forced Sinusoidal Input
# =============================================================================
# This example demonstrates using forced concentrations with a sinusoidal input

# Load libraries (adjust path as needed)
rm(list = ls())

# Load only the necessary files for this example
source('R/crn_reactor.R')
source('R/parser.R')
source('R/io.R')

library(ggplot2)

# =============================================================================
# Define a sinusoidal forcing function
# =============================================================================
sinusoidal_forcing <- function(t, amplitude = 1e-6, frequency = 0.1) {
  # Return the derivative of: amplitude * sin(2*pi*frequency*t)
  # d/dt[amplitude * sin(2*pi*frequency*t)] = 
  #   amplitude * 2*pi*frequency * cos(2*pi*frequency*t)
  omega <- 2 * pi * frequency
  return(amplitude * omega * cos(omega * t))
}

# =============================================================================
# Define CRN: Simple integrator and differentiator
# =============================================================================
species <- c('SignalInput', 'Integrator', 'Differentiator')
ci <- c(5e-7, 0, 0)  # Start SignalInput at a baseline

# Reactions
reactions <- c(
  'SignalInput -> Integrator',      # Integrate the signal
  'SignalInput -> Differentiator',  # Differentiate the signal (approximation)
  'Differentiator -> 0'             # Differentiator output decays
)

ki <- c(
  0.2,    # Integration rate
  1.0,    # Differentiation rate (faster response)
  0.5     # Differentiator decay
)

# Time range
t <- seq(0, 50, length.out = 1000)

# =============================================================================
# Simulate with forced sinusoidal input
# =============================================================================
forced_conc <- list(
  SignalInput = function(t) sinusoidal_forcing(t, amplitude = 5e-7, frequency = 0.2)
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
  species = c('SignalInput', 'Integrator', 'Differentiator')
)

cat("Example completed successfully!\n")
cat("The sinusoidal input demonstrates integration and differentiation behavior.\n")
