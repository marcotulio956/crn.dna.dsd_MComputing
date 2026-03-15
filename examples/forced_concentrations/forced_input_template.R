# =============================================================================
# CRN Simulation Template with Forced Input Concentrations
# =============================================================================
# This template demonstrates how to create a CRN system with forced input
# concentrations using non-homogeneous functions (e.g., square waves, sine waves).
#
# Key concept: Instead of manually specifying input behaviors within reactions,
# you can define time-dependent forcing functions that directly control species
# concentrations during the ODE integration.
# =============================================================================

# Load required libraries
library(DNAr)  # or source the required R files if not using the package

# =============================================================================
# STEP 1: Define Input Forcing Functions
# =============================================================================
# These functions describe how forced species concentrations change over time.
# Each function takes time 't' as input and returns the forcing rate (dC/dt).

# Example 1: Square Wave Input
# Creates a square wave that oscillates between two values
square_wave_input <- function(t, amplitude = 1e-6, period = 10, offset = 0) {
  # Calculate which phase of the period we're in
  phase <- (t %% period) / period
  
  # Return amplitude if in first half of period, otherwise return -amplitude
  # to maintain concentration at offset level
  if (phase < 0.5) {
    return(amplitude / period)  # Rate to increase concentration
  } else {
    return(-amplitude / period)  # Rate to decrease concentration
  }
}

# Example 2: Sinusoidal Input
# Creates a smooth sinusoidal input signal
sinusoidal_input <- function(t, amplitude = 1e-6, frequency = 0.1, offset = 1e-6) {
  # Return the derivative of: offset + amplitude * sin(2*pi*frequency*t)
  # d/dt[offset + amplitude * sin(2*pi*frequency*t)] = 
  #   amplitude * 2*pi*frequency * cos(2*pi*frequency*t)
  omega <- 2 * pi * frequency
  return(amplitude * omega * cos(omega * t))
}

# Example 3: Step Input
# Creates a step function that turns on at a specific time
step_input <- function(t, step_time = 5, step_magnitude = 1e-6, rise_time = 0.1) {
  # Smooth step using a sigmoid-like function
  if (t < step_time) {
    return(0)
  } else if (t < step_time + rise_time) {
    # During rise time, provide positive rate
    return(step_magnitude / rise_time)
  } else {
    # After rise, maintain constant (rate = 0)
    return(0)
  }
}

# Example 4: Pulse Input
# Creates a pulse that turns on and off
pulse_input <- function(t, pulse_start = 10, pulse_width = 5, 
                       pulse_amplitude = 1e-6, rise_time = 0.1) {
  pulse_end <- pulse_start + pulse_width
  
  if (t < pulse_start) {
    return(0)
  } else if (t < pulse_start + rise_time) {
    # Rising edge
    return(pulse_amplitude / rise_time)
  } else if (t < pulse_end - rise_time) {
    # Flat top (maintain concentration)
    return(0)
  } else if (t < pulse_end) {
    # Falling edge
    return(-pulse_amplitude / rise_time)
  } else {
    return(0)
  }
}

# =============================================================================
# STEP 2: Define the CRN System (Gates/Reactions)
# =============================================================================
# Define your chemical reaction network as usual

# Example: Simple reaction system where input A drives production of B
species <- c('A', 'B', 'C')
ci <- c(0, 0, 0)  # Initial concentrations (A will be forced by input function)
reactions <- c(
  'A -> B',      # Input A drives production of B
  'B -> C',      # B converts to C
  'C -> 0'       # C degrades
)
ki <- c(
  0.1,           # Rate of A -> B
  0.05,          # Rate of B -> C  
  0.02           # Rate of C -> 0 (degradation)
)

# =============================================================================
# STEP 3: Specify Time Range
# =============================================================================
t_start <- 0
t_end <- 100
time_points <- seq(t_start, t_end, length.out = 1000)

# =============================================================================
# STEP 4: Define Forced Concentrations
# =============================================================================
# Create a named list where:
#   - Keys are species names that should be forced
#   - Values are functions that return the forcing rate at time t
#
# Note: These are RATE functions (dC/dt), not concentration functions

# Example: Force species 'A' with a sinusoidal input
forced_concentrations <- list(
  A = function(t) sinusoidal_input(t, amplitude = 5e-7, frequency = 0.1, offset = 1e-6)
)

# Alternative examples (uncomment to try):

# Example with square wave:
# forced_concentrations <- list(
#   A = function(t) square_wave_input(t, amplitude = 1e-6, period = 20)
# )

# Example with step input:
# forced_concentrations <- list(
#   A = function(t) step_input(t, step_time = 10, step_magnitude = 2e-6)
# )

# Example with pulse:
# forced_concentrations <- list(
#   A = function(t) pulse_input(t, pulse_start = 20, pulse_width = 10, pulse_amplitude = 1e-6)
# )

# Example with multiple forced species:
# forced_concentrations <- list(
#   A = function(t) sinusoidal_input(t, amplitude = 5e-7, frequency = 0.1),
#   B = function(t) step_input(t, step_time = 30, step_magnitude = 1e-6)
# )

# =============================================================================
# STEP 5: Run the Simulation
# =============================================================================
behavior <- react(
  species = species,
  ci = ci,
  reactions = reactions,
  ki = ki,
  t = time_points,
  forced_concentrations = forced_concentrations
)

# =============================================================================
# STEP 6: Visualize Results
# =============================================================================
plot_behavior(
  behavior,
  x_label = 'Time (s)',
  y_label = 'Concentration (M)',
  legend_name = 'Species'
)

# You can also plot specific species
plot_behavior(
  behavior,
  species = c('A', 'B', 'C'),
  x_label = 'Time (s)',
  y_label = 'Concentration (M)',
  legend_name = 'Species'
)

# =============================================================================
# NOTES AND TIPS
# =============================================================================
# 
# 1. Forcing Functions are RATES (dC/dt), not Concentrations:
#    The forcing functions should return the rate of change of concentration,
#    not the absolute concentration value. This is because they are added to
#    the ODE derivatives.
#
# 2. Initial Concentrations:
#    For forced species, you can set initial concentrations to 0 or any 
#    desired starting value. The forcing function will drive changes from there.
#
# 3. Complex Forcing Functions:
#    You can create arbitrarily complex forcing functions by combining
#    simple patterns or using conditional logic.
#
# 4. Performance:
#    Forcing functions are evaluated at each ODE integration step, so keep
#    them computationally efficient.
#
# 5. Physical Interpretation:
#    In a real chemical system, forced concentrations might represent:
#    - External addition of reactants (e.g., controlled pumping)
#    - Environmental changes (e.g., light, temperature affecting reaction rates)
#    - Input signals from other subsystems
#
# 6. Units:
#    Ensure your forcing function rates are compatible with your concentration
#    units (typically M/s for molar concentrations per second).
#
# =============================================================================
