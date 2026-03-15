# _____________________________________________________________________________
# Two-Tap Filter Demo with Phasing System
# Demonstrates the implementation of a two-tap FIR filter using molecular reactions
# Based on Jiang, Riedel, Parhi - Digital Signal Processing with Molecular Reactions
# _____________________________________________________________________________

# DESCRIPTION:
# This example demonstrates a two-tap Finite Impulse Response (FIR) filter
# implemented with chemical reaction networks (CRNs). The filter computes:
#   y[n] = a0*x[n] + a1*x[n-1]
# where:
#   - x[n] is the input signal at time n
#   - x[n-1] is the input signal delayed by one sample period
#   - a0, a1 are the filter coefficients (tap weights)
#   - y[n] is the filtered output
#
# The implementation includes:
# 1. Phasing system - coordinates sequential operations
# 2. Delay unit - implements the z^-1 delay operation
# 3. Coefficient multipliers - scale signals by tap weights
# 4. Adder - sums the weighted taps to produce output

# _____________________________________________________________________________
# Setup: Clean environment and load required libraries

rm(list = ls())

# Source all required DNAr library files
source('R/crn_reactor.R')
source('R/SIGNALS_N_SYSTEMS.R')
source('R/ANALOG_GATE_LIB.R')
source('R/analysis.R')
source('R/io.R')
source('R/parser.R')

# Load plotting library
library(ggplot2)

# Helper function for string concatenation
jn <- function(...) { paste(..., sep = '') }

# _____________________________________________________________________________
# FUNCTION: Create_Input_Signal
# DESCRIPTION: Creates a time-varying input signal for testing the filter
# The input is modeled as an external species with time-varying concentration

Create_Input_Signal <- function(t, signal_type = "pulse") {
  if (signal_type == "pulse") {
    # Step pulse: high from t=100 to t=300
    ifelse(t >= 100 & t <= 300, 5.0, 0.0)
  } else if (signal_type == "ramp") {
    # Linear ramp
    ifelse(t <= 200, t * 0.05, 10.0)
  } else {
    # Constant signal
    rep(2.0, length(t))
  }
}

# _____________________________________________________________________________
# FUNCTION: Build_TwoTap_Filter_Circuit
# DESCRIPTION: Assembles the complete two-tap filter circuit with all components

Build_TwoTap_Filter_Circuit <- function(coeff0 = 0.6, coeff1 = 0.4) {
  
  cat('\n=== Building Two-Tap Filter Circuit ===\n')
  cat(sprintf('Filter equation: y[n] = %.2f*x[n] + %.2f*x[n-1]\n', coeff0, coeff1))
  
  # Create the two-tap filter with specified coefficients
  filter <- Make_TwoTap_Filter(
    name = 'FIR_filter',
    nameInput = 'X_input',
    nameOutput = 'Y_output',
    coeff0 = coeff0,
    coeff1 = coeff1,
    crange = 10,
    rate = 1e-3
  )
  
  cat(sprintf('Created filter with %d species and %d reactions\n', 
              length(filter$species), length(filter$reactions)))
  
  return(filter)
}

# _____________________________________________________________________________
# FUNCTION: Simulate_Filter
# DESCRIPTION: Run the CRN simulation of the filter circuit

Simulate_Filter <- function(filter, t_end = 500, n_points = 200) {
  
  cat('\n=== Running Simulation ===\n')
  cat(sprintf('Time range: 0 to %d, Points: %d\n', t_end, n_points))
  
  # Define time vector
  t <- seq(0, t_end, length.out = n_points)
  
  # Simulate the reactions
  result <- react(
    species   = filter$species,  # Already a flat vector without duplicates
    ci        = filter$ci,
    reactions = filter$reactions,
    ki        = filter$ki,
    t         = t
  )
  
  cat('Simulation completed successfully\n')
  
  return(result)
}

# _____________________________________________________________________________
# FUNCTION: Plot_Filter_Results
# DESCRIPTION: Visualize the filter input, output, and intermediate signals

Plot_Filter_Results <- function(result, filter, show_phases = TRUE) {
  
  cat('\n=== Generating Plots ===\n')
  
  # Plot 1: Input and Output signals
  cat('Plot 1: Input and Output Signals\n')
  
  input_species <- filter$species_nested$input
  output_species <- filter$species_nested$output
  
  g1 <- plot_behavior(
    result, 
    species = c(input_species, output_species),
    x_label = 'Time (arbitrary units)',
    y_label = 'Concentration (M)',
    legend_name = 'Species',
    geom_list = c('line'),
    variable_line_type = FALSE
  ) + 
    ggtitle('Two-Tap Filter: Input and Output') +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  print(g1)
  
  # Plot 2: Phase signals (if requested)
  if (show_phases) {
    cat('Plot 2: Phasing System\n')
    
    phase1_species <- filter$species_nested$phase_gen$phase1
    phase2_species <- filter$species_nested$phase_gen$phase2
    
    if (!is.null(phase1_species) && !is.null(phase2_species)) {
      g2 <- plot_behavior(
        result,
        species = c(phase1_species, phase2_species),
        x_label = 'Time (arbitrary units)',
        y_label = 'Phase Activity',
        legend_name = 'Phase',
        geom_list = c('line'),
        variable_line_type = FALSE
      ) + 
        ggtitle('Phasing System Activity') +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      
      print(g2)
    }
  }
  
  # Plot 3: Intermediate signals (delayed input and weighted taps)
  cat('Plot 3: Intermediate Signals\n')
  
  delayed_species <- filter$components$delay$species$output
  tap0_species <- filter$components$mult0$species$output
  tap1_species <- filter$components$mult1$species$output
  
  intermediate_species <- c()
  if (!is.null(delayed_species)) intermediate_species <- c(intermediate_species, delayed_species)
  if (!is.null(tap0_species)) intermediate_species <- c(intermediate_species, tap0_species)
  if (!is.null(tap1_species)) intermediate_species <- c(intermediate_species, tap1_species)
  
  if (length(intermediate_species) > 0) {
    g3 <- plot_behavior(
      result,
      species = intermediate_species,
      x_label = 'Time (arbitrary units)',
      y_label = 'Concentration (M)',
      legend_name = 'Signal',
      geom_list = c('line'),
      variable_line_type = FALSE
    ) + 
      ggtitle('Intermediate Signals: Delay and Weighted Taps') +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
    print(g3)
  }
  
  cat('Plotting completed\n')
}

# _____________________________________________________________________________
# FUNCTION: Verify_Filter_Response
# DESCRIPTION: Validate the filter output against expected behavior

Verify_Filter_Response <- function(result, filter) {
  
  cat('\n=== Verifying Filter Response ===\n')
  
  input_species <- filter$species_nested$input
  output_species <- filter$species_nested$output
  
  # Extract concentration trajectories
  input_conc <- result[[input_species]]
  output_conc <- result[[output_species]]
  
  # Basic checks
  cat(sprintf('Input signal range: [%.4f, %.4f]\n', min(input_conc), max(input_conc)))
  cat(sprintf('Output signal range: [%.4f, %.4f]\n', min(output_conc), max(output_conc)))
  
  # Check if output has reasonable values (should be scaled version of input)
  output_max <- max(output_conc)
  input_max <- max(input_conc)
  
  if (input_max > 0) {
    ratio <- output_max / input_max
    expected_ratio <- filter$coeff0 + filter$coeff1
    # Note: Tolerance of 30% is generous to account for transient dynamics
    # Future enhancement: Add separate steady-state verification
    tolerance <- 0.3 * expected_ratio
    cat(sprintf('Output/Input max ratio: %.4f (expected ~%.4f)\n', ratio, expected_ratio))
    
    if (abs(ratio - expected_ratio) < tolerance) {
      cat('✓ Filter response is within expected range\n')
      return(TRUE)
    } else {
      cat('⚠ Filter response differs from expected (may need longer simulation time)\n')
      return(FALSE)
    }
  } else {
    cat('⚠ Input signal is zero, cannot validate response\n')
    return(FALSE)
  }
}

# _____________________________________________________________________________
# FUNCTION: Save_Results
# DESCRIPTION: Save simulation results to CSV file for further analysis

Save_Results <- function(result, filename = 'two_tap_filter_results.csv') {
  
  cat('\n=== Saving Results ===\n')
  
  filepath <- file.path('examples/two_tap_filter', filename)
  save_behavior_csv(result, filepath)
  
  cat(sprintf('Results saved to: %s\n', filepath))
}

# _____________________________________________________________________________
##################################### MAIN #####################################
# _____________________________________________________________________________

cat('\n')
cat('==================================================================\n')
cat('  TWO-TAP FILTER WITH PHASING SYSTEM DEMONSTRATION               \n')
cat('  Implementation of Digital Signal Processing with Molecular     \n')
cat('  Reactions (Jiang, Riedel, Parhi, 2013)                        \n')
cat('==================================================================\n')

# Step 1: Build the filter circuit
# Using coefficients: a0 = 0.6, a1 = 0.4 (moving average filter)
filter_circuit <- Build_TwoTap_Filter_Circuit(coeff0 = 0.6, coeff1 = 0.4)

# Step 2: Run the simulation
# Simulate for 500 time units with 200 data points
simulation_result <- Simulate_Filter(filter_circuit, t_end = 500, n_points = 200)

# Step 3: Visualize the results
# Generate plots showing input, output, phases, and intermediate signals
Plot_Filter_Results(simulation_result, filter_circuit, show_phases = TRUE)

# Step 4: Verify the filter is working correctly
verification_passed <- Verify_Filter_Response(simulation_result, filter_circuit)

# Step 5: Save results for future reference
# Save_Results(simulation_result)  # Uncomment to save results

cat('\n')
cat('==================================================================\n')
cat('  DEMONSTRATION COMPLETE                                          \n')
if (verification_passed) {
  cat('  ✓ Filter verification: PASSED                                   \n')
} else {
  cat('  ⚠ Filter verification: See notes above                         \n')
}
cat('==================================================================\n')
cat('\n')

# _____________________________________________________________________________
# USAGE NOTES:
# _____________________________________________________________________________
#
# To run this example:
#   1. Ensure you are in the root directory of the DNAr package
#   2. Run: source('examples/two_tap_filter/two_tap_filter_demo.R')
#
# To modify the filter:
#   - Change coeff0 and coeff1 in Build_TwoTap_Filter_Circuit() call
#   - Adjust simulation time and resolution in Simulate_Filter() call
#
# To test different input signals:
#   - Modify the Create_Input_Signal() function (currently not used for CRN
#     simulation but can be integrated for time-varying inputs)
#
# Understanding the output:
#   - First plot shows input X_input and output Y_output
#   - Second plot shows the two phases alternating
#   - Third plot shows delayed signal and weighted tap outputs
#   - The output should be a weighted combination of current and delayed input
#
# Filter coefficients:
#   - a0 = 0.6, a1 = 0.4 implements a simple moving average
#   - For low-pass filter: use positive coefficients that sum to ~1
#   - For high-pass filter: use alternating signs (e.g., a0=1, a1=-1)
#
# _____________________________________________________________________________
