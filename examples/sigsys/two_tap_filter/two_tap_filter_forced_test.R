# =============================================================================
# Two-Tap Filter Test Bench with Forced Input Functions
# =============================================================================
# This example demonstrates testing a two-tap FIR filter using forced input
# concentrations with various signal types (square wave, sinusoidal, step, etc.)
#
# The two-tap filter implements: y[n] = a0*x[n] + a1*x[n-1]
# Reference: Jiang, Riedel, Parhi (2013) 
# "Digital Signal Processing with Molecular Reactions"
# =============================================================================

# Clean environment
rm(list = ls())

# Load required DNAr library files
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

# =============================================================================
# Input Signal Forcing Functions
# =============================================================================
# These functions return forcing rates (dC/dt) for different input signals

# Square Wave Input - alternates between high and low values
square_wave_forcing <- function(t, amplitude = 2e-6, period = 50, offset = 1e-6) {
  phase <- (t %% period) / period
  # Generate square wave by computing derivative
  # Use smoothed transitions to avoid numerical issues
  transition_width <- period * 0.05  # 5% of period for transitions
  
  if (phase < 0.5 - transition_width/period) {
    return(0)  # Maintain high level
  } else if (phase < 0.5 + transition_width/period) {
    return(-amplitude / transition_width)  # Falling edge
  } else if (phase < 1.0 - transition_width/period) {
    return(0)  # Maintain low level
  } else {
    return(amplitude / transition_width)  # Rising edge
  }
}

# Sinusoidal Input - smooth periodic signal
sinusoidal_forcing <- function(t, amplitude = 1.5e-6, frequency = 0.05, offset = 1e-6) {
  omega <- 2 * pi * frequency
  # Return derivative of: offset + amplitude * sin(omega * t)
  return(amplitude * omega * cos(omega * t))
}

# Step Input - sudden change at specific time
step_forcing <- function(t, step_time = 100, step_magnitude = 2e-6, rise_time = 10) {
  if (t < step_time) {
    return(0)
  } else if (t < step_time + rise_time) {
    return(step_magnitude / rise_time)  # Rising edge
  } else {
    return(0)  # Maintain constant level
  }
}

# Pulse Input - rectangular pulse
pulse_forcing <- function(t, pulse_start = 100, pulse_width = 50, 
                         pulse_amplitude = 2e-6, rise_time = 5) {
  pulse_end <- pulse_start + pulse_width
  
  if (t < pulse_start) {
    return(0)
  } else if (t < pulse_start + rise_time) {
    return(pulse_amplitude / rise_time)  # Rising edge
  } else if (t < pulse_end - rise_time) {
    return(0)  # Flat top
  } else if (t < pulse_end) {
    return(-pulse_amplitude / rise_time)  # Falling edge
  } else {
    return(0)
  }
}

# Mixed signal - combination of two frequencies
mixed_signal_forcing <- function(t, amp1 = 1e-6, freq1 = 0.02, 
                                amp2 = 0.5e-6, freq2 = 0.1) {
  omega1 <- 2 * pi * freq1
  omega2 <- 2 * pi * freq2
  return(amp1 * omega1 * cos(omega1 * t) + amp2 * omega2 * cos(omega2 * t))
}

# =============================================================================
# Test Function: Run Filter with Specified Input
# =============================================================================
run_filter_test <- function(filter, input_forcing_func, test_name, 
                           t_end = 500, n_points = 500) {
  
  cat('\n=============================================================================\n')
  cat(sprintf('TEST: %s\n', test_name))
  cat('=============================================================================\n')
  
  # Define time vector
  t <- seq(0, t_end, length.out = n_points)
  
  # Set up forced concentrations for the input
  forced_conc <- list()
  forced_conc[[filter$species_nested$input]] <- input_forcing_func
  
  cat(sprintf('Simulating filter from t=0 to t=%d with %d points...\n', t_end, n_points))
  
  # Run simulation with forced input
  behavior <- react(
    species = filter$species,
    ci = filter$ci,
    reactions = filter$reactions,
    ki = filter$ki,
    t = t,
    forced_concentrations = forced_conc
  )
  
  cat('Simulation completed.\n')
  
  return(behavior)
}

# =============================================================================
# Plotting Function: Visualize Filter Input and Output
# =============================================================================
plot_filter_response <- function(behavior, filter, test_name, show_phases = FALSE) {
  
  input_species <- filter$species_nested$input
  output_species <- filter$species_nested$output
  
  # Plot 1: Input and Output Comparison
  g1 <- plot_behavior(
    behavior,
    species = c(input_species, output_species),
    x_label = 'Time (arbitrary units)',
    y_label = 'Concentration (M)',
    legend_name = 'Species',
    geom_list = c('line'),
    variable_line_type = FALSE
  ) +
    ggtitle(sprintf('%s: Input vs Output', test_name)) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position = "bottom"
    )
  
  print(g1)
  
  # Plot 2: Phase signals (optional)
  if (show_phases) {
    phase1_species <- filter$species_nested$phase_gen$phase1
    phase2_species <- filter$species_nested$phase_gen$phase2
    
    if (!is.null(phase1_species) && !is.null(phase2_species)) {
      g2 <- plot_behavior(
        behavior,
        species = c(phase1_species, phase2_species),
        x_label = 'Time (arbitrary units)',
        y_label = 'Phase Activity',
        legend_name = 'Phase',
        geom_list = c('line'),
        variable_line_type = FALSE
      ) +
        ggtitle(sprintf('%s: Phase Coordination', test_name)) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.position = "bottom"
        )
      
      print(g2)
    }
  }
  
  # Plot 3: Intermediate signals (delayed input and weighted taps)
  delayed_species <- filter$components$delay$species$output
  tap0_species <- filter$components$mult0$species$output
  tap1_species <- filter$components$mult1$species$output
  
  intermediate_species <- c()
  if (!is.null(delayed_species)) intermediate_species <- c(intermediate_species, delayed_species)
  if (!is.null(tap0_species)) intermediate_species <- c(intermediate_species, tap0_species)
  if (!is.null(tap1_species)) intermediate_species <- c(intermediate_species, tap1_species)
  
  if (length(intermediate_species) > 0) {
    g3 <- plot_behavior(
      behavior,
      species = intermediate_species,
      x_label = 'Time (arbitrary units)',
      y_label = 'Concentration (M)',
      legend_name = 'Signal',
      geom_list = c('line'),
      variable_line_type = FALSE
    ) +
      ggtitle(sprintf('%s: Intermediate Signals', test_name)) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom"
      )
    
    print(g3)
  }
}

# =============================================================================
# Frequency Response Analysis
# =============================================================================
analyze_frequency_response <- function(filter, frequencies = c(0.02, 0.05, 0.1, 0.2),
                                      t_end = 500, n_points = 500) {
  
  cat('\n=============================================================================\n')
  cat('FREQUENCY RESPONSE ANALYSIS\n')
  cat('=============================================================================\n')
  
  results <- list()
  
  for (freq in frequencies) {
    cat(sprintf('\nTesting frequency: %.3f Hz\n', freq))
    
    # Create sinusoidal forcing function for this frequency
    forcing_func <- function(t) {
      sinusoidal_forcing(t, amplitude = 1.5e-6, frequency = freq, offset = 1e-6)
    }
    
    # Run test
    test_name <- sprintf('Sine Wave %.3f Hz', freq)
    behavior <- run_filter_test(filter, forcing_func, test_name, t_end, n_points)
    
    # Calculate gain (output amplitude / input amplitude)
    # Use second half of signal to avoid transients
    t <- seq(0, t_end, length.out = n_points)
    start_idx <- floor(n_points / 2)
    
    input_vals <- behavior[[filter$species_nested$input]][start_idx:n_points]
    output_vals <- behavior[[filter$species_nested$output]][start_idx:n_points]
    
    input_range <- max(input_vals) - min(input_vals)
    output_range <- max(output_vals) - min(output_vals)
    
    gain <- if (input_range > 0) output_range / input_range else 0
    gain_db <- 20 * log10(gain + 1e-10)  # Add small value to avoid log(0)
    
    cat(sprintf('  Input range: %.6f M\n', input_range))
    cat(sprintf('  Output range: %.6f M\n', output_range))
    cat(sprintf('  Gain: %.4f (%.2f dB)\n', gain, gain_db))
    
    results[[as.character(freq)]] <- list(
      frequency = freq,
      gain = gain,
      gain_db = gain_db,
      behavior = behavior
    )
  }
  
  # Plot frequency response
  freq_vec <- sapply(results, function(r) r$frequency)
  gain_db_vec <- sapply(results, function(r) r$gain_db)
  
  df <- data.frame(
    Frequency = freq_vec,
    Gain_dB = gain_db_vec
  )
  
  g <- ggplot(df, aes(x = Frequency, y = Gain_dB)) +
    geom_line(color = 'blue', linewidth = 1) +
    geom_point(color = 'red', size = 3) +
    labs(
      title = 'Two-Tap Filter Frequency Response',
      x = 'Frequency (Hz)',
      y = 'Gain (dB)'
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
    )
  
  print(g)
  
  return(results)
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

cat('\n')
cat('==================================================================\n')
cat('  TWO-TAP FILTER WITH FORCED INPUT TESTING                        \n')
cat('  Demonstrates DSP filtering with various input signal types      \n')
cat('==================================================================\n')

# Create two-tap filter
# Using moving average coefficients: a0 = 0.5, a1 = 0.5
# This provides simple low-pass filtering
cat('\nBuilding Two-Tap Filter (Moving Average)...\n')
cat('Filter equation: y[n] = 0.5*x[n] + 0.5*x[n-1]\n')

filter_circuit <- Make_TwoTap_Filter(
  name = 'FIR_filter',
  nameInput = 'X_input',
  nameOutput = 'Y_output',
  coeff0 = 0.5,
  coeff1 = 0.5,
  crange = 10,
  rate = 1
)

cat(sprintf('Filter created with %d species and %d reactions\n', 
            length(filter_circuit$species), 
            length(filter_circuit$reactions)))

# =============================================================================
# Test 1: Square Wave Input
# =============================================================================
test1_behavior <- run_filter_test(
  filter_circuit,
  function(t) square_wave_forcing(t, amplitude = 2e-6, period = 100),
  'Square Wave Input',
  t_end = 500,
  n_points = 500
)

plot_filter_response(test1_behavior, filter_circuit, 'Square Wave Test', show_phases = FALSE)

# =============================================================================
# Test 2: Sinusoidal Input (Low Frequency)
# =============================================================================
test2_behavior <- run_filter_test(
  filter_circuit,
  function(t) sinusoidal_forcing(t, amplitude = 1.5e-6, frequency = 0.05),
  'Sinusoidal Input (Low Frequency)',
  t_end = 500,
  n_points = 500
)

plot_filter_response(test2_behavior, filter_circuit, 'Low Frequency Sine Test')

# =============================================================================
# Test 3: Step Function Input
# =============================================================================
test3_behavior <- run_filter_test(
  filter_circuit,
  function(t) step_forcing(t, step_time = 150, step_magnitude = 2e-6, rise_time = 10),
  'Step Function Input',
  t_end = 500,
  n_points = 500
)

plot_filter_response(test3_behavior, filter_circuit, 'Step Input Test')

# =============================================================================
# Test 4: Pulse Input
# =============================================================================
test4_behavior <- run_filter_test(
  filter_circuit,
  function(t) pulse_forcing(t, pulse_start = 100, pulse_width = 100, 
                           pulse_amplitude = 2e-6, rise_time = 10),
  'Pulse Input',
  t_end = 500,
  n_points = 500
)

plot_filter_response(test4_behavior, filter_circuit, 'Pulse Input Test')

# =============================================================================
# Test 5: Mixed Signal (Multiple Frequencies)
# =============================================================================
test5_behavior <- run_filter_test(
  filter_circuit,
  function(t) mixed_signal_forcing(t, amp1 = 1e-6, freq1 = 0.02, 
                                  amp2 = 0.5e-6, freq2 = 0.15),
  'Mixed Signal (Two Frequencies)',
  t_end = 500,
  n_points = 500
)

plot_filter_response(test5_behavior, filter_circuit, 'Mixed Signal Test')

# =============================================================================
# Test 6: Frequency Response Analysis
# =============================================================================
freq_response <- analyze_frequency_response(
  filter_circuit,
  frequencies = c(0.01, 0.03, 0.05, 0.1, 0.15, 0.2),
  t_end = 500,
  n_points = 500
)

# =============================================================================
# Summary
# =============================================================================

cat('\n')
cat('==================================================================\n')
cat('  TWO-TAP FILTER TESTING COMPLETE                                 \n')
cat('==================================================================\n')
cat('\nKey Observations:\n')
cat('1. Moving average filter (a0=0.5, a1=0.5) smooths input signals\n')
cat('2. Filter attenuates high-frequency components (low-pass behavior)\n')
cat('3. Step response shows smooth transition with one sample delay\n')
cat('4. Frequency response shows decreasing gain at higher frequencies\n')
cat('\nFilter Characteristics:\n')
cat(sprintf('  - Coefficients: a0=%.2f, a1=%.2f\n', 
            filter_circuit$coeff0, filter_circuit$coeff1))
cat('  - Type: FIR (Finite Impulse Response)\n')
cat('  - Order: First-order (two taps)\n')
cat('  - Behavior: Low-pass (averaging)\n')
cat('\n')
cat('==================================================================\n')

# =============================================================================
# USAGE NOTES
# =============================================================================
#
# To run this test bench:
#   source('examples/two_tap_filter_forced_test.R')
#
# To test different filter configurations:
#   - Modify coeff0 and coeff1 in Make_TwoTap_Filter() call
#   - Example: coeff0=1.0, coeff1=-1.0 creates a differencing filter (high-pass)
#   - Example: coeff0=0.75, coeff1=0.25 creates weighted average
#
# To add custom input signals:
#   - Define new forcing function (must return dC/dt, not absolute concentration)
#   - Call run_filter_test() with your forcing function
#   - Use plot_filter_response() to visualize results
#
# Understanding forced concentrations:
#   - Forcing functions are called at each ODE integration step
#   - They return the rate of change (derivative) of concentration
#   - This allows external control of input species independent of CRN reactions
#   - Useful for testing filters with known input signals
#
# =============================================================================
