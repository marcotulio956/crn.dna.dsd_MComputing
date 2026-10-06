# =============================================================================
# Biquad (Second-Order IIR) Filter Test Bench
# =============================================================================
# This example demonstrates testing a biquad IIR filter with various
# configurations (low-pass, high-pass, band-pass) using forced input functions.
#
# Transfer function: H(z) = (b0 + b1*z^-1 + b2*z^-2) / (1 + a1*z^-1 + a2*z^-2)
#
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

# Sinusoidal Input
sinusoidal_forcing <- function(t, amplitude = 1.5e-6, frequency = 0.05, offset = 1e-6) {
  omega <- 2 * pi * frequency
  return(amplitude * omega * cos(omega * t))
}

# Square Wave Input
square_wave_forcing <- function(t, amplitude = 2e-6, period = 100, offset = 1e-6) {
  phase <- (t %% period) / period
  transition_width <- period * 0.05
  
  if (phase < 0.5 - transition_width/period) {
    return(0)
  } else if (phase < 0.5 + transition_width/period) {
    return(-amplitude / transition_width)
  } else if (phase < 1.0 - transition_width/period) {
    return(0)
  } else {
    return(amplitude / transition_width)
  }
}

# Impulse-like Input (narrow pulse)
impulse_forcing <- function(t, impulse_time = 100, impulse_magnitude = 5e-6, 
                           impulse_width = 5) {
  if (t >= impulse_time && t < impulse_time + impulse_width/2) {
    return(impulse_magnitude / (impulse_width/2))  # Rising edge
  } else if (t >= impulse_time + impulse_width/2 && t < impulse_time + impulse_width) {
    return(-impulse_magnitude / (impulse_width/2))  # Falling edge
  } else {
    return(0)
  }
}

# Multi-frequency test signal
multi_frequency_forcing <- function(t, low_amp = 1e-6, low_freq = 0.02,
                                   mid_amp = 0.8e-6, mid_freq = 0.08,
                                   high_amp = 0.5e-6, high_freq = 0.15) {
  omega_low <- 2 * pi * low_freq
  omega_mid <- 2 * pi * mid_freq
  omega_high <- 2 * pi * high_freq
  
  return(low_amp * omega_low * cos(omega_low * t) +
         mid_amp * omega_mid * cos(omega_mid * t) +
         high_amp * omega_high * cos(omega_high * t))
}

# =============================================================================
# Biquad Filter Coefficient Presets
# =============================================================================
# These coefficients are designed for a sampling frequency normalized to 1.0
# Adjust frequencies accordingly when interpreting results

get_lowpass_butterworth <- function() {
  # Low-pass Butterworth filter
  # Cutoff frequency ≈ 0.1 * sampling frequency
  list(
    name = 'Low-Pass Butterworth',
    b0 = 0.0201, b1 = 0.0402, b2 = 0.0201,
    a1 = -1.5610, a2 = 0.6414
  )
}

get_highpass_butterworth <- function() {
  # High-pass Butterworth filter
  # Cutoff frequency ≈ 0.1 * sampling frequency
  list(
    name = 'High-Pass Butterworth',
    b0 = 0.8008, b1 = -1.6016, b2 = 0.8008,
    a1 = -1.5610, a2 = 0.6414
  )
}

get_bandpass <- function() {
  # Band-pass filter
  # Center frequency ≈ 0.15 * sampling frequency
  # Bandwidth ≈ 0.1 * sampling frequency
  list(
    name = 'Band-Pass',
    b0 = 0.0318, b1 = 0, b2 = -0.0318,
    a1 = -1.5610, a2 = 0.6364
  )
}

get_notch <- function() {
  # Notch filter (band-reject)
  # Notch frequency ≈ 0.15 * sampling frequency
  list(
    name = 'Notch Filter',
    b0 = 0.9682, b1 = -1.5610, b2 = 0.9682,
    a1 = -1.5610, a2 = 0.9364
  )
}

# =============================================================================
# Test Function: Run Biquad Filter with Specified Input
# =============================================================================
run_biquad_test <- function(filter, input_forcing_func, test_name,
                           t_end = 600, n_points = 600) {
  
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
# Plotting Function: Visualize Biquad Filter Response
# =============================================================================
plot_biquad_response <- function(behavior, filter, test_name, show_internal = FALSE) {
  
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
  
  # Plot 2: Internal state variables (w[n], w[n-1], w[n-2])
  if (show_internal) {
    w_current <- filter$species_nested$w_current
    w_z1 <- filter$species_nested$w_z1
    w_z2 <- filter$species_nested$w_z2
    
    if (!is.null(w_current) && !is.null(w_z1) && !is.null(w_z2)) {
      g2 <- plot_behavior(
        behavior,
        species = c(w_current, w_z1, w_z2),
        x_label = 'Time (arbitrary units)',
        y_label = 'Concentration (M)',
        legend_name = 'State',
        geom_list = c('line'),
        variable_line_type = FALSE
      ) +
        ggtitle(sprintf('%s: Internal State Variables', test_name)) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.position = "bottom"
        )
      
      print(g2)
    }
  }
}

# =============================================================================
# Frequency Response Analysis for Biquad Filters
# =============================================================================
analyze_biquad_frequency_response <- function(filter, filter_type,
                                             frequencies = c(0.02, 0.05, 0.1, 0.15, 0.2, 0.25),
                                             t_end = 600, n_points = 600) {
  
  cat('\n=============================================================================\n')
  cat(sprintf('FREQUENCY RESPONSE ANALYSIS: %s\n', filter_type))
  cat('=============================================================================\n')
  
  results <- list()
  
  for (freq in frequencies) {
    cat(sprintf('\nTesting frequency: %.3f Hz\n', freq))
    
    # Create sinusoidal forcing function for this frequency
    forcing_func <- function(t) {
      sinusoidal_forcing(t, amplitude = 1.5e-6, frequency = freq, offset = 1e-6)
    }
    
    # Run test
    test_name <- sprintf('%s - Sine %.3f Hz', filter_type, freq)
    behavior <- run_biquad_test(filter, forcing_func, test_name, t_end, n_points)
    
    # Calculate gain (use second half to avoid transients)
    t <- seq(0, t_end, length.out = n_points)
    start_idx <- floor(n_points * 2 / 3)  # Use last third for steady-state
    
    input_vals <- behavior[[filter$species_nested$input]][start_idx:n_points]
    output_vals <- behavior[[filter$species_nested$output]][start_idx:n_points]
    
    input_range <- max(input_vals) - min(input_vals)
    output_range <- max(output_vals) - min(output_vals)
    
    gain <- if (input_range > 0) output_range / input_range else 0
    gain_db <- 20 * log10(gain + 1e-10)
    
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
      title = sprintf('%s: Frequency Response', filter_type),
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
# Compare Multiple Filter Types
# =============================================================================
compare_filter_types <- function(filter_configs, input_forcing_func,
                                test_name, t_end = 600, n_points = 600) {
  
  cat('\n=============================================================================\n')
  cat(sprintf('COMPARING FILTER TYPES: %s\n', test_name))
  cat('=============================================================================\n')
  
  all_behaviors <- list()
  
  for (config in filter_configs) {
    cat(sprintf('\nTesting %s...\n', config$name))
    
    # Create filter
    filter <- Make_Biquad_Filter(
      name = gsub(' ', '_', config$name),
      nameInput = 'X_input',
      nameOutput = 'Y_output',
      b0 = config$b0, b1 = config$b1, b2 = config$b2,
      a1 = config$a1, a2 = config$a2,
      crange = 10,
      rate = 1e-3
    )
    
    # Run simulation
    behavior <- run_biquad_test(filter, input_forcing_func, 
                               sprintf('%s - %s', config$name, test_name),
                               t_end, n_points)
    
    all_behaviors[[config$name]] <- list(
      config = config,
      filter = filter,
      behavior = behavior
    )
  }
  
  # Create comparison plot
  plot_data <- data.frame()
  
  for (name in names(all_behaviors)) {
    filter <- all_behaviors[[name]]$filter
    behavior <- all_behaviors[[name]]$behavior
    
    input_species <- filter$species_nested$input
    output_species <- filter$species_nested$output
    
    # Extract time and output
    time <- behavior$time
    output <- behavior[[output_species]]
    
    temp_df <- data.frame(
      Time = time,
      Concentration = output,
      Filter = name
    )
    
    plot_data <- rbind(plot_data, temp_df)
  }
  
  # Also add input signal
  first_filter <- all_behaviors[[1]]$filter
  first_behavior <- all_behaviors[[1]]$behavior
  input_species <- first_filter$species_nested$input
  
  input_df <- data.frame(
    Time = first_behavior$time,
    Concentration = first_behavior[[input_species]],
    Filter = 'Input'
  )
  
  plot_data <- rbind(plot_data, input_df)
  
  g <- ggplot(plot_data, aes(x = Time, y = Concentration, color = Filter, linetype = Filter)) +
    geom_line(linewidth = 0.8) +
    labs(
      title = sprintf('Filter Comparison: %s', test_name),
      x = 'Time (arbitrary units)',
      y = 'Concentration (M)'
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position = "bottom"
    ) +
    scale_linetype_manual(values = c('Input' = 'dashed', 
                                     'Low-Pass Butterworth' = 'solid',
                                     'High-Pass Butterworth' = 'solid',
                                     'Band-Pass' = 'solid',
                                     'Notch Filter' = 'solid'))
  
  print(g)
  
  return(all_behaviors)
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

cat('\n')
cat('==================================================================\n')
cat('  BIQUAD (SECOND-ORDER IIR) FILTER TEST BENCH                     \n')
cat('  Demonstrates various filter types with forced inputs            \n')
cat('==================================================================\n')

# =============================================================================
# TEST SET 1: Low-Pass Butterworth Filter
# =============================================================================

cat('\n\n')
cat('###################################################################\n')
cat('  TEST SET 1: LOW-PASS BUTTERWORTH FILTER                          \n')
cat('###################################################################\n')

lpf_config <- get_lowpass_butterworth()
cat(sprintf('\nBuilding %s...\n', lpf_config$name))
cat(sprintf('Coefficients: b0=%.4f, b1=%.4f, b2=%.4f, a1=%.4f, a2=%.4f\n',
            lpf_config$b0, lpf_config$b1, lpf_config$b2,
            lpf_config$a1, lpf_config$a2))

lpf_filter <- Make_Biquad_Filter(
  name = 'LPF',
  nameInput = 'X_input',
  nameOutput = 'Y_output',
  b0 = lpf_config$b0, b1 = lpf_config$b1, b2 = lpf_config$b2,
  a1 = lpf_config$a1, a2 = lpf_config$a2,
  crange = 10,
  rate = 1e-3
)

cat(sprintf('Filter created with %d species and %d reactions\n',
            length(lpf_filter$species),
            length(lpf_filter$reactions)))

# Test 1.1: Low frequency sine (should pass)
lpf_test1 <- run_biquad_test(
  lpf_filter,
  function(t) sinusoidal_forcing(t, amplitude = 1.5e-6, frequency = 0.03),
  'LPF - Low Frequency Sine (should pass)',
  t_end = 600, n_points = 600
)
plot_biquad_response(lpf_test1, lpf_filter, 'LPF - Low Freq Sine')

# Test 1.2: High frequency sine (should attenuate)
lpf_test2 <- run_biquad_test(
  lpf_filter,
  function(t) sinusoidal_forcing(t, amplitude = 1.5e-6, frequency = 0.2),
  'LPF - High Frequency Sine (should attenuate)',
  t_end = 600, n_points = 600
)
plot_biquad_response(lpf_test2, lpf_filter, 'LPF - High Freq Sine')

# Test 1.3: Square wave (contains multiple harmonics)
lpf_test3 <- run_biquad_test(
  lpf_filter,
  function(t) square_wave_forcing(t, amplitude = 2e-6, period = 120),
  'LPF - Square Wave (harmonic filtering)',
  t_end = 600, n_points = 600
)
plot_biquad_response(lpf_test3, lpf_filter, 'LPF - Square Wave')

# Test 1.4: Impulse response
lpf_test4 <- run_biquad_test(
  lpf_filter,
  function(t) impulse_forcing(t, impulse_time = 150, impulse_magnitude = 5e-6),
  'LPF - Impulse Response',
  t_end = 600, n_points = 600
)
plot_biquad_response(lpf_test4, lpf_filter, 'LPF - Impulse', show_internal = TRUE)

# Frequency response analysis for LPF
lpf_freq_response <- analyze_biquad_frequency_response(
  lpf_filter,
  'Low-Pass Butterworth',
  frequencies = c(0.02, 0.05, 0.08, 0.1, 0.15, 0.2, 0.25),
  t_end = 600, n_points = 600
)

# =============================================================================
# TEST SET 2: High-Pass Butterworth Filter
# =============================================================================

cat('\n\n')
cat('###################################################################\n')
cat('  TEST SET 2: HIGH-PASS BUTTERWORTH FILTER                         \n')
cat('###################################################################\n')

hpf_config <- get_highpass_butterworth()
cat(sprintf('\nBuilding %s...\n', hpf_config$name))
cat(sprintf('Coefficients: b0=%.4f, b1=%.4f, b2=%.4f, a1=%.4f, a2=%.4f\n',
            hpf_config$b0, hpf_config$b1, hpf_config$b2,
            hpf_config$a1, hpf_config$a2))

hpf_filter <- Make_Biquad_Filter(
  name = 'HPF',
  nameInput = 'X_input',
  nameOutput = 'Y_output',
  b0 = hpf_config$b0, b1 = hpf_config$b1, b2 = hpf_config$b2,
  a1 = hpf_config$a1, a2 = hpf_config$a2,
  crange = 10,
  rate = 1e-3
)

# Test 2.1: Low frequency sine (should attenuate)
hpf_test1 <- run_biquad_test(
  hpf_filter,
  function(t) sinusoidal_forcing(t, amplitude = 1.5e-6, frequency = 0.03),
  'HPF - Low Frequency Sine (should attenuate)',
  t_end = 600, n_points = 600
)
plot_biquad_response(hpf_test1, hpf_filter, 'HPF - Low Freq Sine')

# Test 2.2: High frequency sine (should pass)
hpf_test2 <- run_biquad_test(
  hpf_filter,
  function(t) sinusoidal_forcing(t, amplitude = 1.5e-6, frequency = 0.2),
  'HPF - High Frequency Sine (should pass)',
  t_end = 600, n_points = 600
)
plot_biquad_response(hpf_test2, hpf_filter, 'HPF - High Freq Sine')

# Frequency response analysis for HPF
hpf_freq_response <- analyze_biquad_frequency_response(
  hpf_filter,
  'High-Pass Butterworth',
  frequencies = c(0.02, 0.05, 0.08, 0.1, 0.15, 0.2, 0.25),
  t_end = 600, n_points = 600
)

# =============================================================================
# TEST SET 3: Band-Pass Filter
# =============================================================================

cat('\n\n')
cat('###################################################################\n')
cat('  TEST SET 3: BAND-PASS FILTER                                     \n')
cat('###################################################################\n')

bpf_config <- get_bandpass()
cat(sprintf('\nBuilding %s...\n', bpf_config$name))
cat(sprintf('Coefficients: b0=%.4f, b1=%.4f, b2=%.4f, a1=%.4f, a2=%.4f\n',
            bpf_config$b0, bpf_config$b1, bpf_config$b2,
            bpf_config$a1, bpf_config$a2))

bpf_filter <- Make_Biquad_Filter(
  name = 'BPF',
  nameInput = 'X_input',
  nameOutput = 'Y_output',
  b0 = bpf_config$b0, b1 = bpf_config$b1, b2 = bpf_config$b2,
  a1 = bpf_config$a1, a2 = bpf_config$a2,
  crange = 10,
  rate = 1e-3
)

# Test 3.1: Center frequency (should pass)
bpf_test1 <- run_biquad_test(
  bpf_filter,
  function(t) sinusoidal_forcing(t, amplitude = 1.5e-6, frequency = 0.15),
  'BPF - Center Frequency (should pass)',
  t_end = 600, n_points = 600
)
plot_biquad_response(bpf_test1, bpf_filter, 'BPF - Center Freq')

# Test 3.2: Low frequency (should attenuate)
bpf_test2 <- run_biquad_test(
  bpf_filter,
  function(t) sinusoidal_forcing(t, amplitude = 1.5e-6, frequency = 0.03),
  'BPF - Low Frequency (should attenuate)',
  t_end = 600, n_points = 600
)
plot_biquad_response(bpf_test2, bpf_filter, 'BPF - Low Freq')

# Test 3.3: High frequency (should attenuate)
bpf_test3 <- run_biquad_test(
  bpf_filter,
  function(t) sinusoidal_forcing(t, amplitude = 1.5e-6, frequency = 0.25),
  'BPF - High Frequency (should attenuate)',
  t_end = 600, n_points = 600
)
plot_biquad_response(bpf_test3, bpf_filter, 'BPF - High Freq')

# Frequency response analysis for BPF
bpf_freq_response <- analyze_biquad_frequency_response(
  bpf_filter,
  'Band-Pass',
  frequencies = c(0.05, 0.08, 0.1, 0.12, 0.15, 0.18, 0.2, 0.25),
  t_end = 600, n_points = 600
)

# =============================================================================
# TEST SET 4: Notch Filter
# =============================================================================

cat('\n\n')
cat('###################################################################\n')
cat('  TEST SET 4: NOTCH FILTER (BAND-REJECT)                          \n')
cat('###################################################################\n')

notch_config <- get_notch()
cat(sprintf('\nBuilding %s...\n', notch_config$name))
cat(sprintf('Coefficients: b0=%.4f, b1=%.4f, b2=%.4f, a1=%.4f, a2=%.4f\n',
            notch_config$b0, notch_config$b1, notch_config$b2,
            notch_config$a1, notch_config$a2))

notch_filter <- Make_Biquad_Filter(
  name = 'Notch',
  nameInput = 'X_input',
  nameOutput = 'Y_output',
  b0 = notch_config$b0, b1 = notch_config$b1, b2 = notch_config$b2,
  a1 = notch_config$a1, a2 = notch_config$a2,
  crange = 10,
  rate = 1e-3
)

# Test 4.1: Notch frequency (should attenuate)
notch_test1 <- run_biquad_test(
  notch_filter,
  function(t) sinusoidal_forcing(t, amplitude = 1.5e-6, frequency = 0.15),
  'Notch - Notch Frequency (should attenuate)',
  t_end = 600, n_points = 600
)
plot_biquad_response(notch_test1, notch_filter, 'Notch - Notch Freq')

# Test 4.2: Away from notch frequency (should pass)
notch_test2 <- run_biquad_test(
  notch_filter,
  function(t) sinusoidal_forcing(t, amplitude = 1.5e-6, frequency = 0.05),
  'Notch - Off-notch Frequency (should pass)',
  t_end = 600, n_points = 600
)
plot_biquad_response(notch_test2, notch_filter, 'Notch - Off-notch Freq')

# Frequency response analysis for Notch
notch_freq_response <- analyze_biquad_frequency_response(
  notch_filter,
  'Notch Filter',
  frequencies = c(0.05, 0.08, 0.1, 0.12, 0.15, 0.18, 0.2, 0.25),
  t_end = 600, n_points = 600
)

# =============================================================================
# TEST SET 5: Multi-Frequency Input Comparison
# =============================================================================

cat('\n\n')
cat('###################################################################\n')
cat('  TEST SET 5: FILTER COMPARISON WITH MULTI-FREQUENCY INPUT        \n')
cat('###################################################################\n')

filter_configs <- list(
  get_lowpass_butterworth(),
  get_highpass_butterworth(),
  get_bandpass(),
  get_notch()
)

comparison_results <- compare_filter_types(
  filter_configs,
  function(t) multi_frequency_forcing(t, low_amp = 1e-6, low_freq = 0.03,
                                     mid_amp = 0.8e-6, mid_freq = 0.1,
                                     high_amp = 0.6e-6, high_freq = 0.2),
  'Multi-Frequency Input',
  t_end = 600, n_points = 600
)

# =============================================================================
# Summary
# =============================================================================

cat('\n')
cat('==================================================================\n')
cat('  BIQUAD FILTER TESTING COMPLETE                                  \n')
cat('==================================================================\n')
cat('\nKey Observations:\n')
cat('1. Low-Pass Filter:\n')
cat('   - Passes low frequencies, attenuates high frequencies\n')
cat('   - Smooths square waves by removing high-frequency harmonics\n')
cat('   - Impulse response shows characteristic decay\n')
cat('\n2. High-Pass Filter:\n')
cat('   - Attenuates low frequencies, passes high frequencies\n')
cat('   - Emphasizes rapid changes in input signal\n')
cat('   - Inverted frequency response compared to low-pass\n')
cat('\n3. Band-Pass Filter:\n')
cat('   - Passes frequencies around center frequency\n')
cat('   - Attenuates both low and high frequencies\n')
cat('   - Useful for isolating specific frequency components\n')
cat('\n4. Notch Filter:\n')
cat('   - Attenuates specific frequency (notch frequency)\n')
cat('   - Passes all other frequencies\n')
cat('   - Useful for removing interference at known frequency\n')
cat('\n5. Second-Order Characteristics:\n')
cat('   - Sharper roll-off than first-order filters\n')
cat('   - Can exhibit resonance near cutoff frequency\n')
cat('   - More complex transient response due to poles\n')
cat('\n')
cat('==================================================================\n')

# =============================================================================
# USAGE NOTES
# =============================================================================
#
# To run this test bench:
#   source('examples/biquad_filter_test.R')
#
# To design custom biquad filters:
#   1. Calculate coefficients using filter design tools (MATLAB, Python scipy)
#   2. Use bilinear transform or impulse invariance method
#   3. Ensure stability: poles must be inside unit circle
#   4. Create filter with Make_Biquad_Filter() using calculated coefficients
#
# Understanding filter coefficients:
#   - b0, b1, b2: Feedforward (numerator) coefficients
#   - a1, a2: Feedback (denominator) coefficients (note: a0 is normalized to 1)
#   - Negative a1, a2 typically indicate stable filter
#   - Coefficient magnitudes affect frequency response shape
#
# Molecular implementation notes:
#   - Feedback requires careful handling of negative coefficients
#   - Stability depends on reaction rate balancing
#   - Internal state (w[n]) represents filter memory
#   - Phase generator coordinates sequential operations
#
# Performance tips:
#   - Use longer simulation times for accurate frequency response
#   - Avoid very high Q factors (narrow bandwidth) - may be unstable in CRN
#   - Test stability with impulse input before applying to real signals
#
# =============================================================================
