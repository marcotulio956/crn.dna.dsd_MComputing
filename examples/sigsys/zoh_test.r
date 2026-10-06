# =============================================================================
# Zero-Order Hold (ZOH) Test Bench with Forced Input
# =============================================================================
# This example demonstrates testing a zero-order hold using forced input
# concentrations with various continuous signals.
#
# Zero-Order Hold (ZOH) is a signal reconstruction method where sampled values
# are held constant between sampling instants. This is the most common
# approach used in digital-to-analog converters (DACs).
#
# The ZOH uses a three-phase pipeline:
#   1. READ phase: Sample the input signal
#   2. HOLD phase: Maintain the sampled value (state stabilization)
#   3. WRITE phase: Output the held value
#
# Reference: Standard digital signal processing textbooks
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
# Continuous Input Signal Forcing Functions
# =============================================================================
# These functions represent continuous-time signals that will be sampled
# by the zero-order hold

# Sinusoidal Continuous Signal
sinusoidal_continuous <- function(t, amplitude = 2e-6, frequency = 0.08, offset = 1e-6) {
  omega <- 2 * pi * frequency
  # Return derivative for forcing function
  return(amplitude * omega * cos(omega * t))
}

# Ramp Signal
ramp_continuous <- function(t, slope = 2e-8, offset = 0) {
  # Constant slope = constant derivative
  return(slope)
}

# Chirp Signal (frequency sweep)
chirp_continuous <- function(t, amplitude = 2e-6, f_start = 0.02, f_end = 0.15) {
  # Linear chirp: frequency increases linearly with time
  # Instantaneous frequency: f(t) = f_start + (f_end - f_start) * t / t_max
  # For simplicity, use t_max = 500
  t_max <- 500
  k <- (f_end - f_start) / t_max  # Chirp rate
  f_t <- f_start + k * t
  omega_t <- 2 * pi * f_t
  
  # Phase: integral of 2*pi*f(t) = 2*pi*(f_start*t + k*t^2/2)
  phase <- 2 * pi * (f_start * t + k * t^2 / 2)
  
  # Derivative of amplitude * sin(phase)
  return(amplitude * omega_t * cos(phase))
}

# Square Wave Continuous
square_wave_continuous <- function(t, amplitude = 2e-6, period = 100, offset = 1e-6) {
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

# Step Function
step_continuous <- function(t, step_time = 200, step_magnitude = 2e-6, rise_time = 10) {
  if (t < step_time) {
    return(0)
  } else if (t < step_time + rise_time) {
    return(step_magnitude / rise_time)
  } else {
    return(0)
  }
}

# =============================================================================
# Test Function: Run ZOH with Specified Continuous Input
# =============================================================================
run_zoh_test <- function(zoh, input_forcing_func, test_name,
                        t_end = 600, n_points = 1000) {
  
  cat('\n=============================================================================\n')
  cat(sprintf('TEST: %s\n', test_name))
  cat('=============================================================================\n')
  
  # Define time vector (high resolution to see continuous signal)
  t <- seq(0, t_end, length.out = n_points)
  
  # Set up forced concentrations for the input
  forced_conc <- list()
  forced_conc[[zoh$species_nested$input]] <- input_forcing_func
  
  cat(sprintf('Simulating ZOH from t=0 to t=%d with %d points...\n', t_end, n_points))
  cat(sprintf('Sample period: %d time units\n', zoh$sample_period))
  
  # Run simulation with forced input
  behavior <- react(
    species = zoh$species,
    ci = zoh$ci,
    reactions = zoh$reactions,
    ki = zoh$ki,
    t = t,
    forced_concentrations = forced_conc
  )
  
  cat('Simulation completed.\n')
  
  return(behavior)
}

# =============================================================================
# Plotting Function: Visualize ZOH Behavior
# =============================================================================
plot_zoh_response <- function(behavior, zoh, test_name, show_phases = TRUE) {
  
  input_species <- zoh$species_nested$input
  output_species <- zoh$species_nested$output
  storage_species <- zoh$species_nested$storage
  
  # Plot 1: Input (continuous) vs Output (sampled and held)
  g1 <- plot_behavior(
    behavior,
    species = c(input_species, output_species),
    x_label = 'Time (arbitrary units)',
    y_label = 'Concentration (M)',
    legend_name = 'Signal',
    geom_list = c('line'),
    variable_line_type = FALSE
  ) +
    ggtitle(sprintf('%s: Continuous Input vs ZOH Output', test_name)) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position = "bottom"
    )
  
  print(g1)
  
  # Plot 2: Input, Storage, and Output (to show pipeline stages)
  g2 <- plot_behavior(
    behavior,
    species = c(input_species, storage_species, output_species),
    x_label = 'Time (arbitrary units)',
    y_label = 'Concentration (M)',
    legend_name = 'Signal',
    geom_list = c('line'),
    variable_line_type = FALSE
  ) +
    ggtitle(sprintf('%s: Pipeline Stages (Input -> Storage -> Output)', test_name)) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      legend.position = "bottom"
    )
  
  print(g2)
  
  # Plot 3: Phase signals (READ, HOLD, WRITE)
  if (show_phases) {
    phase_read <- zoh$species_nested$phase_read
    phase_hold <- zoh$species_nested$phase_hold
    phase_write <- zoh$species_nested$phase_write
    
    if (!is.null(phase_read) && !is.null(phase_hold) && !is.null(phase_write)) {
      g3 <- plot_behavior(
        behavior,
        species = c(phase_read, phase_hold, phase_write),
        x_label = 'Time (arbitrary units)',
        y_label = 'Phase Activity',
        legend_name = 'Phase',
        geom_list = c('line'),
        variable_line_type = FALSE
      ) +
        ggtitle(sprintf('%s: Three-Phase Pipeline Control', test_name)) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.position = "bottom"
        )
      
      print(g3)
    }
  }
  
  # Plot 4: Zoomed view to see staircase effect clearly
  # Focus on middle portion
  t_start_zoom <- nrow(behavior) * 0.3
  t_end_zoom <- nrow(behavior) * 0.5
  
  behavior_zoom <- behavior[t_start_zoom:t_end_zoom, ]
  
  g4 <- plot_behavior(
    behavior_zoom,
    species = c(input_species, output_species),
    x_label = 'Time (arbitrary units)',
    y_label = 'Concentration (M)',
    legend_name = 'Signal',
    geom_list = c('line'),
    variable_line_type = FALSE
  ) +
    ggtitle(sprintf('%s: Zoomed View - Staircase Effect', test_name)) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      legend.position = "bottom"
    )
  
  print(g4)
}

# =============================================================================
# Analysis Function: Compare Different Sample Rates
# =============================================================================
compare_sample_rates <- function(input_forcing_func, test_name,
                                sample_periods = c(30, 60, 120),
                                t_end = 600, n_points = 1000) {
  
  cat('\n=============================================================================\n')
  cat(sprintf('SAMPLE RATE COMPARISON: %s\n', test_name))
  cat('=============================================================================\n')
  
  all_behaviors <- list()
  
  for (sp in sample_periods) {
    cat(sprintf('\nTesting with sample period: %d\n', sp))
    
    # Create ZOH with this sample period
    zoh <- Make_ZeroOrderHold(
      name = sprintf('ZOH_T%d', sp),
      nameInput = 'X_continuous',
      nameOutput = 'X_discrete',
      sample_period = sp,
      cinput = 0,
      crange = 10,
      rate = 1e-3
    )
    
    # Run test
    behavior <- run_zoh_test(zoh, input_forcing_func,
                            sprintf('Sample Period = %d', sp),
                            t_end, n_points)
    
    all_behaviors[[as.character(sp)]] <- list(
      zoh = zoh,
      behavior = behavior,
      sample_period = sp
    )
  }
  
  # Create comparison plot
  plot_data <- data.frame()
  
  # Add input signal (same for all)
  first_zoh <- all_behaviors[[1]]$zoh
  first_behavior <- all_behaviors[[1]]$behavior
  input_species <- first_zoh$species_nested$input
  
  input_df <- data.frame(
    Time = first_behavior$time,
    Concentration = first_behavior[[input_species]],
    Config = 'Continuous Input'
  )
  
  plot_data <- rbind(plot_data, input_df)
  
  # Add outputs from different sample rates
  for (sp_str in names(all_behaviors)) {
    zoh <- all_behaviors[[sp_str]]$zoh
    behavior <- all_behaviors[[sp_str]]$behavior
    output_species <- zoh$species_nested$output
    
    output_df <- data.frame(
      Time = behavior$time,
      Concentration = behavior[[output_species]],
      Config = sprintf('ZOH (T=%s)', sp_str)
    )
    
    plot_data <- rbind(plot_data, output_df)
  }
  
  g <- ggplot(plot_data, aes(x = Time, y = Concentration, 
                              color = Config, linetype = Config)) +
    geom_line(linewidth = 0.7) +
    labs(
      title = sprintf('Sample Rate Comparison: %s', test_name),
      x = 'Time (arbitrary units)',
      y = 'Concentration (M)'
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position = "bottom"
    ) +
    scale_linetype_manual(values = c('Continuous Input' = 'solid',
                                     'ZOH (T=30)' = 'dashed',
                                     'ZOH (T=60)' = 'dotted',
                                     'ZOH (T=120)' = 'dotdash'))
  
  print(g)
  
  return(all_behaviors)
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

cat('\n')
cat('==================================================================\n')
cat('  ZERO-ORDER HOLD (ZOH) WITH FORCED INPUT TESTING                 \n')
cat('  Demonstrates sample-and-hold with three-phase pipeline          \n')
cat('==================================================================\n')

# =============================================================================
# TEST SET 1: ZOH with Sinusoidal Input
# =============================================================================

cat('\n\n')
cat('###################################################################\n')
cat('  TEST SET 1: ZOH WITH SINUSOIDAL INPUT                            \n')
cat('###################################################################\n')

cat('\nBuilding Zero-Order Hold...\n')
cat('Sample period: 50 time units\n')
cat('Pipeline: READ -> HOLD -> WRITE\n')

zoh1 <- Make_ZeroOrderHold(
  name = 'ZOH_sine',
  nameInput = 'X_continuous',
  nameOutput = 'X_discrete',
  sample_period = 10,
  cinput = 0,
  crange = 10,
  rate = 1e4
)

cat(sprintf('ZOH created with %d species and %d reactions\n',
            length(zoh1$species),
            length(zoh1$reactions)))

# Test 1.1: Low frequency sine wave
test1_behavior <- run_zoh_test(
  zoh1,
  function(t) sinusoidal_continuous(t, amplitude = 10, frequency = 0.002),
  'Sinusoidal Input (Low Frequency)',
  t_end = 6000, n_points = 3000
)

plot_zoh_response(test1_behavior, zoh1, 'Sine Wave (Low Freq)', show_phases = FALSE)

exit

# Test 1.2: Higher frequency sine wave
test2_behavior <- run_zoh_test(
  zoh1,
  function(t) sinusoidal_continuous(t, amplitude = 2e-6, frequency = 0.12),
  'Sinusoidal Input (Higher Frequency)',
  t_end = 600, n_points = 1000
)

plot_zoh_response(test2_behavior, zoh1, 'Sine Wave (High Freq)', show_phases = FALSE)



# =============================================================================
# TEST SET 2: ZOH with Ramp Input
# =============================================================================

cat('\n\n')
cat('###################################################################\n')
cat('  TEST SET 2: ZOH WITH RAMP INPUT                                  \n')
cat('###################################################################\n')

test3_behavior <- run_zoh_test(
  zoh1,
  function(t) ramp_continuous(t, slope = 3e-8),
  'Ramp Input',
  t_end = 600, n_points = 1000
)

plot_zoh_response(test3_behavior, zoh1, 'Ramp Signal', show_phases = FALSE)

# =============================================================================
# TEST SET 3: ZOH with Square Wave Input
# =============================================================================

cat('\n\n')
cat('###################################################################\n')
cat('  TEST SET 3: ZOH WITH SQUARE WAVE INPUT                           \n')
cat('###################################################################\n')

test4_behavior <- run_zoh_test(
  zoh1,
  function(t) square_wave_continuous(t, amplitude = 2e-6, period = 150),
  'Square Wave Input',
  t_end = 600, n_points = 1000
)

plot_zoh_response(test4_behavior, zoh1, 'Square Wave', show_phases = FALSE)

# =============================================================================
# TEST SET 4: ZOH with Step Input
# =============================================================================

cat('\n\n')
cat('###################################################################\n')
cat('  TEST SET 4: ZOH WITH STEP INPUT                                  \n')
cat('###################################################################\n')

test5_behavior <- run_zoh_test(
  zoh1,
  function(t) step_continuous(t, step_time = 200, step_magnitude = 2.5e-6, rise_time = 10),
  'Step Input',
  t_end = 600, n_points = 1000
)

plot_zoh_response(test5_behavior, zoh1, 'Step Function', show_phases = FALSE)

# =============================================================================
# TEST SET 5: ZOH with Chirp Signal (Frequency Sweep)
# =============================================================================

cat('\n\n')
cat('###################################################################\n')
cat('  TEST SET 5: ZOH WITH CHIRP SIGNAL                                \n')
cat('###################################################################\n')

test6_behavior <- run_zoh_test(
  zoh1,
  function(t) chirp_continuous(t, amplitude = 2e-6, f_start = 0.02, f_end = 0.15),
  'Chirp Signal (Frequency Sweep)',
  t_end = 600, n_points = 1000
)

plot_zoh_response(test6_behavior, zoh1, 'Chirp Signal', show_phases = FALSE)

# =============================================================================
# TEST SET 6: Sample Rate Comparison
# =============================================================================

cat('\n\n')
cat('###################################################################\n')
cat('  TEST SET 6: SAMPLE RATE COMPARISON                               \n')
cat('###################################################################\n')

# Compare different sample rates with sinusoidal input
sample_comparison <- compare_sample_rates(
  function(t) sinusoidal_continuous(t, amplitude = 2e-6, frequency = 0.08),
  'Sinusoidal Input',
  sample_periods = c(30, 60, 120),
  t_end = 600, n_points = 1000
)

# =============================================================================
# Summary
# =============================================================================

cat('\n')
cat('==================================================================\n')
cat('  ZERO-ORDER HOLD TESTING COMPLETE                                \n')
cat('==================================================================\n')
cat('\nKey Observations:\n')
cat('1. Three-Phase Pipeline:\n')
cat('   - READ phase: Samples input signal into internal storage\n')
cat('   - HOLD phase: Maintains sampled value (state stabilization)\n')
cat('   - WRITE phase: Transfers held value to output\n')
cat('\n2. Staircase Effect:\n')
cat('   - Output maintains constant value between samples\n')
cat('   - Creates characteristic staircase waveform\n')
cat('   - Approximates continuous signal with discrete levels\n')
cat('\n3. Sample Rate Impact:\n')
cat('   - Faster sampling (smaller period) = better approximation\n')
cat('   - Slower sampling (larger period) = more distortion\n')
cat('   - Nyquist theorem: sample rate > 2x highest frequency\n')
cat('\n4. Signal Types:\n')
cat('   - Works with any continuous input signal\n')
cat('   - Sinusoidal, ramp, square, step, chirp all handled\n')
cat('   - Phase control ensures proper READ-HOLD-WRITE sequence\n')
cat('\n5. Molecular Implementation:\n')
cat('   - Uses phase generator for pipeline coordination\n')
cat('   - Storage species maintains sampled concentration\n')
cat('   - Slow decay during HOLD preserves signal fidelity\n')
cat('\n')
cat('==================================================================\n')

# =============================================================================
# USAGE NOTES
# =============================================================================
#
# To run this test bench:
#   source('examples/zero_order_hold_test.R')
#
# To create custom ZOH:
#   zoh <- Make_ZeroOrderHold(
#     name = 'my_zoh',
#     nameInput = 'Input_signal',
#     nameOutput = 'Output_signal',
#     sample_period = 50,      # Adjust based on signal frequency
#     cinput = 0,
#     crange = 10,
#     rate = 1e-3
#   )
#
# To test with custom input:
#   - Define forcing function (returns dC/dt)
#   - Pass to forced_concentrations in react()
#   - Use high-resolution time vector to see continuous vs discrete
#
# Understanding sample period selection:
#   - For signal with max frequency f_max
#   - Nyquist: sampling frequency f_s > 2*f_max
#   - Sample period T_s < 1/(2*f_max)
#   - Example: f_max = 0.1 Hz => T_s < 5 time units
#
# Applications:
#   - Digital-to-analog conversion (DAC)
#   - Signal reconstruction after sampling
#   - Interface between discrete and continuous systems
#   - Control system output stages
#
# =============================================================================