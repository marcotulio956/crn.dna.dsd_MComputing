saw_wave_input <- function(t, amplitude = 10, period = 10) {
  phase <- (t %% period) / period
  # Linear ramp up from 0 to amplitude, then drops back down
  concentration <- ifelse(phase < 0.5, 
                          (amplitude * 2) * phase, 
                          amplitude - (amplitude * 2) * (phase - 0.5))
  return(pmax(0, concentration)) # Never below 0
}

sinusoidal_input <- function(t, amplitude = 5, frequency = 0.1, offset=5) {
  # Standard oscillating wave shifted up so its baseline is above zero
  concentration <- offset + amplitude * sin(2 * pi * frequency * t)
  return(pmax(0, concentration))
}

step_input <- function(t, time = 5, amplitude = 10) {
  # Baseline 0, steps up to step_magnitude after step_time
  concentration <- ifelse(t < time, 0, amplitude)
  return(pmax(0, concentration))
}

pulse_input <- function(t, time = 5, width = 5, amplitude = 10) {
  pulse_end <- time + width
  # Rectangular concentration pulse
  concentration <- ifelse(t >= time & t <= pulse_end, amplitude, 0)
  return(pmax(0, concentration))
}

square_input <- function(t, period = 10, pulse_width = 5, amplitude = 10) {
  # Determine the position within the current repeating period
  time_in_period <- t %% period
  
  # Apply the step logic within the period
  concentration <- ifelse(time_in_period < pulse_width, amplitude, 0)
  
  return(pmax(0, concentration))
}

# mu_target_func <- function(t) {
#     pulse_input(t, time = 20, width = 40, amplitude = 50)
# }

# # 3. Define your simulation time grid
# time_grid <- seq(0, 100, by = 0.1)

# # 4. Generate the "fuzzy" pulse via the OU process
# fuzzy_pulse_data <- simulate_ou_process(
#     t = time_grid, 
#     x0 = 0,                   # Start at zero to match the pulse start
#     theta = 2.0,              # Tracking speed (how fast it climbs the pulse)
#     mu = mu_target_func,      # <--- Injecting your arbitrary input here
#     sigma = 8.0,              # Volatility / "Fuzziness"
#     clamp_zero = TRUE         # Prevent negative concentrations
# )

# # 5. Convert the generated data into an interpolation function for the CRN
# fuzzy_input_func <- approxfun(
#     x = fuzzy_pulse_data$time, 
#     y = fuzzy_pulse_data$value, 
#     rule = 2
# )