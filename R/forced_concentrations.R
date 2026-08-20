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

square_input <- function(t, period = 10, pulse_width = period, amplitude = 10, delay = 0) {
  # Square wave with specified period, pulse width, amplitude, and delay
  # Works for vector t
  
  # Adjust time by delay
  t_adjusted <- t - delay
  
  # Before delay, output is zero
  concentration <- ifelse(t < delay, 0,
                          ifelse((t_adjusted %% period) / period < (pulse_width / period),
                                 amplitude, 0))
  
  return(concentration)
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


simulate_sin <- function(t,
                         A   = 2.5,      # amplitude
                         DC  = 7.5,      # offset (DC level)
                         PER = 5.045,      # period
                         PHI = 3.1415/2 - (0.15)      # phase shift (in same time units as t)
) {
  # t: numeric vector of time points
  # Returns a numeric vector of the same length with the sinusoidal value
  
  # Angular frequency ω = 2π / period
  omega <- 2 * pi / PER
  
  # Compute y(t) = DC + A * sin(ω * (t - PHI))
  DC + A * sin(omega * (t - PHI))
}
