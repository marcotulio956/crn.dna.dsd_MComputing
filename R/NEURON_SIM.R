# Izhikevich neuron simulator and plotting helpers

# simulate_izhikevich:
# - timing: numeric vector of time points (in ms or seconds; consistent units required)
# - I: scalar, numeric vector same length as timing, or function(timing) returning vector
# - params: list(a,b,c,d, v0=-65, u0=NULL, v_thresh=30)
# - returns: list(time=timing, v, u, spike_times, spike_idx, params)
simulate_izhikevich <- function(timing, I = 10, params = list(a=0.02, b=0.2, c=-65, d=8, v0=-65, u0=NULL, v_thresh = 30)) {
  n <- length(timing)
  if (n < 2) stop("timing must have at least 2 points")
  if (any(diff(timing) <= 0)) stop("timing must be strictly increasing")
  # Input current vector
  if (is.function(I)) {
    Ivec <- I(timing)
  } else if (length(I) == 1) {
    Ivec <- rep(I, n)
  } else if (length(I) == n) {
    Ivec <- as.numeric(I)
  } else stop("I must be scalar, vector of same length as timing, or a function")
  # Parameters (avoid using 'c' name directly)
  a <- params$a; b <- params$b; c_reset <- params$c; d <- params$d
  v_thresh <- ifelse(is.null(params$v_thresh), 30, params$v_thresh)
  v <- numeric(n); u <- numeric(n)
  v[1] <- ifelse(is.null(params$v0), -65, params$v0)
  if (is.null(params$u0)) u[1] <- b * v[1] else u[1] <- params$u0
  spike_times <- numeric(0); spike_idx <- integer(0)
  for (i in seq_len(n-1)) {
    dt_i <- timing[i+1] - timing[i]
    dv <- 0.04 * v[i]^2 + 5 * v[i] + 140 - u[i] + Ivec[i]
    du <- a * (b * v[i] - u[i])
    v[i+1] <- v[i] + dt_i * dv
    u[i+1] <- u[i] + dt_i * du
    if (v[i+1] >= v_thresh) {
      v[i] <- v_thresh
      spike_times <- c(spike_times, timing[i])
      spike_idx <- c(spike_idx, i)
      v[i+1] <- c_reset
      u[i+1] <- u[i+1] + d
    }
  }
  # return params with v_thresh for plotting safety
  params$v_thresh <- v_thresh
  params$c <- c_reset
  return(list(time = timing, v = v, u = u, spike_times = spike_times, spike_idx = spike_idx, params = params, I = Ivec))
}

# Preset parameter sets (classic Izhikevich 2003 examples)
izh_presets <- list(
  "Regular Spiking (RS)" = list(a=0.02, b=0.2, c=-65, d=8, v0=-65),
  "Intrinsically Bursting (IB)" = list(a=0.02, b=0.2, c=-55, d=4, v0=-65),
  "Chattering (CH)" = list(a=0.02, b=0.2, c=-50, d=2, v0=-60),
  "Fast Spiking (FS)" = list(a=0.1, b=0.2, c=-65, d=2, v0=-65),
  "Low-threshold spiking (LTS)" = list(a=0.02, b=0.25, c=-65, d=2, v0=-65),
  "Phasic Spiking (PS)" = list(a=0.02, b=0.25, c=-65, d=6, v0=-64),
  "Tonic Spiking (TS)" = list(a=0.02, b=0.2, c=-65, d=6, v0=-65)
)

# Example: run multiple behaviours and plot (faceted)
example_run_behaviours <- function() {
  # timing in ms: use a fine dt for neuron sims (0.1 ms recommended)
  timing <- seq(0, 200, by = 0.1)  # 200 ms at 0.1 ms resolution
  # constant input current (you can instead pass a vector or function)
  I_inj <- function(t) { ifelse(t >= 20 & t <= 180, 10, 0) } # pulse from 20 to 180 ms
  
  sims <- lapply(names(izh_presets), function(name) {
    sim <- simulate_izhikevich(timing, I = I_inj, params = izh_presets[[name]])
    data.frame(time = sim$time, v = sim$v, u = sim$u, behaviour = name, spike = 0) %>%
      mutate(spike = ifelse(row_number() %in% sim$spike_idx, 1, 0))
  })
  combined <- bind_rows(sims)
  
  # Plot membrane potentials faceted by behaviour
  p <- ggplot(combined, aes(x=time, y=v)) +
    geom_line() +
    geom_point(data = combined %>% filter(spike==1), aes(x=time, y=rep(30, n())), shape = 4, size = 1.5) +
    facet_wrap(~ behaviour, ncol = 1, scales = "free_y") +
    labs(x = "time (ms)", y = "v", title = "Izhikevich behaviours (example input)") +
    theme_minimal()
  print(p)
  invisible(list(timing = timing, combined = combined))
}

# Wrapper to use Plot_behavior instead of plot_izhikevich
plot_izhikevich_with_Plot_behavior <- function(
  timing,
  I = 10,
  params = list(a=0.02, b=0.2, c=-65, d=8, v0=-65, u0=NULL, v_thresh = 30),
  circuit = NULL,          # can be NULL because we pass plot_species explicitly
  gate_numbers = NULL,
  y_min = NULL,            # horizontal green line (min) or NULL
  y_max = NULL,            # horizontal red line (max) or NULL
  plot_species = c("v","u","I"),
  plot_species_dotted = c("I"),
  chart_title = "Izhikevich neuron"
) {
  # run simulation (expects simulate_izhikevich to be defined)
  sim <- simulate_izhikevich(timing = timing, I = I, params = params)
  
  # Build a result data.frame similar to your result_crn
  result <- data.frame(
    time = sim$time,
    v    = sim$v,
    u    = sim$u,
    I    = sim$I
  )
  # also set rownames to timing (many plotting helpers read rownames/time)
  rownames(result) <- as.character(sim$time)
  
  # If you want spike markers as a species column (optional)
  if (length(sim$spike_idx) > 0) {
    spikes <- rep(0, nrow(result))
    spikes[sim$spike_idx] <- 1
    result$spike <- spikes
    # If user didn't request 'spike' in plot_species, we won't add it automatically
  }
  
  # Call your Plot_behavior wrapper
  # Note: Plot_behavior signature: Plot_behavior(result, circuit, gate_numbers, min, max, plot_species, plot_species_dotted, chart_title, timing)
  # We pass circuit (can be NULL) and gate_numbers (NULL), and timing as last arg
  Plot_behavior(
    result = result,
    circuit = circuit,
    gate_numbers = gate_numbers,
    min = y_min,
    max = y_max,
    plot_species = plot_species,
    plot_species_dotted = plot_species_dotted,
    chart_title = chart_title,
    timing = timing
  )
}