rm(list = ls())

source('R/4domain_reactor.R')
source('R/ANALOG_GATE_LIB.R')
source('R/analysis.R')
source('R/crn_reactor.R')
source('R/dsd.R')
source('R/NEURON_SIM.R')
source('R/GATE_LIB.R')
source('R/io.R')
source('R/parser.R')
source('R/util_functions.R')
source('R/metric_functions.R')

jn <- function(...) { paste(..., sep = '') }

library(ggplot2) # plot()
library(dplyr) # mutate()

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

# Example usage (Regular spiking, pulse injection)
timing <- seq(0, 200, by = 0.1)  # fine dt recommended
I_pulse <- function(t) ifelse(t >= 20 & t <= 180, 10, 0)
plot_izhikevich_with_Plot_behavior(timing, I = I_pulse,
                                  params = list(a=0.02, b=0.2, c=-65, d=8, v0=-65),
                                  y_min = -80, y_max = 40,
                                  plot_species = c("v", "u", "I"),
                                  plot_species_dotted = c("I"),
                                  chart_title = "Regular spiking (RS) — v, u, I")