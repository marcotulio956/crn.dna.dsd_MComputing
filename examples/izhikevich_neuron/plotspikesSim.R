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

# Example usage (Regular spiking, pulse injection)
timing <- seq(0, 200, by = 0.1)  # fine dt recommended
I_pulse <- function(t) ifelse(t >= 20 & t <= 180, 10, 0)
plot_izhikevich_with_Plot_behavior(timing, I = I_pulse,
                                  params = list(a=0.02, b=0.2, c=-65, d=8, v0=-65),
                                  y_min = -80, y_max = 40,
                                  plot_species = c("v", "u", "I"),
                                  plot_species_dotted = c("I"),
                                  chart_title = "Regular Spiking (RS) — v, u, I")