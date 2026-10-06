rm(list = ls())

source('R/4domain_reactor.R')
source('R/analysis.R')
source('R/crn_reactor.R')
source('R/dsd.R')
source('R/GATE_LIB.R')
source('R/io.R')
source('R/parser.R')

source('R/forced_concentrations.R')


species <- c('A1', 'A2', 'A3', 'A4')
ci <- c(0, 0, 0, 0)
reactions <- c('w -> w')
ki <- c(1)

t_start <- 0
t_end <- 20
time_points <- seq(t_start, t_end, length.out = 1000)


forced_concentrations <- list(
  A1 = function(t) sinusoidal_input(t),
  A2 = function(t) step_input(t),
  A3 = function(t) saw_wave_input(t),
  A4 = function(t) pulse_input(t)
)

behavior <- react(
  species = species,
  ci = ci,
  reactions = reactions,
  ki = ki,
  t = time_points,
  forced_concentrations = forced_concentrations
)

plot_behavior(
  behavior,
  species = species,
  x_label = 'Time (s)',
  y_label = 'Concentration (M)',
  legend_name = 'Species'
)
