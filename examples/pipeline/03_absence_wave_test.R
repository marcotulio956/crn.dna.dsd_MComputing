# =============================================================================
# Absence-Driven Wave Test
# =============================================================================

rm(list = ls())

source('R/parser.R')
source('R/crn_reactor.R')
source('R/GATE_LIB.R')
source('R/io.R')
source('R/util_functions.R')
source('R/PIPELINE_LIB.R')

timing <- seq(0, 2000, by = 10)

wave <- make_absence_wave_module(
  name = 'clk',
  crange = 1,
  rate = 3e-2,
  fuel_ci = 100,
  absence_slow = 5e-4,
  absence_fast = 2e-2
)

behavior <- react(
  species = wave$all_species,
  ci = wave$ci,
  reactions = wave$reactions,
  ki = wave$ki,
  t = timing
)

Plot_behavior(
  result = behavior,
  circuit = list(gates = list()),
  gate_numbers = c(),
  min = NULL,
  max = NULL,
  plot_species = c(wave$species$value0, wave$species$value1),
  plot_species_dotted = c(wave$species$value0_abs, wave$species$value1_abs),
  chart_title = 'Oscillating Wave With Absence Readout',
  timing = timing
)

cat('Final value0 =', behavior[nrow(behavior), wave$species$value0], '\n')
cat('Final value1 =', behavior[nrow(behavior), wave$species$value1], '\n')