# =============================================================================
# Pipeline Primitives Test
# =============================================================================

rm(list = ls())

source('R/parser.R')
source('R/crn_reactor.R')
source('R/GATE_LIB.R')
source('R/io.R')
source('R/PIPELINE_LIB.R')
source('R/util_functions.R')

# Small sanity test for the absence indicator primitive
mod <- make_absence_indicator_module(
  name = 'xprobe',
  monitored_species = 'x',
  fuel_ci = 1,
  slow = 1e-3,
  fast = 1e-1
)

species <- c('x', unlist(mod$species, use.names = FALSE))
ci <- c(1, unlist(mod$ci, use.names = FALSE))
reactions <- unlist(mod$reactions, use.names = FALSE)
ki <- unlist(mod$ki, use.names = FALSE)

timing <- seq(0, 1200, by = 5)
behavior <- react(
  species = species,
  ci = ci,
  reactions = reactions,
  ki = ki,
  t = timing
)

# Consume x externally and verify x_ab can rise after depletion
species2 <- c(species, 'sink')
ci2 <- c(ci, 0)
reactions2 <- c(reactions, 'x -> sink')
ki2 <- c(ki, 2e-3)

behavior2 <- react(
  species = species2,
  ci = ci2,
  reactions = reactions2,
  ki = ki2,
  t = timing
)

species <- c('x', mod$species$absence, 'sink')
Plot_behavior(
  result = behavior2,
  circuit = list(gates = list()),
  gate_numbers = c(),
  min = NULL,
  max = NULL,
  species = species,
  species_dotted = c(),
  chart_title = 'Pipeline Primitive Test',
  timing = timing
)

x_end <- behavior2[nrow(behavior2), 'x']
xab_end <- behavior2[nrow(behavior2), mod$species$absence]

cat('Primitive test final x     =', x_end, '\n')
cat('Primitive test final x_ab  =', xab_end, '\n')

stopifnot(x_end < 0.2)
stopifnot(xab_end > 0.05)

cat('Pipeline primitive test PASSED\n')
