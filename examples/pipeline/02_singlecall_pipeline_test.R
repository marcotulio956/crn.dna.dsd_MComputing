# =============================================================================
# Single-Call Pipeline Test (stepRLC style)
#
# Assemble all gates/modules, compile one circuit, run exactly one react() call.
# =============================================================================

rm(list = ls())

source('R/parser.R')
source('R/crn_reactor.R')
source('R/GATE_LIB.R')
source('R/io.R')
source('R/util_functions.R')
source('R/PIPELINE_LIB.R')

timing <- seq(0, 500, by = 5)

circuit <- make_singlecall_pipeline_demo_circuit(
  name = 'pipe_sc',
  timing = timing,
  base_rate = 1e-3,
  fast_factor = 25,
  crange = 10,
  i_init = 6,
  a_init = 1,
  b_init = 8
)

# Single simulation call for the whole pipeline circuit
behavior <- react(
  species = circuit$species,
  ci = circuit$ci,
  reactions = circuit$reactions,
  ki = circuit$ki,
  t = circuit$t
)

plot_species <- c(
  #circuit$registers$i,
  circuit$registers$a,
  #circuit$registers$b,
  circuit$registers$b_next,
  circuit$registers$i_next,
  circuit$phases$phase1,
  circuit$phases$phase2,
  circuit$phases$phase3
)

plot_species_dotted <- c(
  circuit$phases$phase1_abs,
  circuit$phases$phase2_abs
)

Plot_behavior(
  result = behavior,
  circuit = circuit,
  gate_numbers = c(),
  min = NULL,
  max = NULL,
  plot_species = plot_species,
  plot_species_dotted = plot_species_dotted,
  chart_title = 'Single-Call Pipelined CRN (Base/Fast Rate Policy)',
  timing = timing
)

i_start <- behavior[1, circuit$registers$i]
i_end <- behavior[nrow(behavior), circuit$registers$i]
b_start <- behavior[1, circuit$registers$b]
b_end <- behavior[nrow(behavior), circuit$registers$b]

cat('single react() call completed\n')
cat('i start/end =', i_start, i_end, '\n')
cat('b start/end =', b_start, b_end, '\n')
