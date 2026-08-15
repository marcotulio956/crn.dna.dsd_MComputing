jn <- function(...) paste0(...)
# source("reconstructed_helpers.R")
source('R/parser.R')
source('R/util_functions.R')
source('R/crn_reactor.R')
source('R/4domain_reactor.R')
source('R/io.R')

source('R/GATE_LIB.R')
source('R/forced_concentrations.R')
source('examples/pipeline/test/PIPELINE_LIB.R')
## ================================================================
## A 3-deep pipeline -- READ / COMPUTE / WRITE -- as three ordinary,
## non-composable sequential reactions, each gated to its own phase
## of a single shared N-phase clock. No absence indicators, no
## hand-built mutexes: the phase oscillator IS the sequencing.
## ================================================================

READ <- list(
  species   = list(a = "IN", b = "R1"),
  reactions = c("IN -> R1"),
  ci        = c(0, 0),
  ki        = list(50)
)

COMPUTE <- list(                      # the "gate": doubles its input
  species   = list(a = "R1", b = "R2"),
  reactions = c("R1 -> 2 R2"),
  ci        = c(0, 0),
  ki        = list(50)
)

WRITE <- list(
  species   = list(a = "R2", b = "OUT"),
  reactions = c("R2 -> OUT"),
  ci        = c(0, 0),
  ki        = list(50)
)

## background data arrival, independent of phase
SUPPLY <- list(
  species = list(a = "IN"), reactions = c("0 -> IN"), ci = c(0), ki = list(2)
)

pipe <- Make_Pipeline(
  stages = list(READ, COMPUTE, WRITE),
  phase_names = c("READ_PH", "COMPUTE_PH", "WRITE_PH"),
  oscillator_rate = 0.02,
  oscillator_dominant_frac = 0.9
)

circuit <- Combine_Circuits(pipe, SUPPLY)

## periodic ZOH on the pipeline's OUTPUT, once per full clock cycle
period <- 3.78   # empirically-calibrated period for this oscillator (see calibrate_period3.R)
zoh <- Make_ZOH_Register("OUT", "OUT_hold", "SNAP", rate_slow = 300)
circuit <- Combine_Circuits(circuit, zoh)
events  <- Make_Periodic_Events("SNAP", period = period, t_start = period, t_end = 150)

tvec <- seq(0, 150, by = 0.02)
out_sto <- react_stochastic_frates_events(circuit$species, circuit$ci, circuit$reactions, circuit$ki,
                                tvec, volume = 10, events = events, seed = 7)

out_det <-react2_patched_events(circuit$species, circuit$ci, circuit$reactions, circuit$ki,
                                                 tvec, events = events)
# write.csv(out, "pipeline_demo_trajectory.csv", row.names = FALSE)
Plot_behavior(out_sto, circuit, specie = c("IN", "R1", "R2", "SNAP"," READ_PH", "SNAP_done" ))
# Plot_behavior(out_det, circuit, specie = c("IN", "R1", "R2", "OUT", "SNAP"," READ_PH", "SNAP_done", "OUT_hold"))

cat("Final state:\n")
print(tail(out_sto, 1))
