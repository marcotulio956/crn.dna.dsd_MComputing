rm(list = ls())

source('R/4domain_reactor.R')
source('R/ANALOG_GATE_LIB.R')
source('R/analysis.R')
source('R/crn_reactor.R')
source('R/dsd.R')
source('R/NEURON_SIM.R')
source('R/NEURON_LIB.R')
source('R/GATE_LIB.R')
source('R/ELECTRO_LIB.R')
source('R/io.R')
source('R/parser.R')
source('R/util_functions.R')
source('R/metric_functions.R')

jn <- function(...) { paste(..., sep = '') }

library(ggplot2) # plot()
library(dplyr) # mutate()


# 1) make component
izh_comp = list()
izh_comp <- Make_Izhikevich_Component(a=0.02, b=0.2, c=-65, d=8, v_thresh=30)
# 2) timing (choose small dt)
timing <- seq(0, 200, by = 0.1)
circuit <- DNArLogic::make_circuit(timing)
# 3) create input driver gate and add (example using your oscillator if available)
if (exists("Make_Oscillator_Dalchau")) {
  g_drv <- Make_Oscillator_Dalchau('sin', 'x', izh_comp$il$I_p, 'z', 1e-3, 1e-3, 15, 4e-1)
  circuit <- circuit_add_gate(circuit, g_drv)
}
# 4) create neuron gates and add to circuit
p_rates <- list(rate_base=1, rate_mul_sq=1, rate_mul_lin=1, rate_add=1, rate_int_v=1, rate_int_u=1, rate_mul_b=1, rate_a_scale=1, mux_rate=1, comp_rate=1)
izh_gates <- Make_Circuit_Izhikevich_Full(izh_comp$name, izh_comp$il, izh_comp$ol, izh_comp$ic, p = p_rates)
circuit <- circuit_add_compile_gates(circuit, izh_gates)
# 5) run CRN sim
# find gates stored in the circuit object (adjust if your circuit stores them in a different field)
gates_list <- circuit$gates
if (is.null(gates_list)) stop("circuit$gates not found; adapt this script to where you store gates in circuit.")

mismatches <- list()
for (i in seq_along(gates_list)) {
  g <- gates_list[[i]]
  nR <- if (!is.null(g$reactions)) length(g$reactions) else 0
  nK <- if (!is.null(g$ki)) length(g$ki) else 0
  if (nR != nK) {
    mismatches[[length(mismatches)+1]] <- list(index=i, name=g$name, n_reactions=nR, n_ki=nK, species=length(g$species))
  }
}
if (length(mismatches)==0) {
  cat("No per-gate reaction/ki mismatches found — the inconsistency must come from circuit assembly (see below).\n")
} else {
  cat("Found", length(mismatches), "gate(s) with reaction/ki length mismatches:\n")
  print(mismatches)
}

sto_crn < React_stochastic(circuit)
res_crn <- React_circuit(circuit)
res_crn <- res_crn[, order(names(res_crn))]
# 6) extract v trace (v_p - v_n) and u if desired
v_p_tr <- res_crn[[izh_comp$ol$v_p]]
v_n_tr <- res_crn[[izh_comp$ol$v_n]]
v_crn <- v_p_tr - v_n_tr
plot(seq_along(v_crn), v_crn, type='l', main='v_crn (v_p - v_n)')



