rm(list = ls())

source('R/parser.R')
source('R/util_functions.R')
source('R/crn_reactor.R')
source('R/4domain_reactor.R')
source('R/io.R')

source('R/GATE_LIB.R')
source('R/forced_concentrations.R')
source('R/PIPELINE_LIB.R')

timing <- seq(0, 20, by = 0.1)

rate = 1
fast_factor = 100

x_absence <- Make_Absence_Indicator('absence_gate', rate, fast_factor, 'x', 'absence')

circuit <- make_circuit(timing)

circuit <- circuit_add_gate(circuit, x_absence)

forced_concentrations = list(
	x = function(t) square_input(t, pulse_width = 5, period = 10, amplitude = 0.5)
)

behavior <- React_circuit(circuit, forced_concentrations = forced_concentrations)

scale = 1
behavior["absence"] <- behavior["absence"] * scale

Plot_behavior(behavior, circuit)


