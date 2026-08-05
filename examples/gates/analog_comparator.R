rm(list = ls())

source('R/4domain_reactor.R')
source('R/ANALOG_GATE_LIB.R')
source('R/analysis.R')
source('R/crn_reactor.R')
source('R/dsd.R')
source('R/GATE_LIB.R')
source('R/io.R')
source('R/parser.R')
source('R/util_functions.R')
source('R/NEURON_LIB.R')

jn <- function(...) { paste(..., sep = '') }

t0 = 0
t1 = 5
points = (t1 - t0) * 80 # * 80 # Using 50 time points
timing  <- seq(t0, t1, length.out = points) # Using 50 time points

circuit <- make_circuit(timing)

component <- c()
component$name = 'compgate'
component$il$input1 = 'ap'
component$il$input2 = 'an'
component$il$input3 = 'bp'
component$il$input4 = 'bn'
component$ol$output1 = 'GT'
component$ol$output2 = 'LT'
component$ic$input1 = 13
component$ic$input2 = 2
component$rate = 1e2

gate <- Make_AnalogComparator_WTA(
	component$name,
	component$rate, 
	component$ic$input1, 0,
	component$ic$input2, 0,
	component$il$input1, component$il$input2,
	component$il$input3, component$il$input4,
	component$ol$output1, component$ol$output2
)

circuit <- circuit_add_gate(circuit, gate)
# Optional plot settings for this standalone example
gate_number <- NULL
minimum <- NULL
maximum <- NULL

result_crn <- React_circuit(circuit)

Plot_behavior(
  result_crn, circuit, gate_number, minimum, maximum,
  plot_species=c('GT', 'LT'),
  chart_title ="comparator a>b",
  timing = timing
)