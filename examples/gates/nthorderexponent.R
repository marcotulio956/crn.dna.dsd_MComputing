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
component$name = 'expgate'
component$il$x = 'x'
component$il$n = 'n'
component$ol$y = 'y'
x = 2.7182
n = 5
component$ic$x = x
component$ic$n = n
component$rate = 1e2

gate <- Make_NthOrderExponent_Wang(
	component$name,
	component$rate, 
	component$ic$x, component$ic$n, 
	component$il$x, component$il$n, component$ol$y
)

circuit <- circuit_add_gate(circuit, gate)
# Optional plot settings for this standalone example
gate_number <- NULL
minimum <- NULL
maximum <- NULL

result_crn <- React_circuit(circuit)

Plot_behavior(
  result_crn, circuit, gate_number, minimum, maximum,
  species=c('x', 'y'),
  chart_title ="exponent y=x^a",
	timing = timing
)