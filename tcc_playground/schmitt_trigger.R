# Load the libraries
# DO NOT USE library(DNAr)
# DO NOT USE library(DNArLogic)
# DO NOT USE library(DNArAnalog)

rm(list = ls())

source('R/4domain_reactor.R')
source('R/ANALOG_GATE_LIB.R')
source('R/analysis.R')
source('R/crn_reactor.R')
source('R/dsd.R')
source('R/ELECTRO_LIB.R')
source('R/GATE_LIB.R')
source('R/io.R')
source('R/neuron_hjelmfelt.R')
source('R/parser.R')
source('R/util_functions.R')

jn <- function(...) { paste(..., sep = '') }

library(ggplot2) # plot()
library(dplyr) # mutate()

Make_Capacitor_ <- function(name, species_input, species_output, ic) {

}

Make_Generic <- function(timing) {
  circuit <- DNArLogic::make_circuit(timing)
  
  # - Dig and Analog
  g1 <- Make_Oscillator_Dalchau('clk', 'clk1', 'clk2', 'clk3', 15, 5, 10, 2e-5)
  # add2circuit
  circuit <- DNArLogic::circuit_add_gate(circuit, g1)
  #circuit <- DNArLogic::circuit_add_gate(circuit, g2)

  return (circuit)
}

timing  <- seq(0, 0.01, length.out = 50) # Using 50 time points
circuit <- Make_Generic(timing)
result_crn <- React_circuit(circuit)

expected_value = 0
minimum = expected_value * 0.95
maximum = expected_value * 1.05
gate_number = 1

Plot_behavior(
  result_crn, circuit, gate_number, minimum, maximum,
 # plot_species=c('c1ol_voltage', 'c1_l_v_rate1','c1ol_current','c1ol_charge'),
  plot_species=c(),
  add_capacitor = FALSE, R = 1000, C = 0.0001, V_max = 15, 
  timing
)
# Plot_behavior(result_crn, circuit, gate_number, minimum, maximum, specify_species = TRUE, plot_species=c('c1ol_current', 'c1sub1_C', 'c1_l_dv_out'))

#resultado_4dom <- React_4domain_circuit(circuit)
#
#Plot_behavior(resultado_4dom$behavior, circuit, gate_number, minimum, maximum, TRUE)
#
#resultado_comb <- Compare_behaviors(resultado_crn, resultado_4dom, circuit, gate_number)
#
#p1 <- Plot_behavior_comb(resultado_comb, circuit, gate_number, minimum, maximum, TRUE)

