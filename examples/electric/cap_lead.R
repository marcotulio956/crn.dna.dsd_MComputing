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
source('R/ELECTRO_SIM.R')
source('R/GATE_LIB.R')
source('R/io.R')
source('R/neuron_hjelmfelt.R')
source('R/parser.R')
source('R/util_functions.R')

jn <- function(...) { paste(..., sep = '') }

library(ggplot2) # plot()
library(dplyr) # mutate()

library(signal)  # For peak detection
library(pracma)  # For findpeaks function if needed


Make_Generic <- function(timing) {
  circuit <- make_circuit(timing)
  
  g_dalchau <- Make_Oscillator_Dalchau('osc', 'y', 'z', 'c1il_v1p', 9, 8, 5, 10e-2)

  c1 <- Make_Capacitor_Component(1, 1)

  c1$il$voltage_positive <- 'c1il_v1p'
  
  # - Electro
  e1_gates <- Make_Circuit_Capacitor(c1$name, c1$il, c1$ol, c1$ic, 1000)
  
  # add2circuit
  circuit <- circuit_add_gate(circuit, g_dalchau)
  circuit <- circuit_add_compile_gates(circuit, e1_gates)

  return (circuit)
}

t0 = 0
t1 = 20
points = (t1 - t0) * 50 # Using 50 time points
timing  <- seq(t0, t1, length.out = points) # Using 50 time points
circuit <- Make_Generic(timing)
result_crn <- React_circuit(circuit)

expected_value = 0
minimum = expected_value * 0.95
maximum = expected_value * 1.05
gate_number = 1

simRC <- simulate_sRC_voltage_source(
  timing, result_crn[['c1il_v1p']], 0.1, 10
)


result_crn['c1ol_iout'] <- 0.001 * ( result_crn['c1ol_ip'] -  result_crn['c1ol_in'] )
result_crn['c1ol_vout'] <- 0.000001 * ( result_crn['c1ol_vp'] -  result_crn['c1ol_vn'] )
result_crn['c1il_v1p'] <- 10 * result_crn['c1il_v1p'] 
result_crn['V(C)'] <- 10 * (simRC$capacitor_voltage)
result_crn['I(R)'] <- 1 * (simRC$current_output)

Plot_behavior(
  result_crn, circuit, gate_number, minimum, maximum,
  #species=c('l1il_i1p', 'l1ol_ip', 'l1ol_vp', 'l1ol_in', 'l1ol_vn', 'l1ol_vout'),
  species=c('c1il_v1p','c1ol_vout', 'c1ol_iout'), #  'y', 'z'),
  species_dotted=c('V(C)', 'I(R)'),
  chart_title = 'Oscilator Capacitor Current Lead DSD Vin=\'c1il_v1p\' R=10[ohm] C=0.1[F]',
  timing
)

#resultado_4dom <- React_4domain_circuit(circuit)

#resultado_4dom$behavior['c1ol_iout'] <- result_crn$behavior['c1ol_ip'] -  result_crn$behavior['c1ol_in']

#Plot_behavior(
#  resultado_4dom$behavior, circuit, gate_number, minimum, maximum,
#  species=c('c1ol_vp', 'c1ol_ip', 'c1ol_in', 'c1ol_iout'),
#  chart_title = 'Oscilator Capacitor Current Lead DSD',
#  timing
#)



