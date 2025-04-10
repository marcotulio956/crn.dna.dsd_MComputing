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

Make_Generic <- function(timing) {
  circuit <- DNArLogic::make_circuit(timing)
  
  v1 <- Make_VoltageSource_Component(1, 5)

  c1 <- Make_Capacitor_Component(2, 5)
  c1$il$voltage_positive <- v1$ol$voltage
  # c1$ic$voltage_positive <- 0
  c1$ic$voltage_positive <- v1$ic$voltage
  # c1$ic$voltage_negative <- 10

  # - Electro
  e1_gates <- Make_Circuit_Capacitor(c1$name, c1$il, c1$ol, c1$ic, 15e-1)
  print(e1_gates)
  # add2circuit
  circuit <- circuit_add_electro_gates(circuit, e1_gates)
  #circuit <- circuit_add_electro_gates(circuit, e2_gates)

  # - Dig and Analog
  #g1 <- Make_Mul2In_Wang(r1$name, r1$il$current, jn(r1$name, '_R'), r1$ol$voltage, r1$ic$current, r1$ic$resistence, 2e3)
  #g2 <- Make_Div2In_Wang(r1$name, r1$il$voltage, jn(r1$name, '_R'), r1$ol$current, r1$ic$voltage, r1$ic$resistence, 1e3, 2e3)
  #g1 <- make_latchd('latch1',2, 1)
  #g2 <- make_flipflopd('ffd1', 2, 1)
  #g1 <- Make_Div2In_Wang('div1w', 'Xd4', 'Yd4', 'Zd4', 1, 5, 0.5, 1e3)
  # add2circuit
  #circuit <- DNArLogic::circuit_add_gate(circuit, g1)
  #circuit <- DNArLogic::circuit_add_gate(circuit, g2)

  return (circuit)
}

t0 = 0
t1 = 10
points = (t1 - t0) * 100 # Using 50 time points
timing  <- seq(t0, t1, length.out = points) # Using 50 time points
circuit <- Make_Generic(timing)
result_crn <- React_circuit(circuit)

expected_value = 0
minimum = expected_value * 0.95
maximum = expected_value * 1.05
gate_number = 1

Plot_behavior(
  result_crn, circuit, gate_number, minimum, maximum,
  #plot_species=c('c1ol_voltage', 'c1_l_v_rate1','c1ol_current','c1ol_charge'),
  #plot_species=c('c1il_voltage_positive', 'c1ol_charge', 'c1ol_voltage', 'c1ol_current'),
  plot_species=c('v1_vcc', 'c2ol_vn', 'c2ol_vp', 'c2l_pcharge', 'c2l_ncharge','c2ol_ip', 'c2ol_in'),
  #plot_species=c('c3l_pcharge', 'c3l_ncharge', 'l4l_pflux', 'l4l_nflux'),
  
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

