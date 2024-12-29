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

Make_Generic <- function()timing {
  circuit <- DNArLogic::make_circuit(timing)


  vcc1 = c()
  vcc1$name <- 'vcc1'
  vcc1$ol$voltage <- 'vcc1_vcc'
  vcc1$ol$current <- 'vcc1_i'
  vcc1$oc$voltage <- 10
  vcc1$oc$current <- 3

  r1 = c()
  r1$name <- 'r1'
  r1$il$current <- 'r1il_current'
  r1$il$voltage <- 'r1il_voltage'
  r1$ol$current <- 'r1ol_current'
  r1$ol$voltage <- 'r1ol_voltage'
  r1$ic$resistence <- 1000
  r1$ic$current <- vcc1$oc$current
  r1$ic$voltage <- vcc1$oc$voltage

  r2 = c()
  r2$name <- 'r2'
  r2$il$current <- 'vcc1_i'
  r2$il$voltage <- 'vcc1_vcc'
  r2$ol$current <- 'r2ol_current'
  r2$ol$voltage <- 'r2ol_voltage'
  r2$ic$resistence <- 10000
  r2$ic$current <- vcc1$oc$current
  r2$ic$voltage <- vcc1$oc$voltage

  c1 = c()
  c1$name <- 'c1'
  c1$il$current <- 'vcc1_i'
  c1$il$voltage <- 'vcc1_vcc'
  c1$il$capacitance <- 'c1il_capacitance'
  c1$il$charge <- 'c1il_charge'
  c1$ol$current <- 'c1ol_current'
  c1$ol$voltage <- 'c1ol_voltage'
  c1$ol$charge <- 'c1ol_charge'
  c1$ic$charge <- 0
  # c1$ic$charge <- 0
  c1$ic$capacitance <- 5 # do 5e-5  q/V=C[farad]
  c1$ic$current <- vcc1$oc$current
  # c1$ic$voltage <- 0
  c1$ic$voltage <- vcc1$oc$voltage

  l1 = c()
  l1$name <- 'c1'
  l1$il$current <- 'vcc1_i'
  l1$il$voltage <- 'vcc1_vcc'
  l1$il$inductance <- 'l1ol_inductance'
  l1$ol$current <- 'l1ol_current'
  l1$ol$voltage <- 'l1ol_voltage'
  l1$ol$flux <- 'l1ol_flux'
  l1$ic$flux <- 0
  l1$ic$inductance <- 100 # do 100  phi/I=L[henry]
  l1$ic$current <- vcc1$ic$current
  l1$ic$voltage <- vcc1$ic$voltage

  # - Electro
  # Resistor in a circuit
  # e1_gates <- Make_Resistor(r1$name, r1$il, r1$ol, r1$ic)

  # Resistor series Vcc
  # e1_gates <- Make_Resistor(r2$name, r2$il, r2$ol, r2$ic)

  # Capacitor series Vcc
  # e1_gates <- Make_Capacitor(c1$name, c1$il, c1$ol, c1$ic)
  # print(e1_gates)

  # Inductor series Vcc
  # e2_gates <- Make_Inductor(l1$name, l1$il, l1$ol, l1$ic)
  # print(e1_gates)

  # circuit <- circuit_add_electro_gates(circuit, e1_gates)
  # circuit <- circuit_add_electro_gates(circuit, e2_gates)
  
  # DNALogic and DNAAnalog >
  #g1 <- Make_Div2In_Wang('div1w', 'Xd4', 'Yd4', 'Zd4', 1, 5, 0.5, 1e3)
  #print(e1_gates)
  #circuit <- DNArLogic::circuit_add_gate(circuit, g1)

  return (circuit)
}

timing  <- seq(0, 0.01, length.out = 50) # Using 50 time points
circuit <- Make_Generic(timing)
print("=Cicuit Made")
print(circuit)
result_crn <- React_circuit(circuit)
print("=Cicuit React")
#print(result_crn)

expected_value = 0
minimum = expected_value * 0.95
maximum = expected_value * 1.05
num_gate = 5

Plot_behavior(result_crn, circuit, num_gate, minimum, maximum, specify_species = FALSE, plot_species=c('c1ol_voltage', 'vcc1_vcc'))

#resultado_4dom <- React_4domain_circuit(circuit)
#
#Plot_behavior(resultado_4dom$behavior, circuit, num_gate, minimum, maximum, TRUE)
#
#resultado_comb <- Compare_behaviors(resultado_crn, resultado_4dom, circuit, num_gate)
#
#p1 <- Plot_behavior_comb(resultado_comb, circuit, num_gate, minimum, maximum, TRUE)

