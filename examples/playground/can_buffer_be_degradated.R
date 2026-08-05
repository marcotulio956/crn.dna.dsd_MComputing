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

Make_Generic <- function() {
  timing  <- seq(0, 0.2, length.out = 50) # Using 50 time points
  circuit <- DNArLogic::make_circuit(timing)
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

  # circuit <- circuit_add_compile_gates(circuit, e1_gates)
  # circuit <- circuit_add_compile_gates(circuit, e2_gates)
  
  # DNALogic and DNAAnalog >
  g1 <- Make_Mul2In_Wang('mul1', 'c', 'v', 'q', 3, 50, 100)
  g2 <- Make_Adder_aBC('add', 'q_init', 'q', 'q_total', 100, 0, 1e1, 100)
  #print(e1_gates)
  circuit <- DNArLogic::circuit_add_gate(circuit, g1)
  circuit <- DNArLogic::circuit_add_gate(circuit, g2)

  return (circuit)
}

circuit <- Make_Generic()

print("=Cicuit Made")
print(circuit)
result_crn <- React_circuit(circuit)
print("=Cicuit React")
#print(result_crn)

expected_value = 0
minimum = expected_value * 0.95
maximum = expected_value * 1.05
gate_number = 2

Plot_behavior(
  result_crn, circuit, gate_number, minimum, maximum,
  plot_species=c('q', 'q_init', 'q_total'),
  timing
)

#resultado_4dom <- React_4domain_circuit(circuit)
#
#Plot_behavior(resultado_4dom$behavior, circuit, num_gate, minimum, maximum, TRUE)
#
#resultado_comb <- Compare_behaviors(resultado_crn, resultado_4dom, circuit, num_gate)
#
#p1 <- Plot_behavior_comb(resultado_comb, circuit, num_gate, minimum, maximum, TRUE)

