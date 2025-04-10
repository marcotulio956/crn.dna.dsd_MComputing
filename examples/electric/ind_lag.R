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

Make_Mux2_balanced <- function(name, nameInput1, nameInput2, nameControl1, nameControl2,
                      nameOutput, cinput1, cinput2, control1, control2, crange,
                      rate) {
  species <- list(
    input1 = nameInput1, #E1
    input2 = nameInput2, #E2
    output1 = nameControl1,  #C1
    output2 = nameControl2,  #C2
    gate1E = jn(name, '_GEn1'), #G1E
    gate2E = jn(name, '_GEn2'), #G2E
    gate1U = jn(name, '_GUn1'), #G1U
    gate2U = jn(name, '_GUn2'), #G2U
    output = nameOutput  #Output
  )

  ci <- c(cinput1, cinput2, control1, control2, control1, control2, crange, crange, 0)

  reactions <- c(
    # 'G1U + C1 -> G1E'
    jn(species$gate1U, ' + ', species$output1, ' -> ', species$output1, species$gate1E),
    # 'G1E + C2 -> G1U'
    jn(species$gate1E, ' + ', species$output2, ' -> ', species$output2, species$gate1U),
    # 'G2U + C2 -> G2'
    jn(species$gate2U, ' + ', species$output2, ' -> ', species$output2, species$gate2E),
    # 'G2E + C1 -> G2U'
    jn(species$gate2E, ' + ', species$output1, ' -> ', species$output1, species$gate2U),
    # 'E1 + G1 -> Output'
    jn(species$input1, ' + ', species$gate1E, ' -> ', species$output),
    # 'E2 + G2 -> Output'
    jn(species$input2, ' + ', species$gate2E, ' -> ', species$output)
  )

  ki <- c(rate, rate, rate, rate, rate, rate)

  mux2_gate <- list(
    name      = name,
    species   = species,
    reactions = reactions,
    ci        = ci,
    ki        = ki
  )

  return(mux2_gate)
}

Make_Generic <- function(timing) {
  circuit <- DNArLogic::make_circuit(timing)
  
  g_dalchau <- Make_Oscillator_Dalchau('osc', 'l1il_i1p', 'y', 'z', 0, 10, 0.1, 5e-2)
  g_dalchau <- Make_Oscillator_Dalchau('osc', 'y', 'z', 'l1il_i1p', 1, 8, 5, 10e-2)

  #c1 <- Make_Capacitor_Component(1, 5)
  l1 <- Make_Inductor_Component(1, 1)

  #c1$il$voltage_positive <- 'x'
  l1$il$current_positive <- 'l1il_i1p'
  
  #c1$ic$voltage_positive <- 1

  # - Electro
  #e1_gates <- Make_Circuit_Capacitor(c1$name, c1$il, c1$ol, c1$ic, 15e-1)
  e2_gates <- Make_Circuit_Inductor(l1$name, l1$il, l1$ol, l1$ic, 1)
  
  #print(e1_gates)
  #print(e2_gates)

  # add2circuit
  #circuit <- circuit_add_electro_gates(circuit, e1_gates)
  circuit <- circuit_add_gate(circuit, g_dalchau)
  circuit <- circuit_add_electro_gates(circuit, e2_gates)

  return (circuit)
}

t0 = 0
t1 = 25
points = (t1 - t0) * 10 # Using 50 time points
timing  <- seq(t0, t1, length.out = points) # Using 50 time points
circuit <- Make_Generic(timing)
result_crn <- React_circuit(circuit)

expected_value = 0
minimum = expected_value * 0.95
maximum = expected_value * 1.05
gate_number = 1

result_crn['l1ol_vout'] <- result_crn['l1ol_vp'] -  result_crn['l1ol_vn']

Plot_behavior(
  result_crn, circuit, gate_number, minimum, maximum,
  #plot_species=c('l1il_i1p', 'l1ol_ip', 'l1ol_vp', 'l1ol_in', 'l1ol_vn', 'l1ol_vout'),
  plot_species=c('l1ol_ip', 'l1ol_vp', 'l1ol_vn', 'l1ol_vout'),
  chart_title = 'Oscilator Inductor Current Lag CRN',
  timing
)

resultado_4dom <- React_4domain_circuit(circuit)

resultado_4dom$behavior['l1ol_vout'] <- resultado_4dom$behavior['l1ol_vp'] -  resultado_4dom$behavior['l1ol_vn']

Plot_behavior(
  resultado_4dom$behavior, circuit, gate_number, minimum, maximum,
  plot_species=c('l1ol_ip', 'l1ol_vp', 'l1ol_vn', 'l1ol_vout'),
  chart_title = 'Oscilator Inductor Current Lag DSD',
  timing
)


