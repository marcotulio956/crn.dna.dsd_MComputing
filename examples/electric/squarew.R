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
  circuit <- make_circuit(timing)
  ###
    # - Dig and Analog
    g_dalchau <- Make_Oscillator_Dalchau('sin', 'x', 'y', 'z', 10, 1, 2, 2e-1)
    c_comparator <- Make_Mux2_balanced(
      'mux1',
      'x', 'y', 
      'low', 'high',
      'comp_out',
      0, 0,
      3, 8,
      0, 7.1e-1
    )
    g_sum <- Make_Adder2In_Wang('sum1', 'x', 'x', 'sum_out', 0 , 0, 1, 1)

    # add2circuit
    circuit <- circuit_add_gate(circuit, g_dalchau)
    circuit <- circuit_add_gate(circuit, c_comparator)
    circuit <- circuit_add_gate(circuit, g_sum)
  ###

  ###
    # - Electro   
    # add2circuit
  ###

  ### Square thingy dont touch

  return (circuit)
}
t0 = 0
t1 = 250
points = (t1 - t0) * 50 # Using 50 time points
timing  <- seq(0, t1, length.out = points) 
circuit <- Make_Generic(timing)
print(circuit)
result_crn <- React_circuit(circuit)

expected_value = 0
minimum = expected_value * 0.95
maximum = expected_value * 1.05
gate_numbers = c() 

Plot_behavior(
  result_crn, circuit, gate_numbers, minimum, maximum,
  #plot_species=c('c1ol_voltage', 'c1_l_v_rate1','c1ol_current','c1ol_charge'),
  # plot_species=c('c1il_charge', 'c1ol_charge', 'c1ol_voltage', 'c1ol_current'),
  #plot_species=c('x', 'pintx', 'pdxdt', 'ndxdt', 'x_dir2'),
  plot_species=c(),
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

