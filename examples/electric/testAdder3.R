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
source('R/metric_functions.R')

jn <- function(...) { paste(..., sep = '') }

library(ggplot2) # plot()
library(dplyr) # mutate()

nameInput1 <- 'I1'
nameInput2 <- 'I2'
nameInput3 <- 'I3'
nameOutput1 <- 'O1'
nameOutput2 <- 'O2_1_Wang'
nameOutput2Wang <- 'O2_2_Wang'
nameIntput1Song <- 'I1_Song'
nameIntput2Song <- 'I2_Song'
nameInput3Song <-  'I3_Song'
nameOutput3 <- 'O3_1_Song'
nameOutput3Song <- 'O3_2_Song'


Make_Generic <- function(timing) {
  circuit <- DNArLogic::make_circuit(timing)



  ci1 <- 3 
  ci2 <- 5 
  ci3 <- 7 
  
  rate <- 10
  rate_song <- 0.1
  fuel <- 100

  # - Dig and Analog
  gate1 <- Make_Add3In(
    'add3', nameInput1, nameInput2, nameInput3, 
    nameOutput1,
    ci1, ci2, ci3, rate
  )

  gate2_1 <- Make_Adder2In_Wang(
    'add2_1_1', nameInput1, nameInput2, 
    nameOutput2, 
    ci1, ci2, 
    fuel, rate
  )
  gate2_2 <- Make_Adder2In_Wang(
    'add2_1_2', nameOutput2, nameInput3, 
    nameOutput2Wang, 
    0, ci3, 
    fuel, rate
  )
  
  gate3 <- Make_Adder2In_Song(
    'add2_2_1', nameIntput1Song, nameIntput2Song, 
    nameOutput3,
    ci1, ci2, 
    fuel, rate_song
  )
  gate4 <- Make_Adder2In_Song(
    'add2_2_2', nameOutput3, nameInput3Song, 
    nameOutput3Song,
    0, ci3, 
    fuel, rate_song
  )
  # add2circuit
  circuit <- circuit_add_gate(circuit, gate1)
  circuit <- circuit_add_gate(circuit, gate2_1)
  circuit <- circuit_add_gate(circuit, gate2_2)
  circuit <- circuit_add_gate(circuit, gate3)
  circuit <- circuit_add_gate(circuit, gate4)
  
  return (circuit)
}

t0 = 0
t1 = 1
points = (t1 - t0) * 100
timing  <- seq(t0, t1, length.out = points) # Using 50 time points
circuit <- Make_Generic(timing)
result_crn <- React_circuit(circuit)

expected_value = 15
minimum = expected_value * 0.97
maximum = expected_value * 1.03
gate_number = 1

Plot_behavior(
  result_crn, circuit, gate_number, minimum, maximum,
  plot_species=c(nameOutput1, nameOutput2, nameOutput2Wang),
  plot_species_dotted=c(),
  chart_title =  sprintf("Add3 Gate and Cascading Wang Add2 Gates CRN\n"), # 'Capacitor Step Response CRN Vcc=10[V] R=500[ohm] C=50e-4[F]',
  timing
)

Plot_behavior(
  result_crn, circuit, gate_number, minimum, maximum,
  plot_species=c(nameOutput1, nameIntput1Song, nameIntput2Song, nameInput3Song, nameOutput3, nameOutput3Song),
  plot_species_dotted=c(),
  chart_title =  sprintf("Add3 Gate and Cascading Song Add2 Gates CRN\n"), # 'Capacitor Step Response CRN Vcc=10[V] R=500[ohm] C=50e-4[F]',
  timing
)


resultado_4dom <- React_4domain_circuit(circuit)

Plot_behavior(
  resultado_4dom$behavior, circuit, gate_number, minimum, maximum,
  plot_species=c(nameInput1, nameInput2, nameInput3, nameOutput1, nameOutput2, nameOutput2Wang, nameIntput1Song, nameIntput2Song, nameInput3Song, nameOutput3, nameOutput3Song),
  plot_species_dotted=c(),
  chart_title = sprintf("Add3 Gates and Cascading Wang Add2 Gates DSD\n"),
  timing
)