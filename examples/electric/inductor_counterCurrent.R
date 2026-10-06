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

Make_Inductor_ <- function(name, species_input, species_output, ic) {
  rate <- 2e3
  gates <- list()

  # F_l = L * I_in
  g1 <- Make_Mul2In_Wang(jn(name, 'mul1'),
    species_input$inductance, species_input$current, species_output$flux,
    ic$inductance, ic$current,
    rate
  )
  gates[[length(gates)+1]] <- g1

  # I_out rate 1= F_l*[1/L]
  l_invL = jn(name, '_l_invL')
  l_i_rate1 = jn(name, '_l_v_out')
  g2 <- Make_Mul2In_Wang(jn(name,'mul2'),
    species_output$flux, l_invL, l_i_rate1,
    0, 1/ic$inductance,
    rate*0.46
  )
  gates[[length(gates)+1]] <- g2

   # I_out rate 2= F_l * [1/L]
  g3 <- Make_Mul2In_Wang(jn(name,'mul3'),
    species_output$flux, l_invL, species_output$current,
    0, 1/ic$inductance,
    rate
  )
  gates[[length(gates)+1]] <- g3
  
  # di_out = species_output$current - l_i_rate1
  l_di = jn(name, '_l_di_out')
  g4 <- Make_Sub2In_Wang(jn(name, 'sub1'),
     species_output$current, l_i_rate1, l_di,
     0, 0,
     1e-2, rate*1e3
  )
  gates[[length(gates)+1]] <- g4

  # V_out=L*di_out
  g5 <- Make_Mul2In_Wang(jn(name, 'mul4'),
   species_input$inductance, l_di, species_output$voltage,
   ic$inductance, 0,
   rate
  )
  gates[[length(gates)+1]] <- g5

  return (gates)
}

Make_Generic <- function(timing) {
  circuit <- DNArLogic::make_circuit(timing)
  
  vcc1 = c()
  vcc1$name <- 'vcc1'
  vcc1$ol$voltage <- 'vcc1_vcc'
  vcc1$ol$current <- 'vcc1_i'
  vcc1$ic$voltage <- 10
  vcc1$ic$current <- 0

  l1 = c()
  l1$name <- 'l1'
  l1$il$current <- 'vcc1_i'
  l1$il$voltage <- 'vcc1_vcc'
  l1$il$inductance <- 'l1ol_inductance'
  l1$ol$current <- 'l1ol_current'
  l1$ol$voltage <- 'l1ol_voltage'
  l1$ol$flux <- 'l1ol_flux'
  l1$ic$flux <- 0
  l1$ic$inductance <- 5 # do 100  phi/I=L[henry]
  l1$ic$current <- vcc1$ic$current
  l1$ic$voltage <- vcc1$ic$voltage
  
  # - Electro
  e1_gates <- Make_Inductor_(l1$name, l1$il, l1$ol, l1$ic)
  # add2circuit
  circuit <- circuit_add_compile_gates(circuit, e1_gates)

  # - Dig and Analog
  #g1 <- Make_Div2In_Wang('div1w', 'Xd4', 'Yd4', 'Zd4', 1, 5, 0.5, 1e3)
  # add2circuit
  #circuit <- DNArLogic::circuit_add_gate(circuit, g1)

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
  species=c('l1ol_current', 'l1ol_voltage'),
  #species=c('l1_l_i_out'),
  timing
)
# Plot_behavior(result_crn, circuit, gate_number, minimum, maximum, specify_species = TRUE, species=c('c1ol_current', 'c1sub1_C', 'c1_l_dv_out'))

#resultado_4dom <- React_4domain_circuit(circuit)
#
#Plot_behavior(resultado_4dom$behavior, circuit, gate_number, minimum, maximum, TRUE)
#
#resultado_comb <- Compare_behaviors(resultado_crn, resultado_4dom, circuit, gate_number)
#
#p1 <- Plot_behavior_comb(resultado_comb, circuit, gate_number, minimum, maximum, TRUE)

