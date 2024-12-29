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

jn <- function(...) { paste(..., sep = '') }

library(ggplot2) # plot()
library(dplyr) # mutate()

React_circuit <- function(circuito) {
  return(react(
    species   = circuito$species,
    ci        = circuito$ci,
    reactions = circuito$reactions,
    ki        = circuito$ki,
    t         = circuito$t
  ))
}

React_4domain_circuit <- function(circuito) {
  return(react_4domain(
    species   = circuito$species,
    ci        = circuito$ci,
    reactions = circuito$reactions,
    ki        = circuito$ki,
    qmax      = 1e6, # maximum strand displacement rate constant
    cmax      = 1e7, # 1e-4  # starting concentration of auxiliary complexes Gi and Ti
    alpha     = 1,   # DSD timescale versus CRN
    beta      = 1,   # DSD concentration scale versus CRN
    t         = circuito$t
  ))
}

Plot_behavior <- function(resultado, circuito, numero, min, max, integrator) {
  g <- plot_behavior(resultado, species = c(circuito$gates[[numero]]$species$input1,
                                            circuito$gates[[numero]]$species$input2,
                                            circuito$gates[[numero]]$species$input3,
                                            circuito$gates[[numero]]$species$input4,
                                            circuito$gates[[numero]]$species$output,
                                            circuito$gates[[numero]]$species$output1,
                                            circuito$gates[[numero]]$species$output2,
                                            circuito$gates[[numero]]$species$output3),
                     x_label     = 'Time (s)',
                     y_label     = 'Concentration (M)',
                     legend_name = 'Species',
                     geom_list   = c('line', 'point'),
                     variable_line_type = FALSE,
                     variable_point_type = TRUE
  )

  if (!integrator){
    g <- g + geom_hline(yintercept=min, linetype="dashed", color = "green", size=1)
    g <- g + geom_hline(yintercept=max, linetype="dashed", color = "red", size=1)

  }

  print(g)

}

Make_Generic <- function() {
# diL = iL - tiL
# dvC = vC - tvC
# dvL = L * diL
# diC = C * dvC
  timing  <- seq(0, 0.1, length.out = 50) # Using 50 time points
  circuit <- DNArLogic::make_circuit(timing)
  
  vcc1 = c()
  vcc1$name <- 'vcc1'
  vcc1$ol$voltage <- 'vcc1_vcc'
  vcc1$ol$current <- 'vcc1_i'
  vcc1$ic$voltage <- 15
  vcc1$ic$current <- 3

  c1 = c()
  c1$name <- 'c1'
  c1$il$current <- 'vcc1_i'
  c1$il$voltage <- 'vcc1_vcc'
  c1$ol$current <- 'c1ol_current'
  c1$ol$voltage <- 'c1ol_voltage'
  c1$ol$charge <- 'c1ol_charge'
  c1$ic$charge <- 0
  c1$ic$capacitance <- 5 # q/V=C[farad]
  c1$ic$current <- vcc1$ic$current
  c1$ic$voltage <- vcc1$ic$voltage
  #print(c1)
  
  l1 = c()
  l1$name <- 'c1'
  l1$il$current <- 'vcc1_i'
  l1$il$voltage <- 'vcc1_vcc'
  l1$ol$current <- 'l1ol_current'
  l1$ol$voltage <- 'l1ol_voltage'
  l1$ol$flux <- 'l1ol_flux'
  l1$ic$flux <- 0
  l1$ic$inductance <- 5 # phi/I=L[henry]
  l1$ic$current <- vcc1$ic$current
  l1$ic$voltage <- vcc1$ic$voltage

  e1_gates <- Make_Inductor(l1$name, l1$il, l1$ol, l1$ic)
  e2_gates <- Make_Capacitor(c1$name, c1$il, c1$ol, c1$ic)
  circuit <- circuit_add_electro_gates(circuit, e1_gates)
  circuit <- circuit_add_electro_gates(circuit, e2_gates)
  
  return (circuit)
}

circuit <- Make_Generic()

result_crn <- React_circuit(circuit)

expected_value = 0
minimum = expected_value * 0.95
maximum = expected_value * 1.05
num_gate = 1

Plot_behavior(result_crn, circuit, num_gate, minimum, maximum, TRUE)
#Plot_all_behaviors(result_crn, circuit, num_gate, minimum, maximum, TRUE)

#resultado_4dom <- React_4domain_circuit(circuit)
#
#Plot_behavior(resultado_4dom$behavior, circuit, num_gate, minimum, maximum, TRUE)
#
#resultado_comb <- Compare_behaviors(resultado_crn, resultado_4dom, circuit, num_gate)
#
#p1 <- Plot_behavior_comb(resultado_comb, circuit, num_gate, minimum, maximum, TRUE)

