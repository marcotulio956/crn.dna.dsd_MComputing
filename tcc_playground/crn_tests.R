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
  timing  <- seq(0, 10, length.out = 50) # Using 50 time points
  circuit <- DNArLogic::make_circuit(timing)

  test1 = c()
  test1$species = c('A','B','C','null')
  test1$ci = c(0, 0, 0, 1000)
  test1$reactions[[1]] = "null -> A" 
  test1$reactions[[2]] = "A -> null" 
  test1$reactions[[3]] = "C + A -> C + B" 
  test1$ki = c(10, 2, 1/3)

  circuit <- test1

  return (circuit)
}

circuit <- Make_Generic()

result_crn <- React_circuit(circuit)
print("=Circuit")
print(circuit)

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

