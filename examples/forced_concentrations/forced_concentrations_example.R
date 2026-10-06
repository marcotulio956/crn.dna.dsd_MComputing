# Load the libraries
# DO NOT USE library(DNAr)
# DO NOT USE library(DNArLogic)
# DO NOT USE library(DNArAnalog)

rm(list = ls())
# setwd("~/MEGAsync/_CEFET/tcc/dnar")

source('R/4domain_reactor.R')
source('R/analysis.R')
source('R/crn_reactor.R')
source('R/dsd.R')
source('R/GATE_LIB.R')
source('R/io.R')
source('R/parser.R')

source('R/forced_concentrations.R')

jn <- function(...) { paste(..., sep = '') }


library(ggplot2) # plot()
library(dplyr) # mutate()

React_circuit <- function(circuit) {
  return(react2(
    species   = circuit$species,
    ci        = circuit$ci,
    reactions = circuit$reactions,
    ki        = circuit$ki,
    t         = circuit$t,
    forced_concentrations =  
      list(
        A1 = function(t) sinusoidal_input(t),
        A2 = function(t) saw_wave_input(t),
        A3 = function(t) step_input(t),
        A4 = function(t) pulse_input(t),
        A5 = function(t) square_input(t)
      )
  )
  )
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

Plot_behavior <- function(result, circuit, intercept=FALSE, min, max) {
  g <- plot_behavior(result, chart_title = 'Template',
                     species = circuit$species,
                     species_dotted = c(),
                     x_label     = 'Time (s)',
                     y_label     = 'Concentration (M)',
                     legend_name = 'Species'
  )
  
  if (intercept){
    g <- g + geom_hline(yintercept=min, linetype="dashed", color = "green", size=1)
    g <- g + geom_hline(yintercept=max, linetype="dashed", color = "red", size=1)
    
  }
  
  print(g)
  
}

Make_Generic <- function() {
  timing <-  seq(0, 100, length.out = 1000)
  rate <- 1e2
  
  test1 = c()
  species <- c('A1', 'A2', 'A3', 'A4', 'A5')
  test1$species = species 
  test1$ci = rep(1, times = length(species))
  reactions = list()
  reactions[[length(reactions) + 1]] <- 'w -> w'
  test1$reactions <- reactions
  test1$ki = rep(rate, times = length(reactions))
  
  
  circuit <- test1
  circuit$t <-timing
  
  
  return (circuit)
}

circuit <- Make_Generic()

result_crn <- React_circuit(circuit)
print(circuit)

intercept = FALSE
expected_value = 3
minimum = expected_value * 0.95
maximum = expected_value * 1.05

Plot_behavior(result_crn, circuit, intercept , minimum, maximum)
#Plot_all_behaviors(result_crn, circuit, num_gate, minimum, maximum, TRUE)

#resultado_4dom <- React_4domain_circuit(circuit)
#
#Plot_behavior(resultado_4dom$behavior, circuit, num_gate, minimum, maximum, TRUE)
#
#resultado_comb <- Compare_behaviors(resultado_crn, resultado_4dom, circuit, num_gate)
#
#p1 <- Plot_behavior_comb(resultado_comb, circuit, num_gate, minimum, maximum, TRUE)

