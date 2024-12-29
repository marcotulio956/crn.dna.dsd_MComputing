# _____________________________________________________________________________
# The R script to implement in DNAr simulator an DNA analog circuit to solve the
# triple integration of the signal
# @author Poliana A. C. Oliveira
# @copyright 2023
# _____________________________________________________________________________

# FUNCTION: Install_And_Load
# DESCRIPTION: Helper function to check if all used packages are installed,
# if not, install and load them.

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

# _____________________________________________________________________________

# Function created to concatenate strings

jn <- function(...) { paste(..., sep = '') }

# _____________________________________________________________________________

# FUNCTION: Make_triple_integrator_circuit
# DESCRIPTION: Call of functions to assemble the DNA analog circuit that solves
# the proposed three integrators in series

Make_triple_integrator_circuit <- function(cinput1, cinput2) {

  # Initializes the gates
  intg_gate1_oy <- Make_Integrator_OishiYordanov('intg1', 'U', 'Up',
                                                 'Yp', 'Yn', cinput1, cinput2, 1e-3)
  intg_gate2_oy <- Make_Integrator_OishiYordanov('intg2', 'Yn', 'Yp',
                                                 'Xp', 'Xn', 0 , 0, 1e-3)

  intg_gate3_oy <- Make_Integrator_OishiYordanov('intg3', 'Xn', 'Xp',
                                                 'Zp', 'Zn', 0 , 0, 1e-3)

  # Sets start, end, and number of simulation points
  temporizacao  <- seq(0, 1.6e4, length.out = 50) # Using 50 time points

  # Creates an empty circuit with the defined timing
  circuito <- DNArLogic::make_circuit(temporizacao)

  # Adds the gates to the circuit and precompiles
  circuito <- DNArLogic::circuit_add_gate(circuito, intg_gate1_oy)
  circuito <- DNArLogic::circuit_add_gate(circuito, intg_gate2_oy)
  circuito <- DNArLogic::circuit_add_gate(circuito, intg_gate3_oy)

  return (circuito)

}

# _____________________________________________________________________________

# FUNCTION: React_circuit
# DESCRIPTION: Call react function of the DNAr to built the CRN reactions of the circuit

React_circuit <- function(circuito) {
  return(react(
    species   = circuito$species,
    ci        = circuito$ci,
    reactions = circuito$reactions,
    ki        = circuito$ki,
    t         = circuito$t
  ))
}

# _____________________________________________________________________________

# FUNCTION: React_4domain_circuit
# DESCRIPTION: Call react function of the DNAr to built the DSD reactions of the circuit

React_4domain_circuit <- function(circuito) {
  return(react_4domain(
    species   = circuito$species,
    ci        = circuito$ci,
    reactions = circuito$reactions,
    ki        = circuito$ki,
    qmax      = 1e6, # maximum strand displacement rate constant
    cmax      = 1e5, # 1e-4  # starting concentration of auxiliary complexes Gi and Ti
    alpha     = 1,   # DSD timescale versus CRN
    beta      = 1,   # DSD concentration scale versus CRN
    t         = circuito$t
  ))
}

# _____________________________________________________________________________

# FUNCTION: Plot_behavior
# DESCRIPTION: Plot the graph of the CRN simulated circuit in DNAr

Plot_behavior <- function(resultado, circuito, numero, min, max, integrator) {
  if (integrator){
    g <- plot_behavior(resultado, species = c(circuito$gates[[numero]]$species$input,
                                              circuito$gates[[numero]]$species$output),
                       x_label     = 'Time (h)',
                       y_label     = 'Concentration (M)',
                       legend_name = 'Species',
                       geom_list   = c('line', 'point'),
                       variable_line_type = FALSE,
                       variable_point_type = TRUE
    )

  } else {
    g <- plot_behavior(resultado, species = c(circuito$gates[[numero]]$species$input1,
                                              circuito$gates[[numero]]$species$input2,
                                              circuito$gates[[numero]]$species$input3,
                                              circuito$gates[[numero]]$species$input4,
                                              circuito$gates[[numero]]$species$output,
                                              circuito$gates[[numero]]$species$output1,
                                              circuito$gates[[numero]]$species$output2,
                                              circuito$gates[[numero]]$species$output3),
                       x_label     = 'Time (h)',
                       y_label     = 'Concentration (M)',
                       legend_name = 'Species',
                       geom_list   = c('line', 'point'),
                       variable_line_type = FALSE,
                       variable_point_type = TRUE
    )
    g <- g + geom_hline(yintercept=min, linetype="dashed", color = "green", size=1)
    g <- g + geom_hline(yintercept=max, linetype="dashed", color = "red", size=1)

  }

  print(g)

}

# _____________________________________________________________________________

# FUNCTION: Compare_behaviors
# DESCRIPTION: Compares simulation results in CRN and DSD, in addition
# generates a frame with the combination of the results

Compare_behaviors <- function(resultado_crn, resultado_4dom, circuito, numero) {

  cat('\nComparing with dna results:\n')
  diff <- compare_behaviors_nrmse(resultado_crn,
                                  resultado_4dom$behavior
                                  [c('time', resultado_4dom$species)]
  )

  print(diff)

  resultado_comb <- resultado_crn[c('time',
                                    circuito$gates[[numero]]$species$input,
                                    circuito$gates[[numero]]$species$input1,
                                    circuito$gates[[numero]]$species$input2,
                                    circuito$gates[[numero]]$species$input3,
                                    circuito$gates[[numero]]$species$input4,

                                    circuito$gates[[numero]]$species$control1,
                                    circuito$gates[[numero]]$species$control2,

                                    circuito$gates[[numero]]$species$output,
                                    circuito$gates[[numero]]$species$output1,
                                    circuito$gates[[numero]]$species$output2,
                                    circuito$gates[[numero]]$species$output3)]

  resultado_comb[jn(circuito$gates[[numero]]$species$input1 , 'DNA')] <-
    resultado_4dom$behavior[circuito$gates[[numero]]$species$input1]
  resultado_comb[jn(circuito$gates[[numero]]$species$input2 , 'DNA')] <-
    resultado_4dom$behavior[circuito$gates[[numero]]$species$input2]
  resultado_comb[jn(circuito$gates[[numero]]$species$input3 , 'DNA')] <-
    resultado_4dom$behavior[circuito$gates[[numero]]$species$input3]
  resultado_comb[jn(circuito$gates[[numero]]$species$input4 , 'DNA')] <-
    resultado_4dom$behavior[circuito$gates[[numero]]$species$input4]

  resultado_comb[jn(circuito$gates[[numero]]$species$control1 , 'DNA')] <-
    resultado_4dom$behavior[circuito$gates[[numero]]$species$control1]
  resultado_comb[jn(circuito$gates[[numero]]$species$control2 , 'DNA')] <-
    resultado_4dom$behavior[circuito$gates[[numero]]$species$control2]

  resultado_comb[jn(circuito$gates[[numero]]$species$output , 'DNA')] <-
    resultado_4dom$behavior[circuito$gates[[numero]]$species$output]
  resultado_comb[jn(circuito$gates[[numero]]$species$output1 , 'DNA')] <-
    resultado_4dom$behavior[circuito$gates[[numero]]$species$output1]
  resultado_comb[jn(circuito$gates[[numero]]$species$output2 , 'DNA')] <-
    resultado_4dom$behavior[circuito$gates[[numero]]$species$output2]
  resultado_comb[jn(circuito$gates[[numero]]$species$output3 , 'DNA')] <-
    resultado_4dom$behavior[circuito$gates[[numero]]$species$output3]

  return (resultado_comb)

}

# _____________________________________________________________________________

# FUNCTION: Plot_behavior_comb
# DESCRIPTION: Plot the graph of the DSD simulated circuit in DNAr

Plot_behavior_comb <- function(resultado, circuito, numero, min, max, integrator) {

  if (integrator){
    g <- plot_behavior(resultado, species = c(circuito$gates[[numero]]$species$input,
                                              jn(circuito$gates[[numero]]$species$input, 'DNA'),
                                              circuito$gates[[numero]]$species$output,
                                              jn(circuito$gates[[numero]]$species$output, 'DNA')
    ),
    x_label     = 'Time (h)',
    y_label     = 'Concentration (M)',
    legend_name = 'Species',
    geom_list   = c('line', 'point'),
    variable_line_type = FALSE,
    variable_point_type = TRUE
    ) + scale_color_brewer(palette="Paired")

  } else {
    g <- plot_behavior(resultado, species = c(circuito$gates[[numero]]$species$input1,
                                              jn(circuito$gates[[numero]]$species$input1, '-DNA'),
                                              circuito$gates[[numero]]$species$input2,
                                              jn(circuito$gates[[numero]]$species$input2, '-DNA'),
                                              #circuito$gates[[numero]]$species$input3,
                                              #jn(circuito$gates[[numero]]$species$input3, '-DNA'),
                                              #circuito$gates[[numero]]$species$input4,
                                              #jn(circuito$gates[[numero]]$species$input4, '-DNA'),

                                              circuito$gates[[numero]]$species$output,
                                              jn(circuito$gates[[numero]]$species$output, '-DNA')
                                              #circuito$gates[[numero]]$species$output1,
                                              #jn(circuito$gates[[numero]]$species$output1, '-DNA'),
                                              #circuito$gates[[numero]]$species$output2,
                                              #jn(circuito$gates[[numero]]$species$output2, '-DNA'),
                                              #circuito$gates[[numero]]$species$output3,
                                              #jn(circuito$gates[[numero]]$species$output3, '-DNA')

                                              #circuito$gates[[numero]]$species$control1,
                                              #jn(circuito$gates[[numero]]$species$control1, '-DNA'),
                                              #circuito$gates[[numero]]$species$control2,
                                              #jn(circuito$gates[[numero]]$species$control2, '-DNA')

    ),
    x_label     = 'Time (h)',
    y_label     = 'Concentration (M)',
    legend_name = 'Species',
    geom_list   = c('line', 'point'),
    variable_line_type = FALSE,
    variable_point_type = TRUE
    ) + scale_color_brewer(palette="Paired")
    g <- g + geom_hline(yintercept=min, linetype="dashed", color = "darkgray", size=1)
    g <- g + geom_hline(yintercept=max, linetype="dashed", color = "darkgray", size=1)

  }

  print(g)

}

##################################### MAIN #####################################

# Installs (if needed) and loads the libraries
# Install_And_Load()

# Creates the circuit to be simulated
circuito <- Make_triple_integrator_circuit(2, 4)
# Restriction Up > Un and Up > 0

# Reacts circuit CRNs in DNAr
resultado_crn <- React_circuit(circuito)

