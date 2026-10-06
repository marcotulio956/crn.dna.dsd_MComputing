# _____________________________________________________________________________
# The R script to implement in DNAr simulator an DNA analog circuit to solve two
# molecular perceptron neurons with a multiplexer 2x1
# @author Poliana A. C. Oliveira
# @copyright 2022
# _____________________________________________________________________________

# FUNCTION: Install_And_Load
# DESCRIPTION: Helper function to check if all used packages are installed,
# if not, install and load them.

Install_And_Load <- function() {
  if ("devtools" %in% installed.packages()[,"Package"] == FALSE){
    install.packages("devtools")
  }

  library(devtools)

  if ("DNAr" %in% installed.packages()[,"Package"] == FALSE){
    devtools::install_git('https://git.nanocomp.dcc.ufmg.br/dnacomputing/dnar')
  }

  if ("DNArLogic" %in% installed.packages()[,"Package"] == FALSE){
    devtools::install_git('https://git.nanocomp.dcc.ufmg.br/dnacomputing/dnar-logic')
  }

  if ("DNArAnalog" %in% installed.packages()[,"Package"] == FALSE){
    devtools::install_git('https://git.nanocomp.dcc.ufmg.br/dnacomputing/dnar-analog')
  }

  if ("ggplot2" %in% installed.packages()[,"Package"] == FALSE){
    install.packages("ggplot2")
  }

  if ("dplyr" %in% installed.packages()[,"Package"] == FALSE){
    install.packages("dplyr")
  }

  # Load the libraries
  library(DNAr)
  library(DNArLogic)
  library(DNArAnalog)
  library(ggplot2) # plot()
  library(dplyr) # mutate()

}

# _____________________________________________________________________________

# Function created to concatenate strings

jn <- function(...) { paste(..., sep = '') }

# _____________________________________________________________________________

# FUNCTION: Make_neuron_circuit
# DESCRIPTION: Call of functions to assemble the DNA analog circuit that solves
# the proposed molecular perceptron neurons with a multiplexer 2x1

Make_neuron_circuit <- function(cinput1, cweight1, cinput2, cweight2, cinput3,
                                cweight3, cinput4, cweight4, cinput5, cweight5,
                                cinput6, cweight6, cinput7, cweight7) {

  # Initialize the gates of the first neuron
  muls_gate1 <- Make_Mult2In_Song('mul1s', 'Ix1', 'Iw1', 'Oxw1', cinput1, cweight1, 8, 2e-3)
  muls_gate2 <- Make_Mult2In_Song('mul2s', 'Ix2', 'Iw2', 'Oxw2', cinput2, cweight2, 8, 2e-3)
  muls_gate3 <- Make_Mult2In_Song('mul3s', 'Ix3', 'Iw3', 'Oxw3', cinput3, cweight3, 8, 2e-3)
  muls_gate4 <- Make_Mult2In_Song('mul4s', 'Ix4', 'Iw4', 'Oxw4', cinput4, cweight4, 8, 2e-3)
  adds_gate5 <- Make_Adder4In_Song('add5S', 'Oxw1', 'Oxw2', 'Oxw3', 'Oxw4', 'Oxa', 0, 0, 0, 0, 64, 2e-3)
  buff_gate6 <- Make_Buffer_Lakin('buf6l', 'Oxa', 'Oka', 0, 100, 50, 2e-3)

  # Initialize the gates of the second neuron
  muls_gate7 <- Make_Mult2In_Song('mul7s', 'Iy1', 'Iz1', 'Oyw1', cinput5, cweight5, 8, 2e-3)
  muls_gate8 <- Make_Mult2In_Song('mul8s', 'Iy2', 'Iz2', 'Oyw2', cinput6, cweight6, 8, 2e-3)
  muls_gate9 <- Make_Mult2In_Song('mul9s', 'Iy3', 'Iz3', 'Oyw3', cinput7, cweight7, 8, 2e-3)
  adds_gate10 <- Make_Adder3In_Song('add10S', 'Oyw1', 'Oyw2', 'Oyw3', 'Oya', 0, 0, 0, 64, 2e-3)
  buff_gate11 <- Make_Buffer_Lakin('buf11l', 'Oya', 'Oma', 0, 50, 100, 2e-3)

  # Initialize the multiplexer
  mux2_gate12 <- Make_Mux2('mux12','Oka','Oma','Ctr1','Ctr2','Out', 0, 0, 0, 80, 100, 2e-3)

  # Sets start, end, and number of simulation points
  temporizacao  <- seq(0, 1.6e5, length.out = 50) # Using 50 time points

  # Creates an empty circuit with the defined timing
  circuito <- DNArLogic::make_circuit(temporizacao)

  # Adds the gates to the circuit and precompiles
  circuito <- DNArLogic::circuit_add_gate(circuito, muls_gate1)   #1
  circuito <- DNArLogic::circuit_add_gate(circuito, muls_gate2)   #2
  circuito <- DNArLogic::circuit_add_gate(circuito, muls_gate3)   #3
  circuito <- DNArLogic::circuit_add_gate(circuito, muls_gate4)   #4
  circuito <- DNArLogic::circuit_add_gate(circuito, adds_gate5)   #5
  circuito <- DNArLogic::circuit_add_gate(circuito, buff_gate6)   #6

  circuito <- DNArLogic::circuit_add_gate(circuito, muls_gate7)   #7
  circuito <- DNArLogic::circuit_add_gate(circuito, muls_gate8)   #8
  circuito <- DNArLogic::circuit_add_gate(circuito, muls_gate9)   #9
  circuito <- DNArLogic::circuit_add_gate(circuito, adds_gate10)  #10
  circuito <- DNArLogic::circuit_add_gate(circuito, buff_gate11)  #11

  circuito <- DNArLogic::circuit_add_gate(circuito, mux2_gate12)  #12

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
    cmax      = 1e4, # 1e-4  # starting concentration of auxiliary complexes Gi and Ti
    alpha     = 1,   # DSD timescale versus CRN
    beta      = 1,   # DSD concentration scale versus CRN
    t         = circuito$t
  ))
}

# _____________________________________________________________________________

# FUNCTION: Plot_behavior
# DESCRIPTION: Plot the graph of the CRN simulated circuit in DNAr

Plot_behavior <- function(resultado, circuito, numero, min, max) {
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

  resultado_comb[jn(circuito$gates[[numero]]$species$input1 , '-DNA')] <-
    resultado_4dom$behavior[circuito$gates[[numero]]$species$input1]
  resultado_comb[jn(circuito$gates[[numero]]$species$input2 , '-DNA')] <-
    resultado_4dom$behavior[circuito$gates[[numero]]$species$input2]
  resultado_comb[jn(circuito$gates[[numero]]$species$input3 , '-DNA')] <-
    resultado_4dom$behavior[circuito$gates[[numero]]$species$input3]
  resultado_comb[jn(circuito$gates[[numero]]$species$input4 , '-DNA')] <-
    resultado_4dom$behavior[circuito$gates[[numero]]$species$input4]

  resultado_comb[jn(circuito$gates[[numero]]$species$control1 , '-DNA')] <-
    resultado_4dom$behavior[circuito$gates[[numero]]$species$control1]
  resultado_comb[jn(circuito$gates[[numero]]$species$control2 , '-DNA')] <-
    resultado_4dom$behavior[circuito$gates[[numero]]$species$control2]

  resultado_comb[jn(circuito$gates[[numero]]$species$output , '-DNA')] <-
    resultado_4dom$behavior[circuito$gates[[numero]]$species$output]
  resultado_comb[jn(circuito$gates[[numero]]$species$output1 , '-DNA')] <-
    resultado_4dom$behavior[circuito$gates[[numero]]$species$output1]
  resultado_comb[jn(circuito$gates[[numero]]$species$output2 , 'DNA')] <-
    resultado_4dom$behavior[circuito$gates[[numero]]$species$output2]
  resultado_comb[jn(circuito$gates[[numero]]$species$output3 , '-DNA')] <-
    resultado_4dom$behavior[circuito$gates[[numero]]$species$output3]

  return (resultado_comb)

}

# _____________________________________________________________________________

# FUNCTION: Plot_behavior_comb
# DESCRIPTION: Plot the graph of the DSD simulated circuit in DNAr

Plot_behavior_comb <- function(resultado, circuito, numero, min, max) {
  g <- plot_behavior(resultado, species = c(circuito$gates[[numero]]$species$input1,
                                            jn(circuito$gates[[numero]]$species$input1, '-DNA'),
                                            circuito$gates[[numero]]$species$input2,
                                            jn(circuito$gates[[numero]]$species$input2, '-DNA'),
                                            #circuito$gates[[numero]]$species$input3,
                                            #jn(circuito$gates[[numero]]$species$input3, '-DNA'),
                                            #circuito$gates[[numero]]$species$input4,
                                            #jn(circuito$gates[[numero]]$species$input4, '-DNA'),

                                            circuito$gates[[numero]]$species$output,
                                            jn(circuito$gates[[numero]]$species$output, '-DNA'),
                                            #circuito$gates[[numero]]$species$output1,
                                            #jn(circuito$gates[[numero]]$species$output1, '-DNA'),
                                            #circuito$gates[[numero]]$species$output2,
                                            #jn(circuito$gates[[numero]]$species$output2, '-DNA'),
                                            #circuito$gates[[numero]]$species$output3,
                                            #jn(circuito$gates[[numero]]$species$output3, '-DNA')

                                            circuito$gates[[numero]]$species$control1,
                                            jn(circuito$gates[[numero]]$species$control1, '-DNA'),
                                            circuito$gates[[numero]]$species$control2,
                                            jn(circuito$gates[[numero]]$species$control2, '-DNA')

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

  print(g)

}

##################################### MAIN #####################################

# Installs (if needed) and loads the libraries
Install_And_Load()

# Creates the circuit to be simulated
circuito <- Make_neuron_circuit(3,5,4,1,3,3,2,4,3,5,4,1,3,3)
# Restrictions: xi * wi < 16, because of multiplicator range

# Reacts circuit CRNs in DNAr
resultado_crn <- React_circuit(circuito)

# Rescals seconds -> hours
resultado_crn <- resultado_crn %>% mutate(time = time/3600)

# Sets the minimum and maximum threshold of the expected result
expected_value = 14
minimum = expected_value * 0.95   # -5%
maximum = expected_value * 1.05   # +5%

# Gate number to be plotted
num_gate = 12

# Plots the CRN simulation graph in DNAr
Plot_behavior(resultado_crn, circuito, num_gate, minimum, maximum)

# Converts CRN to DSD reactions
resultado_4dom <- React_4domain_circuit(circuito)

# Rescaling seconds -> hours
resultado_4dom$behavior <- resultado_4dom$behavior %>% mutate(time = time/3600)

# Plots the DSD simulation graph in DNAr
Plot_behavior(resultado_4dom$behavior, circuito, num_gate, minimum, maximum)

# Compares CRN results with DSD
resultado_comb <- Compare_behaviors(resultado_crn, resultado_4dom, circuito, num_gate)

# Plots both the CRN and DSD simulation graph in DNAr
Plot_behavior_comb(resultado_comb, circuito, num_gate, minimum, maximum)

# Plots both the CRN and DSD simulation graph in DNAr
p1 <- Plot_behavior_comb(resultado_comb, circuito, num_gate, minimum, maximum)

# Exports the simulation results
ggplot2::ggsave(filename="examples/neurons/neurons_gate12_control2on.png", plot=p1, device="png")
ggplot2::ggsave(filename="examples/neurons/neurons_gate12_control2on.pdf", plot=p1, device="pdf")
