# _____________________________________________________________________________
# The R script to implement in DNAr simulator an DNA analog circuit to solve the
# adapted Bhaskara formula
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

# FUNCTION: Make_bhaskara_circuit
# DESCRIPTION: Call of functions to assemble the DNA analog circuit that solves
# the proposed adapted bhaskara formula

Make_bhaskara_circuit <- function(cinput1, cinput2, cinput3, cinput4, cinput5, cinput6) {

  # Initializes the gates
  expw_gate1 <- Make_Exp2_Wang('exp1w', 'Bx', 'EBx', cinput2, 1e-3)
  mulw_gate2 <- Make_Mul2In_Wang('mul2w', 'Dx', 'Ax', 'Oda', cinput4, cinput1, 1e-3)
  mulw_gate3 <- Make_Mul2In_Wang('mul3w', 'Oda', 'Cx', 'Odac', 0, cinput3, 1e-3)
  subw_gate4 <- Make_Sub2In_Wang('sub4w', 'EBx', 'Odac', 'Osx', 0, 0, 3, 1e-3)
  sqrw_gate5 <- Make_Sqrt2_Wang('sqr5w', 'Osx', 'Oqx', 0, 1e-3)
  mulw_gate6 <- Make_Mul2In_Wang('mul6w', 'Fx', 'Bx', 'Ofb', cinput6, cinput2, 1e-3)
  addw_gate7 <- Make_Adder2In_Wang('add7w', 'Ofb', 'Oqx', 'Oax', 0, 0, 2, 1e-3)
  mulw_gate8 <- Make_Mul2In_Wang('mul8w', 'Ex', 'Ax', 'Oea', cinput5, cinput1, 1e-3)
  divw_gate9 <- Make_Div2In_Wang('div9w', 'Oax', 'Oea', 'Odx', 0, 0, 10, 1e-3)

  # Sets start, end, and number of simulation points
  temporizacao  <- seq(0, 1.6e4, length.out = 50) # Using 50 time points

  # Creates an empty circuit with the defined timing
  circuito <- DNArLogic::make_circuit(temporizacao)

  # Adds the gates to the circuit and precompiles
  circuito <- DNArLogic::circuit_add_gate(circuito, expw_gate1)   #1
  circuito <- DNArLogic::circuit_add_gate(circuito, mulw_gate2)   #2
  circuito <- DNArLogic::circuit_add_gate(circuito, mulw_gate3)   #3
  circuito <- DNArLogic::circuit_add_gate(circuito, subw_gate4)   #4
  circuito <- DNArLogic::circuit_add_gate(circuito, sqrw_gate5)   #5
  circuito <- DNArLogic::circuit_add_gate(circuito, mulw_gate6)   #6
  circuito <- DNArLogic::circuit_add_gate(circuito, addw_gate7)   #7
  circuito <- DNArLogic::circuit_add_gate(circuito, mulw_gate8)   #8
  circuito <- DNArLogic::circuit_add_gate(circuito, divw_gate9)   #9

  return (circuito)
}

# _____________________________________________________________________________

# FUNCTION: React_circuit
# DESCRIPTION: Call react function of the DNAr to built the CRN reactions of the circuit

React_circuit <- function(circuito) {
  return(react2(
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

  print(g)

}

##################################### MAIN #####################################

# Installs (if needed) and loads the libraries
Install_And_Load()

# Creates the circuit to be simulated
circuito <- Make_bhaskara_circuit(3, 8, 4, 4, 2, 1)
# Restrictions a,b e c>0; b^2 > 4ac e b>sqrt(delta)

# Reacts circuit CRNs in DNAr
resultado_crn <- React_circuit(circuito)

# Rescals seconds -> hours
resultado_crn <- resultado_crn %>% mutate(time = time/3600)

# Sets the minimum and maximum threshold of the expected result
expected_value = 2
minimum = expected_value * 0.95   # -5%
maximum = expected_value * 1.05   # +5%

# Gate number to be plotted
num_gate = 9

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
p1 <- Plot_behavior_comb(resultado_comb, circuito, num_gate, minimum, maximum)

# Exports the simulation results
ggplot2::ggsave(filename="examples/bhaskara/bhaskara_gate9.png", plot=p1, device="png")
ggplot2::ggsave(filename="examples/bhaskara/bhaskara_gate9.pdf", plot=p1, device="pdf")
