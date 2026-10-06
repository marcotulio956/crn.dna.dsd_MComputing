# _____________________________________________________________________________
# The R script to implement in DNAr simulator an DNA analog artificial neuron
# network (ann)
# @author Poliana A. C. Oliveira
# @copyright 2023
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
# FUNCTION: Make_neural_circuit
# DESCRIPTION: Call of functions to assemble the DNA analog neural circuit
# with 2 inputs and 1 bias

Make_neural_circuit <- function(cinput1, cinput2, cweight1, cweight2, cweight3,
                                cweight4, cweight5, cweight6, cbias1, cBiasWeight1,
                                cbias2, cBiasWeight2, cbias3, cBiasWeight3) {

  name1 = 'Ne1'
  name2 = 'Ne2'
  name3 = 'Ne3'

  nameInput1 = 'In1'
  nameInput2 = 'In2'

  nameWeight1 = 'We11'
  nameWeight2 = 'We12'
  nameWeight3 = 'We21'
  nameWeight4 = 'We22'
  nameWeight5 = 'We31'
  nameWeight6 = 'We32'

  nameBias1 = 'Bn1'
  nameBias2 = 'Bn2'
  nameBias3 = 'Bn3'
  nameBiasWeight1 = 'WeB1'
  nameBiasWeight2 = 'WeB2'
  nameBiasWeight3 = 'WeB3'

#  nameExpNatural = 'en1'
#  cExpNatural = 2718282e-6

  nameOutput1 = 'X'
  nameOutput2 = 'Y'
  nameOutput3 = 'Z'

  # Initialize the neuron 1
  # Gates to calculate the multiplication between input and weight
  signal_gate1 <- Make_Mul2In_Wang(jn('mul1w', nameInput1, nameWeight1),
                                    nameInput1, nameWeight1, jn(nameOutput1, '1'),
                                    cinput1, cweight1, 1e-3)
  signal_gate2 <- Make_Mul2In_Wang(jn('mul2w', nameInput2, nameWeight3),
                                    nameInput2, nameWeight3, jn(nameOutput1, '2'),
                                    cinput2, cweight3, 1e-3)

  signal_gate3 <- Make_Mul2In_Wang(jn('mul3w', nameBias1, nameBiasWeight1),
                                   nameBias1, nameBiasWeight1, jn(nameOutput1, '3'),
                                   cbias1, cBiasWeight1, 1e-3)

  addw_gate4 <- Make_Adder2In_Wang(jn('add1w', name1),
                                   jn(nameOutput1, '1'), jn(nameOutput1, '2'),
                                   jn(nameOutput1, '4'), 0, 0, 2, 1e-3)

  # x = sum of weights minus perturbation
  subw_gate5 <- Make_Sub2In_Wang(jn('sub1w', name1), jn(nameOutput1, '4'),
                                 jn(nameOutput1, '3'), jn(nameOutput1, '5'),
                                 0, 0, 3, 1e-3)

  # approximate sigmoid x/sqrt(1+x^2)
  # x^2
  expw_gate6 <- Make_Exp2_Wang(jn('exp1w', name1), jn(nameOutput1, '5'),
                               jn(nameOutput1, '6'), 0, 1e-3)
  # (x^2)+1
  addw_gate7 <- Make_Adder2In_Wang(jn('add2w', name1),
                                  jn(nameOutput1, '6'), 'C1',
                                  jn(nameOutput1, '7'), 0, 1, 2, 1e-3)

  #sqrt((x^2)+1)
  sqrw_gate8 <- Make_Sqrt2_Wang(jn('sqr1w', name1), jn(nameOutput1, '7'),
                                jn(nameOutput1, '8'), 0, 1e-3)
  # x/(sqrt((x^2)+1))
  divw_gate9 <- Make_Div2In_Wang(jn('div1w', name1), jn(nameOutput1, '5'),
                                 jn(nameOutput1, '8'), jn(nameOutput1, 'out'),
                                 0, 0, 10, 1e-3)

  # Initialize the neuron 2
  # Gates to calculate the multiplication between input and weight
  signal_gate10 <- Make_Mul2In_Wang(jn('mul1w', nameInput1, nameWeight2),
                                   nameInput1, nameWeight2, jn(nameOutput2, '1'),
                                   cinput1, cweight2, 1e-3)
  signal_gate11 <- Make_Mul2In_Wang(jn('mul2w', nameInput2, nameWeight4),
                                   nameInput2, nameWeight4, jn(nameOutput2, '2'),
                                   cinput2, cweight4, 1e-3)

  signal_gate12 <- Make_Mul2In_Wang(jn('mul3w', nameBias2, nameBiasWeight2),
                                   nameBias2, nameBiasWeight2, jn(nameOutput2, '3'),
                                   cbias2, cBiasWeight2, 1e-3)

  addw_gate13 <- Make_Adder2In_Wang(jn('add1w', name2),
                                   jn(nameOutput2, '1'), jn(nameOutput2, '2'),
                                   jn(nameOutput2, '4'), 0, 0, 2, 1e-3)

  # x = sum of weights minus perturbation
  subw_gate14 <- Make_Sub2In_Wang(jn('sub1w', name2), jn(nameOutput2, '4'),
                                 jn(nameOutput2, '3'), jn(nameOutput2, '5'),
                                 0, 0, 3, 1e-3)

  # approximate sigmoid x/sqrt(1+x^2)
  # x^2
  expw_gate15 <- Make_Exp2_Wang(jn('exp1w', name2), jn(nameOutput2, '5'),
                               jn(nameOutput2, '6'), 0, 1e-3)
  # (x^2)+1
  addw_gate16 <- Make_Adder2In_Wang(jn('add2w', name2),
                                   jn(nameOutput2, '6'), 'C1',
                                   jn(nameOutput2, '7'), 0, 1, 2, 1e-3)

  #sqrt((x^2)+1)
  sqrw_gate17 <- Make_Sqrt2_Wang(jn('sqr1w', name2), jn(nameOutput2, '7'),
                                jn(nameOutput2, '8'), 0, 1e-3)
  # x/(sqrt((x^2)+1))
  divw_gate18 <- Make_Div2In_Wang(jn('div1w', name2), jn(nameOutput2, '5'),
                                 jn(nameOutput2, '8'), jn(nameOutput2, 'out'),
                                 0, 0, 10, 1e-3)

  # Initialize the neuron 3
  # Gates to calculate the multiplication between input and weight
  signal_gate19 <- Make_Mul2In_Wang(jn('mul1w', jn(nameOutput1, 'out'), nameWeight5),
                                    jn(nameOutput1, 'out'), nameWeight5, jn(nameOutput3, '1'),
                                    0, cweight5, 1e-3)
  signal_gate20 <- Make_Mul2In_Wang(jn('mul2w', jn(nameOutput2, 'out'), nameWeight6),
                                    jn(nameOutput2, 'out'), nameWeight6, jn(nameOutput3, '2'),
                                    0, cweight6, 1e-3)

  signal_gate21 <- Make_Mul2In_Wang(jn('mul3w', nameBias3, nameBiasWeight3),
                                    nameBias3, nameBiasWeight3, jn(nameOutput3, '3'),
                                    cbias3, cBiasWeight3, 1e-3)

  addw_gate22 <- Make_Adder2In_Wang(jn('add1w', name3),
                                    jn(nameOutput3, '1'), jn(nameOutput3, '2'),
                                    jn(nameOutput3, '4'), 0, 0, 2, 1e-3)

  # x = sum of weights minus perturbation
  subw_gate23 <- Make_Sub2In_Wang(jn('sub1w', name3), jn(nameOutput3, '4'),
                                  jn(nameOutput3, '3'), jn(nameOutput3, '5'),
                                  0, 0, 3, 1e-3)

  # approximate sigmoid x/sqrt(1+x^2)
  # x^2
  expw_gate24 <- Make_Exp2_Wang(jn('exp1w', name3), jn(nameOutput3, '5'),
                                jn(nameOutput3, '6'), 0, 1e-3)
  # (x^2)+1
  addw_gate25 <- Make_Adder2In_Wang(jn('add2w', name3),
                                    jn(nameOutput3, '6'), 'C1',
                                    jn(nameOutput3, '7'), 0, 1, 2, 1e-3)

  #sqrt((x^2)+1)
  sqrw_gate26 <- Make_Sqrt2_Wang(jn('sqr1w', name3), jn(nameOutput3, '7'),
                                 jn(nameOutput3, '8'), 0, 1e-3)
  # x/(sqrt((x^2)+1))
  divw_gate27 <- Make_Div2In_Wang(jn('div1w', name3), jn(nameOutput3, '5'),
                                  jn(nameOutput3, '8'), jn(nameOutput3, 'out'),
                                  0, 0, 10, 1e-3)

  # Sets start, end, and number of simulation points
  temporizacao  <- seq(0, 1.6e5, length.out = 50) # Using 50 time points

  # Creates an empty circuit with the defined timing
  circuito <- DNArLogic::make_circuit(temporizacao)

  # Adds the gates to the circuit and precompiles
  circuito <- DNArLogic::circuit_add_gate(circuito, signal_gate1)   #1
  circuito <- DNArLogic::circuit_add_gate(circuito, signal_gate2)   #2
  circuito <- DNArLogic::circuit_add_gate(circuito, signal_gate3)   #3
  circuito <- DNArLogic::circuit_add_gate(circuito, addw_gate4)     #4
  circuito <- DNArLogic::circuit_add_gate(circuito, subw_gate5)     #5
  circuito <- DNArLogic::circuit_add_gate(circuito, expw_gate6)     #6
  circuito <- DNArLogic::circuit_add_gate(circuito, addw_gate7)     #7
  circuito <- DNArLogic::circuit_add_gate(circuito, sqrw_gate8)     #8
  circuito <- DNArLogic::circuit_add_gate(circuito, divw_gate9)     #9
  circuito <- DNArLogic::circuit_add_gate(circuito, signal_gate10)  #10
  circuito <- DNArLogic::circuit_add_gate(circuito, signal_gate11)  #11
  circuito <- DNArLogic::circuit_add_gate(circuito, signal_gate12)  #12
  circuito <- DNArLogic::circuit_add_gate(circuito, addw_gate13)    #13
  circuito <- DNArLogic::circuit_add_gate(circuito, subw_gate14)    #14
  circuito <- DNArLogic::circuit_add_gate(circuito, expw_gate15)    #15
  circuito <- DNArLogic::circuit_add_gate(circuito, addw_gate16)    #16
  circuito <- DNArLogic::circuit_add_gate(circuito, sqrw_gate17)    #17
  circuito <- DNArLogic::circuit_add_gate(circuito, divw_gate18)    #18
  circuito <- DNArLogic::circuit_add_gate(circuito, signal_gate19)  #19
  circuito <- DNArLogic::circuit_add_gate(circuito, signal_gate20)  #20
  circuito <- DNArLogic::circuit_add_gate(circuito, signal_gate21)  #21
  circuito <- DNArLogic::circuit_add_gate(circuito, addw_gate22)    #22
  circuito <- DNArLogic::circuit_add_gate(circuito, subw_gate23)    #23
  circuito <- DNArLogic::circuit_add_gate(circuito, expw_gate24)    #24
  circuito <- DNArLogic::circuit_add_gate(circuito, addw_gate25)    #25
  circuito <- DNArLogic::circuit_add_gate(circuito, sqrw_gate26)    #26
  circuito <- DNArLogic::circuit_add_gate(circuito, divw_gate27)    #27

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
                                    #circuito$gates[[numero]]$species$input,
                                    circuito$gates[[numero]]$species$input1,
                                    circuito$gates[[numero]]$species$input2,
                                    #circuito$gates[[numero]]$species$input3,
                                    #circuito$gates[[numero]]$species$input4,

                                    #circuito$gates[[numero]]$species$control1,
                                    #circuito$gates[[numero]]$species$control2,

                                    #circuito$gates[[numero]]$species$weight1,

                                    circuito$gates[[numero]]$species$output
                                    #circuito$gates[[numero]]$species$output1,
                                    #circuito$gates[[numero]]$species$output2,
                                    #circuito$gates[[numero]]$species$output3
                                    )]

  resultado_comb[jn(circuito$gates[[numero]]$species$input1 , '-DNA')] <-
    resultado_4dom$behavior[circuito$gates[[numero]]$species$input1]
   resultado_comb[jn(circuito$gates[[numero]]$species$input2 , '-DNA')] <-
     resultado_4dom$behavior[circuito$gates[[numero]]$species$input2]
  # resultado_comb[jn(circuito$gates[[numero]]$species$input3 , '-DNA')] <-
  #   resultado_4dom$behavior[circuito$gates[[numero]]$species$input3]
  # resultado_comb[jn(circuito$gates[[numero]]$species$input4 , '-DNA')] <-
  #   resultado_4dom$behavior[circuito$gates[[numero]]$species$input4]

  # resultado_comb[jn(circuito$gates[[numero]]$species$control1 , '-DNA')] <-
  #   resultado_4dom$behavior[circuito$gates[[numero]]$species$control1]
  # resultado_comb[jn(circuito$gates[[numero]]$species$control2 , '-DNA')] <-
  #   resultado_4dom$behavior[circuito$gates[[numero]]$species$control2]

  #resultado_comb[jn(circuito$gates[[numero]]$species$weight1 , '-DNA')] <-
  #  resultado_4dom$behavior[circuito$gates[[numero]]$species$weight1]

   resultado_comb[jn(circuito$gates[[numero]]$species$output , '-DNA')] <-
     resultado_4dom$behavior[circuito$gates[[numero]]$species$output]
  # resultado_comb[jn(circuito$gates[[numero]]$species$output1 , '-DNA')] <-
  #  resultado_4dom$behavior[circuito$gates[[numero]]$species$output1]
  # resultado_comb[jn(circuito$gates[[numero]]$species$output2 , 'DNA')] <-
  #   resultado_4dom$behavior[circuito$gates[[numero]]$species$output2]
  # resultado_comb[jn(circuito$gates[[numero]]$species$output3 , '-DNA')] <-
  #   resultado_4dom$behavior[circuito$gates[[numero]]$species$output3]

  return (resultado_comb)

}

# _____________________________________________________________________________

# FUNCTION: Plot_behavior_comb
# DESCRIPTION: Plot the graph of the DSD simulated circuit in DNAr

Plot_behavior_comb <- function(resultado, circuito, numero, min, max) {
  g <- plot_behavior(resultado, species = c(circuito$gates[[numero]]$species$input1,
                                            jn(circuito$gates[[numero]]$species$input1, '-DNA'),
                                            #circuito$gates[[numero]]$species$input2,
                                            #jn(circuito$gates[[numero]]$species$input2, '-DNA'),
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

                                            #circuito$gates[[numero]]$species$weight1,
                                            #jn(circuito$gates[[numero]]$species$weight1, '-DNA')

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
circuito <- Make_neural_circuit(4,3,0.1,0.3,0.2,0.4,1,2,1,0.1,1,0.1,1,0.1)

# Restrictions: xi * wi < 16, because of the multiplication range

# Reacts circuit CRNs in DNAr
resultado_crn <- React_circuit(circuito)

# Rescals seconds -> hours
resultado_crn <- resultado_crn %>% mutate(time = time/3600)

# Sets the minimum and maximum threshold of the expected result
expected_value = 0.9232
minimum = expected_value * 0.95   # -5%
maximum = expected_value * 1.05   # +5%

# Gate number to be plotted
num_gate = 27

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
ggplot2::ggsave(filename="examples/neural_network/neural_network_plot_zout.png", plot=p1, device="png")
ggplot2::ggsave(filename="examples/neural_network/neural_network_plot_zout.pdf", plot=p1, device="pdf")
