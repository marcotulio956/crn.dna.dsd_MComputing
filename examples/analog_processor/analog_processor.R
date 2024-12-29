# _____________________________________________________________________________
# The R script to implement in DNAr simulator an DNA analog processor circuit
# with 4 selected inputs.
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

# FUNCTION: Make_analog_processor
# DESCRIPTION: Call of functions to assemble the DNA analog processor circuit
# with 2 inputs and 1 bias

Make_analog_processor <- function(cinput1, cselect11, crange11,
                                  cselect12, crange12, cselect13, crange13,
                                  cinput2, cselect21, crange21,
                                  cselect22, crange22, cselect23, crange23,
                                  cinput3, cselect31, crange31,
                                  cselect32, crange32, cselect33, crange33,
                                  cselect41, crange41,
                                  cselect42, crange42, cselect43, crange43,
                                  cinput4, cselect51, crange51,
                                  cselect52, crange52, cselect53, crange53,
                                  cselect61, crange61,
                                  cselect62, crange62, cselect63, crange63,
                                  crange, crange1) {

  nameInput1 = 'Input1'
  nameInput2 = 'Input2'
  nameInput3 = 'Input3'
  nameInput4 = 'Input4'

  nameSelect11 = 'S11'
  nameSelect12 = 'S12'
  nameSelect13 = 'S13'
  nameSelect21 = 'S21'
  nameSelect22 = 'S22'
  nameSelect23 = 'S23'
  nameSelect31 = 'S31'
  nameSelect32 = 'S32'
  nameSelect33 = 'S33'
  nameSelect41 = 'S41'
  nameSelect42 = 'S42'
  nameSelect43 = 'S43'
  nameSelect51 = 'S51'
  nameSelect52 = 'S52'
  nameSelect53 = 'S53'
  nameSelect61 = 'S61'
  nameSelect62 = 'S62'
  nameSelect63 = 'S63'

  nameOutput11 = 'In11'
  nameOutput12 = 'In12'
  nameOutput13 = 'In13'
  nameOutput21 = 'In21'
  nameOutput22 = 'In22'
  nameOutput23 = 'In23'
  nameOutput31 = 'In31'
  nameOutput32 = 'In32'
  nameOutput33 = 'In33'
  nameOutput41 = 'In41'
  nameOutput42 = 'In42'
  nameOutput43 = 'In43'
  nameOutput51 = 'In51'
  nameOutput52 = 'In52'
  nameOutput53 = 'In53'
  nameOutput61 = 'In61'
  nameOutput62 = 'In62'
  nameOutput63 = 'In63'

  nameOutput1 = 'Out1'
  nameOutput2 = 'Out2'
  nameOutput3 = 'Out3'

  # Initialize the frame 1
  select11_gate1 <- Make_Buffer_Lakin(nameSelect11, nameInput1, nameOutput11,
                                      cinput1, cselect11, crange11, 2e-3)
  select12_gate2 <- Make_Buffer_Lakin(nameSelect12, nameInput1, nameOutput1,
                                      cinput1, cselect12, crange12, 2e-3)
  select13_gate3 <- Make_Buffer_Lakin(nameSelect13, nameInput1, nameOutput13,
                                      cinput1, cselect13, crange13, 2e-3)

  # Initialize the frame 2
  select21_gate4 <- Make_Buffer_Lakin(nameSelect21, nameInput2, nameOutput21,
                                      cinput2, cselect21, crange21, 2e-3)
  select22_gate5 <- Make_Buffer_Lakin(nameSelect22, nameInput2, nameOutput22,
                                      cinput2, cselect22, crange22, 2e-3)
  select23_gate6 <- Make_Buffer_Lakin(nameSelect23, nameInput2, nameOutput23,
                                      cinput2, cselect23, crange23, 2e-3)

  add1_gate7 <- Make_Adder2In_Song('add1', nameOutput11, nameOutput21, nameOutput1,
                                   0, 0, crange, 2e-3)
  sub1_gate8 <- Make_Sub2In_Song('sub1', nameOutput1, nameOutput22,
                                   0, 0, crange, 2e-3)
  mul1_gate9 <- Make_Mult2In_Song('mul1', nameOutput13, nameOutput23, nameOutput1,
                                   0, 0, crange, 2e-3)

  # Initialize the frame 3
  select31_gate10 <- Make_Buffer_Lakin(nameSelect31, nameInput3, nameOutput31,
                                      cinput3, cselect31, crange31, 2e-3)
  select32_gate11 <- Make_Buffer_Lakin(nameSelect32, nameInput3, nameOutput2,
                                      cinput3, cselect32, crange32, 2e-3)
  select33_gate12 <- Make_Buffer_Lakin(nameSelect33, nameInput3, nameOutput33,
                                      cinput3, cselect33, crange33, 2e-3)

  # Initialize the frame 4
  select41_gate13 <- Make_Buffer_Lakin(nameSelect41, nameOutput1, nameOutput41,
                                       0, cselect41, crange41, 2e-3)
  select42_gate14 <- Make_Buffer_Lakin(nameSelect42, nameOutput1, nameOutput42,
                                       0, cselect42, crange42, 2e-3)
  select43_gate15 <- Make_Buffer_Lakin(nameSelect43, nameOutput1, nameOutput43,
                                       0, cselect43, crange43, 2e-3)

  add2_gate16 <- Make_Adder2In_Song('add2', nameOutput31, nameOutput41, nameOutput2,
                                   0, 0, crange, 2e-3)
  sub2_gate17 <- Make_Sub2In_Song('sub2', nameOutput2, nameOutput42,
                                 0, 0, crange, 2e-3)
  mul2_gate18 <- Make_Mult2In_Song('mul2', nameOutput33, nameOutput43, nameOutput2,
                                  0, 0, crange, 2e-3)

  # Initialize the frame 5
  select51_gate19 <- Make_Buffer_Lakin(nameSelect51, nameInput4, nameOutput51,
                                       cinput4, cselect51, crange51, 2e-3)
  select52_gate20 <- Make_Buffer_Lakin(nameSelect52, nameInput4, nameOutput3,
                                       cinput4, cselect52, crange52, 2e-3)
  select53_gate21 <- Make_Buffer_Lakin(nameSelect53, nameInput4, nameOutput53,
                                       cinput4, cselect53, crange53, 2e-3)

  # Initialize the frame 6
  select61_gate22 <- Make_Buffer_Lakin(nameSelect61, nameOutput2, nameOutput61,
                                       0, cselect61, crange61, 2e-3)
  select62_gate23 <- Make_Buffer_Lakin(nameSelect62, nameOutput2, nameOutput62,
                                       0, cselect62, crange62, 2e-3)
  select63_gate24 <- Make_Buffer_Lakin(nameSelect63, nameOutput2, nameOutput63,
                                       0, cselect63, crange63, 2e-3)

  add3_gate25 <- Make_Adder2In_Song('add3', nameOutput51, nameOutput61, nameOutput3,
                                   0, 0, crange, 2e-3)
  sub3_gate26 <- Make_Sub2In_Song('sub3', nameOutput3, nameOutput62,
                                 0, 0, crange, 2e-3)
  mul3_gate27 <- Make_Mult2In_Song('mul3', nameOutput53, nameOutput63, nameOutput3,
                                  0, 0, crange, 2e-3)

  # Sets start, end, and number of simulation points
  temporizacao  <- seq(0, 1.6e5, length.out = 50) # Using 50 time points

  # Creates an empty circuit with the defined timing
  circuito <- DNArLogic::make_circuit(temporizacao)

  # Adds the gates to the circuit and precompiles
  circuito <- DNArLogic::circuit_add_gate(circuito, select11_gate1)   #1
  circuito <- DNArLogic::circuit_add_gate(circuito, select12_gate2)   #2
  circuito <- DNArLogic::circuit_add_gate(circuito, select13_gate3)   #3
  circuito <- DNArLogic::circuit_add_gate(circuito, select21_gate4)   #4
  circuito <- DNArLogic::circuit_add_gate(circuito, select22_gate5)   #5
  circuito <- DNArLogic::circuit_add_gate(circuito, select23_gate6)   #6
  circuito <- DNArLogic::circuit_add_gate(circuito, add1_gate7)       #7
  circuito <- DNArLogic::circuit_add_gate(circuito, sub1_gate8)       #8
  circuito <- DNArLogic::circuit_add_gate(circuito, mul1_gate9)       #9
  circuito <- DNArLogic::circuit_add_gate(circuito, select31_gate10)  #10
  circuito <- DNArLogic::circuit_add_gate(circuito, select32_gate11)  #11
  circuito <- DNArLogic::circuit_add_gate(circuito, select33_gate12)  #12
  circuito <- DNArLogic::circuit_add_gate(circuito, select41_gate13)  #13
  circuito <- DNArLogic::circuit_add_gate(circuito, select42_gate14)  #14
  circuito <- DNArLogic::circuit_add_gate(circuito, select43_gate15)  #15
  circuito <- DNArLogic::circuit_add_gate(circuito, add2_gate16)      #16
  circuito <- DNArLogic::circuit_add_gate(circuito, sub2_gate17)      #17
  circuito <- DNArLogic::circuit_add_gate(circuito, mul2_gate18)      #18
  circuito <- DNArLogic::circuit_add_gate(circuito, select51_gate19)  #19
  circuito <- DNArLogic::circuit_add_gate(circuito, select52_gate20)  #20
  circuito <- DNArLogic::circuit_add_gate(circuito, select53_gate21)  #21
  circuito <- DNArLogic::circuit_add_gate(circuito, select61_gate22)  #22
  circuito <- DNArLogic::circuit_add_gate(circuito, select62_gate23)  #23
  circuito <- DNArLogic::circuit_add_gate(circuito, select63_gate24)  #24
  circuito <- DNArLogic::circuit_add_gate(circuito, add3_gate25)      #25
  circuito <- DNArLogic::circuit_add_gate(circuito, sub3_gate26)      #26
  circuito <- DNArLogic::circuit_add_gate(circuito, mul3_gate27)      #27

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
                                    #circuito$gates[[numero]]$species$output1
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
  #resultado_comb[jn(circuito$gates[[numero]]$species$output1 , '-DNA')] <-
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
                                            circuito$gates[[numero]]$species$input2,
                                            jn(circuito$gates[[numero]]$species$input2, '-DNA')
                                            #circuito$gates[[numero]]$species$input3,
                                            #jn(circuito$gates[[numero]]$species$input3, '-DNA'),
                                            #circuito$gates[[numero]]$species$input4,
                                            #jn(circuito$gates[[numero]]$species$input4, '-DNA'),

                                            #circuito$gates[[numero]]$species$output,
                                            #jn(circuito$gates[[numero]]$species$output, '-DNA')
                                            #circuito$gates[[numero]]$species$output1,
                                            #jn(circuito$gates[[numero]]$species$output1, '-DNA')
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

# Restrictions: multiplicator range
crange = 8

# Creates the circuit to be simulated >> Input4 − Input3 ∗ (Input1 + Input2)
circuito <- Make_analog_processor(1,crange,crange,0,0,0,0,1,crange,crange,0,0,0,0,
                                  3,0,0,0,0,crange,crange,0,0,0,0,crange,crange,
                                  7,0,0,crange,crange,0,0,0,0,crange,crange,0,0,
                                  crange, crange1)

# Reacts circuit CRNs in DNAr
resultado_crn <- React_circuit(circuito)

# Rescals seconds -> hours
resultado_crn <- resultado_crn %>% mutate(time = time/3600)

# Sets the minimum and maximum threshold of the expected result
expected_value = 1
minimum = expected_value * 0.95   # -5%
maximum = expected_value * 1.05   # +5%

# Gate number to be plotted
num_gate = 26

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
ggplot2::ggsave(filename="examples/analog_processor/analog_processor_plot_sub3_on.png", plot=p1, device="png")
ggplot2::ggsave(filename="examples/analog_processor/analog_processor_plot_sub3_on.pdf", plot=p1, device="pdf")
