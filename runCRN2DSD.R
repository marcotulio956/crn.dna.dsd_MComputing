Load_Local_Lib <- function () {
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
}

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

run_CRN <- function() {
    result <- react_4domain(
        species   = c('A', 'B', 'C'),
        ci        = c(1e3, 1e3, 0),
        reactions = c('A + B -> C'),
        ki        = c(1e-7),
        qmax      = 1e-3,
        cmax      = 1e5,
        alpha     = 1,
        beta      = 1,
        t         = seq(0, 72000, 10)
    )
    behavior <- result$behavior[,1:(3 + 1)]
}


Make_triple_integrator_circuit <- function(cinput1, cinput2, cinput3, cinput4) {

    # Initializes the input u = 2t
    # <- Make_Integrator_OishiYordanov('intg01', 'Ip', 'In','Op', 'On', 2, 0, 1e-3)

    intg_gate01_oy <- Make_Oscillator_Dalchau("dalchau1", 'clk1', 'clk2', 'clk3', 15, 5, 10, 2e-5)

    # Sets start, end, and number of simulation points
    temporizacao  <- seq(0, 1.6e5, length.out = 50) # Using 50 time points

    # Creates an empty circuit with the defined timing
    circuito <- DNArLogic::make_circuit(temporizacao)

    # Adds the gates to the circuit and precompiles
    circuito <- DNArLogic::circuit_add_gate(circuito, intg_gate01_oy)

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
        cmax      = 1e7, # 1e-4  # starting concentration of auxiliary complexes Gi and Ti
        alpha     = 1,   # DSD timescale versus CRN
        beta      = 1,   # DSD concentration scale versus CRN
        t         = circuito$t
    ))
}

# _____________________________________________________________________________

# FUNCTION: Plot_behavior
# DESCRIPTION: Plot the graph of the CRN simulated circuit in DNAr

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
    resultado_comb[jn(circuito$gates[[numero]]$species$output2 , '-DNA')] <-
        resultado_4dom$behavior[circuito$gates[[numero]]$species$output2]
    resultado_comb[jn(circuito$gates[[numero]]$species$output3 , '-DNA')] <-
        resultado_4dom$behavior[circuito$gates[[numero]]$species$output3]

    return (resultado_comb)

}

# _____________________________________________________________________________

# FUNCTION: Plot_behavior_comb
# DESCRIPTION: Plot the graph of the DSD simulated circuit in DNAr

Plot_behavior_comb <- function(resultado, circuito, numero, min, max, integrator) {
    g <- plot_behavior(resultado, species = c(circuito$gates[[numero]]$species$input1,
                                              jn(circuito$gates[[numero]]$species$input1, '-DNA'),
                                              circuito$gates[[numero]]$species$input2,
                                              jn(circuito$gates[[numero]]$species$input2, '-DNA'),
                                              #circuito$gates[[numero]]$species$input3,
                                              #jn(circuito$gates[[numero]]$species$input3, '-DNA'),
                                              #circuito$gates[[numero]]$species$input4,
                                              #jn(circuito$gates[[numero]]$species$input4, '-DNA'),

                                              #circuito$gates[[numero]]$species$output,
                                              #jn(circuito$gates[[numero]]$species$output, '-DNA')
                                              circuito$gates[[numero]]$species$output1,
                                              jn(circuito$gates[[numero]]$species$output1, '-DNA'),
                                              circuito$gates[[numero]]$species$output2,
                                              jn(circuito$gates[[numero]]$species$output2, '-DNA')
                                              #circuito$gates[[numero]]$species$output3,
                                              #jn(circuito$gates[[numero]]$species$output3, '-DNA')

                                              #circuito$gates[[numero]]$species$control1,
                                              #jn(circuito$gates[[numero]]$species$control1, '-DNA'),
                                              #circuito$gates[[numero]]$species$control2,
                                              #jn(circuito$gates[[numero]]$species$control2, '-DNA')

    ),
    x_label     = 'Time (s)',
    y_label     = 'Concentration (M)',
    legend_name = 'Species',
    geom_list   = c('line', 'point'),
    variable_line_type = FALSE,
    variable_point_type = TRUE
    ) + scale_color_brewer(palette="Paired")

    if (!integrator){
        g <- g + geom_hline(yintercept=min, linetype="dashed", color = "darkgray", size=1)
        g <- g + geom_hline(yintercept=max, linetype="dashed", color = "darkgray", size=1)

    }

    print(g)

}

##################################### MAIN #####################################

# Installs (if needed) and loads the libraries
Install_And_Load()

# Creates the circuit to be simulated

circuito <- Make_triple_integrator_circuit(5, 2, 3, 1)

# Restriction Up > Un and Up > 0

# Reacts circuit CRNs in DNAr
resultado_crn <- React_circuit(circuito)

# Rescaling seconds -> hours
#resultado_crn <- resultado_crn %>% mutate(time = time/3600)

# Sets the minimum and maximum threshold of the expected result
expected_value = 0
minimum = expected_value * 0.0
maximum = expected_value * 10

# Gate number to be plotted
num_gate = 1

# Plots the CRN simulation graph in DNAr
# Use TRUE if integrator; FALSE otherwise
Plot_behavior(resultado_crn, circuito, num_gate, minimum, maximum, TRUE)

end

# Converts CRN to DSD reactions
resultado_4dom <- React_4domain_circuit(circuito)

# Rescaling seconds -> hours
resultado_4dom$behavior <- resultado_4dom$behavior %>% mutate(time = time/3600)

# Plots the DSD simulation graph in DNAr
# Use TRUE if integrator; FALSE otherwise
# Plot_behavior(resultado_4dom$behavior, circuito, num_gate, minimum, maximum, FALSE)

# Compares CRN results with DSD
#resultado_comb <- Compare_behaviors(resultado_crn, resultado_4dom, circuito, num_gate)

# Plots both the CRN and DSD simulation graph in DNAr
#p1 <- Plot_behavior_comb(resultado_comb, circuito, num_gate, minimum, maximum, FALSE)

# Exports the simulation results
#ggplot2::ggsave(filename="examples/integrator/integrator_test.png", plot=p1, device="png")
#ggplot2::ggsave(filename="examples/integrator/integrator_test.pdf", plot=p1, device="pdf")

