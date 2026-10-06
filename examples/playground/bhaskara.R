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


Make_bhaskara <- function(cinput1, cinput2, cinput3, cinput4, cinput5, cinput6) {

    # ax 3, bx 8, cx 4, dx 4, ex 2, fx 1
    # -b + sqrt(b^2 -4*a*c)  /  2*a
    # -b = bx * fx  2*a=ex*ax   -4*a*c=dx*ax*cx   

    # Initializes the gates
    in1_bx_exp_x2 <- Make_Exp2_Wang('g1', 'in2_bx', 'ebx', cinput2, 1e-3)
    in2_3_dc_ax_mul_oda <- Make_Mul2In_Wang('g2', 'in4_dx', 'in1_ax', 'oda', cinput4, cinput1, 1e-3)
    oda_cx_mul_odac_ <- Make_Mul2In_Wang('g3', 'oda', 'in3_cx', 'odac', 0, cinput3, 1e-3)
    ebx_sub_odac_osx <- Make_Sub2In_Wang('g4', 'ebx', 'odac', 'osx', 0, 0, 3, 1e-3)
    osx_sqrr_oqx <- Make_Sqrt2_Wang('g5', 'osx', 'oqx', 0, 1e-3)
    fx_mul_bx_ofb <- Make_Mul2In_Wang('g6', 'in6_fx', 'in2_bx', 'ofb', cinput6, cinput2, 1e-3)
    ofb_add_oqx <- Make_Adder2In_Wang('g7', 'ofb', 'oqx', 'oax', 0, 0, 2, 1e-3)
    ex_mul_ax <- Make_Mul2In_Wang('g8', 'in5_ex', 'in1_ax', 'oea', cinput5, cinput1, 1e-3)
    oax_div_oea_odx <- Make_Div2In_Wang('g9', 'oax', 'oea', 'odx', 0, 0, 10, 1e-3)

    # Sets start, end, and number of simulation points
    temporizacao  <- seq(0, 1.6e4, length.out = 50) # Using 50 time points

    # Creates an empty circuit with the defined timing
    circuito <- DNArLogic::make_circuit(temporizacao)

    # Adds the gates to the circuit and precompiles
    circuito <- DNArLogic::circuit_add_gate(circuito, in1_bx_exp_x2)
    circuito <- DNArLogic::circuit_add_gate(circuito, in2_3_dc_ax_mul_oda)
    circuito <- DNArLogic::circuit_add_gate(circuito, oda_cx_mul_odac_)
    circuito <- DNArLogic::circuit_add_gate(circuito, ebx_sub_odac_osx)
    circuito <- DNArLogic::circuit_add_gate(circuito, osx_sqrr_oqx)
    circuito <- DNArLogic::circuit_add_gate(circuito, fx_mul_bx_ofb)
    circuito <- DNArLogic::circuit_add_gate(circuito, ofb_add_oqx)
    circuito <- DNArLogic::circuit_add_gate(circuito, ex_mul_ax)
    circuito <- DNArLogic::circuit_add_gate(circuito, oax_div_oea_odx)

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
# Install_And_Load()

# Creates the circuit to be simulated

circuito <- Make_bhaskara(3, 8, 4, 4, 2, 1) # a,b e c>0; b^2 > 4ac e b>sqrt(delta)

# Reacts circuit CRNs in DNAr
resultado_crn <- React_circuit(circuito)

# Rescaling seconds -> hours
#resultado_crn <- resultado_crn %>% mutate(time = time/3600)

# Sets the minimum and maximum threshold of the expected result
# expected_value = 0
# minimum = expected_value * 0.0
# maximum = expected_value * 10

# Gate number to be plotted
# num_gate = 9

# Plots the CRN simulation graph in DNAr
# Use TRUE if integrator; FALSE otherwise
# Plot_behavior(resultado_crn, circuito, num_gate, minimum, maximum, TRUE)

# Converts CRN to DSD reactions
# resultado_4dom <- React_4domain_circuit(circuito)

# Rescaling seconds -> hours
#resultado_4dom$behavior <- resultado_4dom$behavior %>% mutate(time = time/3600)

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

