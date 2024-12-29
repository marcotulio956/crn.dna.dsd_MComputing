# Load the libraries
library(DNAr)
library(DNArLogic)
library(DNArAnalog)
library(ggplot2) # plot()
library(dplyr) # mutate()

jn <- function(...) { paste(..., sep = '') }

Make_Resistor <- function(name, t1, t2, vcc, R) {
    rate = 2e-3

    species <- list(
        input1 = jn(name,t1, '_vcc'),
        input2 = name,
        output = jn(name,t2, 'thru'),
        intermediate1 = jn(name, '_D'),
        intermediate2 = jn(name, '_DD')
    )

    ci <- c(vcc, R, 0, 1, 1)

    reactions <- c(
        # Breakdown of the trimolecular reaction into three bimolecular reactions
        # 'z4 + Dw -> Dw + D'
        jn(species$output, ' + ', species$intermediate2, ' -> ',
           species$intermediate2, ' + ', species$intermediate1),

        # 'y4 + D -> Dw'
        jn(species$input2, ' + ', species$intermediate1, ' -> ',
           species$intermediate2),

        # 'Dw -> y4 + D'
        jn(species$intermediate2, ' -> ',
           species$input2, ' + ', species$intermediate1),

        # 'D + x4 -> z4 + x4'
        jn(species$intermediate1, ' + ', species$input1, ' -> ',
           species$output, ' + ', species$input1)

    )

    ki        <- c(rate, rate, rate, rate)

    div_gate_w <- list(
        name      = name,
        species   = species,
        reactions = reactions,
        ci        = ci,
        ki        = ki
    )

    return(div_gate_w)
}

Make_Circuit_Current_Divider <- function(name, cssname, rname1, rname2, ir1, ir2, css, r1, r2, r1_thru, r2_thru) {
    rate = 2e-3
    cfuel = 1

    # Sets start, end, and number of simulation points
    temporizacao  <- seq(0, 1, length.out = 50) # Using 50 time points

    # Creates an empty circuit with the defined timing
    circuito <- DNArLogic::make_circuit(temporizacao)

    # r1_r2_pT = r1 + r2
    r1_r2_pT_name=jn(rname1, rname2, '_pT')
    species <- list(
        input1 = rname1,
        input2 = rname2,
        output = r1_r2_pT_name,
        intermediate1 = jn(name, '_B'),
        intermediate2 = jn(name, '_waste')
    )

    ci <- c(r1, r2, 0, cfuel, 0)

    reactions <- c(
        # 'x1 -> x1 + z1'
        jn(species$input1, ' -> ', species$input1, ' + ', species$output),
        # 'z1 + B -> z1 + z1'
        jn(species$output, ' + ', species$intermediate1, ' -> ',
           species$output, ' + ', species$output),
        # 'y1 -> y1 + B'
        jn(species$input2, ' -> ', species$input2, ' + ', species$intermediate1),
        # 'z1 -> waste'
        jn(species$output, ' -> ', species$intermediate2)
    )

    ki        <- c(rate, rate, rate, rate)

    add_gate_w1 <- list(
        name      = jn(name, 'add1'),
        species   = species,
        reactions = reactions,
        ci        = ci,
        ki        = ki
    )
    circuito <- DNArLogic::circuit_add_gate(circuito, add_gate_w1)
    #

    # r1_css_m = r1 * css
    r1_css_m_name=jn(rname1, cssname)
    species <- list(
        input1 = rname1,
        input2 = cssname,
        output = r1_css_m_name,
        intermediate1 = jn(name, '_waste')
    )

    ci <- c(r1, css, 0, 0)

    reactions <- c(
        # 'x3 + y3 -> x3 + y3 + z3'
        jn(species$input1, ' + ', species$input2, ' -> ', species$input1,
           ' + ', species$input2, ' + ', species$output),
        # 'z3 -> waste'
        jn(species$output, ' -> ', species$intermediate1)
    )

    ki        <- c(rate, rate)

    mul_gate_w1 <- list(
        name      = jn(name,'mul1'),
        species   = species,
        reactions = reactions,
        ci        = ci,
        ki        = ki
    )
    circuito <- DNArLogic::circuit_add_gate(circuito, mul_gate_w1)
    #
    # i2 = (r1 * css)/(r1*r2)
    # i2 = r1_css_m / r1_r2_pT
    species <- list(
        input1 = r1_css_m_name,
        input2 = r1_r2_pT_name,
        output = ir,
        intermediate1 = jn(name, '_D'),
        intermediate2 = jn(name, '_DD')
    )

    ci <- c(0, 0, 0, 1, 1)

    reactions <- c(
        # Breakdown of the trimolecular reaction into three bimolecular reactions
        # 'z4 + Dw -> Dw + D'
        jn(species$output, ' + ', species$intermediate2, ' -> ',
           species$intermediate2, ' + ', species$intermediate1),

        # 'y4 + D -> Dw'
        jn(species$input2, ' + ', species$intermediate1, ' -> ',
           species$intermediate2),

        # 'Dw -> y4 + D'
        jn(species$intermediate2, ' -> ',
           species$input2, ' + ', species$intermediate1),

        # 'D + x4 -> z4 + x4'
        jn(species$intermediate1, ' + ', species$input1, ' -> ',
           species$output, ' + ', species$input1)

    )

    ki        <- c(rate, rate, rate, rate)

    div_gate_w1 <- list(
        name      = ir2,
        species   = species,
        reactions = reactions,
        ci        = ci,
        ki        = ki
    )
    circuito <- DNArLogic::circuit_add_gate(circuito, div_gate_w1)
    #

    # r2_css_m = r2 * css
    r2_css_m_name=jn(rname2, cssname)
    species <- list(
        input1 = rname2,
        input2 = cssname,
        output = r2_css_m_name,
        intermediate1 = jn(name, '_waste')
    )

    ci <- c(r2, css, 0, 0)

    reactions <- c(
        # 'x3 + y3 -> x3 + y3 + z3'
        jn(species$input1, ' + ', species$input2, ' -> ', species$input1,
           ' + ', species$input2, ' + ', species$output),
        # 'z3 -> waste'
        jn(species$output, ' -> ', species$intermediate1)
    )

    ki        <- c(rate, rate)

    mul_gate_w2 <- list(
        name      = jn(name,'mul2'),
        species   = species,
        reactions = reactions,
        ci        = ci,
        ki        = ki
    )
    circuito <- DNArLogic::circuit_add_gate(circuito, mul_gate_w2)
    #

    # ir1 = r2_css_m / r1_r2_pT
    species <- list(
        input1 = r2_css_m_name,
        input2 = r1_r2_pT_name,
        output = ir1,
        intermediate1 = jn(name, '_D'),
        intermediate2 = jn(name, '_DD')
    )

    ci <- c(0, 0, 0, 1, 1)

    reactions <- c(
        # Breakdown of the trimolecular reaction into three bimolecular reactions
        # 'z4 + Dw -> Dw + D'
        jn(species$output, ' + ', species$intermediate2, ' -> ',
           species$intermediate2, ' + ', species$intermediate1),

        # 'y4 + D -> Dw'
        jn(species$input2, ' + ', species$intermediate1, ' -> ',
           species$intermediate2),

        # 'Dw -> y4 + D'
        jn(species$intermediate2, ' -> ',
           species$input2, ' + ', species$intermediate1),

        # 'D + x4 -> z4 + x4'
        jn(species$intermediate1, ' + ', species$input1, ' -> ',
           species$output, ' + ', species$input1)

    )

    ki        <- c(rate, rate, rate, rate)

    div_gate_w2 <- list(
        name      = ir1,
        species   = species,
        reactions = reactions,
        ci        = ci,
        ki        = ki
    )
    circuito <- DNArLogic::circuit_add_gate(circuito, div_gate_w2)

    # ir1 + ir2
    i1_i2=jn(rname1, rname2, 'I')
    species <- list(
        input1 = ir1,
        input2 = ir2,
        output = i1_i2,
        intermediate1 = jn(name, '_B'),
        intermediate2 = jn(name, '_waste')
    )

    ci <- c(0, 0, 0, cfuel, 0)

    reactions <- c(
        # 'x1 -> x1 + z1'
        jn(species$input1, ' -> ', species$input1, ' + ', species$output),
        # 'z1 + B -> z1 + z1'
        jn(species$output, ' + ', species$intermediate1, ' -> ',
           species$output, ' + ', species$output),
        # 'y1 -> y1 + B'
        jn(species$input2, ' -> ', species$input2, ' + ', species$intermediate1),
        # 'z1 -> waste'
        jn(species$output, ' -> ', species$intermediate2)
    )

    ki        <- c(rate, rate, rate, rate)

    add_gate_w2 <- list(
        name      = jn(name, 'add2'),
        species   = species,
        reactions = reactions,
        ci        = ci,
        ki        = ki
    )
    circuito <- DNArLogic::circuit_add_gate(circuito, add_gate_w2)
    #
}

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
    r1 = 2.5
    r2 = 3
    vcc = 5
    ccs = 12e-3

    #g3 <- Make_Adder2In_Wang('I1', 'ci1', 'ci2', 'o1', cinput1, cinput2, 2e-3,  2e-3)

    #g4 <- Make_Mul2In_Wang('mul1w', 'ci1', 'ci2', 'o2', cinput1, cinput2, 1e-3)

    # g6 <- Make_Resistor('r1', 'n1', 'n2', vcc, r1)

    circuito <- Make_Circuit_Current_Divider('cd1', 'cssname', 'r1', 'r2', 'ir1', 'ir2', ccs, r1, r2, r1_thru, r2_thru)

    #g6 <- Make_Sub2In_Song('sub1', 'ci1', 'ci2', cinput1, cinput2, 30, 2e-3) # ci1=ci1-ci2; ci2=ci2-ci2

    # Sets start, end, and number of simulation points
    #temporizacao  <- seq(0, 1.6e5, length.out = 50) # Using 50 time points

    # Creates an empty circuit with the defined timing
    #circuito <- DNArLogic::make_circuit(temporizacao)

    # Adds the gates to the circuit and precompiles
    #circuito <- DNArLogic::circuit_add_gate(circuito, g3)
    #circuito <- DNArLogic::circuit_add_gate(circuito, g4)
    #circuito <- DNArLogic::circuit_add_gate(circuito, g5)
    #circuito <- DNArLogic::circuit_add_gate(circuito, g6)
    #circuito <- DNArLogic::circuit_add_gate(circuito, g7)

    return (circuito)

}


Install_And_Load()

circuito <- Make_Generic()

resultado_crn <- React_circuit(circuito)

expected_value = 0
minimum = expected_value * 0.95
maximum = expected_value * 1.05
num_gate = 1

# Plot_behavior(resultado_crn, circuito, num_gate, minimum, maximum, TRUE)
