jn <- function(...) { paste(..., sep = '') }

circuit_add_compile_gates <- function(circuit, gates) {
  # Merge gates into the circuit
  for (gate in gates) {
    circuit$gates <- append(circuit$gates, list(gate))
    circuit <- circuit_compile(circuit)
  }
  return(circuit)
}


make_compiled_circuit_from_gates <- function(timing, gates) {
  species <- c()
  ci <- c()
  reactions <- c()
  ki <- c()

  for (gate in gates) {
    species <- append(species, unlist(gate$species, use.names = FALSE))
    ci <- append(ci, unlist(gate$ci, use.names = FALSE))
    reactions <- append(reactions, unlist(gate$reactions, use.names = FALSE))
    ki <- append(ki, unlist(gate$ki, use.names = FALSE))

    not_duplicated_species <- !duplicated(species)
    species <- species[not_duplicated_species]
    ci <- ci[not_duplicated_species]
  }

  list(
    gates = gates,
    species = species,
    ci = ci,
    reactions = reactions,
    ki = ki,
    t = timing
  )
}

#' @export
#'
#' @title Make_Add3In_CRN
#'
#' @description Analog adder with three inputs implemented as a catalytic CRN
#'
#' @usage Make_Add3In_CRN(name, nameInput1, nameInput2, nameInput3, nameOutput,
#'                        cinput1, cinput2, cinput3, rate)
#'
#' @param name         The analog gate/unit name
#' @param nameInput1   The name of input species 1
#' @param nameInput2   The name of input species 2
#' @param nameInput3   The name of input species 3
#' @param nameOutput   The output species name
#' @param cinput1      Initial concentration of input 1
#' @param cinput2      Initial concentration of input 2
#' @param cinput3      Initial concentration of input 3
#' @param rate         Reaction rate constant for all reactions
#'
#' @return A list representing the adder gate with fields:
#'   \itemize{
#'     \item{name:} gate name
#'     \item{species:} list of species names (inputs, output, waste)
#'     \item{reactions:} character vector of CRN reactions
#'     \item{ci:} initial concentrations vector
#'     \item{ki:} vector of rate constants
#'   }
#'
#' @examples
#' Make_Add3In_CRN('add3', 'X1', 'X2', 'X3', 'S', 5, 3, 2, 1e-3)
Make_Add3In <- function(name,
                            nameInput1, nameInput2, nameInput3,
                            nameOutput,
                            cinput1, cinput2, cinput3,
                            rate) {
  species <- list(
    input1       = nameInput1,
    input2       = nameInput2,
    input3       = nameInput3,
    output       = nameOutput,
    waste        = paste0(name, '_waste')
  )

  ci <- c(cinput1, cinput2, cinput3, 0, 0)
  names(ci) <- unlist(species)

  reactions <- c(
    # Input1 -> Input1 + Output
    paste(species$input1, '->', species$input1, '+', species$output),
    # Input2 -> Input2 + Output
    paste(species$input2, '->', species$input2, '+', species$output),
    # Input3 -> Input3 + Output
    paste(species$input3, '->', species$input3, '+', species$output),
    # Output -> Waste
    paste(species$output, '->', species$waste)
  )

  ki <- rep(rate, length(reactions))

  adder3_gate <- list(
    name      = name,
    species   = species,
    reactions = reactions,
    ci        = ci,
    ki        = ki
  )

  return(adder3_gate)
}

Make_Circuit_RLC <- function(name, species_input, species_output, ic, p) { 
  rate_base <- p[1]
  rate_mul1   <- rate_base * p[2]
  rate_mul2   <- rate_base * p[3]
  rate_mul3   <- rate_base * p[4]
  rate_mul4   <- rate_base * p[5]
  rate_int1   <- rate_base * p[6]
  rate_int2   <- rate_base * p[7]
  rate_add3   <- rate_base * p[8]

  gates <- list()

  l_1ol <- jn(name, 'l_1ol')
  l_mul1_p <- jn(name, 'l_mul1_p')
  l_mul1_n <- jn(name, 'l_mul1_n')

  l_mul2_p <- jn(name, 'l_mul2_p')
  l_mul2_n <- jn(name, 'l_mul2_n')
  
  l_mul3_p <- jn(name, 'l_mul3_p')
  l_mul3_n <- jn(name, 'l_mul3_n')

  l_mul4_p <- jn(name, 'l_mul4_p')
  l_mul4_n <- jn(name, 'l_mul4_n')

  l_add3_1_p_carry <- jn(name, 'l_add3_1_p_carry')
  l_add3_1_n_carry <- jn(name, 'l_add3_1_n_carry')

  g_mul1_p <- Make_Mul2In_Wang(jn(name, 'mul1_p'),
    species_input$voltage_positive, l_1ol, l_mul1_p,
    0, 1/ic$inductance,
    rate_mul1
  )
  gates[[length(gates)+1]] <- g_mul1_p
       g_mul1_n <- Make_Mul2In_Wang(jn(name, 'mul1_n'),
        species_input$voltage_negative, l_1ol, l_mul1_n,
        0, 1/ic$inductance,
        rate_mul1
      )
      gates[[length(gates)+1]] <-  g_mul1_n

  g_add3_1_p <- Make_Add3In(
    jn(name, 'g_add3_1_p'),
    l_mul1_p, l_mul2_n, l_mul4_n, l_add3_1_p_carry,
    0, 0, 0,
    rate_add3
  ) 
  gates[[length(gates)+1]] <- g_add3_1_p

      g_add3_1_n <- Make_Add3In(
        jn(name, 'g_add3_1_n'),
        l_mul1_n, l_mul2_p, l_mul4_p, l_add3_1_n_carry,
        0, 0, 0,
        rate_add3
      )
      gates[[length(gates)+1]] <- g_add3_1_n

  g_int1 <- Make_Integrator_OishiYordanov(
    jn(name, 'g_int1'),
    l_add3_1_p_carry, l_add3_1_n_carry,
    species_output$current_positive, species_output$current_negative,
    0, 0,
    rate_int1
  )
  gates[[length(gates)+1]] <- g_int1

  g_mul2_p <- Make_Mul2In_Wang(jn(name, 'mul2_p'),
    species_output$current_positive, jn(name, 'l_rol'), l_mul2_p, # species_output$current_positive, jn(name, 'l_rol'), l_mul1_p,
    0, ic$resistance/ic$inductance,
    rate_mul2
  )
  gates[[length(gates)+1]] <- g_mul2_p
      l_mul2_n <- jn(name, 'l_mul2_n')
      g_mul2_n <- Make_Mul2In_Wang(jn(name, 'mul2_n'),
        species_output$current_negative, jn(name, 'l_rol'), l_mul2_n, # species_output$current_negative, jn(name, 'l_rol'), l_mul1_n,
        0, ic$resistance/ic$inductance,
        rate_mul2
      )
      gates[[length(gates)+1]] <- g_mul2_n

  g_mul3_p <- Make_Mul2In_Wang(jn(name, 'mul3_p'),
    species_output$current_positive, jn(name, '_1oc'), l_mul3_p, # species_output$current_positive, jn(name, '_1oc'), l_mul3_p,
    0, 1/ic$capacitance,
    rate_mul3
  )
  gates[[length(gates)+1]] <- g_mul3_p
      g_mul3_n <- Make_Mul2In_Wang(jn(name, 'mul3_n'),
        species_output$current_negative, jn(name, '_1oc'), l_mul3_n, # species_output$current_negative, jn(name, '_1oc'), l_mul3_n,
        0, 1/ic$capacitance,
        rate_mul3
      )
      gates[[length(gates)+1]] <- g_mul3_n

  g_int2 <- Make_Integrator_OishiYordanov(
    jn(name, 'g_int2'),
    l_mul3_p, l_mul3_n,
    species_output$voltage_positive, species_output$voltage_negative,
    0, 0,
    rate_int2
  )
  gates[[length(gates)+1]] <- g_int2

  g_mul4_p <- Make_Mul2In_Wang(jn(name, 'mul4_p'),
    species_output$voltage_positive, jn(name, '_m1ol'), l_mul4_p,
    0, 1/ic$inductance,
    rate_mul4
  )
  gates[[length(gates)+1]] <- g_mul4_p
      g_mul4_n <- Make_Mul2In_Wang(jn(name, 'mul4_n'),
        species_output$voltage_negative, jn(name, '_m1ol'), l_mul4_n,
        0, 1/ic$inductance,
        rate_mul4
      )
      gates[[length(gates)+1]] <- g_mul4_n

  return (gates)
}


Make_Highpass_Cardelli <- function(name, nameInput1, nameInput2,
                    nameOutput1, nameOutput2,  
                    nameOutput3, nameOutput4, 
                    cinput1, cinput2, 
                    R, L, 
                    rate) {

  species <- list(
    input1 = nameInput1,  # vinp
    input2 = nameInput2,  # vinn
    output1 = nameOutput1,# voutp
    output2= nameOutput2, # voutn
    output3 = nameOutput3,# ioutp
    output4 = nameOutput4 # ioutn
  )

  ci <- c(cinput1, cinput2, 0, 0, 0, 0)

  reactions <- c(
    jn(species$input1, '-> ', species$input1, '+', species$output3), # p
    jn(species$input2, '-> ', species$input2, '+', species$output4), # p
    jn(species$input1, '->', species$input1, '+', species$output1), # q
    jn(species$input2, '->', species$input2, '+', species$output2), # q
    jn(species$output2, '->', species$output2, '+', species$output1), # r
    jn(species$output1, '->', species$output1, '+', species$output2), # r
    jn(species$output4, '->', species$output4, '+', species$output3), # p
    jn(species$output3, '->', species$output3, '+', species$output4), # p
    jn(species$output4 , '->', species$output4, '+', species$output1), # q
    jn(species$output3 , '->', species$output3, '+', species$output2), # q
    jn(species$output3, '+ ', species$output4, '-> 0'), # y
    jn(species$output1, '+', species$output2, '-> 0') # y
    
  )

  rate <- 1
  p <- 1 * rate
  q <- 10000 * p * ( L / R )
  r <- 1 * q 
  y <- r

  ki <- c(p, p, q, q, r, r, p, p, q, q, y, y)

  gate <- list(
    name      = name,
    species   = species,
    reactions = reactions,
    ci        = ci,
    ki        = ki
  )

  return(gate)
}

# PID Control of Biochemical Reaction Networks
#  Max Whitby, Luca Cardelli1, Marta Kwiatkowska1, Luca Laurenti1, Mirco Tribastone2, Max Tschaikowski

Make_Derivative <- function(name, nameInput1, nameInput2,
                    nameOutput1, nameOutput2, cinput1, cinput2, rate) {

  species <- list(
    input1 = nameInput1,   #Ep
    input2 = nameInput2,   #En
    output1 = nameOutput1, # Dp
    output2= nameOutput2, # Dn
    intermediate1 = jn(name, '_Ap'), 
    intermediate2= jn(name, '_An')
  )

  ci <- c(cinput1, cinput2, 0, 0, 0, 0)

  reactions <- c(
    # Ep -rv-> Ep + Ap
    jn(species$input1, ' -> ', species$input1, ' + ', species$intermediate1),
    # Ep -rvs-> Ep + Dp
    jn(species$input1, ' -> ', species$input1, ' + ', species$output1),
    # En -rv-> En + An
    jn(species$input2, ' -> ', species$input2, ' + ', species$intermediate2),
    # En -rvs-> En + Dn
    jn(species$input2, ' -> ', species$input2, ' + ', species$output2),

    # Ap -v-> 0
    jn(species$intermediate1, ' -> 0'),
    # Ap -vs-> Ap + Dn
    jn(species$intermediate1, ' -> ', species$intermediate1, ' + ', species$output2),
    # An -v-> 0
    jn(species$intermediate2, ' -> 0'),
    # An -vs-> An + Dp
    jn(species$intermediate2, ' -> ', species$intermediate2, ' + ', species$output1),

    # Dp -s-> 0
    jn(species$output1, ' -> 0'),
    # Dn -s-> 0
    jn(species$output2, ' -> 0'),
    # Dp + Dn -q-> 0
    jn(species$output1, '+', species$output2, ' -> 0')
  )

  v <- 1
  s <- 1
  vs <- 10
  q <- 10
  rv <- rate*v
  rvs <- rate*vs

  ki        <- c(rv, rvs, rv, rvs, v, vs, v, vs, s, s, q)

  derivative_gate <- list(
    name      = name,
    species   = species,
    reactions = reactions,
    ci        = ci,
    ki        = ki
  )

  return(derivative_gate)
}


Make_Mux2_balanced <- function(name, nameInput1, nameInput2, nameControl1, nameControl2,
                      nameOutput, cinput1, cinput2, control1, control2, crange,
                      rate) {
  species <- list(
    input1 = nameInput1, #E1
    input2 = nameInput2, #E2
    output1 = nameControl1,  #C1
    output2 = nameControl2,  #C2
    gate1E = jn(name, '_GEn1'), #G1E
    gate2E = jn(name, '_GEn2'), #G2E
    gate1U = jn(name, '_GUn1'), #G1U
    gate2U = jn(name, '_GUn2'), #G2U
    output = nameOutput  #Output
  )

  ci <- c(cinput1, cinput2, control1, control2, control1, control2, crange, crange, 0)

  reactions <- c(
    # 'G1U + C1 -> G1E'
    jn(species$gate1U, ' + ', species$output1, ' -> ', species$output1, species$gate1E),
    # 'G1E + C2 -> G1U'
    jn(species$gate1E, ' + ', species$output2, ' -> ', species$output2, species$gate1U),
    # 'G2U + C2 -> G2'
    jn(species$gate2U, ' + ', species$output2, ' -> ', species$output2, species$gate2E),
    # 'G2E + C1 -> G2U'
    jn(species$gate2E, ' + ', species$output1, ' -> ', species$output1, species$gate2U),
    # 'E1 + G1 -> Output'
    jn(species$input1, ' + ', species$gate1E, ' -> ', species$output),
    # 'E2 + G2 -> Output'
    jn(species$input2, ' + ', species$gate2E, ' -> ', species$output)
  )

  ki <- c(rate, rate, rate, rate, rate, rate)

  mux2_gate <- list(
    name      = name,
    species   = species,
    reactions = reactions,
    ci        = ci,
    ki        = ki
  )

  return(mux2_gate)
}


Make_Circuit_Inductor_Iin <- function(name, species_input, species_output, ic, rate) {
  gates <- list()

  l_vep <- jn(name, '_l_vep')
  l_ven <- jn(name, '_l_ven')

  # Derivate the current input
  g_di_in <- Make_Derivative(
    jn(name, 'g_di_in'),
    species_input$current_positive, species_input$current_negative,
    l_vep, l_ven,
    0, 0,
    rate
  )
  gates[[length(gates)+1]] <- g_di_in

  g_pir <- Make_Mul2In_Wang(
    jn(name, 'g_pir'),
    jn(name, 'cte1'),  l_vep, 
    species_output$current_positive,
    ic$inductance, 0,
    rate
  )
  gates[[length(gates)+1]] <- g_pir

      g_nir <- Make_Mul2In_Wang(
        jn(name, 'g_nir'),
        jn(name,'cte2'),  l_ven, 
        species_output$current_negative,
        ic$inductance, 0,
        rate
      )
      gates[[length(gates)+1]] <- g_nir

  l_flux_p <- jn(name, '_l_flux_p')
  l_flux_n <- jn(name, '_l_flux_n')

  g_ve_integrator <- Make_Integrator_OishiYordanov(
    jn(name, '_g_i_int'), 
    species_output$current_positive, species_output$current_negative,
    l_flux_p, l_flux_n,
    0, 0,
    rate
  )
  gates[[length(gates)+1]] <- g_ve_integrator

  # Computer voltage in the inductor
  g_pvl <- Make_Mul2In_Wang(
    jn(name, 'g_pvl'),
    jn(name,'cte3'), l_flux_p,
    species_output$voltage_positive,
    1/ic$inductance, 0,
    rate
  )
  gates[[length(gates)+1]] <- g_pvl    
      g_nvl <- Make_Mul2In_Wang(
        jn(name, 'g_nvl'),
        jn(name, 'cte4'),  l_flux_n,
        species_output$voltage_negative,
        1/ic$inductance, 0,
        rate
      )
      gates[[length(gates)+1]] <- g_nvl

  # Compute Equivalent Voltage  at the Inductor Terminal from Current Source 

  # V_L (RL series) : v_l = L * di/dt, i = 1/L \int v_l dt or i = V_L / R 

  # V_L = V_E (RL parallel) : v_e = R * i_R (by resistor), v_e = L * i_L/dt (by inductor), i_l = 1/L \int V_E dt

  # Compute Flux in Inductor
  # F = \int V_E dt

  # Exract Current through Inductor from Flux
  # i_l = 1/L * F 
  
  # Compute voltage: v = R * di/dt
  
  # Recover the current from the flux: i = (1/L)*F

  return(gates)
}

Make_Capacitor_Component <- function(id, capacitance=1, resistance=1) {
  c1 <- c()
  c1$name <- jn('c', id)
  
  # Component properties
  c1$il$capacitance <- jn(c1$name, '_cap')
  c1$ic$capacitance <- capacitance 
  
  c1$il$resistance <- jn(c1$name, '_res')
  c1$ic$resistance <- resistance 
  
  # Dual rail species for voltage and current
  c1$il$voltage_positive <- jn(c1$name, 'il_vp')
  c1$il$voltage_negative <- jn(c1$name, 'il_vn')
  c1$il$current_positive <- jn(c1$name, 'il_ip')
  c1$il$current_negative <- jn(c1$name, 'il_in')
  
  c1$ol$voltage_positive <- jn(c1$name, 'ol_vp')
  c1$ol$voltage_negative <- jn(c1$name, 'ol_vn')
  c1$ol$current_positive <- jn(c1$name, 'ol_ip')
  c1$ol$current_negative <- jn(c1$name, 'ol_in')
  
  # Initial Conditions
  c1$ic$voltage_positive <- 0
  c1$ic$voltage_negative <- 0
  c1$ic$current_positive <- 0
  c1$ic$current_negative <- 0
  
  return(c1)
}

#' @title Make_Inductor_Component
#' @description Initializes the component properties and dual-rail names for an inductor 
#' in series with an internal resistor.
#' @param id A unique identifier for the component (e.g., 0, 1)
#' @param inductance The inductance value (L)
#' @param resistance The internal resistance value (R)
#' @return A list containing the component's I/O naming structure and initial parameters
Make_Inductor_Component <- function(id, inductance, resistance) {
  
  l1 <- c()
  l1$name <- jn('l', id)
  
  # Parameters
  l1$il$inductance <- jn(l1$name, '_ind')
  l1$ic$inductance <- inductance 
  l1$il$resistance <- jn(l1$name, '_res')
  l1$ic$resistance <- resistance
  
  # Input Dual rail species (Voltage applied to component)
  l1$il$voltage_positive <- jn(l1$name, 'il_vp')
  l1$il$voltage_negative <- jn(l1$name, 'il_vn')
  l1$il$current_positive <- jn(l1$name, 'il_ip')
  l1$il$current_negative <- jn(l1$name, 'il_in')
  
  # Output Dual rail species (Current flowing through, Voltage across inductor)
  l1$ol$voltage_positive <- jn(l1$name, 'ol_vp')
  l1$ol$voltage_negative <- jn(l1$name, 'ol_vn')
  l1$ol$current_positive <- jn(l1$name, 'ol_ip')
  l1$ol$current_negative <- jn(l1$name, 'ol_in')
  
  # Initial Conditions
  l1$ic$voltage_positive <- 0
  l1$ic$voltage_negative <- 0
  l1$ic$current_positive <- 0
  l1$ic$current_negative <- 0
  
  return(l1)
}

Make_RLC_Component <- function(resistance, inductance, capacitance, regime = 'U') {
  rlc <- c()
  rlc$regime <- regime
  # Dual rail species for voltages, and current

  rlc$name <- jn('rlc')
  rlc$il$resistance <- jn(rlc$name,'_red')
  rlc$ic$resistance <- resistance 
  rlc$il$inductance <- jn(rlc$name,'_ind')
  rlc$ic$inductance <- inductance 
  rlc$il$capacitance <- jn(rlc$name,'_cap')
  rlc$ic$capacitance <- capacitance

  rlc$il$voltage_positive <- jn(rlc$name,'il_vp')
  rlc$ic$voltage_positive <- 0
  rlc$il$voltage_negative <- jn(rlc$name,'il_vn')
  rlc$ic$voltage_negative <- 0

  rlc$ol$voltage_positive <- jn(rlc$name,'ol_vcp')
  rlc$ol$voltage_negative <- jn(rlc$name,'ol_vcn')
  rlc$ol$current_positive <- jn(rlc$name,'ol_ip')
  rlc$ol$current_negative <- jn(rlc$name,'ol_in')

  return(rlc)
}

Make_Circuit_RC <- function(name, species_input, species_output, ic, rate) {
  gates <- list()

  # ============================================================
  # Capacitor Model (RC Circuit):
  # V_R = V_in - V_C
  # I = V_R / R
  # dV_C/dt = I / C
  # V_C = Integral(dV_C/dt)
  # ============================================================

  vr_p <- jn(name, '_vr_p')
  vr_n <- jn(name, '_vr_n')
  dummy_0 <- jn(name, '_dummy_0') # Unused input for 3-input adder

  # 1. Subtraction: V_R = V_in - V_C 
  g_add_vr_p <- Make_Add3In(
    jn(name, '_add_vr_p'),
    species_input$voltage_positive, species_output$voltage_negative, dummy_0,
    vr_p,
    0, 0, 0, rate
  )
  gates[[length(gates) + 1]] <- g_add_vr_p

  g_add_vr_n <- Make_Add3In(
    jn(name, '_add_vr_n'),
    species_input$voltage_negative, species_output$voltage_positive, dummy_0,
    vr_n,
    0, 0, 0, rate
  )
  gates[[length(gates) + 1]] <- g_add_vr_n

  # 2. Current: I = V_R * (1/R)
  g_mul_ip <- Make_Mul2In_Wang(
    jn(name, '_mul_ip'),
    vr_p, jn(name, '_1oR'), species_output$current_positive,
    0, 1 / ic$resistance, rate
  )
  gates[[length(gates) + 1]] <- g_mul_ip

  g_mul_in <- Make_Mul2In_Wang(
    jn(name, '_mul_in'),
    vr_n, jn(name, '_1oR'), species_output$current_negative,
    0, 1 / ic$resistance, rate
  )
  gates[[length(gates) + 1]] <- g_mul_in

  # 3. Capacitor Derivative: dV_C/dt = I * (1/C)
  dvc_p <- jn(name, '_dvc_p')
  dvc_n <- jn(name, '_dvc_n')

  g_mul_dvcp <- Make_Mul2In_Wang(
    jn(name, '_mul_dvcp'),
    species_output$current_positive, jn(name, '_1oC'), dvc_p,
    0, 1 / ic$capacitance, rate
  )
  gates[[length(gates) + 1]] <- g_mul_dvcp

  g_mul_dvcn <- Make_Mul2In_Wang(
    jn(name, '_mul_dvcn'),
    species_output$current_negative, jn(name, '_1oC'), dvc_n,
    0, 1 / ic$capacitance, rate
  )
  gates[[length(gates) + 1]] <- g_mul_dvcn

  # 4. Capacitor Voltage Integration: V_C = Integral(dV_C/dt)
  g_int_vc <- Make_Integrator_OishiYordanov(
    jn(name, '_int_vc'),
    dvc_p, dvc_n,
    species_output$voltage_positive, species_output$voltage_negative,
    0, 0, rate
  )
  gates[[length(gates) + 1]] <- g_int_vc

  return(gates)
}


Make_Circuit_Pure_Capacitor <- function(name, species_input, species_output, ic, rate) {
  gates <- list()

  # ============================================================
  # Pure Capacitor Model:
  # dv/dt = Derivative(V_in)
  # i = C * dv/dt
  # Q = Integral(i)
  # V_C = Q / C
  # ============================================================

  # ------------------------------------------------------------
  # 1. Derivative: dV_in/dt
  # ------------------------------------------------------------
  dv_p <- jn(name, '_dv_p')
  dv_n <- jn(name, '_dv_n')

  g_dv <- Make_Derivative(
    jn(name, '_derivative'),
    species_input$voltage_positive, species_input$voltage_negative,
    dv_p, dv_n,
    ic$voltage_positive, ic$voltage_negative, 
    rate
  )
  gates[[length(gates) + 1]] <- g_dv

  # ------------------------------------------------------------
  # 2. Capacitor Current: i = C * dV/dt
  # ------------------------------------------------------------
  g_ip <- Make_Mul2In_Wang(
    jn(name, '_mul_ip'),
    dv_p, jn(name, '_C'), species_output$current_positive,
    0, ic$capacitance, rate
  )
  gates[[length(gates) + 1]] <- g_ip

  g_in <- Make_Mul2In_Wang(
    jn(name, '_mul_in'),
    dv_n, jn(name, '_C'), species_output$current_negative,
    0, ic$capacitance, rate
  )
  gates[[length(gates) + 1]] <- g_in

  # ------------------------------------------------------------
  # 3. Integrate Current for Charge: Q = Integral(i dt)
  # ------------------------------------------------------------
  q_p <- jn(name, '_q_p')
  q_n <- jn(name, '_q_n')

  g_int_q <- Make_Integrator_OishiYordanov(
    jn(name, '_int_q'),
    species_output$current_positive, species_output$current_negative,
    q_p, q_n,
    0, 0, # Initial charge assumed 0
    rate
  )
  gates[[length(gates) + 1]] <- g_int_q

  # ------------------------------------------------------------
  # 4. Output Capacitor Voltage: V_C = Q / C
  # ------------------------------------------------------------
  g_vp <- Make_Mul2In_Wang(
    jn(name, '_mul_vp'),
    q_p, jn(name, '_1oC'), species_output$voltage_positive,
    0, 1 / ic$capacitance, rate
  )
  gates[[length(gates) + 1]] <- g_vp

  g_vn <- Make_Mul2In_Wang(
    jn(name, '_mul_vn'),
    q_n, jn(name, '_1oC'), species_output$voltage_negative,
    0, 1 / ic$capacitance, rate
  )
  gates[[length(gates) + 1]] <- g_vn

  return(gates)
}

Make_Circuit_Pure_Inductor <- function(name, species_input, species_output, ic, rate) {
  gates <- list()

  # ============================================================
  # Pure Inductor Model:
  # di/dt = V_in / L
  # i = Integral(di/dt)
  # V_L = L * di/dt (Reconstructed Output Voltage)
  # ============================================================

  # ------------------------------------------------------------
  # 1. Rate of Change of Current: di/dt = V_in * (1/L)
  # ------------------------------------------------------------
  di_p <- jn(name, '_di_p')
  di_n <- jn(name, '_di_n')

  g_dip <- Make_Mul2In_Wang(
    jn(name, '_mul_dip'),
    species_input$voltage_positive, jn(name, '_1oL'), di_p,
    0, 1 / ic$inductance, rate
  )
  gates[[length(gates) + 1]] <- g_dip

  g_din <- Make_Mul2In_Wang(
    jn(name, '_mul_din'),
    species_input$voltage_negative, jn(name, '_1oL'), di_n,
    0, 1 / ic$inductance, rate
  )
  gates[[length(gates) + 1]] <- g_din

  # ------------------------------------------------------------
  # 2. Inductor Current: i = Integral(di/dt dt)
  # ------------------------------------------------------------
  g_int_i <- Make_Integrator_OishiYordanov(
    jn(name, '_int_i'),
    di_p, di_n,
    species_output$current_positive, species_output$current_negative,
    0, 0, # Initial current assumed 0
    rate
  )
  gates[[length(gates) + 1]] <- g_int_i

  # ------------------------------------------------------------
  # 3. Output Inductor Voltage: V_L = di/dt * L
  # ------------------------------------------------------------
  g_vp <- Make_Mul2In_Wang(
    jn(name, '_mul_vp'),
    di_p, jn(name, '_L'), species_output$voltage_positive,
    0, ic$inductance, rate
  )
  gates[[length(gates) + 1]] <- g_vp

  g_vn <- Make_Mul2In_Wang(
    jn(name, '_mul_vn'),
    di_n, jn(name, '_L'), species_output$voltage_negative,
    0, ic$inductance, rate
  )
  gates[[length(gates) + 1]] <- g_vn

  return(gates)
}

#' @title Make_Circuit_RL
#' @description Assembles the analog CRN gates to simulate a parametrized series RL inductor.
#' @param name Component name generated by Make_Inductor_Component
#' @param species_input Dual-rail input species strings
#' @param species_output Dual-rail output species strings
#' @param ic Initial conditions/constants for the circuit component
#' @param rate Global reaction rate
Make_Circuit_RL <- function(name, species_input, species_output, ic, rate) {

  gates <- list()

  # ============================================================
  # Inductor with internal resistance:
  # di/dt = Vin/L - (R/L)*i
  # i = integral(di/dt)
  # V_L = L * di/dt
  # ============================================================

  # ------------------------------------------------------------
  # 1. Multiply Vin by 1/L  ( Vin / L )
  # ------------------------------------------------------------
  vp_over_L <- jn(name, '_vp_over_L')
  vn_over_L <- jn(name, '_vn_over_L')

  g_vp_oL <- Make_Mul2In_Wang(
    jn(name, '_g_vp_oL'),
    species_input$voltage_positive, jn(name, '_1oL'), vp_over_L,
    0, 1 / ic$inductance, rate
  )
  gates[[length(gates) + 1]] <- g_vp_oL

  g_vn_oL <- Make_Mul2In_Wang(
    jn(name, '_g_vn_oL'),
    species_input$voltage_negative, jn(name, '_1oL'), vn_over_L,
    0, 1 / ic$inductance, rate
  )
  gates[[length(gates) + 1]] <- g_vn_oL

  # ------------------------------------------------------------
  # 2. Multiply Output Current by R/L ( i * R/L )
  # ------------------------------------------------------------
  ip_RoL <- jn(name, '_ip_RoL')
  in_RoL <- jn(name, '_in_RoL')

  g_ip_RoL <- Make_Mul2In_Wang(
    jn(name, '_g_ip_RoL'),
    species_output$current_positive, jn(name, '_RoL'), ip_RoL,
    0, ic$resistance / ic$inductance, rate
  )
  gates[[length(gates) + 1]] <- g_ip_RoL

  g_in_RoL <- Make_Mul2In_Wang(
    jn(name, '_g_in_RoL'),
    species_output$current_negative, jn(name, '_RoL'), in_RoL,
    0, ic$resistance / ic$inductance, rate
  )
  gates[[length(gates) + 1]] <- g_in_RoL

  # ------------------------------------------------------------
  # 3. Add parts to compute di/dt
  # di/dt+ = Vin+/L + i- * (R/L)  --> Subtraction uses cross-rail addition
  # di/dt- = Vin-/L + i+ * (R/L)
  # ------------------------------------------------------------
  dip <- jn(name, '_dip')
  din <- jn(name, '_din')

  # Using Make_Add3In with a dummy 0-concentration 3rd input
  g_dip <- Make_Add3In(
    jn(name, '_g_dip'),
    vp_over_L, in_RoL, jn(name, '_dummy1'), dip,
    0, 0, 0, rate
  )
  gates[[length(gates) + 1]] <- g_dip

  g_din <- Make_Add3In(
    jn(name, '_g_din'),
    vn_over_L, ip_RoL, jn(name, '_dummy2'), din,
    0, 0, 0, rate
  )
  gates[[length(gates) + 1]] <- g_din

  # ------------------------------------------------------------
  # 4. Integrate di/dt to output the Inductor Current (i)
  # ------------------------------------------------------------
  g_i_integrator <- Make_Integrator_OishiYordanov(
    jn(name, '_i_integrator'),
    dip, din,
    species_output$current_positive, species_output$current_negative,
    0, 0, rate
  )
  gates[[length(gates) + 1]] <- g_i_integrator

  # ------------------------------------------------------------
  # 5. Output Inductor Voltage: V_L = L * di/dt
  # ------------------------------------------------------------
  g_vl_p <- Make_Mul2In_Wang(
    jn(name, '_g_vl_p'),
    dip, jn(name, '_L'), species_output$voltage_positive,
    0, ic$inductance, rate
  )
  gates[[length(gates) + 1]] <- g_vl_p

  g_vl_n <- Make_Mul2In_Wang(
    jn(name, '_g_vl_n'),
    din, jn(name, '_L'), species_output$voltage_negative,
    0, ic$inductance, rate
  )
  gates[[length(gates) + 1]] <- g_vl_n

  return(gates)
}

Make_Circuit_RL2 <- function(name, species_input, species_output, ic, rate) {
  gates <- list()

  # ============================================================
  # Inductor Model (RL Circuit):
  # Reorganized to match RC topology to prevent 2nd order delays
  # V_R = i * R
  # V_L = V_in - V_R
  # di/dt = V_L / L
  # i = Integral(di/dt)
  # ============================================================

  vr_p <- jn(name, '_vr_p')
  vr_n <- jn(name, '_vr_n')
  dummy_0 <- jn(name, '_dummy_0') # Unused input for 3-input adder

  # ------------------------------------------------------------
  # 1. Resistor Voltage: V_R = i * R
  # ------------------------------------------------------------
  g_mul_vrp <- Make_Mul2In_Wang(
    jn(name, '_mul_vrp'),
    species_output$current_positive, jn(name, '_R'), vr_p,
    0, ic$resistance, rate
  )
  gates[[length(gates) + 1]] <- g_mul_vrp

  g_mul_vrn <- Make_Mul2In_Wang(
    jn(name, '_mul_vrn'),
    species_output$current_negative, jn(name, '_R'), vr_n,
    0, ic$resistance, rate
  )
  gates[[length(gates) + 1]] <- g_mul_vrn

  # ------------------------------------------------------------
  # 2. Subtraction: V_L = V_in - V_R
  # Dual rail subtraction: V_Lp = V_inp + V_Rn; V_Ln = V_inn + V_Rp
  # ------------------------------------------------------------
  g_add_vlp <- Make_Add3In(
    jn(name, '_add_vlp'),
    species_input$voltage_positive, vr_n, dummy_0,
    species_output$voltage_positive, # Output V_L
    0, 0, 0, rate
  )
  gates[[length(gates) + 1]] <- g_add_vlp

  g_add_vln <- Make_Add3In(
    jn(name, '_add_vln'),
    species_input$voltage_negative, vr_p, dummy_0,
    species_output$voltage_negative, # Output V_L
    0, 0, 0, rate
  )
  gates[[length(gates) + 1]] <- g_add_vln

  # ------------------------------------------------------------
  # 3. Inductor Derivative: di/dt = V_L * (1/L)
  # ------------------------------------------------------------
  di_p <- jn(name, '_di_p')
  di_n <- jn(name, '_di_n')

  g_mul_dip <- Make_Mul2In_Wang(
    jn(name, '_mul_dip'),
    species_output$voltage_positive, jn(name, '_1oL'), di_p,
    0, 1 / ic$inductance, rate
  )
  gates[[length(gates) + 1]] <- g_mul_dip

  g_mul_din <- Make_Mul2In_Wang(
    jn(name, '_mul_din'),
    species_output$voltage_negative, jn(name, '_1oL'), di_n,
    0, 1 / ic$inductance, rate
  )
  gates[[length(gates) + 1]] <- g_mul_din

  # ------------------------------------------------------------
  # 4. Inductor Current Integration: i = Integral(di/dt)
  # ------------------------------------------------------------
  g_int_i <- Make_Integrator_OishiYordanov(
    jn(name, '_int_i'),
    di_p, di_n,
    species_output$current_positive, species_output$current_negative,
    0, 0, rate
  )
  gates[[length(gates) + 1]] <- g_int_i

  return(gates)
}

#' @title Make_Circuit_Resistive_Inductor
#' @description Assembles the analog CRN gates to simulate a series RL inductor with internal resistance.
Make_Circuit_Resistive_Inductor <- function(name, species_input, species_output, ic, rate) {
  gates <- list()

  vr_p <- jn(name, '_vr_p')
  vr_n <- jn(name, '_vr_n')
  dummy_0 <- jn(name, '_dummy_0')

  # 1. Resistor Voltage Drop: V_R = i * R
  g_mul_vrp <- Make_Mul2In_Wang(
    jn(name, '_mul_vrp'),
    species_output$current_positive, jn(name, '_R'), vr_p,
    0, ic$resistance, rate
  )
  gates[[length(gates) + 1]] <- g_mul_vrp

  g_mul_vrn <- Make_Mul2In_Wang(
    jn(name, '_mul_vrn'),
    species_output$current_negative, jn(name, '_R'), vr_n,
    0, ic$resistance, rate
  )
  gates[[length(gates) + 1]] <- g_mul_vrn

  # 2. Inductor Voltage Subtraction: V_L = V_in - V_R (Dual Rail)
  g_add_vlp <- Make_Add3In(
    jn(name, '_add_vlp'),
    species_input$voltage_positive, vr_n, dummy_0,
    species_output$voltage_positive, 
    0, 0, 0, rate
  )
  gates[[length(gates) + 1]] <- g_add_vlp

  g_add_vln <- Make_Add3In(
    jn(name, '_add_vln'),
    species_input$voltage_negative, vr_p, dummy_0,
    species_output$voltage_negative, 
    0, 0, 0, rate
  )
  gates[[length(gates) + 1]] <- g_add_vln

  # 3. Inductor Derivative: di/dt = V_L * (1/L)
  di_p <- jn(name, '_di_p')
  di_n <- jn(name, '_di_n')

  g_mul_dip <- Make_Mul2In_Wang(
    jn(name, '_mul_dip'),
    species_output$voltage_positive, jn(name, '_1oL'), di_p,
    0, 1 / ic$inductance, rate
  )
  gates[[length(gates) + 1]] <- g_mul_dip

  g_mul_din <- Make_Mul2In_Wang(
    jn(name, '_mul_din'),
    species_output$voltage_negative, jn(name, '_1oL'), di_n,
    0, 1 / ic$inductance, rate
  )
  gates[[length(gates) + 1]] <- g_mul_din

  # 4. Current Integration: i = Integral(di/dt)
  g_int_i <- Make_Integrator_OishiYordanov(
    jn(name, '_int_i'),
    di_p, di_n,
    species_output$current_positive, species_output$current_negative,
    0, 0, rate
  )
  gates[[length(gates) + 1]] <- g_int_i

  return(gates)
}

#' @title Make_Circuit_Pure_Capacitor
#' @description Implements i = C * (dv/dt) using a differentiator gate
Make_Circuit_Pure_Capacitor_Derivative <- function(name, species_input, species_output, ic, rate) {
  gates <- list()
  dv_p <- jn(name, '_dv_p')
  dv_n <- jn(name, '_dv_n')

  # 1. Differentiator: dv/dt
  g_diff <- Make_Derivative(
    jn(name, '_diff'),
    species_input$voltage_positive, species_input$voltage_negative,
    dv_p, dv_n, 
    0, 0, rate
  )
  gates[[length(gates) + 1]] <- g_diff

  # 2. Multiplier: i_C = C * dv/dt
  g_mul_p <- Make_Mul2In_Wang(
    jn(name, '_mul_p'),
    dv_p, jn(name, '_C'), species_output$current_positive,
    0, ic$capacitance, rate
  )
  gates[[length(gates) + 1]] <- g_mul_p

  g_mul_n <- Make_Mul2In_Wang(
    jn(name, '_mul_n'),
    dv_n, jn(name, '_C'), species_output$current_negative,
    0, ic$capacitance, rate
  )
  gates[[length(gates) + 1]] <- g_mul_n

  return(gates)
}

#' @title Make_Circuit_Pure_Inductor
#' @description Implements i = (1/L) * integral(v dt) using an integrator gate
Make_Circuit_Pure_Inductor_Integrator <- function(name, species_input, species_output, ic, rate) {
  gates <- list()
  flux_p <- jn(name, '_flux_p')
  flux_n <- jn(name, '_flux_n')

  # 1. Integrator: Flux (lambda) = Integral(v) dt
  g_int <- Make_Integrator_OishiYordanov(
    jn(name, '_int'),
    species_input$voltage_positive, species_input$voltage_negative,
    flux_p, flux_n, 
    0, 0, rate
  )
  gates[[length(gates) + 1]] <- g_int

  # 2. Multiplier: i_L = Flux * (1/L)
  g_mul_p <- Make_Mul2In_Wang(
    jn(name, '_mul_p'),
    flux_p, jn(name, '_1oL'), species_output$current_positive,
    0, 1 / ic$inductance, rate
  )
  gates[[length(gates) + 1]] <- g_mul_p

  g_mul_n <- Make_Mul2In_Wang(
    jn(name, '_mul_n'),
    flux_n, jn(name, '_1oL'), species_output$current_negative,
    0, 1 / ic$inductance, rate
  )
  gates[[length(gates) + 1]] <- g_mul_n

  return(gates)
}

flatten_gate_groups <- function(...) {

  gates <- list()

  for (group in list(...)) gates <- append(gates, group)

  gates

}

make_signed_add3 <- function(name, positive_inputs, negative_inputs, output_positive,

                             output_negative, rate) {

  positive_gate <- Make_Add3In(

    jn(name, '_p'), positive_inputs[1], positive_inputs[2], positive_inputs[3],

    output_positive, 0, 0, 0, rate

  )

  negative_gate <- Make_Add3In(

    jn(name, '_n'), negative_inputs[1], negative_inputs[2], negative_inputs[3],

    output_negative, 0, 0, 0, rate

  )

  list(positive_gate, negative_gate)

}



make_signed_multiplier <- function(name, input1, input2, output_positive,

                                   output_negative, rate) {

  pp <- Make_Mul2In_Wang(jn(name, '_pp'), input1[1], input2[1],

                         jn(name, '_pp_out'), 0, 0, rate)

  nn <- Make_Mul2In_Wang(jn(name, '_nn'), input1[2], input2[2],

                         jn(name, '_nn_out'), 0, 0, rate)

  pn <- Make_Mul2In_Wang(jn(name, '_pn'), input1[1], input2[2],

                         jn(name, '_pn_out'), 0, 0, rate)

  np <- Make_Mul2In_Wang(jn(name, '_np'), input1[2], input2[1],

                         jn(name, '_np_out'), 0, 0, rate)

  list(

    pp, nn, pn, np,

    Make_Add3In(jn(name, '_sum_p'), pp$species$output, nn$species$output,

                jn(name, '_dummy_p'), output_positive, 0, 0, 0, rate),

    Make_Add3In(jn(name, '_sum_n'), pn$species$output, np$species$output,

                jn(name, '_dummy_n'), output_negative, 0, 0, 0, rate)

  )

}

#' Constant dual-rail scalar multiplier: Y = scalar * X
#' Requires only 2 gates (swaps rails for negative scalars)
make_signed_scalar_mul <- function(name, input_signed, output_signed, scalar, rate) {
  k_val <- abs(scalar)
  k_species <- jn(name, '_k')
  
  # Swap positive/negative input rails if multiplying by a negative constant
  if (scalar >= 0) {
    p_in <- input_signed[1]
    n_in <- input_signed[2]
  } else {
    p_in <- input_signed[2]
    n_in <- input_signed[1]
  }
  
  g_p <- Make_Mul2In_Wang(jn(name, '_p'), p_in, k_species, output_signed[1], 0, k_val, rate)
  g_n <- Make_Mul2In_Wang(jn(name, '_n'), n_in, k_species, output_signed[2], 0, k_val, rate)
  
  list(g_p, g_n)
}

#' Build a Chua oscillator using modular analog CRN primitives
Make_Circuit_Chua <- function(circuit, capacitor1, capacitor2, inductor, 
                              resistance = 1.0, rate = 1.0,
                              diode_linear = -1.2, diode_cubic = 0.2) {
  name <- "chua"
  
  # --- 1. Extract Port Signals and Physical Parameters ---
  x1_p <- capacitor1$ol$voltage_positive
  x1_n <- capacitor1$ol$voltage_negative
  x2_p <- capacitor2$ol$voltage_positive
  x2_n <- capacitor2$ol$voltage_negative
  il_p <- inductor$ol$current_positive
  il_n <- inductor$ol$current_negative
  
  c1 <- capacitor1$ic$capacitance
  c2 <- capacitor2$ic$capacitance
  L  <- inductor$ic$inductance
  
  x1 <- c(x1_p, x1_n)
  x2 <- c(x2_p, x2_n)
  il <- c(il_p, il_n)

  # --- 2. Define Internal Dual-Rail Species ---
  x1_sq        <- c(jn(name, '_x1_sq_p'), jn(name, '_x1_sq_n'))
  x1_cube      <- c(jn(name, '_x1_cube_p'), jn(name, '_x1_cube_n'))
  diode_lin    <- c(jn(name, '_diode_lin_p'), jn(name, '_diode_lin_n'))
  diode_cub    <- c(jn(name, '_diode_cub_p'), jn(name, '_diode_cub_n'))
  diode        <- c(jn(name, '_diode_p'), jn(name, '_diode_n'))
  
  dv1_unscaled <- c(jn(name, '_dv1_u_p'), jn(name, '_dv1_u_n'))
  dv2_unscaled <- c(jn(name, '_dv2_u_p'), jn(name, '_dv2_u_n'))
  
  dv1_scaled   <- c(jn(name, '_dv1_s_p'), jn(name, '_dv1_s_n'))
  dv2_scaled   <- c(jn(name, '_dv2_s_p'), jn(name, '_dv2_s_n'))
  di_scaled    <- c(jn(name, '_di_s_p'),  jn(name, '_di_s_n'))
  
  dummy_zero   <- jn(name, '_zero')

  # --- 3. Construct Diode Characteristic: i_N(v1) = a*v1 + b*v1^3 ---
  g_sq   <- make_signed_multiplier(jn(name, '_sq'), x1, x1, x1_sq[1], x1_sq[2], rate)
  g_cube <- make_signed_multiplier(jn(name, '_cube'), x1_sq, x1, x1_cube[1], x1_cube[2], rate)
  
  g_lin_scale <- make_signed_scalar_mul(jn(name, '_lin_scale'), x1, diode_lin, diode_linear, rate)
  g_cub_scale <- make_signed_scalar_mul(jn(name, '_cub_scale'), x1_cube, diode_cub, diode_cubic, rate)
  
  g_diode <- make_signed_add3(jn(name, '_diode_sum'),
                              c(diode_lin[1], diode_cub[1], dummy_zero),
                              c(diode_lin[2], diode_cub[2], dummy_zero),
                              diode[1], diode[2], rate)

  # --- 4. Form State Differential Terms ---
  # dv1 = (v2 - v1)/R - i_N(v1)
  g_dv1 <- make_signed_add3(jn(name, '_add_dv1'),
                            c(x2_p, x1_n, diode[2]),
                            c(x2_n, x1_p, diode[1]),
                            dv1_unscaled[1], dv1_unscaled[2], rate)

  # dv2 = (v1 - v2)/R + i_L
  g_dv2 <- make_signed_add3(jn(name, '_add_dv2'),
                            c(x1_p, x2_n, il_p),
                            c(x1_n, x2_p, il_n),
                            dv2_unscaled[1], dv2_unscaled[2], rate)

  # --- 5. Scale Derivatives by (1/C1, 1/C2, 1/L) ---
  g_c1_scale <- make_signed_scalar_mul(jn(name, '_c1_scale'), dv1_unscaled, dv1_scaled, 1 / (c1 * resistance), rate)
  g_c2_scale <- make_signed_scalar_mul(jn(name, '_c2_scale'), dv2_unscaled, dv2_scaled, 1 / (c2 * resistance), rate)
  
  # di_L = -v2 / L (Direct rail swap eliminates extra subtraction stage)
  g_l_scale  <- make_signed_scalar_mul(jn(name, '_l_scale'), c(x2_n, x2_p), di_scaled, 1 / L, rate)

  # --- 6. State Integrators ---
  g_int_v1 <- Make_Integrator_OishiYordanov(jn(name, '_int_v1'), dv1_scaled[1], dv1_scaled[2], x1_p, x1_n, 0, 0, rate)
  g_int_v2 <- Make_Integrator_OishiYordanov(jn(name, '_int_v2'), dv2_scaled[1], dv2_scaled[2], x2_p, x2_n, 0, 0, rate)
  g_int_i  <- Make_Integrator_OishiYordanov(jn(name, '_int_i'),  di_scaled[1],  di_scaled[2],  il_p, il_n, 0, 0, rate)

  # --- 7. Register and Compile ---
  all_gates <- flatten_gate_groups(
    g_sq, g_cube, g_lin_scale, g_cub_scale, g_diode,
    g_dv1, g_dv2, g_c1_scale, g_c2_scale, g_l_scale,
    list(g_int_v1, g_int_v2, g_int_i)
  )

  circuit <- circuit_add_compile_gates(circuit, all_gates)
  return(circuit)
}

#' Modular Non-Linear Resistor (Chua Diode CRN Sub-module)
#' Computes i_N = diode_linear * v1 + diode_cubic * v1^3
Make_Chua_Diode_CRN <- function(name, v1_p, v1_n, i_diode_p, i_diode_n, 
                                diode_linear = -1.2, diode_cubic = 0.2, rate = 1.0) {
  
  # Species definitions
  x1       <- c(v1_p, v1_n)
  x1_sq    <- c(jn(name, '_sq_p'), jn(name, '_sq_n'))
  x1_cube  <- c(jn(name, '_cube_p'), jn(name, '_cube_n'))
  i_lin    <- c(jn(name, '_ilin_p'), jn(name, '_ilin_n'))
  i_cub    <- c(jn(name, '_icub_p'), jn(name, '_icub_n'))
  dummy_0  <- jn(name, '_zero')

  # 1. Polynomial Terms: v1^2 and v1^3
  g_sq   <- make_signed_multiplier(jn(name, '_sq'), x1, x1, x1_sq[1], x1_sq[2], rate)
  g_cube <- make_signed_multiplier(jn(name, '_cube'), x1_sq, x1, x1_cube[1], x1_cube[2], rate)

  # 2. Linear Scaling (i_lin = diode_linear * v1)
  g_lin <- make_signed_scalar_mul(jn(name, '_lin_scale'), x1, i_lin, diode_linear, rate)

  # 3. Cubic Scaling (i_cub = diode_cubic * v1^3)
  g_cub <- make_signed_scalar_mul(jn(name, '_cub_scale'), x1_cube, i_cub, diode_cubic, rate)

  # 4. Summation: i_N = i_lin + i_cub
  g_sum <- make_signed_add3(jn(name, '_sum'),
                             c(i_lin[1], i_cub[1], dummy_0),
                             c(i_lin[2], i_cub[2], dummy_0),
                             i_diode_p, i_diode_n, rate)

  flatten_gate_groups(g_sq, g_cube, g_lin, g_cub, g_sum)
}

#' Composited Chua Circuit using standard component gates + Diode CRN
Make_Circuit_Chua_Composited <- function(circuit, c1_comp, c2_comp, l_comp,
                                         resistance = 1.0, rate = 1.0,
                                         diode_linear = -1.2, diode_cubic = 0.2) {
  name <- "chua"

  # Extract signals from standard component objects
  v1_p <- c1_comp$ol$voltage_positive
  v1_n <- c1_comp$ol$voltage_negative
  
  v2_p <- c2_comp$ol$voltage_positive
  v2_n <- c2_comp$ol$voltage_negative
  
  il_p <- l_comp$ol$current_positive
  il_n <- l_comp$ol$current_negative

  # Dual-rail internal signals
  i_diode_p <- jn(name, '_idiode_p')
  i_diode_n <- jn(name, '_idiode_n')
  
  i_res_p   <- jn(name, '_ires_p')
  i_res_n   <- jn(name, '_ires_n')
  
  dv1_p     <- jn(name, '_dv1_p')
  dv1_n     <- jn(name, '_dv1_n')
  
  dv2_p     <- jn(name, '_dv2_p')
  dv2_n     <- jn(name, '_dv2_n')
  
  vl_p      <- jn(name, '_vl_p')
  vl_n      <- jn(name, '_vl_n')

  C1 <- c1_comp$ic$capacitance
  C2 <- c2_comp$ic$capacitance
  L  <- l_comp$ic$inductance

  # -------------------------------------------------------------
  # 1. Diode CRN Sub-module
  # -------------------------------------------------------------
  diode_gates <- Make_Chua_Diode_CRN(jn(name, '_diode'), v1_p, v1_n, 
                                     i_diode_p, i_diode_n, 
                                     diode_linear, diode_cubic, rate)

  # -------------------------------------------------------------
  # 2. Linear Resistor Coupling: i_R = (v1 - v2) / R
  # -------------------------------------------------------------
  g_ires_p <- Make_Add3In(jn(name, '_add_ires_p'), v1_p, v2_n, jn(name, '_z1'), i_res_p, 0, 0, 0, rate / resistance)
  g_ires_n <- Make_Add3In(jn(name, '_add_ires_n'), v1_n, v2_p, jn(name, '_z2'), i_res_n, 0, 0, 0, rate / resistance)

  # -------------------------------------------------------------
  # 3. Node Dynamics for C1: dv1/dt = (1/C1) * (-i_R - i_N)
  # -------------------------------------------------------------
  g_dv1_p <- Make_Add3In(jn(name, '_add_dv1_p'), i_res_n, i_diode_n, jn(name, '_z3'), dv1_p, 0, 0, 0, rate / C1)
  g_dv1_n <- Make_Add3In(jn(name, '_add_dv1_n'), i_res_p, i_diode_p, jn(name, '_z4'), dv1_n, 0, 0, 0, rate / C1)

  g_int_v1 <- Make_Integrator_OishiYordanov(jn(name, '_int_v1'), dv1_p, dv1_n, v1_p, v1_n, 0, 0, rate)

  # -------------------------------------------------------------
  # 4. Node Dynamics for C2: dv2/dt = (1/C2) * (i_R + i_L)
  # -------------------------------------------------------------
  g_dv2_p <- Make_Add3In(jn(name, '_add_dv2_p'), i_res_p, il_p, jn(name, '_z5'), dv2_p, 0, 0, 0, rate / C2)
  g_dv2_n <- Make_Add3In(jn(name, '_add_dv2_n'), i_res_n, il_n, jn(name, '_z6'), dv2_n, 0, 0, 0, rate / C2)

  g_int_v2 <- Make_Integrator_OishiYordanov(jn(name, '_int_v2'), dv2_p, dv2_n, v2_p, v2_n, 0, 0, rate)

  # -------------------------------------------------------------
  # 5. Inductor Dynamics L: di_L/dt = -v2 / L
  # -------------------------------------------------------------
  g_dil_p <- Make_Mul2In_Wang(jn(name, '_dil_p'), v2_n, jn(name, '_k_l'), vl_p, 0, 1 / L, rate)
  g_dil_n <- Make_Mul2In_Wang(jn(name, '_dil_n'), v2_p, jn(name, '_k_l'), vl_n, 0, 1 / L, rate)

  g_int_il <- Make_Integrator_OishiYordanov(jn(name, '_int_il'), vl_p, vl_n, il_p, il_n, 0, 0, rate)

  # Combine all gate structures
  all_gates <- flatten_gate_groups(
    diode_gates,
    list(g_ires_p, g_ires_n),
    list(g_dv1_p, g_dv1_n, g_int_v1),
    list(g_dv2_p, g_dv2_n, g_int_v2),
    list(g_dil_p, g_dil_n, g_int_il)
  )

  circuit <- circuit_add_compile_gates(circuit, all_gates)
  return(circuit)
}

Make_Circuit_RLC_Composited <- function(timing, regime) {

    behaviours <- list(
    'O' = c(R = 4, L = 1, C = 1),# 2
    'C' = c(R = 2, L = 1, C = 1),# 1 
    'U' = c(R = 1, L = 1, C = 1) # 0.5
  )
  # Load parameters for the requested regime
  params <- behaviours[[regime]]
  R <- params["R"]
  L <- params["L"]
  C <- params["C"]

  rate <- 1e-3
  
  circuit <- make_circuit(timing)
  
  # 2. Define Shared Species for the Series RLC Connection
  # V_RL = V_in - V_C
  v_rl_p <- jn('comp_', regime, '_vrl_p')
  v_rl_n <- jn('comp_', regime, '_vrl_n')
  
  i_series_p <- jn('comp_', regime, '_i_p')
  i_series_n <- jn('comp_', regime, '_i_n')
  
  vc_p <- jn('comp_', regime, '_vc_p')
  vc_n <- jn('comp_', regime, '_vc_n')
  
  dummy_0 <- 'dummy_0'

  # 3. Voltage Subtraction (Feedback Loop): V_RL = V_in - V_C
  g_add_vrl_p <- Make_Add3In(
    jn('add_vrl_p_', regime),
    'v1p', vc_n, dummy_0, # V_inp + V_Cn (dual rail subtraction)
    v_rl_p,
    0, 0, 0, rate
  )
  g_add_vrl_n <- Make_Add3In(
    jn('add_vrl_n_', regime),
    'v1n', vc_p, dummy_0, # V_inn + V_Cp
    v_rl_n,
    0, 0, 0, rate
  )
  circuit <- circuit_add_compile_gates(circuit, list(g_add_vrl_p, g_add_vrl_n))

  # 4. Instantiate the RL Block (Resistor + Inductor)
  rl_ic <- list(resistance = R, inductance = L)
  rl_input <- list(voltage_positive = v_rl_p, voltage_negative = v_rl_n)
  rl_output <- list(current_positive = i_series_p, current_negative = i_series_n, 
                    voltage_positive = 'vl_p_dummy', voltage_negative = 'vl_n_dummy')
  
  rl_gates <- Make_Circuit_RL2(jn('RL_', regime), rl_input, rl_output, rl_ic, rate)
  circuit <- circuit_add_compile_gates(circuit, rl_gates)

  # 5. Instantiate the C Block (Capacitor)
  # Since the RC model expects a voltage to calculate its own current, we bypass 
  # the RC's internal V->I conversion and directly integrate the series current.
  dvc_p <- jn('C_', regime, '_dvc_p')
  dvc_n <- jn('C_', regime, '_dvc_n')
  
  g_mul_dvcp <- Make_Mul2In_Wang(
    jn('C_mul_dvcp_', regime),
    i_series_p, jn('C_1oC_', regime), dvc_p,
    0, 1 / C, rate
  )
  g_mul_dvcn <- Make_Mul2In_Wang(
    jn('C_mul_dvcn_', regime),
    i_series_n, jn('C_1oC_', regime), dvc_n,
    0, 1 / C, rate
  )
  
  g_int_vc <- Make_Integrator_OishiYordanov(
    jn('C_int_vc_', regime),
    dvc_p, dvc_n,
    vc_p, vc_n,
    0, 0, rate
  )
  
  circuit <- circuit_add_compile_gates(circuit, list(g_mul_dvcp, g_mul_dvcn, g_int_vc))

  return(circuit)
}

Make_Circuit_RLC_Composited2 <- function(circuit, rlc_comp, rate = 1) {
  name <- rlc_comp$name
  
  # Extract Parameters
  R <- rlc_comp$ic$resistance
  L <- rlc_comp$ic$inductance
  C <- rlc_comp$ic$capacitance
  
  # Input Signals
  v1p <- rlc_comp$il$voltage_positive
  v1n <- rlc_comp$il$voltage_negative
  
  # Output Signals
  vc_p <- rlc_comp$ol$voltage_positive
  vc_n <- rlc_comp$ol$voltage_negative
  i_series_p <- rlc_comp$ol$current_positive
  i_series_n <- rlc_comp$ol$current_negative

  # Internal Species
  v_rl_p <- jn(name, '_vrl_p')
  v_rl_n <- jn(name, '_vrl_n')
  dummy_0 <- jn(name, '_dummy_0')

  # 1. Voltage Subtraction (Feedback Loop): V_RL = V_in - V_C
  g_add_vrl_p <- Make_Add3In(
    jn(name, '_add_vrl_p'),
    v1p, vc_n, dummy_0, # V_inp + V_Cn (dual rail subtraction)
    v_rl_p,
    0, 0, 0, rate
  )
  g_add_vrl_n <- Make_Add3In(
    jn(name, '_add_vrl_n'),
    v1n, vc_p, dummy_0, # V_inn + V_Cp
    v_rl_n,
    0, 0, 0, rate
  )
  circuit <- circuit_add_compile_gates(circuit, list(g_add_vrl_p, g_add_vrl_n))

  # 2. Instantiate the RL Block (Resistor + Inductor)
  rl_ic <- list(resistance = R, inductance = L)
  rl_input <- list(voltage_positive = v_rl_p, voltage_negative = v_rl_n)
  rl_output <- list(current_positive = i_series_p, current_negative = i_series_n, 
                    voltage_positive = jn(name, '_vl_p_dummy'), 
                    voltage_negative = jn(name, '_vl_n_dummy'))
  
  rl_gates <- Make_Circuit_RL2(jn(name, '_RL'), rl_input, rl_output, rl_ic, rate)
  circuit <- circuit_add_compile_gates(circuit, rl_gates)

  # 3. Instantiate the C Block (Capacitor)
  dvc_p <- jn(name, '_C_dvc_p')
  dvc_n <- jn(name, '_C_dvc_n')
  
  g_mul_dvcp <- Make_Mul2In_Wang(
    jn(name, '_C_mul_dvcp'),
    i_series_p, jn(name, '_C_1oC'), dvc_p,
    0, 1 / C, rate
  )
  g_mul_dvcn <- Make_Mul2In_Wang(
    jn(name, '_C_mul_dvcn'),
    i_series_n, jn(name, '_C_1oC'), dvc_n,
    0, 1 / C, rate
  )
  
  g_int_vc <- Make_Integrator_OishiYordanov(
    jn(name, '_C_int_vc'),
    dvc_p, dvc_n,
    vc_p, vc_n,
    0, 0, rate
  )
  
  circuit <- circuit_add_compile_gates(circuit, list(g_mul_dvcp, g_mul_dvcn, g_int_vc))

  return(circuit)
}