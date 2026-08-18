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

Make_Circuit_Capacitor <- function(name, species_input, species_output, ic, rate) {
  gates <- list()

  # I_C (RC series) : i_c = C * dv_l/dt 

  l_dvp <- jn(name, 'l_dvp')
  l_dvn <- jn(name, 'l_dvn')
  
  # dvp, dvn = dv/dt
  g_v_derivate <- Make_Derivative(jn(name, 'g_dv_in'),
    species_input$voltage_positive, species_input$voltage_negative,
    l_dvp, l_dvn,
    ic$voltage_positive, ic$voltage_negative,
    rate
  )
  gates[[length(gates)+1]] <- g_v_derivate 
  
  # ip = C dvp
  g_ip <- Make_Mul2In_Wang(jn(name, 'g_ip'),
    species_input$capacitance, l_dvp, species_output$current_positive,
    ic$capacitance, 0, 
    rate
  )
  gates[[length(gates)+1]] <- g_ip
      # in = C dvn
      g_in <- Make_Mul2In_Wang(jn(name, 'g_in'),
        species_input$capacitance, l_dvn, species_output$current_negative,
        ic$capacitance, 0, 
        rate
      )
      gates[[length(gates)+1]] <- g_in

  # Q = \int i dt
  l_pcharge <- jn(name, 'l_pcharge')
  l_ncharge <- jn(name, 'l_ncharge')
  g_charge_integrator <- Make_Integrator_OishiYordanov(
    jn(name, '_g_dv_int'), 
    species_output$current_positive, species_output$current_negative,
    l_pcharge, l_ncharge,
    ic$current_positive, ic$current_negative,
    rate
  )
  gates[[length(gates)+1]] <- g_charge_integrator

      # vp = Qp * 1/C
      l_vp = jn(name, 'l_vp')
      g_vp <- Make_Mul2In_Wang(jn(name, 'g_vp'),
        jn(name,'_1oC'), l_pcharge, species_output$voltage_positive,
        1/ic$capacitance, 0,  # 1/(ic$capacitance* * resistance)
        rate
      )
      gates[[length(gates)+1]] <- g_vp

          # # vp_out = vp + vp0
          # g_vp_out <- Make_Adder_apBeC(jn(name, '_g_vp_out'),
          #   l_vp, jn(name, '_vp0'), species_output$voltage_positive,
          #   0, ic$initial_voltage_positive,
          #   1e3, rate
          # )
          # gates[[length(gates)+1]] <- g_vp_out

      # vn = Qn * 1/C
      l_vn = jn(name, 'l_vp')
      g_vn <- Make_Mul2In_Wang(jn(name, 'g_vn'),
        jn(name,'_1oC'), l_ncharge, species_output$voltage_negative,
        1/ic$capacitance, 0,  # 1/(ic$capacitance * resistance)
        rate
      )
      gates[[length(gates)+1]] <- g_vn

          # # vn_out = vn + vn0
          # g_vn_out <- Make_Adder_apBeC(jn(name, '_g_vn_out'),
          #   l_vn, jn(name, '_vn0'), species_output$voltage_negative,
          #   0, ic$iniital_voltage_negative,
          #   1e3, rate
          # )
          # gates[[length(gates)+1]] <- g_vn_out

  return (gates)
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

Make_RLC_Component <- function(resistance, inductance, capacitance) {
  rlc <- c()
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

  # ------------------------------------------------------------
  # 1. Subtraction: V_R = V_in - V_C 
  # Dual rail subtraction: V_Rp = V_inp + V_Cn; V_Rn = V_inn + V_Cp
  # ------------------------------------------------------------
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

  # ------------------------------------------------------------
  # 2. Current: I = V_R * (1/R)
  # ------------------------------------------------------------
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

  # ------------------------------------------------------------
  # 3. Capacitor Derivative: dV_C/dt = I * (1/C)
  # ------------------------------------------------------------
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

  # ------------------------------------------------------------
  # 4. Capacitor Voltage Integration: V_C = Integral(dV_C/dt)
  # ------------------------------------------------------------
  g_int_vc <- Make_Integrator_OishiYordanov(
    jn(name, '_int_vc'),
    dvc_p, dvc_n,
    species_output$voltage_positive, species_output$voltage_negative,
    0, 0, rate
  )
  gates[[length(gates) + 1]] <- g_int_vc

  return(gates)
}

Make_Capacitor_mermaid <- function(name,
                           species_input,
                           species_output,
                           ic,
                           rate) {

  gates <- list()

  # ============================================================
  # Capacitor:
  #
  #   v_in -> d/dt -> C -> integral -> 1/C -> v_C
  #
  # i = C dv/dt
  # Q = integral(i dt)
  # v = Q/C
  # ============================================================

  # ------------------------------------------------------------
  # 1. Derivative
  #
  # dv_in+ / dt -> dv_positive
  # dv_in- / dt -> dv_negative
  # ------------------------------------------------------------

  dv_positive <- jn(name, '_dv_positive')
  dv_negative <- jn(name, '_dv_negative')

  g_dv <- Make_Derivative(
    jn(name, '_derivative'),
    species_input$voltage_positive,
    species_input$voltage_negative,
    dv_positive,
    dv_negative,
    ic$voltage_positive, ic$voltage_negative,
    rate
  )

  gates[[length(gates) + 1]] <- g_dv


  # ------------------------------------------------------------
  # 2. Multiply by C
  #
  # i+ = C * dv+ / dt
  # i- = C * dv- / dt
  # ------------------------------------------------------------

  g_ip <- Make_Mul2In_Wang(
    jn(name, '_mul2_ip'),
    dv_positive,
    jn(name, '_C'),
    species_output$current_positive,
    0, ic$capacitance,
    rate
  )

  gates[[length(gates) + 1]] <- g_ip


  g_in <- Make_Mul2In_Wang(
    jn(name, '_mul2_in'),
    dv_negative,
    jn(name, '_C'),
    species_output$current_negative,
    0, ic$capacitance,
    rate
  )

  gates[[length(gates) + 1]] <- g_in


  # ------------------------------------------------------------
  # 3. Integrate current
  #
  # Q+ = integral(i+ dt)
  # Q- = integral(i- dt)
  # ------------------------------------------------------------

  charge_positive <- jn(name, '_charge_positive')
  charge_negative <- jn(name, '_charge_negative')

  g_charge_integrator <- Make_Integrator_OishiYordanov(
    jn(name, '_charge_integrator'),
    species_output$current_positive,
    species_output$current_negative,
    charge_positive,
    charge_negative,
    ic$current_positive, ic$current_negative,
    rate
  )

  gates[[length(gates) + 1]] <- g_charge_integrator


  # ------------------------------------------------------------
  # 4. Multiply by 1/C
  #
  # vC+ = Q+ / C
  # vC- = Q- / C
  # ------------------------------------------------------------

  g_vp <- Make_Mul2In_Wang(
    jn(name, '_g_vp'),
    charge_positive,
    jn(name, '_1oC'),
    species_output$voltage_positive,
    0, 1 / ic$capacitance,
    rate
  )

  gates[[length(gates) + 1]] <- g_vp


  g_vn <- Make_Mul2In_Wang(
    jn(name, '_g_vn'),
    charge_negative,
    jn(name, '_1oC'),
    species_output$voltage_negative,
    0, 1 / ic$capacitance,
    rate
  )

  gates[[length(gates) + 1]] <- g_vn


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