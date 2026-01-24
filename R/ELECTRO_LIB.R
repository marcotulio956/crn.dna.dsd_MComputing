jn <- function(...) { paste(..., sep = '') }

circuit_add_electro_gates <- function(circuit, gates) {
  # Merge gates into the circuit
  for (gate in gates) {
    circuit$gates <- append(circuit$gates, list(gate))
    circuit <- circuit_compile(circuit)
  }
  return(circuit)
}

Make_Adder_apBeC <- function(name, nameInput1, nameInput2, nameOutput,
                             cinput1, cinput2, cfuel, rate) {
  species <- list(
    input1 = nameInput1,
    input2 = nameInput2,
    output = nameOutput,
    intermediate3 = jn(name, '_A'),
    intermediate1 = jn(name, '_B'),
    intermediate2 = jn(name, '_waste')
  )

  ci <- c(cinput1, cinput2, 0, 0, cfuel, 0)

  reactions <- c(
    # 'x1 -> A + 'waste'
    jn(species$input1, ' -> ', species$intermediate3, ' + ', species$intermediate2),
    # 'A -> A + z1'
    jn(species$intermediate3, ' -> ', species$intermediate3, ' + ', species$output),
    # 'z1 + B -> z1 + z1'
    jn(species$output, ' + ', species$intermediate1, ' -> ',
       species$output, ' + ', species$intermediate2),
    # 'y1 -> y1 + B'
    jn(species$input2, ' -> ', species$input2, ' + ', species$intermediate1),
    # 'z1 -> waste'
    jn(species$output, ' -> ', species$intermediate2)
  )

  ki        <- c(rate, rate, rate, rate, rate)

  add_gate_w <- list(
    name      = name,
    species   = species,
    reactions = reactions,
    ci        = ci,
    ki        = ki
  )

  return(add_gate_w)
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

Make_Circuit_Inductor <- function(name, species_input, species_output, ic, rate) {
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

Make_Circuit_Inductor_old <- function(name, species_input, species_output, ic, rate) {
  gates <- list()

  l_vep <- jn(name, '_l_vep')
  l_ven <- jn(name, '_l_ven')

  # Integrate Current input voltage to Equivalent tension from Current Source
  g_ve_integrator <- Make_Integrator_OishiYordanov(
    jn(name, '_g_i_int'), 
    species_input$voltage_positive, species_input$voltage_negative,
    l_vep, l_ven,
    0, 0,
    rate
  )
  gates[[length(gates)+1]] <- g_ve_integrator

  # Computer current in the resistor 
  # i_r = 1/L v_e  
  g_pir <- Make_Mul2In_Wang(
    jn(name, 'g_pir'),
    '1ol',  l_vep, 
    species_output$current_positive,
    1/(ic$inductance * ic$resistance), 0, # # 1/(ic$inductance * resistance)
    rate
  )
  gates[[length(gates)+1]] <- g_pir
      g_nir <- Make_Mul2In_Wang(
        jn(name, 'g_nir'),
        '1ol',  l_ven, 
        species_output$current_negative,
        1/(ic$inductance * ic$resistance), 0, # # 1/(ic$inductance * resistance)
        rate
      )
      gates[[length(gates)+1]] <- g_nir

  # Derivate the voltage in the inductor
  l_vep_2 <- jn(name, '_l_vep_2')
  l_ven_2 <- jn(name, '_l_ven_2')
  g_dv_in <- Make_Derivative(
    jn(name, 'g_dv_in'),
    species_output$current_positive, species_output$current_negative,
    l_vep_2, l_ven_2,
    0, 0,
    rate*10
  )
  gates[[length(gates)+1]] <- g_dv_in

  # Compute voltage in the inductor
  g_pil <- Make_Mul2In_Wang(
    jn(name, 'g_pil'),
    species_input$inductance, l_vep_2, species_output$voltage_positive,
    ic$inductance, 0,
    rate*10
  )
  gates[[length(gates)+1]] <- g_pil
      g_nil <- Make_Mul2In_Wang(
        jn(name, 'g_nil'),
        species_input$inductance, l_ven_2, species_output$voltage_negative,
        ic$inductance, 0,
        rate*10
      )
      gates[[length(gates)+1]] <- g_nil

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

# attempt by SS representation
Make_Circuit_RL <- function(name, species_input, species_output, ic, rate) {
  # Parameters (matching indices from your RLC version)
  rate_base    <- rate
  rate_mul1    <- rate_base * 1   # 1/L term
  rate_mul2    <- rate_base * 1   # R/L term
  rate_int1    <- rate_base * 1   # integrator for di/dt

  ic$resistance <- 1
  ic$inductance <- 3

  gates <- list()

  # Define junction names
  l_1ol     <- jn(name, 'l_1ol')      # node for 1·L⁻¹ (inductance inversion)
  l_mul1_p  <- jn(name, 'l_mul1_p')   # positive branch of voltage→di/dt
  l_mul1_n  <- jn(name, 'l_mul1_n')   # negative branch

  l_mul2_p  <- jn(name, 'l_mul2_p')   # positive branch of current·(R/L)
  l_mul2_n  <- jn(name, 'l_mul2_n')

  # l_state_p <- jn(name, 'l_state_p')  # store current (+)
  # l_state_n <- jn(name, 'l_state_n')  # store current (−)

  # 1) Multiply input voltage difference by 1/L
  g_mul1_p <- Make_Mul2In_Wang(
    jn(name, 'mul1_p'),
    species_input$voltage_positive, l_1ol, l_mul1_p,
    0, 1 / ic$inductance,
    rate_mul1
  )
  gates[[length(gates)+1]] <- g_mul1_p

  g_mul1_n <- Make_Mul2In_Wang(
    jn(name, 'mul1_n'),
    species_input$voltage_negative, l_1ol, l_mul1_n,
    0, 1 / ic$inductance,
    rate_mul1
  )
  gates[[length(gates)+1]] <- g_mul1_n

  # 2) Multiply in-circuit current by R/L (voltage drop over resistor)
  g_mul2_p <- Make_Mul2In_Wang(
    jn(name, 'mul2_p'),
    species_output$current_positive, jn(name, 'l_rol'), l_mul2_p,
    0, ic$resistance / ic$inductance,
    rate_mul2
  )
  gates[[length(gates)+1]] <- g_mul2_p

  g_mul2_n <- Make_Mul2In_Wang(
    jn(name, 'mul2_n'),
    species_output$current_negative, jn(name, 'l_rol'), l_mul2_n,
    0, ic$resistance / ic$inductance,
    rate_mul2
  )
  gates[[length(gates)+1]] <- g_mul2_n

  # 3) Integrate (di/dt) = (V_L – V_R)/L → current state
  g_int1 <- Make_Integrator_OishiYordanov(
    jn(name, 'g_int1'),
    l_mul1_p, l_mul1_n,            # positive and negative 1/L·V input
    species_output$current_positive, species_output$current_negative,
    0, 0,
    rate_int1
  )
  gates[[length(gates)+1]] <- g_int1

  r_mul_p <- jn(name, 'r_mul_p')
  r_mul_n <- jn(name, 'r_mul_n')

  # multiply positive current species by R
  g_res_p <- Make_Derivative(
    jn(name, 'g_res_p'),
    species_output$current_positive,   # i⁺
    species_output$current_negative,                 # a “reference” junction (zero‐bias)
    species_output$voltage_positive,                           # output species v_R⁺
    species_output$voltage_negative,                           # output species v_R⁺
    0,                                  # offset zero
    0,                      # factor = R
    rate_base * 100                       # choose a suitable rate constant
  )
  gates[[length(gates)+1]] <- g_res_p

  # multiply negative current species by R
  # g_res_n <- Make_Mul2In_Wang(
  #   jn(name, 'g_res_n'),
  #   species_output$current_negative,   # i⁻
  #   jn(name, 'l_rol'),
  #   species_output$voltage_negative,                           # output species v_R⁻
  #   0,
  #   ic$resistance,
  #   rate_base
  # )
  # gates[[length(gates)+1]] <- g_res_n
  
  return(gates)
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

  # All reactions use same rate constant
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



# Make_Circuit_RLC <- function(name, species_input, species_output, ic, rate, fuel) {
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

Make_Circuit_RLC_stepbystep <- function(name, species_input, species_output, ic, p) { 
  rate_base <- p[1]
  fuel_base <- p[2]
  range_base_add3 <- p[3] 
  rate_mul1   <- rate_base * p[4]
  rate_mul2   <- rate_base * p[5]
  rate_mul3   <- rate_base * p[6]
  rate_mul4   <- rate_base * p[7]
  rate_int1   <- rate_base * p[8]
  rate_int2   <- rate_base * p[9]
  rate_add3   <- rate_base * p[10]
  range_add3  <- range_base_add3 * p[11]
  fuel_states <- fuel_base * p[12]
  rate_states1 <- rate_base * p[13]
  rate_states2 <- rate_base * p[14]

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
  l_add3_2_p_carry <- jn(name, 'l_add3_2_p_carry')
  l_add3_2_n_carry <- jn(name, 'l_add3_2_n_carry')


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


  g_add3_1_p <- Make_Adder2In_Wang(jn(name, 'g_add3_1_p'),
    l_mul1_p, l_mul2_n, l_add3_1_p_carry,
    0, 0,
    fuel_base, rate_add3
  )
  gates[[length(gates)+1]] <- g_add3_1_p
          g_add3_2_p <- Make_Adder2In_Wang(jn(name, 'g_add3_2_p'),
            l_add3_1_p_carry, l_mul4_n, l_add3_2_p_carry,
            0, 0,
            fuel_base, rate_add3
          )
          gates[[length(gates)+1]] <- g_add3_2_p

  g_add3_1_n <- Make_Adder2In_Wang(jn(name, 'g_add3_1_n'),
    l_mul1_n, l_mul2_p, l_add3_1_n_carry,
    0, 0,
    fuel_base, rate_add3
  )
  gates[[length(gates)+1]] <- g_add3_1_n
          g_add3_2_n <- Make_Adder2In_Wang(jn(name, 'g_add3_2_n'),
            l_add3_1_n_carry, l_mul4_p, l_add3_2_n_carry,
            0, 0,
            fuel_base, rate_add3
          )
          gates[[length(gates)+1]] <- g_add3_2_n

  # g_der1 <- Make_Derivative(
  #   jn(name, 'g_der1'),
  #   l_add3_2_p_carry, l_add3_2_n_carry, # l_add3_2_p_carry
  #   jn(name, 'l_der1_p'), jn(name, 'l_der1_n'),
  #   0, 0,
  #   1e3
  # )
  # gates[[length(gates)+1]] <- g_der1

  # g_int1 <- Make_Integrator_OishiYordanov(
  #   jn(name, 'g_int1'),
  #   l_add3_2_p_carry, l_add3_2_n_carry,
  #   species_output$current_positive, species_output$current_negative,
  #   0, 0,
  #   rate_int1
  # )
  # gates[[length(gates)+1]] <- g_int1

  l_state1p <- jn(name, 'l_state1_p')
  l_state1n <- jn(name, 'l_state1_n')
  l_state2p <- jn(name, 'l_state2_p')
  l_state2n <- jn(name, 'l_state2_n')
  l_state3p <- jn(name, 'l_state2_n')
  

  g_state1p <- Make_Adder2In_Wang(
    jn(name, 'g_state1p'),
    species_output$current_positive, l_state1p,
    l_state2p,
    0, 0,
    fuel_states, rate_states1
  ) 
  gates[[length(gates)+1]] <- g_state1p
  g_state1n <- Make_Adder2In_Wang(
    jn(name, 'g_state1n'),
    species_output$current_negative, l_state3p,
    species_output$current,
    0, 0,
    fuel_states, rate_states1
  ) 
  gates[[length(gates)+1]] <- g_state1n

  
  # g_state1p <- Make_Adder_apBeC(
  #   jn(name, 'g_state1p'),
  #   species_output$current_positive, l_add3_2_p_carry,
  #   species_output$current_positive,
  #   0, 0,
  #   fuel_states, rate_states1
  # ) 
  # gates[[length(gates)+1]] <- g_state1p
  # g_state1n <- Make_Adder_apBeC(
  #   jn(name, 'g_state1n'),
  #   species_output$current_negative, l_add3_2_n_carry,
  #   species_output$current_negative,
  #   0, 0,
  #   fuel_states, rate_states1
  # ) 
  # gates[[length(gates)+1]] <- g_state1n

  
  l_delay1_p <- jn(name, 'l_delay1_p')
  l_delay1_n <- jn(name, 'l_delay1_n')

l_scaler1_p <- jn(name, 'l_scaler1_p')
  g_scaler1_p <- Make_Mul2In_Wang(jn(name, 'g_scaler1_p'),
    jn(name, '_1em3'), l_add3_2_p_carry, l_scaler1_p,
    1e-3, 0,
    rate_mul3
  )
  gates[[length(gates)+1]] <- g_scaler1_p
      l_scaler1_n <- jn(name, 'l_scaler1_n')
      g_scaler1_n <- Make_Mul2In_Wang(jn(name, 'g_scaler1_n'),
        jn(name, '_1em3'), l_add3_2_n_carry, l_scaler1_n,
        1e-3, 0,
        rate_mul3
      )
      gates[[length(gates)+1]] <- g_scaler1_n

  l_consume1_p <- jn(name, 'l_consume1_p')
  g_consume1_p <- Make_Adder_apBeC(
    jn(name, 'g_consume1_p'),
    l_scaler1_p, l_consume1_p,
    l_consume1_p,
    0, 0,
    fuel_states, rate_states1
  )
  gates[[length(gates)+1]] <- g_consume1_p
      l_consume1_n <- jn(name, 'l_consume1_n')
      g_consume1_n <- Make_Adder_apBeC(
        jn(name, 'g_consume1_n'),
        l_scaler1_n, l_consume1_n,
        l_consume1_n,
        0, 0,
        fuel_states, rate_states1
      )
      gates[[length(gates)+1]] <- g_consume1_n
  
  g_delay1 <- Make_Integrator_OishiYordanov(jn(name, 'g_int1'),
    l_scaler1_p, l_scaler1_n,
    l_delay1_p, l_delay1_n,
    0, 0,
    rate_int1
  )
  # Make_Derivative(
  #   jn(name, 'g_der1'),
  #   l_add3_2_p_carry, l_add3_2_n_carry,
  #   l_delay1_p, l_delay1_n,
  #   0, 0,
  #   rate_int1
  # )
  gates[[length(gates)+1]] <- g_delay1

  g_mul2_p <- Make_Mul2In_Wang(jn(name, 'mul1_p'),
    l_delay1_p, jn(name, 'l_rol'), l_mul2_p, #  species_output$current_positive, jn(name, 'l_rol'), l_mul2_p,
    0, ic$resistance/ic$inductance,
    rate_mul2
  )
  gates[[length(gates)+1]] <- g_mul2_p
      g_mul2_n <- Make_Mul2In_Wang(jn(name, 'mul1_n'),
        l_delay1_n, jn(name, 'l_rol'), l_mul2_n, # species_output$current_negative, jn(name, 'l_rol'), l_mul1_n,
        0, ic$resistance/ic$inductance,
        rate_mul2
      )
      gates[[length(gates)+1]] <- g_mul2_n

  g_mul3_p <- Make_Mul2In_Wang(jn(name, 'mul3_p'),
    l_delay1_p, jn(name, '_1oc'), l_mul3_p, # species_output$current_positive, jn(name, '_1oc'), l_mul3_p,
    0, 1/ic$capacitance,
    rate_mul3
  )
  gates[[length(gates)+1]] <- g_mul3_p
      g_mul3_n <- Make_Mul2In_Wang(jn(name, 'mul3_n'),
        l_delay1_n, jn(name, '_1oc'), l_mul3_n, # species_output$current_negative, jn(name, '_1oc'), l_mul3_n, 
        0, 1/ic$capacitance,
        rate_mul3
      )
      gates[[length(gates)+1]] <- g_mul3_n

  l_scaler2_p <- jn(name, 'l_scaler2_p')
  scaler2_p <- Make_Mul2In_Wang(jn(name, 'g_scaler2_p'),
    jn(name, '_1em3'), l_mul3_p, l_scaler2_p,
    1e-3, 0,
    rate_mul3
  )
  gates[[length(gates)+1]] <- scaler2_p
      l_scaler2_n <- jn(name, 'l_scaler2_n')
      scaler2_n <- Make_Mul2In_Wang(jn(name, 'g_scaler2_n'),
        jn(name, '_1em3'), l_mul3_n, l_scaler2_n,
        1e-3, 0,
        rate_mul3
      )
      gates[[length(gates)+1]] <- scaler2_n

  l_consume2_p <- jn(name, 'l_consume2_p')
  g_consume2_p <- Make_Adder_apBeC(jn(name, 'g_consume2_p'),
    l_scaler2_p, l_consume2_p,
    l_consume2_p,
    0, 0,
    fuel_states, rate_states2
  )
  gates[[length(gates)+1]] <- g_consume2_p
      l_consume2_n <- jn(name, 'l_consume2_n')
      g_consume2_n <- Make_Adder_apBeC(jn(name, 'g_consume2_n'),
        l_scaler2_n, l_consume2_n,
        l_consume2_n,
        0, 0,
        fuel_states, rate_states2
      )
      gates[[length(gates)+1]] <- g_consume2_n

  l_delay2_p <- jn(name, 'l_delay2_p')
  l_delay2_n <- jn(name, 'l_delay2_n')
  g_delay2_p <- Make_Integrator_OishiYordanov(jn(name, 'g_int1'),
    l_scaler2_p, l_scaler2_n,
    l_delay2_p, l_delay2_n,
    0, 0,
    rate_int1
  )
  # Make_Derivative(jn(name, 'g_der2'),
  #   l_mul3_p, l_mul3_n,
  #   l_delay2_p, l_delay2_n,
  #   0, 0,
  #   rate_int1
  # )
  gates[[length(gates)+1]] <- g_delay2_p
      # g_delay2_n <- Make_Adder2In_Wang(jn(name, 'g_delay2_n'),
      #   l_mul3_n, 'zero',
      #   l_delay2_n,
      #   0, 0,
      #   fuel_states, rate_states2
      # )
      # gates[[length(gates)+1]] <- g_delay2_n

  # g_int2 <- Make_Integrator_OishiYordanov(
  #   jn(name, 'g_int2'),
  #   l_mul3_p, l_mul3_n,
  #   species_output$capacitor_voltage_positive, species_output$capacitor_voltage_negative,
  #   0, 0,
  #   rate_int2
  # )
  # gates[[length(gates)+1]] <- g_int2

  # g_state2p <- Make_Adder_apBeC( 
  #   jn(name, 'g_state2p'),
  #   species_output$capacitor_voltage_positive, l_mul3_p,
  #   species_output$capacitor_voltage_positive,
  #   0, 0,
  #   fuel_states, rate_states2
  # ) 
  # gates[[length(gates)+1]] <- g_state2p
  # g_state2n <- Make_Adder_apBeC(
  #   jn(name, 'g_state2n'),
  #   species_output$capacitor_voltage_negative, l_mul3_n,
  #   species_output$capacitor_voltage_negative,
  #   0, 0,
  #   fuel_states, rate_states2
  # ) 
  # gates[[length(gates)+1]] <- g_state2n

  

  g_mul4_p <- Make_Mul2In_Wang(jn(name, 'mul4_p'),
    l_delay2_p, jn(name, '_m1ol'), l_mul4_p, # species_output$capacitor_voltage_positive, jn(name, '_m1ol'), l_mul4_p,
    0, 1/ic$inductance,
    rate_mul4
  )
  gates[[length(gates)+1]] <- g_mul4_p
      g_mul4_n <- Make_Mul2In_Wang(jn(name, 'mul4_n'),
        l_delay2_n, jn(name, '_m1ol'), l_mul4_n, # species_output$capacitor_voltage_negative, jn(name, '_m1ol'), l_mul4_n,
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

Make_VoltageSource_Component <- function(id, voltage) {
  vcc1 <- c()
  vcc1$name <- jn('v',id)
  vcc1$ol$voltage <- jn(vcc1$name,'_vcc')
  vcc1$ol$current <- jn(vcc1$name,'_i')
  vcc1$ic$voltage <- voltage
  vcc1$ic$current <- 0
  return(vcc1)
}

Make_CurrentSource_Component <- function(id, current) {
  drain <- c()
  drain$name <- jn('i',id)
  drain$ol$voltage <- jn(drain$name,'_vcc')
  drain$ol$current <- jn(drain$name,'_i')
  drain$ic$voltage <- 0
  drain$ic$current <- current
  return(drain)
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


Make_Capacitor_Component <- function(id, capacitance) {

  c1 <- c()
  # Dual rail species for charge, voltage, and current

  c1$name <- jn('c',id)
  c1$il$capacitance <- jn(c1$name,'_cap')
  c1$ic$capacitance <- capacitance # q/V=C[farad]
  # Dual rail species for charge, voltage, and current
  c1$il$voltage_positive <- jn(c1$name,'il_vp')
  c1$il$voltage_negative <- jn(c1$name,'il_vn')
  c1$il$current_positive <- jn(c1$name,'il_ip')
  c1$il$current_negative <- jn(c1$name,'il_in')
  
  c1$ol$voltage_positive <- jn(c1$name,'ol_vp')
  c1$ol$voltage_negative <- jn(c1$name,'ol_vn')
  c1$ol$current_positive <- jn(c1$name,'ol_ip')
  c1$ol$current_negative <- jn(c1$name,'ol_in')
  c1$ic$voltage_positive <- 0
  c1$ic$voltage_negative <- 0
  c1$ic$current_positive <- 0
  c1$ic$current_negative <- 0
  return(c1)
}

Make_Inductor_Component <- function(id, inductance) {

  l1 <- c()
  # Dual rail species for charge, voltage, and current

  l1$name <- jn('l',id)
  l1$il$inductance <- jn(l1$name,'_ind')
  l1$ic$inductance <- inductance # henry
  # Dual rail species for charge, voltage, and current
  # l1$il$voltage_positive <- jn(l1$name,'il_vp')
  # l1$il$voltage_negative <- jn(l1$name,'il_vn')
  l1$il$current_positive <- jn(l1$name,'il_ip')
  l1$il$current_negative <- jn(l1$name,'il_in')

  l1$ol$voltage_positive <- jn(l1$name,'ol_vp')
  l1$ol$voltage_negative <- jn(l1$name,'ol_vn')
  l1$ol$current_positive <- jn(l1$name,'ol_ip')
  l1$ol$current_negative <- jn(l1$name,'ol_in')

  # l1$ic$voltage_positive <- 0
  # l1$ic$voltage_negative <- 0
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