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
    ic$capacitance/2, 0,
    rate
  )
  gates[[length(gates)+1]] <- g_ip
  
  # in = C dvn
  g_in <- Make_Mul2In_Wang(jn(name, 'g_in'),
    species_input$capacitance, l_dvn, species_output$current_negative,
    ic$capacitance/2, 0,
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
        1/ic$capacitance*2, 0,
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
        1/ic$capacitance*2, 0,
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
  
  # Compute derivative of current: di/dt
  l_dip <- jn(name, 'l_dip')
  l_din <- jn(name, 'l_din')
  
  g_i_derivative <- Make_Derivative(
    jn(name, 'g_di_in'),
    species_input$current_positive, species_input$current_negative,
    l_dip, l_din,
    ic$current_positive, ic$current_negative,
    rate
  )
  gates[[length(gates)+1]] <- g_i_derivative
  
  # Compute voltage: v = L * di/dt
  g_vp <- Make_Mul2In_Wang(
    jn(name, 'g_vp'),
    species_input$inductance, l_dip, species_output$voltage_positive,
    ic$inductance/2, 0,
    rate
  )
  gates[[length(gates)+1]] <- g_vp
  
  g_vn <- Make_Mul2In_Wang(
    jn(name, 'g_vn'),
    species_input$inductance, l_din, species_output$voltage_negative,
    ic$inductance/2, 0,
    rate
  )
  gates[[length(gates)+1]] <- g_vn
  
  # Integrate voltage to obtain magnetic flux: F = int v dt
  l_pflux <- jn(name, 'l_pflux')
  l_nflux <- jn(name, 'l_nflux')
  g_flux_integrator <- Make_Integrator_OishiYordanov(
    jn(name, '_g_i_int'), 
    species_output$voltage_positive, species_output$voltage_negative,
    l_pflux, l_nflux,
    0, 0,   # initial flux values; adjust if needed
    rate
  )
  gates[[length(gates)+1]] <- g_flux_integrator
  
      # Recover the current from the flux: i = (1/L)*F
      g_ip <- Make_Mul2In_Wang(
        jn(name, 'g_ip'),
        jn(name, '_1oL'), l_pflux, species_output$current_positive,
        2/ic$inductance, 0,
        rate
      )
      gates[[length(gates)+1]] <- g_ip
      
      g_in <- Make_Mul2In_Wang(
        jn(name, 'g_in'),
        jn(name, '_1oL'), l_nflux, species_output$current_negative,
        2/ic$inductance, 0,
        rate
      )
      gates[[length(gates)+1]] <- g_in
  
  return(gates)
}

# Make_Circuit_RLC_blocks <- function(name, species_input, species_output, ic, rate) {
#   # v -1/L-> s1 -\int-> x_1 - 
# }


Make_Wire <- function(name, li_input1, lo_output1, ic_value){
  rate <- 2e3
  gates <- list()
  g1 <- Make_Mul2In_Wang(jn(name, 'wire1'),
    li_input1, jn(li_input1,'_'), lo_output1,
    ic_value, 1,
    rate
  )
  gates[[1]] <- g1
  return(gates)
}

Make_Resistor <- function(name, species_input, species_output, ic) {
  rate <- 2e3
  gates <- list()

  # V_out=R*I_in
  g1 <- Make_Mul2In_Wang(jn(name, 'mul1'),
    species_input$current, jn(name,'_R'), species_output$voltage,
    ic$resistence, ic$current,
    rate
  )
  gates[[1]] <- g1

  # # I_out=I_in
  # g2 <- Make_Buffer_Lakin(jn(name, 'buf1'),
  #   species_input$current, jn(species_input$current,'_waste'),
  #   ic$current,
  #   1e3, 1e3, 1e-1 # ex1: 1e-1 computacao de V em funcao i(t)
  # )
  # gates[[2]] <- g2

  g3 <- Make_Wire(jn(name, "wire1"), species_input$current, species_output$current, ic$current)
  gates[[3]] <- g3

  return (gates)
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

Make_RLC_crn_simple <- function(
    name, R, L, C,          
    nameInputPos, nameInputNeg,      # vinp, vinn
    # capacitor voltage outputs: v_C^+, v_C^-
    # inductor current outputs: i_L^+, i_L^-
    cinputPos, cinputNeg,            # initial conditions for the input species
    rate) {

  # Define dual–rail species: each circuit variable is split into a positive and negative rail.
  species <- list(
    # Dual–rail input for v_in:
    input_pos    = nameInputPos,  # represents v_in^+
    input_neg    = nameInputNeg,  # represents v_in^-
    # Dual–rail state variables:
    state1_pos   = jn(name, "_iL+"),  # inductor current positive rail (i_L^+)
    state1_neg   = jn(name, "_iL-"),  # inductor current negative rail (i_L^-)
    state2_pos   = jn(name, "_vC+"),  # capacitor voltage positive rail (v_C^+)
    state2_neg   = jn(name, "_vC-")   # capacitor voltage negative rail (v_C^-)
  )
  ci <- c(cinputPos, cinputNeg, 0, 0, 0, 0)

  reactions <- c(
    # (1) Production of inductor current from input:
    #    v_in^+ -> v_in^+ + i_L^+
    jn(species$input_pos, " -> ", species$input_pos, " + ", species$state1_pos),
    #    v_in^- -> v_in^- + i_L^-
    jn(species$input_neg, " -> ", species$input_neg, " + ", species$state1_neg),
    
    # (2) Resistive decay of inductor current:
    #    i_L^+ -> 0
    jn(species$state1_pos, " -> 0"),
    #    i_L^- -> 0
    jn(species$state1_neg, " -> 0"),
    
    # (3) Coupling: capacitor voltage cancels inductor current.
    #    For the positive rail, v_C^+ removes i_L^+:
    jn(species$state2_pos, " + ", species$state1_pos," -> ",  species$state2_pos),
    #    For the negative rail, v_C^- removes i_L^-:
    jn(species$state2_neg, " + ", species$state1_neg," -> ",  species$state2_neg),
    
    # (4) Capacitor charging: inductor current produces capacitor voltage.
    #    i_L^+ produces v_C^+
    jn(species$state1_pos, " -> ", species$state1_pos, " + ", species$state2_pos),
    #    i_L^- produces v_C^-
    jn(species$state1_neg, " -> ", species$state1_neg, " + ", species$state2_neg),
    
    # (5) Annihilation (cancellation) reactions for state variables:
    #    i_L^+ + i_L^- -> 0 (ensures net i_L = i_L^+ - i_L^-)
    jn(species$state1_pos, " + ", species$state1_neg, " -> 0"),
    #    v_C^+ + v_C^- -> 0 (ensures net v_C = v_C^+ - v_C^-)
    jn(species$state2_pos, " + ", species$state2_neg, " -> 0")
  )
  
  # Define rate constants for each group of reactions:
  k_vin       <- (1 / L) * rate       # (1) Production of i_L from input
  k_R         <- 0 # (R / L) * rate       # (2) Resistive decay of i_L
  k_couple    <- (1 / L) * rate       # (3) Coupling: v_C cancels i_L
  k_cap       <- (1 / C) * rate       # (4) Capacitor charging: i_L produces v_C
  # Set an annihilation rate constant (should be high so that opposing rails cancel fast)
  k_annihil_val <- rate         # (5) and (10) Cancellation reactions
  
  # Assemble the vector of rate constants in the same order as the reactions above:
  ki <- c(
    # (1) Production reactions:
    k_vin, k_vin,
    # (2) Resistive decay:
    k_R, k_R,
    # (3) Coupling reactions:
    k_couple, k_couple,
    # (4) Capacitor charging:
    k_cap, k_cap,
    # (5) Annihilation for state variables:
    k_annihil_val, k_annihil_val
  )
  
  dual_rail_circuit <- list(
    name      = name,
    species   = species,
    reactions = reactions,
    ci        = ci,
    ki        = ki
  )
  
  return(dual_rail_circuit)
}


Make_RLC_crn <- function(
    name, R, L, C,
    nameInputPos, nameInputNeg,
    nameOutput1Pos, nameOutput1Neg,  # capacitor voltage outputs: v_C^+, v_C^-
    nameOutput2Pos, nameOutput2Neg,  # inductor current outputs: i_L^+, i_L^-
    nameOutput3Pos, nameOutput3Neg,  # capacitor current outputs: i_C^+, i_C^-
    nameOutput4Pos, nameOutput4Neg,  # inductor voltage outputs: v_L^+, v_L^-
    cinputPos, cinputNeg,           # initial conditions for the input species
    rate) {


# e1_gates <- Make_Circuit_RLC_dualRail(
# 'rlc',
# 10, 17, 13,
# 'vin', 'zero',
# 'vCp', 'vCn',
# 'iLp', 'iLn',
# 'iCp', 'iCn',
# 'vLp', 'vLn',
# 15, 5,
# 1 # rate
# )

  # Define dual–rail species: each circuit variable is split into a positive and negative rail.
  species <- list(
    # Dual–rail input for v_in:
    input_pos    = nameInputPos,  # represents v_in^+
    input_neg    = nameInputNeg,  # represents v_in^-
    # Dual–rail state variables:
    state1_pos   = jn(name, "_iL+"),  # inductor current positive rail (i_L^+)
    state1_neg   = jn(name, "_iL-"),  # inductor current negative rail (i_L^-)
    state2_pos   = jn(name, "_vC+"),  # capacitor voltage positive rail (v_C^+)
    state2_neg   = jn(name, "_vC-"),  # capacitor voltage negative rail (v_C^-)
    # Dual–rail outputs:
    output1_pos  = nameOutput1Pos,    # output for v_C^+
    output1_neg  = nameOutput1Neg,    # output for v_C^-
    output2_pos  = nameOutput2Pos,    # output for i_L^+
    output2_neg  = nameOutput2Neg,    # output for i_L^-
    output3_pos  = nameOutput3Pos,    # output for i_C^+
    output3_neg  = nameOutput3Neg,    # output for i_C^-
    output4_pos  = nameOutput4Pos,    # output for v_L^+
    output4_neg  = nameOutput4Neg,    # output for v_L^-
    # Dummy and intermediate species for catalytic reactions:
    dummy_pos    = jn(name, "_dummy+"),
    dummy_neg    = jn(name, "_dummy-"),
    intermedA_pos = jn(name, "_A+"),
    intermedA_neg = jn(name, "_A-")
  )

  # Initial concentrations: first two for input; all others start at zero.
  ci <- c(cinputPos, cinputNeg, rep(0, 16))
  
  # Build the reaction network.
  # Note: In each reaction the net effect on the dual–rail variable is given by the difference
  # between the positive and negative rails.
  reactions <- c(
    # (1) Production of inductor current from input:
    #    v_in^+ -> v_in^+ + i_L^+
    jn(species$input_pos, " -> ", species$input_pos, " + ", species$state1_pos),
    #    v_in^- -> v_in^- + i_L^-
    jn(species$input_neg, " -> ", species$input_neg, " + ", species$state1_neg),
    
    # (2) Resistive decay of inductor current:
    #    i_L^+ -> 0
    jn(species$state1_pos, " -> 0"),
    #    i_L^- -> 0
    jn(species$state1_neg, " -> 0"),
    
    # (3) Coupling: capacitor voltage cancels inductor current.
    #    For the positive rail, v_C^+ removes i_L^+:
    jn(species$state2_pos, " -> ", species$state2_pos, " + ", species$state1_neg),
    #    For the negative rail, v_C^- removes i_L^-:
    jn(species$state2_neg, " -> ", species$state2_neg,  " + ", species$state1_pos),
    
    # (4) Capacitor charging: inductor current produces capacitor voltage.
    #    i_L^+ produces v_C^+
    jn(species$state1_pos, " -> ", species$state1_pos, " + ", species$state2_pos),
    #    i_L^- produces v_C^-
    jn(species$state1_neg, " -> ", species$state1_neg, " + ", species$state2_neg),
    
    # (5) Annihilation (cancellation) reactions for state variables:
    #    i_L^+ + i_L^- -> 0 (ensures net i_L = i_L^+ - i_L^-)
    jn(species$state1_pos, " + ", species$state1_neg, " -> 0"),
    #    v_C^+ + v_C^- -> 0 (ensures net v_C = v_C^+ - v_C^-)
    jn(species$state2_pos, " + ", species$state2_neg, " -> 0"),
    
    # (6) Output mapping for capacitor voltage:
    #    v_C^+ -> v_C^+ + y_1^+
    jn(species$state2_pos, " -> ", species$state2_pos, " + ", species$output1_pos),
    #    v_C^- -> v_C^- + y_1^-
    jn(species$state2_neg, " -> ", species$state2_neg, " + ", species$output1_neg),
    #    Clear the outputs:
    jn(species$output1_pos, " -> 0"),
    jn(species$output1_neg, " -> 0"),
    
    # (7) Output mapping for inductor current:
    #    i_L^+ -> i_L^+ + y_2^+
    jn(species$state1_pos, " -> ", species$state1_pos, " + ", species$output2_pos),
    #    i_L^- -> i_L^- + y_2^-
    jn(species$state1_neg, " -> ", species$state1_neg, " + ", species$output2_neg),
    #    Clear the outputs:
    jn(species$output2_pos, " -> 0"),
    jn(species$output2_neg, " -> 0"),
    
    # (8) Catalytic generation of capacitor current:
    #    For positive rail: dummy^+ + i_L^+ -> dummy^+ + A^+
    jn(species$dummy_pos, " + ", species$state1_pos, " -> ", species$dummy_pos, " + ", species$intermedA_pos),
    #    For negative rail: dummy^- + i_L^- -> dummy^- + A^-
    jn(species$dummy_neg, " + ", species$state1_neg, " -> ", species$dummy_neg, " + ", species$intermedA_neg),
    #    Convert intermediate to output (positive rail): A^+ -> i_L^+ + y_3^+
    jn(species$intermedA_pos, " -> ", species$state1_pos, " + ", species$output3_pos),
    #    Convert intermediate to output (negative rail): A^- -> i_L^- + y_3^-
    jn(species$intermedA_neg, " -> ", species$state1_neg, " + ", species$output3_neg),
    #    Clear capacitor current outputs:
    jn(species$output3_pos, " -> 0"),
    jn(species$output3_neg, " -> 0"),
    
    # (9) Proxy for inductor voltage (copy capacitor voltage to output):
    #    v_C^+ -> v_C^+ + y_4^+
    jn(species$state2_pos, " -> ", species$state2_pos, " + ", species$output4_pos),
    #    v_C^- -> v_C^- + y_4^-
    jn(species$state2_neg, " -> ", species$state2_neg, " + ", species$output4_neg),
    #    Clear the outputs:
    jn(species$output4_pos, " -> 0"),
    jn(species$output4_neg, " -> 0"),
    
    # (10) Cancellation reactions for output species (optional but helps maintain net differences):
    jn(species$output1_pos, " + ", species$output1_neg, " -> 0"),  # cancel v_C outputs
    jn(species$output2_pos, " + ", species$output2_neg, " -> 0"),  # cancel i_L outputs
    jn(species$output3_pos, " + ", species$output3_neg, " -> 0"),  # cancel i_C outputs
    jn(species$output4_pos, " + ", species$output4_neg, " -> 0")   # cancel v_L outputs
  )
  
  # Define rate constants for each group of reactions:
  k_vin       <- (1 / L) * rate       # (1) Production of i_L from input
  k_R         <- 0 # (R / L) * rate       # (2) Resistive decay of i_L
  k_couple    <- (1 / L) * rate       # (3) Coupling: v_C cancels i_L
  k_cap       <- (1 / C) * rate       # (4) Capacitor charging: i_L produces v_C
  k_copy      <- rate                 # (6), (7), (9) For copying outputs
  k_clear     <- rate * 5             # (6), (7), (9) For clearing outputs
  k_cat       <- (1 / C) * rate       # (8) Catalytic generation (first step)
  k_cat2      <- (1 / C) * rate       # (8) Catalytic conversion (second step)
  k_cat_clear <- k_cat * 5            # (8) For clearing catalytic outputs
  # Set an annihilation rate constant (should be high so that opposing rails cancel fast)
  k_annihil_val <- 0 # 100 * rate         # (5) and (10) Cancellation reactions
  
  # Assemble the vector of rate constants in the same order as the reactions above:
  ki <- c(
    # (1) Production reactions:
    k_vin, k_vin,
    # (2) Resistive decay:
    k_R, k_R,
    # (3) Coupling reactions:
    k_couple, k_couple,
    # (4) Capacitor charging:
    k_cap, k_cap,
    # (5) Annihilation for state variables:
    k_annihil_val, k_annihil_val,
    # (6) Output mapping for capacitor voltage:
    k_copy, k_copy, k_clear, k_clear,
    # (7) Output mapping for inductor current:
    k_copy, k_copy, k_clear, k_clear,
    # (8) Catalytic generation of capacitor current:
    k_cat, k_cat, k_cat2, k_cat2, k_cat_clear, k_cat_clear,
    # (9) Proxy for inductor voltage:
    k_copy, k_copy, k_clear, k_clear,
    # (10) Cancellation for output species:
    k_annihil_val, k_annihil_val, k_annihil_val, k_annihil_val
  )
  
  dual_rail_circuit <- list(
    name      = name,
    species   = species,
    reactions = reactions,
    ci        = ci,
    ki        = ki
  )
  
  return(dual_rail_circuit)
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
  l1$il$voltage_positive <- jn(l1$name,'il_vp')
  l1$il$voltage_negative <- jn(l1$name,'il_vn')
  l1$il$current_positive <- jn(l1$name,'il_ip')
  l1$il$current_negative <- jn(l1$name,'il_in')
  l1$ol$voltage_positive <- jn(l1$name,'ol_vp')
  l1$ol$voltage_negative <- jn(l1$name,'ol_vn')
  l1$ol$current_positive <- jn(l1$name,'ol_ip')
  l1$ol$current_negative <- jn(l1$name,'ol_in')
  l1$ic$voltage_positive <- 0
  l1$ic$voltage_negative <- 0
  l1$ic$current_positive <- 0
  l1$ic$current_negative <- 0
      # Positive Representation  
      # c1$il$charge <- 'c1il_charge'
      # c1$il$voltage_positive <- 'c1il_voltage'
      # c1$il$voltage_negative <- 'c1il_voltage'
      # c1$il$current <- 'c1il_current'

      # c1$ol$charge <- 'c1ol_charge'
      # c1$ol$voltage <- 'c1ol_voltage'
      # c1$ol$current <- 'c1ol_current'

      # c1$ic$charge <- 50
      # c1$ic$voltage <- vcc1$ic$voltage
      # c1$ic$current <- vcc1$ic$current
  return(l1)
}

Make_RLC_Component_dualRail <- function(id, R, L, C, init_ip, init_in, init_vCp, init_vCn) {
  rlc <- list()
  
  # (1) Unique name for the component.
  rlc$name <- jn("rlc", id)
  
  # (2) Input structure: dual–rail voltage input.
  rlc$input <- list(
    v_inp = jn(rlc$name, "_vinp"),  # positive rail for input voltage
    v_inn = jn(rlc$name, "_vinn")   # negative rail for input voltage
  )
  
  # (3) State variables: dual–rail representation of inductor current and capacitor voltage.
  #     The net current: i_L = ip - in, and the net capacitor voltage: v_C = vCp - vCn.
  rlc$st <- list(
    ipos  = jn(rlc$name, "_ip"),    # positive rail for inductor current
    ineg  = jn(rlc$name, "_in"),    # negative rail for inductor current
    vCp = jn(rlc$name, "_vCp"),   # positive rail for capacitor voltage
    vCn = jn(rlc$name, "_vCn")    # negative rail for capacitor voltage
  )
  
  # (4) Output variables: dual–rail outputs for resistor voltage and inductor voltage.
  #     Resistor voltage is computed as v_R = R * i_L, and inductor voltage as v_L = L * d(i_L)/dt.
  rlc$out <- list(
    vRp = jn(rlc$name, "_vRp"),  # resistor voltage positive rail
    vRn = jn(rlc$name, "_vRn"),  # resistor voltage negative rail
    vLp = jn(rlc$name, "_vLp"),  # inductor voltage positive rail
    vLn = jn(rlc$name, "_vLn")   # inductor voltage negative rail
  )
  
  # (5) Initial conditions for the state variables.
  rlc$ic <- list(
    ipos  = init_ip,    # initial positive component of current
    ineg  = init_in,    # initial negative component of current
    vCp = init_vCp,   # initial positive capacitor voltage
    vCn = init_vCn    # initial negative capacitor voltage
  )
  
  # (6) Store circuit parameters.
  rlc$params <- list(R = R, L = L, C = C)
  
  return(rlc)
}


