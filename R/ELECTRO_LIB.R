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
        1/ic$capacitance, 0,
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
        1/ic$capacitance, 0,
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

  # Integrate Current input voltage to Equivalent tension from Current Source
  g_ve_integrator <- Make_Integrator_OishiYordanov(
    jn(name, '_g_i_int'), 
    species_input$current_positive, species_input$current_negative,
    l_vep, l_ven,
    0, 0,
    rate
  )
  gates[[length(gates)+1]] <- g_ve_integrator

  # Computer current in the resistor 
  # i_r = 1/R v_e  
  g_pir <- Make_Mul2In_Wang(
    jn(name, 'g_pir'),
    'res',  l_vep, 
    species_output$current_positive_resistor,
    ic$inductance, 0,
    rate
  )
  gates[[length(gates)+1]] <- g_pir
      g_nir <- Make_Mul2In_Wang(
        jn(name, 'g_nir'),
        'res',  l_ven, 
        species_output$current_negative_resistor,
        ic$inductance, 0,
        rate
      )
      gates[[length(gates)+1]] <- g_nir

  # Derivate the voltage in the inductor
  g_dv_in <- Make_Derivative(
    jn(name, 'g_dv_in'),
    l_vep, l_ven,
    species_output$voltage_positive, species_output$voltage_negative,
    0, 0,
    rate
  )
  gates[[length(gates)+1]] <- g_dv_in

  # Computer current in the inductor
  g_pil <- Make_Mul2In_Wang(
    jn(name, 'g_pil'),
    species_input$inductance, species_output$voltage_positive, species_output$current_positive_inductor,
    1/ic$inductance, 0,
    rate
  )
  gates[[length(gates)+1]] <- g_pil
      g_nil <- Make_Mul2In_Wang(
        jn(name, 'g_nil'),
        species_input$inductance, species_output$voltage_negative, species_output$current_negative_inductor,
        1/ic$inductance, 0,
        rate
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

Make_Circuit_RLC <- function(name, species_input, species_output, ic, rate, fuel) {
  gates <- list()

  l_1ol <- jn(name, 'l_1ol')
  l_g1_p <- jn(name, 'l_g1_p')
  g1_p <- Make_Mul2In_Wang(jn(name, 'g1_p'),
    species_input$voltage_positive, l_1ol, l_g1_p,
    0, 1/ic$inductance,
    rate
  )
  gates[[length(gates)+1]] <- g1_p

      l_g1_n <- jn(name, 'l_g1_n')
      g1_n <- Make_Mul2In_Wang(jn(name, 'g1_n'),
        species_input$voltage_negative, l_1ol, l_g1_n,
        0, 1/ic$inductance,
        rate
      )
      gates[[length(gates)+1]] <- g1_n

  l_g2_carry1 <- jn(name, 'l_g2_carry1')
  l_g2_carry2 <- jn(name, 'l_g2_carry2')
  l_g2_p <- jn(name, 'l_g2_p')
  l_g2_n <- jn(name, 'l_g2_n')
  # v/l + (-r/l x1) + (-1/l x2)
     # ap = [r/l x1n -> g4_n + vp/l -> g1_p] + 1/l x2n -> g7_n
     # an = [r/l x1p -> g4_p + vn/l -> g1_n] + 1/l x2p -> g7_p
  l_g4_n <- jn(name, 'l_g4_n')
  l_g4_p <- jn(name, 'l_g4_p')
  l_g7_p <- jn(name, 'l_g7_p')
  l_g7_n <- jn(name, 'l_g7_n')

  l_g4_pFIXED <- jn(name, 'l_g4_pFIXED')
  l_g4_nFIXED <- jn(name, 'l_g4_nFIXED')
  l_g7_pFIXED <- jn(name, 'l_g7_pFIXED')
  l_g7_nFIXED <- jn(name, 'l_g7_nFIXED')
  l_g1_pFIXED <- jn(name, 'l_g1_pFIXED')
  l_g1_nFIXED <- jn(name, 'l_g1_nFIXED')

  g2_0 <- Make_Sub2In_Wang(jn(name, 'g2_0'),
    l_g4_n, l_g4_p, l_g4_nFIXED,
    0, 0,
    fuel, rate
  ) 
  gates[[length(gates)+1]] <- g2_0
  g2_1 <- Make_Sub2In_Wang(jn(name, 'g2_1'),
    l_g7_n, l_g7_p, l_g7_nFIXED,
    0, 0,
    fuel, rate
  )
  gates[[length(gates)+1]] <- g2_1
  g2_2 <- Make_Sub2In_Wang(jn(name, 'g2_2'),
    l_g1_n, l_g1_p, l_g1_nFIXED,
    0, 0,
    fuel, rate
  )
  gates[[length(gates)+1]] <- g2_2
  g2_p <- Make_Adder2In_Wang(jn(name, 'g2_p'),
    l_g4_n, l_g7_n, l_g2_carry1, # l_g4_nFIXED, l_g7_nFIXED, l_g2_carry1
    0, 0,
    fuel, rate
  )
  gates[[length(gates)+1]] <- g2_p

        l_g2_p_ <- Make_Adder2In_Wang(jn(name, 'g2_p_'),
          l_g2_carry1, l_g1_n, l_g2_p, # l_g2_carry1, l_g1_nFIXED, l_g2_p,
          0, 0,
          fuel, rate
        )
        gates[[length(gates)+1]] <- l_g2_p_
  
  g2_3 <- Make_Sub2In_Wang(jn(name, 'g2_3'),
    l_g4_p, l_g4_n, l_g4_pFIXED,
    0, 0,
    fuel, rate
  )
  gates[[length(gates)+1]] <- g2_3
  g2_4 <- Make_Sub2In_Wang(jn(name, 'g2_4'),
    l_g7_p, l_g7_n, l_g7_pFIXED,
    0, 0,
    fuel, rate
  )
  gates[[length(gates)+1]] <- g2_4
  g2_5 <- Make_Sub2In_Wang(jn(name, 'g2_5'),
    l_g1_p, l_g1_n, l_g1_pFIXED,
    0, 0,
    fuel, rate
  )
  gates[[length(gates)+1]] <- g2_5
  g2_n <- Make_Adder2In_Wang(jn(name, 'g2_n'),
    l_g4_p, l_g7_p, l_g2_carry2, # l_g4_pFIXED, l_g7_pFIXED, l_g2_carry2,
    0, 0,
    fuel, rate
  )
  gates[[length(gates)+1]] <- g2_n

        l_g2_n_ <- Make_Adder2In_Wang(jn(name, 'g2_n_'),
          l_g2_carry2, l_g1_pFIXED, l_g2_n, # l_g2_carry2, l_g1_pFIXED, l_g2_n,
          0, 0,
          fuel, rate
        )
        gates[[length(gates)+1]] <- l_g2_n_

  g3 <- Make_Integrator_OishiYordanov(
    jn(name, 'g3'),
    l_g2_p, l_g2_n,
    species_output$current_positive, species_output$current_negative,
    0, 0,
    rate
  )
  gates[[length(gates)+1]] <- g3

  g4_p <- Make_Mul2In_Wang(jn(name, 'g4_p'),
    species_output$current_positive, jn(name, '_rol'), l_g4_p,
    0, ic$resistance/ic$inductance,
    rate
  )
  gates[[length(gates)+1]] <- g4_p

      g4_n <- Make_Mul2In_Wang(jn(name, 'g4_n'),
        species_output$current_negative, jn(name, '_rol'), l_g4_n,
        0, ic$resistance/ic$inductance,
        rate
      )
      gates[[length(gates)+1]] <- g4_n

  l_g5_p <- jn(name, 'l_g5_p')
  g5_p <- Make_Mul2In_Wang(jn(name, 'g5_p'),
    species_output$current_positive, jn(name, '_1oc'), l_g5_p,
    0, 1/ic$capacitance,
    rate
  )
  gates[[length(gates)+1]] <- g5_p

      l_g5_n <- jn(name, 'l_g5_n')
      g5_n <- Make_Mul2In_Wang(jn(name, 'g5_n'),
        species_output$current_negative, jn(name, '_1oc'), l_g5_n,
        0, 1/ic$capacitance,
        rate
      )
      gates[[length(gates)+1]] <- g5_n

  g6 <- Make_Integrator_OishiYordanov(
    jn(name, 'g6_p'),
    l_g5_p, l_g5_n,
    species_output$capacitor_voltage_positive, species_output$capacitor_voltage_negative,
    0, 0,
    rate
  )
  gates[[length(gates)+1]] <- g6

  g7_p <- Make_Mul2In_Wang(jn(name, 'g7_p'),
    species_output$capacitor_voltage_positive, jn(name, '_m1ol'), l_g7_p,
    0, 1/ic$inductance,
    rate
  ) 
  gates[[length(gates)+1]] <- g7_p

      g7_n <- Make_Mul2In_Wang(jn(name, 'g7_n'),
        species_output$capacitor_voltage_negative, jn(name, '_m1ol'), l_g7_n,
        0, 1/ic$inductance,
        rate
      )
      gates[[length(gates)+1]] <- g7_n

  return (gates)
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
  l1$il$current_positive <- jn(l1$name,'il_ip')
  l1$il$current_negative <- jn(l1$name,'il_in')

  l1$ol$voltage_positive <- jn(l1$name,'ol_vp')
  l1$ol$voltage_negative <- jn(l1$name,'ol_vn')
  l1$ol$current_positive <- jn(l1$name,'ol_ip')
  l1$ol$current_negative <- jn(l1$name,'ol_in')

  l1$ic$current_positive <- 0
  l1$ic$current_negative <- 0

  # l1$ol$current_positive_inductor <- jn(l1$name,'ol_pil')
  # l1$ol$current_negative_inductor <- jn(l1$name,'ol_nil')
  # l1$ol$current_positive_resistor <- jn(l1$name,'ol_pir')
  # l1$ol$current_negative_resistor <- jn(l1$name,'ol_nir')
  # l1$ic$current_positive_inductor <- 0
  # l1$ic$current_negative_inductor <- 0
  # l1$ic$current_positive_resistor <- 0
  # l1$ic$current_negative_resistor <- 0

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

  rlc$ol$capacitor_voltage_positive <- jn(rlc$name,'ol_vcp')
  rlc$ol$capacitor_voltage_negative <- jn(rlc$name,'ol_vcn')
  rlc$ol$current_positive <- jn(rlc$name,'ol_ip')
  rlc$ol$current_negative <- jn(rlc$name,'ol_in')

  return(rlc)
}
