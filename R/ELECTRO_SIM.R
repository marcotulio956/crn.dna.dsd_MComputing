simulate_sRC_voltage_source <- function(timing, source_voltage, resistance, capacitance) {
  n <- length(timing)

  capacitor_voltage <- numeric(n)
  resistor_voltage <- numeric(n)
  current_output <- numeric(n)

  capacitor_voltage[1] <- 0

  # -I_in = I_C = I_R = I
  # I_C = C * dV_C/dt
  # V_C = V_in - V_R
  # dV_C/dt = (V_in - V(C) ) / (R * C)
  for (i in 1:(n - 1)) {
    dt <- timing[i + 1] - timing[i] # Time step
    dVc_dt <- (source_voltage[i] - capacitor_voltage[i]) / (resistance * capacitance)
    capacitor_voltage[i + 1] <- capacitor_voltage[i] + dt * dVc_dt
  }

  # V_R = V_in - V_C
  resistor_voltage <- source_voltage - capacitor_voltage

  # I = V(R) / R
  current_output <- resistor_voltage / resistance

  return(list(
    capacitor_voltage = capacitor_voltage,
    resistor_voltage = resistor_voltage,
    current_output = current_output
  ))
}

# 1) Series RL with Voltage Source (you already had this)
simulate_sRL_voltage_source <- function(timing, source_voltage, resistance, inductance) {
  n <- length(timing)
  current_output <- numeric(n)
  current_output[1] <- 0

  # di/dt = (v_in - R*i) / L
  for (i in 1:(n - 1)) {
    dt <- timing[i + 1] - timing[i]
    di_dt <- (source_voltage[i] - resistance * current_output[i]) / inductance
    current_output[i + 1] <- current_output[i] + dt * di_dt
  }

  resistor_voltage <- resistance * current_output
  inductor_voltage <- source_voltage - resistor_voltage

  return(list(
    source_voltage   = source_voltage,
    current_output = current_output,
    resistor_voltage = resistor_voltage,
    inductor_voltage = inductor_voltage
  ))
}

simulate_C_voltage_source <- function(timing, source_voltage, capacitance,
                                      initial_voltage = 0) {
  n <- length(timing)

  capacitor_voltage <- numeric(n)
  current_output <- numeric(n)

  capacitor_voltage[1] <- initial_voltage

  # i_C = C * dV_C/dt
  # dV_C/dt = i_C / C
  #
  # For a voltage source directly connected to an ideal capacitor,
  # the current depends on dV/dt:
  # i_C = C * dV_source/dt
  #
  # Use forward finite differences.
  for (i in 1:(n - 1)) {
    dt <- timing[i + 1] - timing[i]

    dV_dt <- (source_voltage[i + 1] - source_voltage[i]) / dt

    capacitor_voltage[i + 1] <- source_voltage[i + 1]
    current_output[i] <- capacitance * dV_dt
  }

  # Current at the last point
  dt <- timing[n] - timing[n - 1]
  dV_dt <- (source_voltage[n] - source_voltage[n - 1]) / dt
  current_output[n] <- capacitance * dV_dt

  return(list(
    source_voltage = source_voltage,
    capacitor_voltage = capacitor_voltage,
    current_output = current_output
  ))
}


simulate_L_voltage_source <- function(timing, source_voltage, inductance,
                                      initial_current = 0) {
  n <- length(timing)

  current_output <- numeric(n)
  inductor_voltage <- numeric(n)

  current_output[1] <- initial_current

  # V_L = L * di/dt
  # di/dt = V_L / L
  #
  # For an ideal inductor directly connected to a voltage source:
  # di/dt = V_source / L

  for (i in 1:(n - 1)) {
    dt <- timing[i + 1] - timing[i]

    di_dt <- source_voltage[i] / inductance

    current_output[i + 1] <-
      current_output[i] + dt * di_dt
  }

  inductor_voltage <- source_voltage

  return(list(
    source_voltage = source_voltage,
    current_output = current_output,
    inductor_voltage = inductor_voltage
  ))
}

simulate_R_voltage_source <- function(timing, source_voltage, resistance) {
  n <- length(timing)

  resistor_voltage <- source_voltage
  resistor_current <- source_voltage / resistance

  return(list(
    source_voltage = source_voltage,
    resistor_voltage = resistor_voltage,
    resistor_current = resistor_current
  ))
}

# 2) Parallel RL with Current Source (you already had this)
simulate_pRL_current_source <- function(timing, source_current, resistance, inductance) {
  n <- length(timing)
  ind_current <- numeric(n)
  ind_current[1] <- 0

  # di_L/dt = (R/L)*(i_source - i_L)
  for (i in 1:(n - 1)) {
    dt <- timing[i + 1] - timing[i]
    di_dt <- (resistance / inductance) * (source_current[i] - ind_current[i])
    ind_current[i + 1] <- ind_current[i] + dt * di_dt
  }

  # node voltage is same across R and L
  node_voltage <- resistance * (source_current - ind_current)

  return(list( 
    source_current   = source_current,
    inductor_current = ind_current,
    inductor_voltage= node_voltage,
    resistor_voltage = node_voltage
  ))
}


# 3) Parallel RL with Voltage Source
simulate_pRL_voltage_source <- function(timing, source_voltage, resistance, inductance) {
  n <- length(timing)
  # Inductor current integrates di/dt = v/L
  ind_current <- numeric(n)
  ind_current[1] <- 0

  for (i in 1:(n - 1)) {
    dt <- timing[i + 1] - timing[i]
    di_dt <- source_voltage[i] / inductance
    ind_current[i + 1] <- ind_current[i] + dt * di_dt
  }

  # Resistor branch current
  resistor_current <- source_voltage / resistance
  # Total source current splits between R and L
  source_current <- resistor_current + ind_current

  return(list(
    source_voltage    = source_voltage,
    source_current    = source_current,
    resistor_current  = resistor_current,
    inductor_current  = ind_current,
    resistor_voltage  = source_voltage,
    inductor_voltage  = source_voltage
  ))
}

# 3.5) R1 || (R2 L) with voltage source
simulate_pR_pRL_voltage_source <- function(timing, source_voltage, R1, R2, L) {
  n <- length(timing)
  # The voltage across R2 and L is the same as across R1 (parallel branches)
  # Let v_node be the voltage across R2-L branch (and across R1)
  v_node <- numeric(n)
  v_node[1] <- 0

  # Current through inductor in R2-L branch
  iL <- numeric(n)
  iL[1] <- 0

  for (i in 1:(n - 1)) {
    dt <- timing[i + 1] - timing[i]
    # Current through R1: iR1 = v_node / R1
    # Current through R2-L: iRL = iL
    # KCL: iR1 + iL = total current from source
    # KVL for R2-L: v_node = R2 * iL + L * diL/dt
    # But v_node = source_voltage[i] (since both branches are in parallel)
    # So: source_voltage[i] = R2 * iL[i] + L * diL/dt
    # Rearranged: diL/dt = (source_voltage[i] - R2 * iL[i]) / L
    diL_dt <- (source_voltage[i] - R2 * iL[i]) / L
    iL[i + 1] <- iL[i] + dt * diL_dt
    v_node[i + 1] <- source_voltage[i + 1] # voltage across R2 (and L) at next step
  }

  resistor2_voltage <- R2 * iL
  inductor_current <- iL

  return(list(
    resistor2_voltage = resistor2_voltage,
    inductor_current  = inductor_current
  ))
}

# 4) Series RL with Current Source
simulate_sRL_current_source <- function(timing, source_current, resistance, inductance) {
  n <- length(timing)
  # In a series connection the inductor current equals the source current
  ind_current <- numeric(n)
  ind_current <- source_current

  # Resistor voltage drop v_R = R * i
  resistor_voltage <- resistance * ind_current

  # Inductor voltage v_L = L * di/dt
  inductor_voltage <- numeric(n)
  inductor_voltage[1] <- (ind_current[2] - ind_current[1]) / (timing[2] - timing[1]) * inductance
  for (i in 1:(n - 1)) {
    dt <- timing[i + 1] - timing[i]
    di_dt <- (ind_current[i + 1] - ind_current[i]) / dt
    inductor_voltage[i + 1] <- inductance * di_dt
  }

  # Total source voltage is sum of drops
  source_voltage <- resistor_voltage + inductor_voltage

  return(list(
    source_current   = source_current,
    resistor_voltage = resistor_voltage,
    inductor_voltage = inductor_voltage,
    source_voltage   = source_voltage
  ))
}

simulate_pL_RL_voltage_source <- function(timing, source_voltage, R, L1, L2) {
  n <- length(timing)
  stopifnot(length(source_voltage) == n)
  
  # Pre-allocate
  i1 <- numeric(n)   # current through series R–L1
  i2 <- numeric(n)   # current through pure inductor L2
  
  # Initial conditions
  i1[1] <- 0
  i2[1] <- 0
  
  # Euler integrate both branches
  for (k in seq_len(n-1)) {
    dt <- timing[k+1] - timing[k]
    
    # di1/dt = (v_source - R*i1) / L1
    di1 <- (source_voltage[k] - R * i1[k]) / L1
    # di2/dt = v_source / L2  (since no series R)
    di2 <- source_voltage[k] / L2
    
    i1[k+1] <- i1[k] + dt * di1
    i2[k+1] <- i2[k] + dt * di2
  }
  
  # Voltages across each inductor
  #   v_L1 = L1 * di1/dt  OR  v_source - R*i1  (equivalent)
  #   v_L2 = L2 * di2/dt  OR  v_source
  vL1 <- source_voltage - R * i1
  vL2 <- source_voltage
  
  # Total source current (sums both branches)
  i_source <- i1 + i2

  resistor_voltage <- R * i1
  
  return(list(
    timing         = timing,
    resistor_voltage = resistor_voltage,
    i_branch1      = i1,
    i_branch2      = i2,
    i_source       = i_source,
    vL1            = vL1,
    vL2            = vL2
  ))
}

simulate_sRLC_voltage_source <- function(timing, source_voltage, resistance, inductance, capacitance) {
  n <- length(timing)
  
  x1 <- numeric(n)
  x2 <- numeric(n)

  x1[1] <- 0
  x2[1] <- 0

  sum_dx1 <- numeric(n)
  sum_dx2 <- numeric(n)
  sum_dx1[1] <- 0
  sum_dx2[1] <- 0

  for (i in 1:(n - 1)) {
    dt <- timing[i + 1] - timing[i]
    dx2_dt <- (1 / capacitance) * x1[i]
    dx1_dt <- -(1 / inductance) * x2[i] - (resistance / inductance) * x1[i] + (1 / inductance) * source_voltage[i]
    sum_dx1[i + 1] <- sum_dx1[i] + dx1_dt
    sum_dx2[i + 1] <- sum_dx2[i] + dx2_dt
    x1[i + 1] <- x1[i] + dt * dx1_dt
    x2[i + 1] <- x2[i] + dt * dx2_dt
  }

  ind_current <- x1
  cap_voltage <- x2

  return(list(
    capacitor_voltage = cap_voltage,
    inductor_current = ind_current,
    source_voltage = source_voltage,
    sum_dx1 = sum_dx1,
    sum_dx2 = sum_dx2
  ))
}

simulate_Vcc <- function(t,
                         V1 = 0, # tensão inicial
                         V2 = 10, # tensão de pulso
                         TD = 10, # atraso antes do primeiro pulso
                         TR = 0.001, # tempo de subida
                         TF = 0.001, # tempo de descida
                         PW = 13.27, # largura do pulso em nível V2
                         PER = 40 # período total
) {
  # t: vetor de tempos (mesmo que você usar em simulate_sRLC, etc.)
  # Retorna vetor de mesmo comprimento com a tensão em cada instante

  # Função auxiliar: dado um único tempo ti, calcula V(ti)
  pulse_at <- function(ti) {
    if (ti < TD) {
      return(V1)
    }
    tau <- (ti - TD) %% PER
    # fase de subida
    if (tau < TR) {
      return(V1 + (V2 - V1) * (tau / TR))
    }
    # nível alto
    if (tau < PW) {
      return(V2)
    }
    # fase de descida
    if (tau < PW + TF) {
      return(V2 - (V2 - V1) * ((tau - PW) / TF))
    }
    # repouso nível baixo até o próximo período
    V1
  }

  # Gera vetor aplicando a função a cada tempo (vectorize)
  vapply(t, pulse_at, numeric(1))
}