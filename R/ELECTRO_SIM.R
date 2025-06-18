dnarElectric_simRC <- function(timing, source_voltage, capacitance, resistance) {
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
    dt <- timing[i + 1] - timing[i]  # Time step
    dVc_dt <- (source_voltage[i] - capacitor_voltage[i]) / (resistance * capacitance)
    capacitor_voltage[i + 1] <- capacitor_voltage[i] + dt * dVc_dt
  }
  
  # V_R = V_in - V_C
  resistor_voltage <- source_voltage - capacitor_voltage
  
  # I = V(R) / R
  current_output <- resistor_voltage / resistance
  
  return(list( capacitor_voltage = capacitor_voltage,
              resistor_voltage = resistor_voltage,
              current_output = current_output))
}

simulate_sRL_voltage_source <- function(timing, source_voltage, inductance, resistance) {
  # Number of time points
  n <- length(timing)
  
  # Initialize the state (inductor current) vector
  inductor_current <- numeric(n)
  inductor_current[1] <- 0  # Initial condition: zero current
  
  # Euler integration of the state-space model
  # State equation: dx/dt = -R/L * x + 1/L * v_in
  for (i in 1:(n - 1)) {
    dt <- timing[i + 1] - timing[i]
    di_dt <- (source_voltage[i] - resistance * inductor_current[i]) / inductance
    inductor_current[i + 1] <- inductor_current[i] + dt * di_dt
  }
  
  # Compute additional circuit variables:
  # Resistor voltage: v_R = R * i(t)
  resistor_voltage <- resistance * inductor_current
  
  # Inductor voltage: v_L = v_in - v_R
  inductor_voltage <- source_voltage - resistor_voltage
  
  # Return all pertinent outputs in a list
  return(list(
    source_voltage   = source_voltage,
    inductor_current = inductor_current,      # state variable, i(t)
    resistor_voltage = resistor_voltage,
    inductor_voltage = inductor_voltage
  ))
}

simulate_pRL_current_source <- function(timing, source_current, inductance, resistance) {
  n <- length(timing)
  # State: inductor current, i_L
  ind_current <- numeric(n)
  ind_current[1] <- 0  # initial condition
  
  # Euler integration for the state
  for (i in 1:(n - 1)) {
    dt <- timing[i + 1] - timing[i]
    # di/dt = (R/L) * (u - i)
    di_dt <- (resistance / inductance) * (source_current[i] - ind_current[i])
    ind_current[i + 1] <- ind_current[i] + dt * di_dt
  }
  
  # Node (and resistor) voltage: v = R * (u - i_L)
  node_voltage <- resistance * (source_current - ind_current)
  resistor_voltage <- node_voltage  # in a parallel circuit
  
  return(list(inductor_current = ind_current,
              node_voltage      = node_voltage,
              resistor_voltage  = resistor_voltage,
              source_current    = source_current))
}

simulate_sRLC_voltage_source <- function(timing, source_voltage, resistance, inductance, capacitance) {
  n <- length(timing)
  # x1: inductor current, x2:capacitor voltage
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
    dx2_dt <-   (1 / capacitance) * x1[i]
    dx1_dt <- - (1 / inductance ) * x2[i] - (resistance / inductance) * x1[i] + (1 / inductance) * source_voltage[i]
    sum_dx1[i + 1] <- sum_dx1[i] + dx1_dt
    sum_dx2[i + 1] <- sum_dx2[i] + dx2_dt
    x1[i + 1] <- x1[i] + dt * dx1_dt
    x2[i + 1] <- x2[i] + dt * dx2_dt
  }
  
  ind_current <- x1
  cap_voltage <- x2
  
  return(list(capacitor_voltage = cap_voltage,
              inductor_current  = ind_current,
              source_voltage    = source_voltage,
              sum_dx1 = sum_dx1,
              sum_dx2 = sum_dx2))
}

# -----------------------------------------------------------------------------
# simulate_Vcc: gera PULSE(V1→V2) com delay, subida/queda, largura e período
# -----------------------------------------------------------------------------
simulate_Vcc <- function(t,
                         V1  = 0,    # tensão inicial
                         V2  = 10,   # tensão de pulso
                         TD  = 10,   # atraso antes do primeiro pulso
                         TR  = 0.001,# tempo de subida
                         TF  = 0.001,# tempo de descida
                         PW  = 20,   # largura do pulso em nível V2
                         PER = 50    # período total
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
