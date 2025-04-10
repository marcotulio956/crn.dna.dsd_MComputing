dnarElectric_simRC <- function(timing, source_voltage, capacitance, resistance) {
  n <- length(timing)
  
  capacitor_voltage <- numeric(n)
  resistor_voltage <- numeric(n)
  current_output <- numeric(n)
  
  capacitor_voltage[1] <- 0
  
  # dV_C/dt = (V_in - V(C) ) / (R * C)
  for (i in 1:(n - 1)) {
    dt <- timing[i + 1] - timing[i]  # Time step
    dVc_dt <- (source_voltage[i] - capacitor_voltage[i]) / (resistance * capacitance)
    capacitor_voltage[i + 1] <- capacitor_voltage[i] + dt * dVc_dt
  }
  
  # V(R) = Vin - V(C)
  resistor_voltage <- source_voltage - capacitor_voltage
  
  # I = V(R) / R
  current_output <- resistor_voltage / resistance
  
  return(list(capacitor_voltage = capacitor_voltage,
              resistor_voltage = resistor_voltage,
              current_output = current_output))
}

dnarElectric_simRL <- function(timing, source_voltage, inductance, resistance) {
  n <- length(timing)
  
  current_output <- numeric(n)
  inductor_voltage <- numeric(n)
  resistor_voltage <- numeric(n)
  
  current_output[1] <- 0
  
  # di/dt = (V_in - i * R) / L
  for (i in 1:(n - 1)) {
    dt <- timing[i + 1] - timing[i]
    di_dt <- (source_voltage[i] - current_output[i] * resistance ) / inductance
    current_output[i + 1] <- current_output[i] + dt * di_dt
  }
  
  # V(R) = i * R
  resistor_voltage <- current_output * resistance
  
  # V(L) = V_in - V(R)
  inductor_voltage <- source_voltage - resistor_voltage
  
  return(list(inductor_voltage = inductor_voltage,
              resistor_voltage = resistor_voltage,
              current = current_output))
}