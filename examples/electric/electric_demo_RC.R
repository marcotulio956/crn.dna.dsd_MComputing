rm(list = ls())

source('R/parser.R')
source('R/util_functions.R')
source('R/crn_reactor.R')
source('R/4domain_reactor.R')
source('R/io.R')

source('R/GATE_LIB.R')
source('R/ANALOG_GATE_LIB.R')
source('R/ELECTRO_LIB.R')
source('R/ELECTRO_SIM.R')

source('R/forced_concentrations.R')



timing <- seq(0,45, by = 0.001)

rate = 1

capacitance = 10

resistance <- 1    # New internal resistance parameter


cap <- Make_Capacitor_Component(0, capacitance, resistance)
cap1 <- Make_Capacitor_Component(1, capacitance, resistance)
cap2 <- Make_Capacitor_Component(2, capacitance, resistance)
cap3 <- Make_Capacitor_Component(3, capacitance, resistance)

cap$il$voltage_positive <- 'v_in'
cap1$il$voltage_positive <- 'v_in'
cap2$il$voltage_positive <- 'v_in'
cap3$il$voltage_positive <- 'v_in'


# c0 <- Make_Capacitor_mermaid(cap$name, cap$il, cap$ol, cap$ic, rate)
# c1 <- Make_Circuit_Capacitor(cap1$name, cap1$il, cap1$ol, cap1$ic, rate)
c2 <- Make_Circuit_RC(cap2$name, cap2$il, cap2$ol, cap2$ic, rate)
c3 <- Make_Circuit_Pure_Capacitor(cap3$name, cap3$il, cap3$ol, cap3$ic, rate)


circuit <- make_circuit(timing)

# circuit <- circuit_add_compile_gates(circuit, c0)
# circuit <- circuit_add_compile_gates(circuit, c1)
# circuit <- circuit_add_compile_gates(circuit, c2)
circuit <- circuit_add_compile_gates(circuit, c3)

forced_concentrations = list(
  v_in = function(t) square_input(t, pulse_width = 30, period = 60, amplitude = 10)
)

behavior <- React_circuit(circuit, forced_concentrations = forced_concentrations, engine = 'desolve')

# euler method for simulations of ODE
simRC <- simulate_sRC_voltage_source(
  timing, behavior[['v_in']], resistance, capacitance
)
behavior['v_in'] <- behavior[['v_in']]
behavior['V(C)'] <- simRC$capacitor_voltage
behavior['I(R,C)'] <- simRC$current_output

# simC <- simulate_C_voltage_source(
#   timing, behavior[['v_in']], capacitance
# )
# behavior['I(Cp)'] <- simC$current_output



# behavior['c0ol_v'] <- ( behavior['c0ol_vp'] - behavior['c0ol_vn'] )
# behavior['c0ol_i'] <- ( behavior['c0ol_ip'] - behavior['c0ol_in'] )


# behavior['c1ol_v'] <- (behavior['c1ol_vp'] - behavior['c1ol_vn'] )
# behavior['c1ol_i'] <- (behavior['c1ol_ip'] - behavior['c1ol_in'] )


# behavior['c2ol_v'] <- (behavior['c2ol_vp'] - behavior['c2ol_vn'] )
# behavior['c2ol_i'] <- (behavior['c2ol_ip'] - behavior['c2ol_in'] )

behavior['c_v'] <- (behavior['c3ol_vp'] - behavior['c3ol_vn'] )
behavior['c_i'] <- (behavior['c3ol_ip'] - behavior['c3ol_in'] )

title <- jn("rate:", rate, " RC:", resistance, ",",capacitance)

# Plot_behavior(title = title, behavior, circuit, species=c('c0ol_v', 'c0ol_i'), species_dotted=c('V(C)','I(R,C)'))
# Plot_behavior(title = title, behavior, circuit, species=c('c1ol_v', 'c1ol_i'), species_dotted=c('V(C)','I(R,C)'))
# Plot_behavior(title = title, behavior, circuit, species=c('c2ol_v', 'c2ol_i'), species_dotted=c('V(C)','I(R,C)'))
# Plot_behavior(title = title, behavior, circuit, species=c('c3ol_v', 'c3ol_i'), species_dotted=c('V(C)','I(Cp)'))

circuit_dsd <- Translate_4domain(circuit)

print(circuit_dsd)

behavior_dsd <- React_4domain(circuit, forced_concentrations = forced_concentrations, engine = 'desolve')

behavior_dsd['V(C)'] <- simRC$capacitor_voltage
behavior_dsd['I(R,C)'] <- simRC$current_output
behavior_dsd['c_v'] <- behavior_dsd$behavior['c3ol_vp'] - behavior_dsd$behavior['c3ol_vn']
behavior_dsd['c_i'] <- behavior_dsd$behavior['c3ol_ip'] - behavior_dsd$behavior['c3ol_in'] 


title <- jn("dsd rate:", rate, " RC:", resistance, ",",capacitance)
Plot_behavior(behavior_dsd, circuit, title = title, species=c('c_v', 'c_i'), species_dotted=c('V(C)','I(Cp)'))

  