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



timing <- seq(0,45, by = 0.01)

rate = 1

capacitance = 1

resistance <- 1    # New internal resistance parameter


cap <- Make_Capacitor_Component(0, capacitance, resistance)
cap1 <- Make_Capacitor_Component(1, capacitance, resistance)
cap2 <- Make_Capacitor_Component(2, capacitance, resistance)

c0 <- Make_Capacitor_mermaid(cap$name, cap$il, cap$ol, cap$ic, rate)
c1 <- Make_Circuit_Capacitor(cap1$name, cap1$il, cap1$ol, cap1$ic, rate)
c2 <- Make_Circuit_RC(cap2$name, cap2$il, cap2$ol, cap2$ic, rate)

circuit <- make_circuit(timing)

circuit <- circuit_add_compile_gates(circuit, c0)
circuit <- circuit_add_compile_gates(circuit, c1)
circuit <- circuit_add_compile_gates(circuit, c2)

forced_concentrations = list(
  c0il_vp = function(t) square_input(t, pulse_width = 30, period = 60, amplitude = 10)
  , c1il_vp = function(t) square_input(t, pulse_width = 30, period = 60, amplitude = 10)
  , c2il_vp = function(t) square_input(t, pulse_width = 30, period = 60, amplitude = 10)
)

behavior <- React_circuit(circuit, forced_concentrations = forced_concentrations, engine = 'desolve')

# euler method for simulations of ODE
simRC <- simulate_sRC_voltage_source(
  timing, behavior[['c0il_vp']], resistance, capacitance
)

behavior['v_in'] <- behavior[['c0il_vp']]
behavior['V(C)'] <- simRC$capacitor_voltage
behavior['I(R,C)'] <- simRC$current_output

behavior['c0ol_v'] <- ( behavior['c0ol_vp'] - behavior['c0ol_vn'] )
behavior['c0ol_i'] <- ( behavior['c0ol_ip'] - behavior['c0ol_in'] )


behavior['c1ol_v'] <- (behavior['c1ol_vp'] - behavior['c1ol_vn'] )
behavior['c1ol_i'] <- ( behavior['c1ol_ip'] - behavior['c1ol_in'] )


behavior['c2ol_v'] <- (behavior['c2ol_vp'] - behavior['c2ol_vn'] )
behavior['c2ol_i'] <- ( behavior['c2ol_ip'] - behavior['c2ol_in'] )

title <- jn("rate:", rate, " RC:", resistance, ",",capacitance)
Plot_behavior(title = title, behavior, circuit, species=c('c0ol_v', 'c0ol_i'), species_dotted=c('V(C)','I(R,C)'))
Plot_behavior(title = title, behavior, circuit, species=c('c1ol_v', 'c1ol_i'), species_dotted=c('V(C)','I(R,C)'))
Plot_behavior(title = title, behavior, circuit, species=c('c2ol_v', 'c2ol_i'), species_dotted=c('V(C)','I(R,C)'))

# behavior <- React_4domain(circuit, forced_concentrations = forced_concentrations, engine = 'desolve')

  