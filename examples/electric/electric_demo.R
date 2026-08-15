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


timing <- seq(0, 60, by = 0.1)

rate = 1

capacitance = 10

id=0
cap <- Make_Capacitor_Component(id, capacitance)

c0 <- Make_Capacitor_mermaid(cap$name, cap$il, cap$ol, cap$ic, rate)

circuit <- make_circuit(timing)

circuit <- circuit_add_compile_gates(circuit, c0)

forced_concentrations = list(
  c0il_vp = function(t) square_input(t, pulse_width = 30, period = 60, amplitude = 10)
)

behavior <- React_circuit(circuit, forced_concentrations = forced_concentrations)

simRC <- simulate_sRC_voltage_source(
  timing, behavior[['c0il_vp']], 1, capacitance
)


behavior['V(C)'] <- simRC$capacitor_voltage
behavior['V(R)'] <- simRC$resistor_voltage
# behavior['I(R,C)'] <- 1e3 * simRC$current_output
scale = 1
behavior['c0ol_v'] <- scale/10 * ( behavior['c0ol_vp'] - behavior['c0ol_vn'] )
behavior['c0ol_i'] <- scale/10000 * ( behavior['c0ol_ip'] - behavior['c0ol_in'] )

Plot_behavior(behavior, circuit, species=c('c0il_vp', 'c0ol_v', 'c0ol_i'))


