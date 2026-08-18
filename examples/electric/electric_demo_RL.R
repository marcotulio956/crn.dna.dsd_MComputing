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

rate = 1 # the greater the rate lesser is its resistivity 

inductance = 10

resistance <- 1    # New internal resistance parameter


ind <- Make_Inductor_Component(0, inductance, resistance)
ind1<- Make_Inductor_Component(1, inductance, resistance)
ind2<- Make_Inductor_Component(2, inductance, resistance)

ind$il$voltage_positive <- 'v_in'
ind1$il$voltage_positive <- 'v_in'
ind2$il$voltage_positive <- 'v_in'


l0 <- Make_Circuit_RL(ind$name, ind$il, ind$ol, ind$ic, rate)
l1 <- Make_Circuit_RL2(ind1$name, ind1$il, ind1$ol, ind1$ic, rate)
l2 <- Make_Circuit_Pure_Inductor(ind2$name, ind2$il, ind2$ol, ind2$ic, rate)


circuit <- make_circuit(timing)

circuit <- circuit_add_compile_gates(circuit, l0)
circuit <- circuit_add_compile_gates(circuit, l1)
circuit <- circuit_add_compile_gates(circuit, l2)

forced_concentrations = list(
  v_in = function(t) square_input(t, pulse_width = 30, period = 60, amplitude = 10)
)

behavior <- React_circuit(circuit, forced_concentrations = forced_concentrations, engine = 'desolve')

# euler method for simulations of ODE
simRL <- simulate_sRL_voltage_source(
  timing, behavior[['v_in']], resistance, inductance
)

simL <- simulate_L_voltage_source(
  timing, behavior[['v_in']], inductance
)

behavior['v_in'] <- behavior[['v_in']]

behavior['V(L)'] <- simRL$inductor_voltage
behavior['I(R,L)'] <- simRL$current_outupt

behavior['I(Lp)'] <- simL$current_outupt

behavior['l0ol_v'] <- ( behavior['l0ol_vp'] - behavior['l0ol_vn'] )
behavior['l0ol_i'] <- ( behavior['l0ol_ip'] - behavior['l0ol_in'] )


behavior['l1ol_v'] <- ( behavior['l1ol_vp'] - behavior['l1ol_vn'] )
behavior['l1ol_i'] <- ( behavior['l1ol_ip'] - behavior['l1ol_in'] )


behavior['l2ol_v'] <- ( behavior['l2ol_vp'] - behavior['l2ol_vn'] )
behavior['l2ol_i'] <- ( behavior['l2ol_ip'] - behavior['l2ol_in'] )

title <- jn("rate:", rate, " RL:", resistance, ",",inductance)
Plot_behavior(title = title, behavior, circuit, species=c('l0ol_v', 'l0ol_i'), species_dotted=c('V(L)','I(R,L)'), normalize = FALSE)

Plot_behavior(title = title, behavior, circuit, species=c('l1ol_v', 'l1ol_i'), species_dotted=c('V(L)','I(R,L)'), normalize = FALSE)

Plot_behavior(title = title, behavior, circuit, species=c('l2ol_v', 'l2ol_i'), species_dotted=c('I(Lp)'), normalize = FALSE)
