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

inductance = 15

resistance <- 10    # New internal resistance parameter


ind <- Make_Inductor_Component(0, inductance, resistance)
ind1<- Make_Inductor_Component(1, inductance, resistance)

l0 <- Make_Circuit_RL(ind$name, ind$il, ind$ol, ind$ic, rate)

circuit <- make_circuit(timing)

circuit <- circuit_add_compile_gates(circuit, l0)


forced_concentrations = list(
  # l0il_vp = function(t) square_input(t, pulse_width = 30, period = 60, amplitude = 10)
  l0il_vp = function(t) sinusoidal_input(t)
  
)

behavior <- React_circuit(circuit, forced_concentrations = forced_concentrations, engine = 'desolve')

# euler method for simulations of ODE
simRL <- simulate_sRL_voltage_source(
  timing, behavior[['l0il_vp']], resistance, inductance
)

behavior['v_in'] <- behavior[['l0il_vp']]
behavior['V(L)'] <- simRL$inductor_voltage
behavior['I(R,L)'] <- simRL$inductor_current

behavior['l0ol_v'] <- ( behavior['l0ol_vp'] - behavior['l0ol_vn'] )
behavior['l0ol_i'] <- ( behavior['l0ol_ip'] - behavior['l0ol_in'] )

title <- jn("rate:", rate, " RL:", resistance, ",",inductance)
Plot_behavior(title = title, behavior, circuit, species=c('l0ol_v', 'l0ol_i'), species_dotted=c('V(L)','I(R,L)'), normalize = FALSE)
  