# Load the libraries
# DO NOT USE library(DNAr)
# DO NOT USE library(DNArLogic)
# DO NOT USE library(DNArAnalog)

rm(list = ls())

source('R/4domain_reactor.R')
source('R/ANALOG_GATE_LIB.R')
source('R/analysis.R')
source('R/crn_reactor.R')
source('R/dsd.R')
source('R/ELECTRO_LIB.R')
source('R/ELECTRO_SIM.R')
source('R/GATE_LIB.R')
source('R/io.R')
source('R/neuron_hjelmfelt.R')
source('R/parser.R')
source('R/util_functions.R')
source('R/metric_functions.R')

jn <- function(...) { paste(..., sep = '') }

library(ggplot2) # plot()
library(dplyr) # mutate()

R=5* 1e2 
L=5* 1e-3

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

Make_Generic <- function(timing) {
  circuit <- DNArLogic::make_circuit(timing)
  
  # - Dig and Analog
  g_dalchau <- Make_Oscillator_Dalchau('sin', 'x', 'c1il_v1p', 'z', 0.001, 0.001, 15, 4e-1)
  c_comparator <- Make_Mux2_balanced(
    'mux1',
    'x', 'c1il_v1p', 
    'low', 'high',
    'comp_out',
    0, 0,
    3, 8,
    0, 7.0e-1
  )
  
  # add2circuit
  circuit <- circuit_add_gate(circuit, g_dalchau)
  circuit <- circuit_add_gate(circuit, c_comparator)
  
  c1 <- Make_Capacitor_Component(1, L * 1e3) # 3.32 = v{v} c{~} ; 6.32 = v{~} c{^} ; 9.32 = v{^} c{^}
  
  c1$il$voltage_positive <- 'c1il_v1p'
  
  
  # - Electro
  rate <- 1e3
  
  e1_gates <- Make_Circuit_Capacitor(c1$name, c1$il, c1$ol, c1$ic, rate)
  
  print(e1_gates)
  
  # add2circuit
  circuit <- circuit_add_electro_gates(circuit, e1_gates)
  
  return (circuit)
}

t0 = 0
t1 = 40
points = (t1 - t0) * 100 # Using 50 time points
timing  <- seq(t0, t1, length.out = points) # Using 50 time points
circuit <- Make_Generic(timing)
result_crn <- React_circuit(circuit)

expected_value = 10
minimum = expected_value * 0.97
maximum = expected_value * 1.03
gate_number = 1

result_crn['c1il_v1p'] <- result_crn['c1il_v1p']

v_scale <- 1e-7
result_crn['c1ol_vp'] <- result_crn['c1ol_vp'] * v_scale
result_crn['c1ol_vn'] <- result_crn['c1ol_vn'] * v_scale
result_crn['c1ol_v'] <- result_crn['c1ol_vp'] - result_crn['c1ol_vn']
result_crn['c1ol_vp'] <- NULL
result_crn['c1ol_vn'] <- NULL

i_scale <- 1e-4
result_crn['c1ol_ip'] <- result_crn['c1ol_ip'] * i_scale
result_crn['c1ol_in'] <- result_crn['c1ol_in'] * i_scale
result_crn['c1ol_i'] <- result_crn['c1ol_ip'] - result_crn['c1ol_in']
result_crn['c1ol_ip'] <- NULL
result_crn['c1ol_in'] <- NULL

simRC <- simulate_sRC_voltage_source(
  timing, result_crn[['c1il_v1p']], R, L
)

simRL <- simulate_pRL_current_source(timing, result_crn[['c1il_v1p']], L, R)
simRL2 <- simulate_sRL_voltage_source(timing, result_crn[['c1il_v1p']], L, R)

result_crn['V(L)'] <- 1e3 * simRC$current_output
result_crn['V(R)'] <- simRC$resistor_voltage
result_crn['I(R,L)'] <- simRC$capacitor_voltage

result_crn['l1il_i1p'] <- result_crn['c1il_v1p']
result_crn['l1ol_i'] <- result_crn['c1ol_v']
result_crn['l1ol_v'] <- result_crn['c1ol_i']

Plot_behavior(
  result_crn, circuit, gate_number, minimum, maximum,
  #plot_species=c('x', 'c1l_dvp', 'c1l_dvn', 'c1ol_ip', 'c1ol_in',  'c1ol_vp', 'c1ol_vn'),
  #plot_species=c('x', 'c1ol_ip', 'c1ol_vp'),
  #plot_species=c('x', 'y', 'z'),
  #plot_species=c('c1il_v1p', 'c1ol_i', 'c1ol_v', 'V(C)', 'V(R)', 'I(C,R)'),
  plot_species=c('l1il_i1p', 'l1ol_i', 'l1ol_v'),
  plot_species_dotted=c('V(L)', 'I(R,L)'),
  chart_title = 'Inductor Step Response DSD i=10[A] R=500[ohm] L=50e-4[H]',
  timing
)

ind_model <- estimate_tau_inductor_current(timing, result_crn[['l1il_i1p']], result_crn[['l1ol_i']])
cat("model: \n")
print(ind_model)

cap_sim <- estimate_tau_inductor_current(timing, result_crn[['l1il_i1p']],  simRC$current_output)
cat("sim: \n")
print(cap_sim)



#resultado_4dom <- React_4domain_circuit(circuit)

#resultado_4dom$behavior['c1ol_vp'] <- resultado_4dom$behavior['c1ol_vp'] * v_scale
#resultado_4dom$behavior['c1ol_vn'] <- resultado_4dom$behavior['c1ol_vn'] * v_scale
#resultado_4dom$behavior['c1ol_v'] <- resultado_4dom$behavior['c1ol_vp'] - resultado_4dom$behavior['c1ol_vn']
#resultado_4dom$behavior['c1ol_vp'] <- NULL
#resultado_4dom$behavior['c1ol_vn'] <- NULL

#resultado_4dom$behavior['c1ol_ip'] <- resultado_4dom$behavior['c1ol_ip'] * i_scale
#resultado_4dom$behavior['c1ol_in'] <- resultado_4dom$behavior['c1ol_in'] * i_scale
#resultado_4dom$behavior['c1ol_i'] <- resultado_4dom$behavior['c1ol_ip'] - resultado_4dom$behavior['c1ol_in']
#resultado_4dom$behavior['c1ol_ip'] <- NULL
#resultado_4dom$behavior['c1ol_in'] <- NULL

#resultado_4dom$behavior['V(C)'] <- simRC$capacitor_voltage
#resultado_4dom$behavior['V(R)'] <- simRC$resistor_voltage
#resultado_4dom$behavior['I(R,C)'] <- 1e3 * simRC$current_output

# fake sim here
#resultado_4dom$behavior['V(L)'] <- 1e3 * simRC$current_output
#resultado_4dom$behavior['I(R,L)'] <- simRC$capacitor_voltage
# fake model here
#resultado_4dom$behavior['l1il_i'] <- resultado_4dom$behavior['c1il_v1p']
#resultado_4dom$behavior['l1ol_i'] <- resultado_4dom$behavior['c1ol_v']
#resultado_4dom$behavior['l1ol_v'] <- resultado_4dom$behavior['c1ol_i']



#Plot_behavior(
#  resultado_4dom$behavior, circuit, gate_number, minimum, maximum,
#  plot_species=c('l1il_i', 'l1ol_i', 'l1ol_v'),
#  plot_species_dotted=c('V(L)', 'I(R,L)'),
#  chart_title = 'Inductor Response DSD i=10[mA] R=500[ohm] L=5e-3[F]',
#  timing
#)

