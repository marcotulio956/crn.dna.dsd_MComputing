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

R = 0.1
L = 10


Make_Generic <- function(timing) {
  circuit <- DNArLogic::make_circuit(timing)
  
  g_dalchau <- Make_Oscillator_Dalchau('osc', 'y', 'z', 'l1il_v1p', 9, 8, 5, 10e-2)
  
  # c1 <- Make_Capacitor_Component(1, L * 1e-2)
  
  
  # c1$il$voltage_positive <- 'c1il_v1p'
  
  # # - Electro
  # e1_gates <- Make_Circuit_Capacitor(c1$name, c1$il, c1$ol, c1$ic, 10000)
  
  l1 <- Make_Highpass_Cardelli(
    'cardelliRC', 'l1il_v1p', 'l1il_vn',
    'l1ol_vp', 'l1ol_vn',
    'l1ol_ip', 'l1ol_in',
    0, 0,
    R, L, # r l
    100
  )
  
  # add2circuit
  circuit <- circuit_add_gate(circuit, g_dalchau)
  circuit <- circuit_add_gate(circuit, l1)
  # circuit <- circuit_add_electro_gates(circuit, e1_gates)
  
  return (circuit)
}

t0 = 0
t1 = 20
points = (t1 - t0) * 50 # Using 50 time points
timing  <- seq(t0, t1, length.out = points) # Using 50 time points
circuit <- Make_Generic(timing)
result_crn <- React_circuit(circuit)

expected_value = 0
minimum = expected_value * 0.95
maximum = expected_value * 1.05
gate_number = 1

simRC <- simulate_sRC_voltage_source(
  timing, result_crn[['l1il_v1p']], R, L
)

simRL <- simulate_sRL_voltage_source(
  timing, result_crn[['l1il_v1p']], R, L
)

result_crn['l1ol_iout'] <- 1 * ( result_crn['l1ol_ip'] -  result_crn['l1ol_in'] )
result_crn['l1ol_vout'] <- 1 * ( result_crn['l1ol_vp'] -  result_crn['l1ol_vn'] )
result_crn['l1il_v1p'] <- 1 * result_crn['l1il_v1p'] 
result_crn['I(L)'] <- 1 * (simRC$capacitor_voltage)
result_crn['V(R)'] <- 0.1 * (simRC$current_output)
result_crn['I(L)2'] <- 10 * (simRL$inductor_current)
result_crn['V(L)2'] <- 1 * (simRL$inductor_voltage)


#result_crn['l1il_v1p'] <- result_crn['c1il_v1p']
#result_crn['l1ol_iout'] <- 1e-2 * result_crn['c1ol_vout']
#result_crn['l1ol_vout'] <- 1e-1 * result_crn['c1ol_iout']



Plot_behavior(
  result_crn, circuit, gate_number, minimum, maximum,
  #plot_species=c('l1il_i1p', 'l1ol_ip', 'l1ol_vp', 'l1ol_in', 'l1ol_vn', 'l1ol_vout'),
  plot_species=c('l1il_v1p', 'l1ol_vout', 'l1ol_iout'), #  'y', 'z'),
  plot_species_dotted=c('I(L)', 'V(R)'), # 'I(L)2', 'V(L)2'),
  chart_title = 'Oscilator Inductor Current Lag DSD i=\'l1il_v1p\' R=0.1[ohm] L=10[H]',
  timing
)

# prepare your vectors:
input <- result_crn[['c1il_v1p']]
mc  <- result_crn[['c1ol_iout']]      # e.g. model current
mv  <- result_crn[['c1ol_vout']]      # model voltage
sv  <- simRC$capacitor_voltage   # sim current
sc  <- simRC$current_output      # sim voltage

t <- as.numeric(timing)

# 1) get periods
periods_voltages <- estimate_ac_cycle(t, input, input, sv)
periods_currents <- estimate_ac_cycle(t, input, mc, sc)

# 2) phase shifts between I & V in each domain
phases  <- compute_phase_shift(t, mc, mv, sc, sv)

# 3) compare sim vs model
cmp     <- compare_model_sim(t, mc, mv, sc, sv)

print(periods_voltages)
print(periods_currents)
print(phases)
print(cmp)

#resultado_4dom <- React_4domain_circuit(circuit)

#resultado_4dom$behavior['c1ol_iout'] <- result_crn$behavior['c1ol_ip'] -  result_crn$behavior['c1ol_in']

#Plot_behavior(
#  resultado_4dom$behavior, circuit, gate_number, minimum, maximum,
#  plot_species=c('c1ol_vp', 'c1ol_ip', 'c1ol_in', 'c1ol_iout'),
#  chart_title = 'Oscilator Capacitor Current Lead DSD',
#  timing
#)

print(max(result_crn[['l1ol_vout']][timing >= 10], na.rm = TRUE))
print(max(result_crn[['V(R)']][timing >= 10], na.rm = TRUE))

print(min(result_crn[['l1ol_vout']][timing >= 10], na.rm = TRUE))
print(min(result_crn[['V(R)']][timing >= 10], na.rm = TRUE))

print(max(result_crn[['l1ol_iout']][timing >= 10], na.rm = TRUE))
print(max(result_crn[['I(L)']][timing >= 10], na.rm = TRUE))

print(min(result_crn[['l1ol_iout']][timing >= 10], na.rm = TRUE))
print(min(result_crn[['I(L)']][timing >= 10], na.rm = TRUE))




