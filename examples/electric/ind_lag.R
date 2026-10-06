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
source('R/forced_concentrations.R')

jn <- function(...) { paste(..., sep = '') }

library(ggplot2) # plot()
library(dplyr) # mutate()

R = 1
L = 100


Make_Generic <- function(timing) {
  circuit <- make_circuit(timing)
  
  l1 <- Make_Inductor_Component(1, R, L)

  l1$il$voltage_positive <- 'v_in'
  
  # # - Electro
  rate <- 10
  l1 <- Make_Circuit_Pure_Inductor(l1$name, l1$il, l1$ol, l1$ic, rate)

  circuit <- circuit_add_compile_gates(circuit, l1)
  
  return (circuit)
}

t0 = 0
t1 = 60
points = (t1 - t0) * 60 # Using 50 time points
timing  <- seq(t0, t1, length.out = points) # Using 50 time points
circuit <- Make_Generic(timing)



result_crn <- React_circuit(circuit, forced_concentrations = list(
  v_in = function(timing) sinusoidal_input(timing,)
), engine = 'desolve')




simRL <- simulate_sRL_voltage_source(
  result_crn[['time']], result_crn[['v_in']], R, L
)

result_crn[['i']] <-   result_crn[['l1ol_ip']] -  result_crn[['l1ol_in']] 
result_crn[['vl']] <-  result_crn[['l1ol_vp']] -  result_crn[['l1ol_vn']] 
result_crn[['I(R,L)']]<- simRL$current_output
result_crn[['V(L)']]<- simRL$inductor_voltage


Plot_behavior(
  result_crn, circuit,
  #species=c('l1il_i1p', 'l1ol_ip', 'l1ol_vp', 'l1ol_in', 'l1ol_vn', 'l1ol_vout'),
  species=c('v_in', 'i', 'vl'), #  'y', 'z'),
  species_dotted=c('I(R,L)', 'V(L)'), # 'I(C)', 'V(R)'
  title = 'Oscilator Inductor Current Lag DSD Vin=sin(t), R=1[dΩ], L=1[dH]',
  timing,
      normalize = TRUE

)

# # prepare your vectors:
# input <- result_crn[['c1il_v1p']]
# mc  <- result_crn[['c1ol_iout']]      # e.g. model current
# mv  <- result_crn[['c1ol_vout']]      # model voltage
# sv  <- simRC$capacitor_voltage   # sim current
# sc  <- simRC$current_output      # sim voltage

# t <- as.numeric(timing)

# # 1) get periods
# periods_voltages <- estimate_ac_cycle(t, input, input, sv)
# periods_currents <- estimate_ac_cycle(t, input, mc, sc)

# # 2) phase shifts between I & V in each domain
# phases  <- compute_phase_shift(t, mc, mv, sc, sv)

# # 3) compare sim vs model
# cmp     <- compare_model_sim(t, mc, mv, sc, sv)

# print(periods_voltages)
# print(periods_currents)
# print(phases)
# print(cmp)

# #resultado_4dom <- React_4domain_circuit(circuit)

# #resultado_4dom$behavior['c1ol_iout'] <- result_crn$behavior['c1ol_ip'] -  result_crn$behavior['c1ol_in']

# #Plot_behavior(
# #  resultado_4dom$behavior, circuit, gate_number, minimum, maximum,
# #  species=c('c1ol_vp', 'c1ol_ip', 'c1ol_in', 'c1ol_iout'),
# #  chart_title = 'Oscilator Capacitor Current Lead DSD',
# #  timing
# #)

# print(max(result_crn[['l1ol_vout']][timing >= 10], na.rm = TRUE))
# print(max(result_crn[['V(R)']][timing >= 10], na.rm = TRUE))

# print(min(result_crn[['l1ol_vout']][timing >= 10], na.rm = TRUE))
# print(min(result_crn[['V(R)']][timing >= 10], na.rm = TRUE))

# print(max(result_crn[['l1ol_iout']][timing >= 10], na.rm = TRUE))
# print(max(result_crn[['I(L)']][timing >= 10], na.rm = TRUE))

# print(min(result_crn[['l1ol_iout']][timing >= 10], na.rm = TRUE))
# print(min(result_crn[['I(L)']][timing >= 10], na.rm = TRUE))




