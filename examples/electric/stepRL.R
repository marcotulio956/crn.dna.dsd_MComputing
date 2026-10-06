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

jn <- function(...) { paste(..., sep = '') }

library(ggplot2) # plot()
library(dplyr) # mutate()

R = 1 # * 1e2
L = 1 # * 1e-3

Make_Generic <- function(timing) {
  circuit <- DNArLogic::make_circuit(timing)
  
  # - Dig and Analog
  g_dalchau <- Make_Oscillator_Dalchau('sin', 'x', 'l1il_vp', 'z', 1e-3, 1e-3, 15, 4e-1)
  c_comparator <- Make_Mux2_balanced(
    'mux1',
    'x', 'l1il_vp', 
    'low', 'high',
    'comp_out',
    0, 0,
    3, 8,
    0, 7.0e-1
  )
  
  # add2circuit
  circuit <- circuit_add_gate(circuit, g_dalchau)
  circuit <- circuit_add_gate(circuit, c_comparator)
  
  id <- 1
  modelL <- 1
  l1 <- Make_Inductor_Component(id, modelL)
  
  l1$il$voltage_positive <- 'l1il_vp'
  l1$ic$resistance <- R
  
  print(l1)
  
  # - Electro
  rate <- 1000
  l1_gate <- Make_Circuit_Inductor_old(l1$name, l1$il, l1$ol, l1$ic, rate)
  
  
  print(l1_gate)
  
  # add2circuit
  circuit <- circuit_add_compile_gates(circuit, l1_gate)
  
  return (circuit)
}

t0 = 0
t1 = 40
points = (t1 - t0) * 50 # Using 50 time points
timing  <- seq(t0, t1, length.out = points) # Using 50 time points
circuit <- Make_Generic(timing)
result_crn <- React_circuit(circuit)

expected_value = 10
minimum = expected_value * 0.95
maximum = expected_value * 1.05
gate_number = 1

# result_crn['l1il_i1p'] <- result_crn['l1il_i1p']



#ol_il_scale <- 1e-2
#result_crn['l1ol_pil'] <- result_crn['l1ol_pil'] * ol_il_scale
#result_crn['l1ol_nil'] <- result_crn['l1ol_nil'] * ol_il_scale
#result_crn['l1ol_il'] <- result_crn['l1ol_pil'] - result_crn['l1ol_nil']

# ol_ir_scale <- 1
# result_crn['l1ol_pir'] <- result_crn['l1ol_pir'] * ol_ir_scale
# result_crn['l1ol_nir'] <- result_crn['l1ol_nir'] * ol_ir_scale
# result_crn['l1ol_ir'] <- result_crn['l1ol_pir'] - result_crn['l1ol_nir']


#ol_v_scale <- 1
#result_crn['l1ol_vp'] <- result_crn['l1ol_vp'] * ol_v_scale
#result_crn['l1ol_vn'] <- result_crn['l1ol_vn'] * ol_v_scale
#result_crn['l1ol_v'] <- result_crn['l1ol_vp'] - result_crn['l1ol_vn']

# inductor OLD outputs
ol_v_scale <- 1e-8
result_crn['l1ol_vp'] <- result_crn['l1ol_vp'] * ol_v_scale
result_crn['l1ol_vn'] <- result_crn['l1ol_vn'] * ol_v_scale
result_crn['l1ol_v'] <- result_crn['l1ol_vp'] - result_crn['l1ol_vn']

ol_i_scale <- 1e-4
result_crn['l1ol_ip'] <- result_crn['l1ol_ip'] * ol_i_scale
result_crn['l1ol_in'] <- result_crn['l1ol_in'] * ol_i_scale
result_crn['l1ol_i'] <- result_crn['l1ol_ip'] - result_crn['l1ol_in']

# ---- Compare four RL simulations ----
# 1) Series RL, voltage source
s_vs <- simulate_sRL_voltage_source(timing, result_crn[['l1il_vp']], R, L)
result_crn['V(S)_vs'] <- s_vs$source_voltage
result_crn['I(L)_vs'] <- s_vs$inductor_current
result_crn['V(R)_vs'] <- s_vs$resistor_voltage
result_crn['V(L)_vs'] <- s_vs$inductor_voltage

# 2) Parallel RL, current source
p_cs <- simulate_pRL_current_source(timing, result_crn[['l1il_vp']], R, L)
result_crn['I(S)_cs'] <- p_cs$source_current
result_crn['I(L)_cs'] <- p_cs$inductor_current
result_crn['V(L)_cs'] <- p_cs$node_voltage
result_crn['V(R)_cs'] <- p_cs$resistor_voltage

# 3) Parallel RL, voltage source
p_vs <- simulate_pRL_voltage_source(timing, result_crn[['l1il_vp']], R, L)
result_crn['I(S)_pv'] <- p_vs$source_current
result_crn['I(R)_pv'] <- p_vs$resistor_current
result_crn['I(L)_pv'] <- 1e-2 * p_vs$inductor_current
result_crn['V(R)_pv'] <- p_vs$resistor_voltage
result_crn['V(L)_pv'] <- p_vs$inductor_voltage

# 3.5) Series R || ( R L ), voltage source
p2_vs <- simulate_pR_pRL_voltage_source(timing, result_crn[['l1il_vp']], R, R, L)
result_crn['I(L)_p2v'] <- 1e-2 * p2_vs$inductor_current
result_crn['V(R2)_p2v'] <- p2_vs$resistor2_voltage

# 4) Series RL, current source
s_cs <- simulate_sRL_current_source(timing, result_crn[['l1il_vp']], R, L)
result_crn['I(S)_sc'] <- s_cs$source_current
result_crn['V(R)_sc'] <- s_cs$resistor_voltage
result_crn['V(L)_sc'] <- s_cs$inductor_voltage
result_crn['V(S)_sc'] <- s_cs$source_voltage

# 5) Parallel L || ( R L ), voltage source
p3_vs <- simulate_pL_RL_voltage_source(timing, result_crn[['l1il_vp']], R*0.5, L, L)
result_crn['V(R)_p3v'] <- p3_vs$resistor_voltage
result_crn['I(L1)_p3v'] <- p3_vs$i_branch1
result_crn['I(L2)_p3v'] <- 1e-1 * p3_vs$i_branch2
result_crn['I(S)_p3v'] <- p3_vs$i_source
result_crn['V(L1)_p3v'] <- p3_vs$vL1
result_crn['V(L2)_p3v'] <- p3_vs$vL2


# shows final
result_crn['V(R)'] <- result_crn['V(R)_p3v']
result_crn['I(L2)'] <- result_crn['I(L2)_p3v']

Plot_behavior(
  result_crn, circuit, gate_number, minimum, maximum,
  #species=c('l1il_i1p'), # show model result
  # species=c('V(S)_vs', 'I(L)_vs', 'V(R)_vs', 'V(L)_vs'), # show results 1
  # species=c('I(S)_cs', 'I(L)_cs', 'V(L)_cs', 'V(R)_cs'), # show results 2
  species=c('l1il_vp', 'l1ol_v', 'l1ol_i'), # 'l1il_vp', 'l1ol_v', 'l1ol_i'
  species_dotted = c(#'I(L)_vs','V(L)_vs',
    #'I(L)_cs','V(L)_cs'#,
    #'I(L)_pv', #  works
    #'I(S)_sc', 'V(L)_sc' # works
    #'V(R2)_p2v'#  works
    # show  final
    # 'V(R)_p3v',  'I(L2)_p3v' # final works
    # show re name
    'V(R)',  'I(L2)'
    
  ), #  #  species_dotted=c('I(L)_vs', 'V(R)_vs', 'V(L)_vs'), //  species_dotted=c('I(L)_cs', 'V(R)_cs', 'V(L)_cs'),
  chart_title = sprintf('Inductor Step Response DSD Vin=10[V] R=%s[ohm] L1,2=%s[H]', R, L),
  timing
)
# Plot_behavior(result_crn, circuit, gate_number, minimum, maximum, specify_species = TRUE, species=c('c1ol_current', 'c1sub1_C', 'c1_l_dv_out'))

#resultado_4dom <- React_4domain_circuit(circuit)

#Plot_behavior(
#  resultado_4dom$behavior, circuit, gate_number, minimum, maximum,
#  species=c('l1il_i1p', 'l1ol_in', 'l1ol_vp', 'l1ol_vn'),
#  chart_title = 'Step Response DSD Iin=6.32[mA] L=4.75[mH]',
#  timing
#)

#
#Plot_behavior(resultado_4dom$behavior, circuit, gate_number, minimum, maximum, TRUE)
#
#resultado_comb <- Compare_behaviors(resultado_crn, resultado_4dom, circuit, gate_number)
#
# p1 <- Plot_behavior_comb(resultado_comb, circuit, gate_number, minimum, maximum, TRUE)

