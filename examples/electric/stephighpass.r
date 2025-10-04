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

R <- 0.5 # * 1e2
L <- 3 # * 1e-3

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
  
  rate <- 100
  l1 <- Make_Highpass_Cardelli('cardelliLC', 'l1il_vp', 'l1il_vn',
							   'l1ol_vp', 'l1ol_vn',
							   'l1ol_ip', 'l1ol_in',
							   0, 0,
							   1, 1, # r l
							   rate)

  circuit <- circuit_add_gate(circuit, l1)

  return (circuit)
}

t0 <- 0
t1 <- 45
points = (t1 - t0) * 100 # Using 50 time points
timing  <- seq(t0, t1, length.out = points) # Using 50 time points
circuit <- Make_Generic(timing)
result_crn <- React_circuit(circuit)

expected_value = 10
minimum = expected_value * 0.95
maximum = expected_value * 1.05
gate_number = 1

ol_v_scale <- 1
result_crn['l1ol_vp'] <- result_crn['l1ol_vp'] * ol_v_scale
result_crn['l1ol_vn'] <- result_crn['l1ol_vn'] * ol_v_scale
result_crn['l1ol_v'] <- result_crn['l1ol_vp'] - result_crn['l1ol_vn']

ol_i_scale <- 1
result_crn['l1ol_ip'] <- result_crn['l1ol_ip'] * ol_i_scale
result_crn['l1ol_in'] <- result_crn['l1ol_in'] * ol_i_scale
result_crn['l1ol_i'] <- result_crn['l1ol_ip'] - result_crn['l1ol_in']

s_vs <- simulate_sRL_voltage_source(timing, result_crn[['l1il_vp']], R, L)
result_crn['V(S)_vs'] <- s_vs$source_voltage
result_crn['I(L)_vs'] <- s_vs$inductor_current
result_crn['V(R)_vs'] <- s_vs$resistor_voltage
result_crn['V(L)_vs'] <- s_vs$inductor_voltage

Plot_behavior(
  result_crn, circuit, gate_number, minimum, maximum,
  #plot_species=c('l1il_i1p'), # show model result
  # plot_species=c('V(S)_vs', 'I(L)_vs', 'V(R)_vs', 'V(L)_vs'), # show results 1
  # plot_species=c('I(S)_cs', 'I(L)_cs', 'V(L)_cs', 'V(R)_cs'), # show results 2
  plot_species=c('l1il_vp', 'l1ol_v', 'l1ol_i'),
  plot_species_dotted = c('V(L)_vs', 'I(L)_vs'), 
  chart_title = sprintf('Cardelli High Pass LC Step Response CRN Vin=10[V] R=%s[ohm] L=%s[H]', R, L),
  timing
)

resultado_4dom <- React_4domain_circuit(circuit)

resultado_4dom['l1ol_v'] <- ol_v_scale * ( resultado_4dom['l1ol_vp'] - resultado_4dom['l1ol_vn'] )
resultado_4dom['l1ol_i'] <- ol_i_scale * ( resultado_4dom['l1ol_ip'] - resultado_4dom['l1ol_in'] )

Plot_behavior(
  resultado_4dom$behavior, circuit, gate_number, minimum, maximum,
  plot_species=c('l1il_vp', 'l1ol_v', 'l1ol_i'),
  plot_species_dotted = c('V(L)_vs', 'I(L)_vs'), 
  chart_title = sprintf('Cardelli High Pass LC Step Response DSD Vin=10[V] R=%s[ohm] L=%s[H]', R, L),
  timing
)

#
#Plot_behavior(resultado_4dom$behavior, circuit, gate_number, minimum, maximum, TRUE)
#
#resultado_comb <- Compare_behaviors(resultado_crn, resultado_4dom, circuit, gate_number)
#
# p1 <- Plot_behavior_comb(resultado_comb, circuit, gate_number, minimum, maximum, TRUE)

