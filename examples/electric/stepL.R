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
    g_dalchau <- Make_Oscillator_Dalchau('sin', 'x', 'l1il_ip', 'z', 1e-3, 1e-3, 15, 4e-1)
    c_comparator <- Make_Mux2_balanced(
      'mux1',
      'x', 'l1il_ip', 
      'low', 'high',
      'comp_out',
      0, 0,
      3, 8,
      0, 7.0e-1
    )

    # add2circuit
    circuit <- circuit_add_gate(circuit, g_dalchau)
    circuit <- circuit_add_gate(circuit, c_comparator)

  l1 <- Make_Inductor_Component(1, 0.05)

  print(l1)
    
  # - Electro
  rate <- 1
  l1_gate <- Make_Circuit_Inductor_old(l1$name, l1$il, l1$ol, l1$ic, rate)
  
  
  print(l1_gate)
  
  # add2circuit
  circuit <- circuit_add_electro_gates(circuit, l1_gate)

  return (circuit)
}

t0 = 0
t1 = 40
points = (t1 - t0) * 100 # Using 50 time points
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

ol_v_scale <- 1e-1
result_crn['l1ol_vp'] <- result_crn['l1ol_vp'] * ol_v_scale
result_crn['l1ol_vn'] <- result_crn['l1ol_vn'] * ol_v_scale
result_crn['l1ol_v'] <- result_crn['l1ol_vp'] - result_crn['l1ol_vn']

ol_i_scale <- 1e-3
result_crn['l1ol_ip'] <- result_crn['l1ol_ip'] * ol_i_scale
result_crn['l1ol_in'] <- result_crn['l1ol_in'] * ol_i_scale
result_crn['l1ol_i'] <- result_crn['l1ol_ip'] - result_crn['l1ol_in']

# simsRL_vs <- simulate_sRL_voltage_source( # l1il_ip is not a i*R wave, so this is unfair comparison
#   timing, result_crn[['l1il_ip']], 500, 500
# )

simpRL_cs <- simulate_pRL_current_source(
  timing, result_crn[['l1il_ip']], 500, 500
)

# Result 1 : Series RL circuit simulation with Voltage Source - Compare with DNAr Model
    # result_crn['V(S)_vs'] <- 1 * simsRL_vs$source_voltage
    # result_crn['I(L)_vs'] <- 1e3 * simsRL_vs$inductor_current
    # result_crn['V(R)_vs'] <- 1 * simsRL_vs$resistor_voltage 
    # result_crn['V(L)_vs'] <- 1 * simsRL_vs$inductor_voltage

# Result 2 : Parallel RL circuit simulation with Current Source - Compare with DNAr Model
    result_crn['I(S)_cs'] <- 1* simpRL_cs$source_current
    result_crn['I(L)_cs'] <- 1 * simpRL_cs$inductor_current
    result_crn['V(L)_cs'] <- 1e-3 * simpRL_cs$node_voltage 
    result_crn['V(R)_cs'] <- 1e-3 * simpRL_cs$resistor_voltage # V(R) = V(L)

Plot_behavior(
  result_crn, circuit, gate_number, minimum, maximum,
  #plot_species=c('l1il_i1p'), # show model result
  # plot_species=c('V(S)', 'I(L)', 'V(R)', 'V(L)'), # show results 1
  # plot_species=c('I(S)', 'I(L)', 'V(L)', 'V(R)'), # show results 2
  # plot_species=c('l1il_ip', 'V(L)', 'I(L)', 'l1ol_vp'),
  plot_species=c('l1il_ip', 'l1ol_v', 'l1ol_i'),
  plot_species_dotted=c('I(L)_cs', 'V(R)_cs', 'V(L)_cs'),
  chart_title = 'Inductor Step Response CRN Iin=10[A] R=500[ohm] L=1k[H]',
  timing
)
# Plot_behavior(result_crn, circuit, gate_number, minimum, maximum, specify_species = TRUE, plot_species=c('c1ol_current', 'c1sub1_C', 'c1_l_dv_out'))

#resultado_4dom <- React_4domain_circuit(circuit)

#Plot_behavior(
#  resultado_4dom$behavior, circuit, gate_number, minimum, maximum,
#  plot_species=c('l1il_i1p', 'l1ol_in', 'l1ol_vp', 'l1ol_vn'),
#  chart_title = 'Step Response DSD Iin=6.32[mA] L=4.75[mH]',
#  timing
#)

#
#Plot_behavior(resultado_4dom$behavior, circuit, gate_number, minimum, maximum, TRUE)
#
#resultado_comb <- Compare_behaviors(resultado_crn, resultado_4dom, circuit, gate_number)
#
# p1 <- Plot_behavior_comb(resultado_comb, circuit, gate_number, minimum, maximum, TRUE)

