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
    g_dalchau <- Make_Oscillator_Dalchau('sin', 'x', 'v1p', 'z', 1e-3, 1e-3, 15, 4e-1)
    c_comparator <- Make_Mux2_balanced(
      'mux1',
      'x', 'v1p', 
      'low', 'high',
      'comp_out',
      0, 0,
      3, 8,
      0, 7.1e-1
    )

    # add2circuit
    circuit <- circuit_add_gate(circuit, g_dalchau)
    circuit <- circuit_add_gate(circuit, c_comparator)
  
  rlc <- Make_RLC_Component(5, 10, 3)

  rlc$il$voltage_positive <- 'v1p'

  rate <- 1
  fuel <- 100
  rlc_gate <- Make_Circuit_RLC(rlc$name, rlc$il, rlc$ol, rlc$ic, rate, fuel)

  circuit <- circuit_add_electro_gates(circuit, rlc_gate)

  return (circuit)
}

t0 = 0
t1 = 150
points = (t1 - t0) * 50 # * 80 # Using 50 time points
timing  <- seq(t0, t1, length.out = points) # Using 50 time points
circuit <- Make_Generic(timing)

result_crn <- React_circuit(circuit)
result_crn <- result_crn[, order(names(result_crn))]

expected_value = 10
minimum = expected_value * 0.95
maximum = expected_value * 1.05
gate_number = 1

vc_scale <- 1
result_crn['rlcol_vcp'] <- result_crn['rlcol_vcp'] * vc_scale
result_crn['rlcol_vcn'] <- result_crn['rlcol_vcn'] * vc_scale
result_crn['rlcol_vc'] <- result_crn['rlcol_vcp'] - result_crn['rlcol_vcn']

i_scale <- 1
result_crn['rlcol_ip'] <- result_crn['rlcol_ip'] * i_scale
result_crn['rlcol_in'] <- result_crn['rlcol_in'] * i_scale
result_crn['rlcol_i'] <- result_crn['rlcol_ip'] - result_crn['rlcol_in']


R <- 5#1e3         # 1 kOhm
L <- 10#15e-3       # 15 mH
C <- 3#15e-4       # 150 uF

simRLC <- simulate_sRLC_voltage_source(
  timing, result_crn[['v1p']], R, L, C
)

# Result 1 : Series RLC circuit simulation with Voltage Source
result_crn['V(C)'] <- 1 * simRLC$capacitor_voltage
result_crn['I(L)'] <- 1 * simRLC$inductor_current
result_crn['V(S)'] <- 1 * simRLC$source_voltage
#result_crn['V(R)'] <- 1 * simRLC$resistor_voltage
#result_crn['V(L)'] <- 1 * simRLC$inductor_voltage


chart_t <- cat(sprintf("RLC Step Response CRN Vin=10[V] R=\"%s\" L=\"%s\" C=\"%s\"\n", R, L, C))

Plot_behavior(
  result_crn, circuit, gate_number, minimum, maximum,
  #plot_species=c('V(S)', 'V(C)', 'I(L)'), # show results 1: 'V(S)', 'V(C)', 'I(L)', 'V(R)', 'V(L)'
  plot_species=c('v1p', 'rlcol_vc', 'rlcol_i', 'V(C)', 'I(L)'), # show model 'v1p', 'rlcol_vc', 'rlcol_i'
  chart_title = chart_t,
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

