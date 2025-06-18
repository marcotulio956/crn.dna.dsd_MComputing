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

R <- 0.5#1e3         # 1 kOhm
L <- 0.5#15e-3       # 15 mH
C <- 1#15e-4       # 150 uF

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
      0, 7.0e-1
    )

    # add2circuit
    circuit <- circuit_add_gate(circuit, g_dalchau)
    circuit <- circuit_add_gate(circuit, c_comparator)
  
  rlc <- Make_RLC_Component(R,L,C)

  rlc$il$voltage_positive <- 'v1p'

  # init_p <- c(
  #   a1 = 2, # rate base
  #   a2 = 100, # fuel base
  #   a3 = 10, # range base add3
  #   a4 = 2.32,    # rate_mul1 
  #   a5 = 0.059,    # rate_mul2 
  #   a6 = 1.181,    # rate_mul3 
  #   a7 = 0.2360,    # rate_mul4 
  #   a8 = 0.7535,    # rate_int1 
  #   a9 = 2.699,    # rate_int2 
  #   a10 = 1.2330,    # rate_add3 
  #   a11 = 1.6467,    # range_add3
  #   a12 = 10,    # fuel_states
  #   a13 = 1.61649,  # rate_states1
  #   a14 = 0.1218   # rate_states2
  # )
  
  init_p <- c(
    a1  = 1e3, # rate base
    a2  = 10, # fuel base
    a3  = 1, # range base add3
    a4  = 1,    # rate_mul1 
    a5  = 1,    # rate_mul2 
    a6  = 1,    # rate_mul3 
    a7  = 1,    # rate_mul4 
    a8  = 1,    # rate_int1 
    a9  = 1,    # rate_int2 
    a10 = 1,    # rate_add3 
    a11 = 1,    # range_add3
    a12 = 10,    # fuel_states
    a13 = 2,  # rate_states1
    a14 = 2   # rate_states2
  )
  

  rlc_gate <- Make_Circuit_RLC_stepbystep(
    rlc$name,
    rlc$il,
    rlc$ol,
    rlc$ic,
    # Pass in exactly the six values we just computed:
    init_p
  )
  circuit <- circuit_add_electro_gates(circuit, rlc_gate)

  return (circuit)
}

t0 = 0
t1 = 35
points = (t1 - t0) * 300 # * 80 # Using 50 time points
timing  <- seq(t0, t1, length.out = points) # Using 50 time points
circuit <- Make_Generic(timing)

result_crn <- React_circuit(circuit)
result_crn <- result_crn[, order(names(result_crn))]

expected_value = 10
minimum = expected_value * 0.95
maximum = expected_value * 1.05
gate_number = 1



# for (col_name in names(result_crn)) {
#   if (grepl("_p$", col_name)) {
#     # Get base name (remove '_p')
#     base_name <- sub("_p$", "", col_name)
#     neg_col <- paste0(base_name, "_n")
    
#     # Ensure the corresponding _n column exists
#     if (neg_col %in% names(result_crn)) {
#       # Calculate the max and min of the difference (_p - _n)
#       diff_vals <- result_crn[[col_name]] - result_crn[[neg_col]]
#       max_val <- max(diff_vals, na.rm = TRUE)
#       min_val <- min(diff_vals, na.rm = TRUE)
      
#       # Avoid division by zero
#       if (max_val != min_val) {
#         scale_value <- 1 / (max_val - min_val)
#       } else {
#         scale_value <- 1  # or 0, depending on how you want to handle constant values
#       }
      
#       # Apply the normalization
#       result_crn[[base_name]] <- scale_value * diff_vals
#     }
#   }
# }

simRLC <- simulate_sRLC_voltage_source(
  timing, result_crn[['v1p']], R, L, C
)

#simVcc <- simulate_Vcc(timing)
#result_crn['Vcc'] <- simVcc

vc_scale <- 1
#result_crn['rlcol_vc'] <- vc_scale * ( result_crn['rlcol_vcp'] - result_crn['rlcol_vcn'])

i_scale <- 1
#result_crn['rlcol_i'] <- i_scale * (result_crn['rlcol_ip'] - result_crn['rlcol_in'])


### Plot intenal gates
state1_scale <- 0.01
state2_scale <- 0.01
result_crn['rlcl_mul1'] <- result_crn['rlcl_mul1_p'] - result_crn['rlcl_mul1_n']
result_crn['rlcl_add3_1'] <- result_crn['rlcl_add3_1_p_carry'] - result_crn['rlcl_add3_1_n_carry']
result_crn['rlcl_add3_2'] <- result_crn['rlcl_add3_2_p_carry'] - result_crn['rlcl_add3_2_n_carry']
result_crn['rlcl_mul2'] <- result_crn['rlcl_mul2_p'] - result_crn['rlcl_mul2_n']
result_crn['rlcl_mul3'] <- result_crn['rlcl_mul3_p'] - result_crn['rlcl_mul3_n']
result_crn['rlcl_mul4'] <- result_crn['rlcl_mul4_p'] - result_crn['rlcl_mul4_n']
#result_crn['rlcl_state1'] <- state1_scale * ( result_crn['rlcl_state1_p'] - result_crn['rlcl_state1_n'])
#result_crn['rlcl_state2'] <- state2_scale * ( result_crn['rlcl_state2_p'] - result_crn['rlcl_state2_n'])
result_crn['l_delay1'] <- 0.1 * (result_crn['rlcl_delay1_p'] - result_crn['rlcl_delay1_n'])
result_crn['l_delay2'] <- 0.1 * ( result_crn['rlcl_delay2_p'] - result_crn['rlcl_delay2_n'])
result_crn['l_consume1'] <- result_crn['rlcl_consume1_p'] - result_crn['rlcl_consume1_n']
result_crn['l_consume2'] <- result_crn['rlcl_consume2_p'] - result_crn['rlcl_consume2_n']
result_crn['l_scaler1'] <- result_crn['rlcl_scaler1_p'] - result_crn['rlcl_scaler1_n']
result_crn['l_scaler2'] <- result_crn['rlcl_scaler2_p'] - result_crn['rlcl_scaler2_n']

# Result 1 : Series RLC circuit simulation with Voltage Source
result_crn['V(C)'] <- 1 * simRLC$capacitor_voltage
result_crn['I(L)'] <- 1 * simRLC$inductor_current
result_crn['sum_dx1'] <- 0.01 * simRLC$sum_dx1
result_crn['sum_dx2'] <- 0.01 * simRLC$sum_dx2
#result_crn['V(S)'] <- 1 * simRLC$source_voltage
#result_crn['V(R)'] <- 1 * simRLC$resistor_voltage
#result_crn['V(L)'] <- 1 * simRLC$inductor_voltage

#true_vc <- simRLC$capacitor_voltage
#true_il <- simRLC$inductor_current
#crn_vc <- result_crn['rlcol_vc']
#crn_il <- result_crn['rlcol_i']

# Compute the sum of squared errors over all time points:
#SSE_vc <- sum( (crn_vc - true_vc)^2 )
#SSE_il <- sum( (crn_il - true_il)^2 )
#total_error <- SSE_vc + SSE_il
#cat("totalerror= ", total_error, "(", SSE_vc, "+", SSE_il, ')\n')


Plot_behavior(
  result_crn, circuit, gate_number, minimum, maximum,
  #plot_species=c('V(S)', 'V(C)', 'I(L)'), # show results 1: 'V(S)', 'V(C)', 'I(L)', 'V(R)', 'V(L)'
  #plot_species=c('v1p', 'rlcol_vc', 'rlcol_i'), # show model 'v1p', 'rlcol_vc', 'rlcol_i'
  #  plot_species=c('v1p', 'rlcol_vc', 'rlcol_i', 'rlcl_mul1_p', 'V(C)', 'I(L)'), # show comparision
  plot_species= c('l_scaler1', 'l_scaler2', 'l_delay1', 'l_delay2'), # c('v1p','rlcol_i', 'rlcol_vc', 'rlcl_state1', 'rlcl_state2', 'rlcl_add3_2', 'rlcl_mul2', 'rlcl_mul3', 'rlcl_mul4',  'l_consume1', 'l_consume2',
  plot_species_dotted=c('V(C)','I(L)'),
# 
  chart_title = sprintf("RLC Step Response CRN Vin=10[V] R=%s L=%s C=%s\n", R, L, C),
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

