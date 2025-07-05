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

behaviours <- list(
  'Overdamped' = c(R = 2.5, L = 1, C = 1), 
  'Critically damped' = c(R = 2, L = 1, C = 1),
  'Underdamped' = c(R = 1, L = 1.5, C = 1.5)

)

init_p_values <- list(
  'Overdamped' = c(
    a1 = 9.5645737, a2 = 0.2892851, a3 = 9.5628290, a4 = 19.1258191,
    a5 = 9.5627359, a6 = 4.7823812, a7 = 0.1173948, a8 = 101.7993858
  ),
  'Critically damped' = c(
    a1 = 9.9977977, a2 = 0.2470188, a3 = 10.0000000, a4 = 19.9945977,
    a5 = 9.9972182, a6 = 4.9984647, a7 = 0.1158029, a8 = 106.1014152
  ),
  'Underdamped' = c(
    a1 = 1.9298698, a2 = 1.5563071, a3 = 4.7412489, a4 = 9.3631423,
    a5 = 4.6948411, a6 = 0.6383476, a7 = 0.5079602, a8 = 108.4715251
  )
)

regime <- 'Underdamped'# 'Overdamped' or Critically damped or Underdamped

params <- behaviours[[regime]]
R <- params["R"]
L <- params["L"]
C <- params["C"]

init_p <- init_p_values[[regime]]
  
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
  
  # init_p <- c(
  #  9.5645737,   0.2892851,   9.5628290,  19.1258191,   9.5627359,   4.7823812,   0.1173948, 101.7993858 ### OVERDAMPED
  # )
  # [1] "sse_vc: 23.7162894841844 sse_il: 64.668393596737"
  # final  value 88.384683 
  # stopped after 11 iterations
#  print(metrics)
#              Metric     Model       Sim  AbsError RelError
# 1         Peak Time 24.996249 24.996249 0.0000000 0.000000
# 2 Max Overshoot (%)  0.000000  0.000000 0.0000000      NaN
# 3         Rise Time  5.105993  4.781347 0.3246469 6.789863
# 4     Settling Time        NA        NA        NA       NA
# 5     Damping Ratio        NA        NA        NA       NA

  # init_p <- c(
  #   9.9977977,   0.2470188,  10.0000000,  19.9945977,   9.9972182,   4.9984647,   0.1158029, 106.1014152 ### Critically DAMPED
  # )
  # [1] "sse_vc: 85.5262838076758 sse_il: 156.137013592685"
  # final  value 241.663297 
  #stopped after 11 iterations
  # 9.9977977   0.2470188  10.0000000  19.9945977   9.9972182   4.9984647   0.1158029 106.1014152 
#              Metric        Model          Sim     AbsError  RelError
# 1         Peak Time 2.487622e+01 2.438610e+01 0.4901225306  2.009844
# 2 Max Overshoot (%) 1.497730e-03 9.290808e-03 0.0077930781 83.879443
# 3         Rise Time 4.170541e+00 3.607964e+00 0.5625771152 15.592650
# 4     Settling Time 2.488622e+01 2.439610e+01 0.4901225306  2.009020
# 5     Damping Ratio 4.767387e-06 2.957219e-05 0.0000248048 83.878815

  #[1] "sse_vc: 208.653471535878 sse_il: 170.487077254526"
  #final  value 379.140448 
  #stopped after 11 iterations
  #a1          a2          a3          a4          a5          a6          a7          a8 
  #1.9298698   1.5563071   4.7412489   9.3631423   4.6948411   0.6383476   0.5079602 108.4715251 
  #init_p <- c(
  # 1.9298698 ,  1.5563071,   4.7412489,   9.3631423 ,  4.6948411,   0.6383476,   0.5079602, 108.4715251  ### underdamped DAMPED
  #)
#                Metric       Model         Sim    AbsError  RelError
# 1         Peak Time 17.07426857 17.25431358 0.180045011  1.043478
# 2 Max Overshoot (%) 13.23699020 16.55846926 3.321479056 20.059095
# 3         Rise Time  2.54202565  2.64763753 0.105611874  3.988910
# 4     Settling Time 19.73493373 20.56514129 0.830207552  4.036965
# 5     Damping Ratio  0.03953902  0.04871444 0.009175423 18.835120
  
  rlc_gate <- Make_Circuit_RLC(
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
t1 = 40
points = (t1 - t0) * 100 # * 80 # Using 50 time points
timing  <- seq(t0, t1, length.out = points) # Using 50 time points
circuit <- Make_Generic(timing)

result_crn <- React_circuit(circuit)
print(result_crn)
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
# result_crn['rlcol_vc'] <- vc_scale * ( result_crn['rlcl_state1_p'] - result_crn['rlcl_state1_n'])
result_crn['rlcol_vc'] <- vc_scale * ( result_crn['rlcol_vcp'] - result_crn['rlcol_vcn'])
 

i_scale <- 1
# result_crn['rlcol_i'] <- i_scale * (result_crn['rlcl_state2_p'] - result_crn['rlcl_state2_n'])
result_crn['rlcol_i'] <- i_scale * (result_crn['rlcol_ip'] - result_crn['rlcol_in'])


### Plot intenal gates
#state1_scale <- 0.01
#state2_scale <- 0.01
# result_crn['rlcl_mul1'] <- result_crn['rlcl_mul1_p'] - result_crn['rlcl_mul1_n']
# result_crn['rlcl_add3_1'] <- result_crn['rlcl_add3_1_p_carry'] - result_crn['rlcl_add3_1_n_carry']
# result_crn['rlcl_add3_2'] <- result_crn['rlcl_add3_2_p_carry'] - result_crn['rlcl_add3_2_n_carry']
# result_crn['rlcl_mul2'] <- result_crn['rlcl_mul2_p'] - result_crn['rlcl_mul2_n']
# result_crn['rlcl_mul3'] <- result_crn['rlcl_mul3_p'] - result_crn['rlcl_mul3_n']
# result_crn['rlcl_mul4'] <- result_crn['rlcl_mul4_p'] - result_crn['rlcl_mul4_n']
#result_crn['rlcl_state1'] <- state1_scale * ( result_crn['rlcl_state1_p'] - result_crn['rlcl_state1_n'])
#result_crn['rlcl_state2'] <- state2_scale * ( result_crn['rlcl_state2_p'] - result_crn['rlcl_state2_n'])
# result_crn['l_delay1'] <- 0.1 * (result_crn['rlcl_delay1_p'] - result_crn['rlcl_delay1_n'])
# result_crn['l_delay2'] <- 0.1 * ( result_crn['rlcl_delay2_p'] - result_crn['rlcl_delay2_n'])
# result_crn['l_consume1'] <- result_crn['rlcl_consume1_p'] - result_crn['rlcl_consume1_n']
# result_crn['l_consume2'] <- result_crn['rlcl_consume2_p'] - result_crn['rlcl_consume2_n']
# result_crn['l_scaler1'] <- result_crn['rlcl_scaler1_p'] - result_crn['rlcl_scaler1_n']
# result_crn['l_scaler2'] <- result_crn['rlcl_scaler2_p'] - result_crn['rlcl_scaler2_n']

# Result 1 : Series RLC circuit simulation with Voltage Source
result_crn['V(C)'] <- 1 * simRLC$capacitor_voltage
result_crn['I(L)'] <- 1 * simRLC$inductor_current
#result_crn['sum_dx1'] <- 0.01 * simRLC$sum_dx1
#result_crn['sum_dx2'] <- 0.01 * simRLC$sum_dx2
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
  plot_species= c('v1p', 'rlcol_i', 'rlcol_vc'), # c('v1p','rlcol_i', 'rlcol_vc', 'rlcl_state1', 'rlcl_state2', 'rlcl_add3_2', 'rlcl_mul2', 'rlcl_mul3', 'rlcl_mul4',  'l_consume1', 'l_consume2', 'l_scaler1', 'l_scaler2', 'l_delay1', 'l_delay2'
  plot_species_dotted=c('V(C)','I(L)' ), #
# 
  chart_title = sprintf("%s RLC Circuit Response DSD Vin=10[V] R=%s L=%s C=%s\n", regime, R, L, C), # "RLC Step Response 
  timing
)

metrics <- analyze_transient_metrics(
  timing = timing,
  vc_model = result_crn[['rlcol_vc']],
  vc_sim = simRLC$capacitor_voltage,
  t0 = 10,
  t1 = 25
)

print(metrics)

# Plot_behavior(result_crn, circuit, gate_number, minimum, maximum, specify_species = TRUE, plot_species=c('c1ol_current', 'c1sub1_C', 'c1_l_dv_out'))

#resultado_4dom <- React_4domain_circuit(circuit)

#Plot_behavior(
#  resultado_4dom$behavior, circuit, gate_number, minimum, maximum,
#  plot_species= c('v1p', 'rlcol_i', 'rlcol_vc'), # c('v1p','rlcol_i', 'rlcol_vc', 'rlcl_state1', 'rlcl_state2', 'rlcl_add3_2', 'rlcl_mul2', 'rlcl_mul3', 'rlcl_mul4',  'l_consume1', 'l_consume2', 'l_scaler1', 'l_scaler2', 'l_delay1', 'l_delay2'
#  plot_species_dotted=c('V(C)','I(L)' ), #
  # 
#  chart_title = sprintf("Overdamped RLC Circuit Response DSD Vin=10[V] R=%s L=%s C=%s\n", R, L, C), # "RLC Step Response 
#  timing
#)
