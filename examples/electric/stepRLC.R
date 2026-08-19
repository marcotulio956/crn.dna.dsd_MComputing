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
  'O' = c(R = 2.2, L = 1, C = 1),# 2
  'C' = c(R = 1.2, L = 1, C = 1),# 1 
  'U' = c(R = 0.25, L = 1, C = 1) # 0.5
)



init_p_values <- list(
  'O' = c(
    a1 = 9.5645737, a2 = 0.2892851, a3 = 9.5628290, a4 = 19.1258191,
    a5 = 9.5627359, a6 = 4.7823812, a7 = 0.1173948, a8 = 101.7993858
  ),
  'C' = c(
    a1 = 9.9977977, a2 = 0.2470188, a3 = 10.0000000, a4 = 19.9945977,
    a5 = 9.9972182, a6 = 4.9984647, a7 = 0.1158029, a8 = 106.1014152
  ),
  'U' = c(
    a1 = 1.9298698, a2 = 1.5563071, a3 = 4.7412489, a4 = 9.3631423,
    a5 = 4.6948411, a6 = 0.6383476, a7 = 0.5079602, a8 = 108.4715251
  )
)





requireNamespace('diffeqr',quietly = TRUE)
solver <- 'diffeqr'
  
Make_RLC <- function(timing, regime) {
  # regime <- 'Underdamped'# 'Overdamped' or Critically damped or Underdamped

  params <- behaviours[[regime]]
  R <- params["R"]
  L <- params["L"]
  C <- params["C"]

  init_p <- init_p_values[[regime]]

  circuit <- make_circuit(timing)
  
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
  circuit <- circuit_add_compile_gates(circuit, rlc_gate)

  return (circuit)
}

t0 = 0
t1 = 80
points = (t1 - t0) * 400 # * 80 # Using 50 time points
timing  <- seq(t0, t1, length.out = points) # Using 50 time points

behavior <- data.frame(time = timing)

for (regime in names(behaviours)) {
  params <- behaviours[[regime]]
  R <- params["R"]
  L <- params["L"]
  C <- params["C"]

  circuit <- Make_RLC(timing, regime)


  cat(sprintf("Simulating %s RLC circuit...\n", regime))
  result <- react2(
    species = circuit$species,
    ci = circuit$ci,
    reactions = circuit$reactions,
    ki = circuit$ki,
    t = circuit$t,
    engine = solver,
    verbose = FALSE
  )


  if (!"vc_in" %in% names(behavior)) {
    behavior[["v_in"]] <- result[, "v1p"]
  }
  # all_result[[jn("i_",regime)]] <- result[, "rlcol_ip"] - result[, "rlcol_in"]
  behavior[[jn("vc_",regime)]] <- result[, "rlcol_vcp"] - result[, "rlcol_vcn"] 

  simRLC <- simulate_sRLC_voltage_source(
    timing, result[['v1p']], R, L, C
  )

  behavior[[jn("V(C)_",regime)]] <- simRLC$capacitor_voltage
  # all_result[[jn("I(L)_",regime)]] <- simRLC$inductor_current

  # assign(paste0("result_", gsub(" ", "_", regime)), result)

  metrics <- analyze_transient_metrics(
    timing = timing,
    v_in = behavior[["v_in"]],
    vc_model = result[['rlcol_vcp']] - result[['rlcol_vcn']],
    vc_sim = simRLC$capacitor_voltage,
    t0 = 10,
    t1 = 25,
    resistance = R,
    inductance = L,
    capacitance = C
  )

    print(metrics)
}


plot_behavior(
  behavior, 
  title = sprintf("RLC Response DSD Vin=10[V]\n"), # "RLC Step Response 
  species = c('v_in', 'vc_O', 'vc_C', 'vc_U'),
  species_dotted= c('V(C)_O', 'V(C)_C', 'V(C)_U'),
)

