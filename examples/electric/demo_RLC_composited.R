source('R/parser.R')
source('R/util_functions.R')
source('R/crn_reactor.R')
source('R/4domain_reactor.R')
source('R/io.R')

source('R/GATE_LIB.R')
source('R/ANALOG_GATE_LIB.R')
source('R/ELECTRO_LIB.R')
source('R/ELECTRO_SIM.R')

source('R/forced_concentrations.R')

behaviours <- list(
  'O' = c(R = 4, L = 1, C = 1),# 2
  'C' = c(R = 2, L = 1, C = 1),# 1 
  'U' = c(R = 1, L = 1, C = 1) # 0.5
)


requireNamespace('diffeqr',quietly = TRUE)
solver <- 'diffeqr'
  


t0 = 0
t1 = 60
points = (t1 - t0) * 400 # * 80 # Using 50 time points
timing  <- seq(t0, t1, length.out = points) # Using 50 time points

behavior <- data.frame(time = timing)

for (regime in names(behaviours)) {
  params <- behaviours[[regime]]
  R <- params["R"]
  L <- params["L"]
  C <- params["C"]

  circuit <- make_circuit(timing)

  circuit <- Make_Circuit_RLC_Composited_RLC(timing, regime)


  cat(sprintf("Simulating %s RLC circuit...\n", regime))
  result <- react4(
    species = circuit$species,
    ci = circuit$ci,
    reactions = circuit$reactions,
    ki = circuit$ki,
    t = circuit$t,
    engine = solver,
    verbose = FALSE,
    forced_concentrations = list(
    v1p = function(t) square_input(t, pulse_width = 60, period = 60, amplitude = 10)
    )
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
    t0 = 0,
    t1 = 30,
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

dsd <- Translate_4domain(circuit)