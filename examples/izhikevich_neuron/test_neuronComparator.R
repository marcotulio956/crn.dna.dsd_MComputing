rm(list = ls())

source('R/4domain_reactor.R')
source('R/ANALOG_GATE_LIB.R')
source('R/analysis.R')
source('R/crn_reactor.R')
source('R/dsd.R')
source('R/NEURON_SIM.R')
source('R/NEURON_LIB.R')
source('R/ELECTRO_LIB.R')
source('R/GATE_LIB.R')
source('R/io.R')
source('R/parser.R')
source('R/util_functions.R')
source('R/metric_functions.R')


jn <- function(...) { paste(..., sep = '') }



library(ggplot2) # plot()
library(dplyr) # mutate()

# timing for tests (short durations are enough)
timing <- seq(0, 50, by = 0.1)   # 50 time units at 0.1 resolution

# threshold we will test against (this must match the comparator c_threshold below)
THRESH <- 20

# helper to build a minimal circuit with comparator and a constant v_p input
run_comparator_test_const <- function(vp_value, test_name, timing, c_threshold = THRESH) {
  cat(sprintf("Running comparator test '%s' with v_p = %g\n", test_name, vp_value))

  # 1) make circuit and component mapping (for naming consistency)
  circuit <- DNArLogic::make_circuit(timing)
  izh_comp <- Make_Izhikevich_Component(a = 0.02, b = 0.2, c = -65, d = 8, v_thresh = c_threshold)

  # 2) Create a constant species gate that sets v_p initial concentration to vp_value
  # We'll add a "const" gate whose ci sets the initial concentration of izh_comp$ol$v_p
  g_vp_const <- list(
    name = jn('testvp_const_', format(vp_value)),
    species = list(vp = izh_comp$ol$v_p),
    reactions = c(),        # no reactions: species acts as a constant initial condition
    ci = c(vp_value),       # initial concentration set to vp_value
    ki = c(0)
  )
  circuit <- circuit_add_gate(circuit, g_vp_const)

  # Ensure v_n stays zero by creating a const0 species if needed
  g_vn_const0 <- list(
    name = jn('testvn_const0_', format(vp_value)),
    species = list(vn = izh_comp$ol$v_n),
    reactions = c(),
    ci = c(0),
    ki = c(0)
  )
  circuit <- circuit_add_gate(circuit, g_vn_const0)

  # 3) Create comparator gate (we manually construct it so we can inspect internal species names)
  cmp_name <- jn('cmp_test_', format(vp_value))
  g_cmp <- Make_Comparator(cmp_name, izh_comp$ol$v_p, izh_comp$ol$v_n, nameThreshold = jn(cmp_name, '_th'), nameOutput = izh_comp$ol$comp_out, c_threshold = c_threshold, rate = 1.0)
  circuit <- circuit_add_gate(circuit, g_cmp)

  # 4) (optional) add a spike marker so we can visualize comp -> spike if desired
  # Already done inside earlier neuron builder, but we can reuse comp_out as needed.

  # 5) Simulate CRN
  res_crn <- React_circuit(circuit)
  res_crn <- res_crn[, order(names(res_crn))]

  # 6) Extract relevant traces
  v_p_tr <- if (izh_comp$ol$v_p %in% names(res_crn)) res_crn[[izh_comp$ol$v_p]] else rep(0, nrow(res_crn))
  v_n_tr <- if (izh_comp$ol$v_n %in% names(res_crn)) res_crn[[izh_comp$ol$v_n]] else rep(0, nrow(res_crn))
  v_trace <- v_p_tr - v_n_tr

  # internal comparator species names (from gate)
  xpos_name <- g_cmp$species$x_pos
  xneg_name <- g_cmp$species$x_neg
  thresh_bound_name <- g_cmp$species$thresh_bound
  seed_name <- g_cmp$species$ctrl_seed
  control_name <- g_cmp$species$control

  xpos_tr <- if (xpos_name %in% names(res_crn)) res_crn[[xpos_name]] else rep(0, nrow(res_crn))
  xneg_tr <- if (xneg_name %in% names(res_crn)) res_crn[[xneg_name]] else rep(0, nrow(res_crn))
  thresh_tr <- if (thresh_bound_name %in% names(res_crn)) res_crn[[thresh_bound_name]] else rep(0, nrow(res_crn))
  seed_tr <- if (seed_name %in% names(res_crn)) res_crn[[seed_name]] else rep(0, nrow(res_crn))
  ctrl_tr <- if (control_name %in% names(res_crn)) res_crn[[control_name]] else rep(0, nrow(res_crn))

  # 7) Build result data.frame for Plot_behavior and plot
  result <- data.frame(time = timing,
                       v_in = v_trace,
                       xpos = xpos_tr,
                       xneg = xneg_tr,
                       thresh_bound = thresh_tr,
                       seed = seed_tr,
                       control = ctrl_tr)

  rownames(result) <- as.character(timing)

  # Choose species to plot (solid) and nothing dotted for this test
  plot_species_model <- c('v_in', 'xpos', 'xneg', 'seed', 'control')
  plot_species_sim   <- c()  # nothing dotted

  Plot_behavior(
    result = result,
    circuit = circuit,
    gate_numbers = NULL,
    min = NULL,
    max = NULL,
    plot_species = plot_species_model,
    plot_species_dotted = plot_species_sim,
    chart_title = sprintf("Comparator test — %s — v_p=%g thresh=%g", test_name, vp_value, c_threshold),
    timing = timing
  )

  return(invisible(list(result = result, circuit = circuit, g_cmp = g_cmp)))
}

# helper to attempt oscillator-based pulsed driver (if available)
run_comparator_test_pulse <- function(amplitude, duty = 0.1, freq = 1, timing, c_threshold = THRESH) {
  cat(sprintf("Running comparator pulsed test amplitude=%g\n", amplitude))

  # build circuit & mapping
  circuit <- DNArLogic::make_circuit(timing)
  izh_comp <- Make_Izhikevich_Component(a = 0.02, b = 0.2, c = -65, d = 8, v_thresh = c_threshold)

  # comparator gate
  cmp_name <- jn('cmp_pulse_', format(amplitude))
  g_cmp <- Make_Comparator(cmp_name, izh_comp$ol$v_p, izh_comp$ol$v_n, nameThreshold = jn(cmp_name, '_th'), nameOutput = izh_comp$ol$comp_out, c_threshold = c_threshold, rate = 2.0)
  circuit <- circuit_add_gate(circuit, g_cmp)

  # If Make_Oscillator_Dalchau exists, try to create an oscillator driving izh_comp$ol$v_p
  if (exists("Make_Oscillator_Dalchau", mode = "function")) {
    # Try to construct a pulse-like oscillator that writes to izh_comp$ol$v_p.
    # The exact signature varies; this is a best-effort call based on earlier usage in your project.
    # You may need to adapt arguments if your Make_Oscillator_Dalchau signature differs.
    try({
      # Using 'sin' variant but tuning amplitude via internal cinputs — ADAPT if needed.
      g_drv <- Make_Oscillator_Dalchau('sin', 'drv_pulse', izh_comp$ol$v_p, 'drv_ctrl', 1e-3, 1e-3, 10, amplitude)
      circuit <- circuit_add_gate(circuit, g_drv)
    }, silent = TRUE)

    # if g_drv not created, warn and fall back to a static high constant
    if (!exists("g_drv")) {
      warning("Could not create oscillator driver via Make_Oscillator_Dalchau. Falling back to static constant input for pulse test.")
      g_vp_const <- list(name = jn('vp_const_pulse', amplitude), species = list(vp = izh_comp$ol$v_p), reactions = c(), ci = c(amplitude), ki = c(0))
      circuit <- circuit_add_gate(circuit, g_vp_const)
      g_vn_const0 <- list(name = jn('vn_const_pulse', amplitude), species = list(vn = izh_comp$ol$v_n), reactions = c(), ci = c(0), ki = c(0))
      circuit <- circuit_add_gate(circuit, g_vn_const0)
    }
  } else {
    warning("Make_Oscillator_Dalchau not found; pulse driver not available. Create a driver that toggles izh_comp$ol$v_p to test pulses.")
    # create static const as fallback
    g_vp_const <- list(name = jn('vp_const_pulse', amplitude), species = list(vp = izh_comp$ol$v_p), reactions = c(), ci = c(amplitude), ki = c(0))
    circuit <- circuit_add_gate(circuit, g_vp_const)
    g_vn_const0 <- list(name = jn('vn_const_pulse', amplitude), species = list(vn = izh_comp$ol$v_n), reactions = c(), ci = c(0), ki = c(0))
    circuit <- circuit_add_gate(circuit, g_vn_const0)
  }

  # simulate
  res_crn <- React_circuit(circuit)
  res_crn <- res_crn[, order(names(res_crn))]

  # extract traces
  v_p_tr <- if (izh_comp$ol$v_p %in% names(res_crn)) res_crn[[izh_comp$ol$v_p]] else rep(0, nrow(res_crn))
  v_n_tr <- if (izh_comp$ol$v_n %in% names(res_crn)) res_crn[[izh_comp$ol$v_n]] else rep(0, nrow(res_crn))
  v_trace <- v_p_tr - v_n_tr

  xpos_tr <- if (g_cmp$species$x_pos %in% names(res_crn)) res_crn[[g_cmp$species$x_pos]] else rep(0, nrow(res_crn))
  xneg_tr <- if (g_cmp$species$x_neg %in% names(res_crn)) res_crn[[g_cmp$species$x_neg]] else rep(0, nrow(res_crn))
  ctrl_tr  <- if (g_cmp$species$control %in% names(res_crn)) res_crn[[g_cmp$species$control]] else rep(0, nrow(res_crn))

  result <- data.frame(time = timing, v_in = v_trace, xpos = xpos_tr, xneg = xneg_tr, control = ctrl_tr)
  rownames(result) <- as.character(timing)

  Plot_behavior(result = result, circuit = circuit, gate_numbers = NULL, min = NULL, max = NULL,
                plot_species = c('v_in','xpos','xneg','control'),
                plot_species_dotted = c(),
                chart_title = sprintf("Comparator pulsed test amplitude=%g (thresh=%g)", amplitude, c_threshold),
                timing = timing)

  return(invisible(list(result = result, circuit = circuit, g_cmp = g_cmp)))
}


# -----------------------
# Run a small battery of tests
# -----------------------
# static below threshold
run_comparator_test_const(vp_value = THRESH * 0.5, test_name = "below_threshold", timing = timing)

# static near threshold
run_comparator_test_const(vp_value = THRESH * 0.95, test_name = "near_threshold", timing = timing)

# static above threshold
run_comparator_test_const(vp_value = THRESH * 1.2, test_name = "above_threshold", timing = timing)

# try pulsed driver (best-effort — depends on your Make_Oscillator_Dalchau)
run_comparator_test_pulse(amplitude = THRESH * 1.2, timing = timing)

cat("Comparator tests finished.\n")
