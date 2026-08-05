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


library(ggplot2) # plot()
library(dplyr) # mutate()

jn <- function(...) { paste(..., sep = '') }


# --- timing (fine dt recommended)
timing <- seq(0, 200, by = 0.1)  # 200 ms, 0.1 ms steps

# --- define input-current patterns tuned to each behaviour
I_patterns <- list(
  # constant pulse (20-180 ms). amplitude tuned for each behaviour below
  "RS"  = function(t) ifelse(t >= 20 & t <= 180, 10, 0),
  # lower tonic + brief strong kick to reveal bursts
  "IB"  = function(t) {base <- ifelse(t >= 20 & t <= 180, 6, 0);kick <- ifelse(t >= 50 & t <= 55, 20, 0);base + kick},
  # strong tonic current
  "CH"  = function(t) ifelse(t >= 20 & t <= 180, 20, 0),
  "FS"  = function(t) ifelse(t >= 20 & t <= 180, 15, 0),
  "LTS" = function(t) ifelse(t >= 30 & t <= 170, 8, 0),
  # short pulse only
  "PS"  = function(t) ifelse(t >= 30 & t <= 40, 12, 0),
  "TS"  = function(t) ifelse(t >= 20 & t <= 180, 8, 0)
)

# utility to pick I pattern by preset name
pick_I_by_name <- function(name) {
  if (grepl("Regular Spiking", name)) return(I_patterns[["RS"]])
  if (grepl("Intrinsically Bursting", name)) return(I_patterns[["IB"]])
  if (grepl("Chattering", name)) return(I_patterns[["CH"]])
  if (grepl("Fast Spiking", name)) return(I_patterns[["FS"]])
  if (grepl("Low-threshold", name)) return(I_patterns[["LTS"]])
  if (grepl("Phasic", name)) return(I_patterns[["PS"]])
  if (grepl("Tonic", name)) return(I_patterns[["TS"]])
  # fallback: small pulse
  return(function(t) ifelse(t >= 20 & t <= 180, 10, 0))
}

describe_I <- function(I_obj, max_len = 300) {
  # If numeric scalar or vector, summarize
  if (!is.function(I_obj)) {
    if (is.numeric(I_obj)) {
      return(sprintf("numeric vector (len=%d) min=%.3g max=%.3g", length(I_obj), min(I_obj), max(I_obj)))
    }
    return(as.character(I_obj))
  }
  
  body_str <- paste(deparse(body(I_obj)), collapse = " ")
  body_str <- gsub("\\s+", " ", trimws(body_str))
  
  # find all ifelse(...) occurrences
  matches <- gregexpr("ifelse\\s*\\(([^\\)]*)\\)", body_str, perl = TRUE)
  reg_matches <- regmatches(body_str, matches)[[1]]
  
  if (length(reg_matches) == 0) {
    # no ifelse patterns — return collapsed body (truncated)
    return(if (nchar(body_str) <= max_len) body_str else paste0(substr(body_str, 1, max_len), "..."))
  }
  
  parse_one_ifelse <- function(ifelse_text) {
    inner <- sub("^ifelse\\s*\\((.*)\\)$", "\\1", ifelse_text, perl = TRUE)
    # split into parts by comma; assume typical form: condition, value, else
    parts <- strsplit(inner, ",")[[1]]
    parts <- trimws(parts)
    cond <- if (length(parts) >= 1) parts[1] else ""
    val  <- if (length(parts) >= 2) parts[2] else ""
    # try to extract interval numbers from condition like "t >= 20 & t <= 180"
    nums <- regmatches(cond, gregexpr("-?\\d+\\.?\\d*", cond, perl = TRUE))[[1]]
    if (length(nums) >= 2 && grepl("&", cond)) {
      return(paste0("t in [", nums[1], ",", nums[2], "] -> ", val))
    }
    # fallback: show short condition
    cond_short <- if (nchar(cond) > 80) paste0(substr(cond, 1, 80), "...") else cond
    return(paste0("if(", cond_short, ") -> ", val))
  }
  
  summaries <- vapply(reg_matches, parse_one_ifelse, FUN.VALUE = character(1))
  out <- paste(summaries, collapse = "; ")
  if (nchar(out) > max_len) out <- paste0(substr(out, 1, max_len), "...")
  out
}

title_from_I_smart <- function(name, I_fun) {
  paste0(name, " — input: ", describe_I(I_fun))
}

# ---------- Run ODE + CRN and plot comparison for each preset ----------
for (name in names(izh_presets)) {
  params <- izh_presets[[name]]
  I_fun <- pick_I_by_name(name)
  title <- title_from_I_smart(name, I_fun)

  cat("Running behaviour:", name, " — title:", title, "\n")

  # --- 1) ODE simulation (reference)
  sim <- simulate_izhikevich(timing = timing, I = I_fun, params = params)

  # --- 2) Build CRN circuit
  circuit <- DNArLogic::make_circuit(timing)

  # Create neuron component (species mapping)
  izh_comp <- Make_Izhikevich_Component(a = params$a, b = params$b, c = params$c, d = params$d, v_thresh = params$v0 %||% 30)
  # Note: v_thresh default used here; adjust if you want different thresholds per preset.

  # Attach a driver to izh_comp$il$I_p.
  # Attempt to use Make_Oscillator_Dalchau if available. If your Make_Oscillator_* supports other signatures,
  # replace this block with the correct driver gate that writes into izh_comp$il$I_p.
  if (exists("Make_Oscillator_Dalchau", mode = "function")) {
    # We try to approximate piecewise I_fun by driving a pulse window:
    # This is a *best-effort* connector: adapt parameters to your Make_Oscillator_Dalchau signature.
    # Many of your previous calls used: Make_Oscillator_Dalchau('sin', 'x', 'v1p', 'z', 1e-3, 1e-3, 15, 4e-1)
    # Here we call a simple pulse-like oscillator that writes to I_p:
    try({
      g_drv <- Make_Oscillator_Dalchau('pulse', 'drv', izh_comp$il$I_p, 'drv_ctrl', 1e-3, 1e-3, 1, 1.0)
      circuit <- circuit_add_gate(circuit, g_drv)
    }, silent = TRUE)
    if (!exists("g_drv")) {
      warning("Could not create driver with Make_Oscillator_Dalchau — you should replace driver creation with a gate that writes izh_comp$il$I_p according to I_fun.")
    }
  } else {
    warning("Make_Oscillator_Dalchau not available. Please add a suitable driver gate that maps your I_fun into the CRN input species: ", izh_comp$il$I_p)
  }

  # --- 3) Build neuron gates and add to circuit
  # use default rate vector p_rates (you will tune these later)
  p_rates <- list(rate_base=1, rate_mul_sq=1, rate_mul_lin=1, rate_add=1, rate_int_v=1, rate_int_u=1, rate_mul_b=1, rate_a_scale=1, mux_rate=1, comp_rate=1)
  izh_gates <- Make_Circuit_Izhikevich_Full(izh_comp$name, izh_comp$il, izh_comp$ol, izh_comp$ic, p = p_rates)
  circuit <- circuit_add_compile_gates(circuit, izh_gates)

  # If you need to set initial concentrations for const species (e.g. const_140, const_c, const_d), do so here:
  # Example pattern (adapt to your circuit API if needed):
  # circuit$ci[ jn(izh_comp$name, '_const140') ] <- 140
  # circuit$ci[ jn(izh_comp$name, '_const_c') ] <- params$c
  # circuit$ci[ jn(izh_comp$name, '_const_d') ] <- params$d

  # --- 4) React the circuit (CRN simulation)
  res_crn <- React_circuit(circuit)
  res_crn <- res_crn[, order(names(res_crn))]

  # --- 5) Extract CRN traces: v = v_p - v_n, u = u_p - u_n, I = I_p - I_n
  v_p_name <- izh_comp$ol$v_p
  v_n_name <- izh_comp$ol$v_n
  u_p_name <- izh_comp$ol$u_p
  u_n_name <- izh_comp$ol$u_n
  I_p_name <- izh_comp$il$I_p
  I_n_name <- izh_comp$il$I_n
  spike_name <- izh_comp$ol$spike
  comp_name <- izh_comp$ol$comp_out

  # Safe extraction with fallbacks
  v_p_tr <- if (v_p_name %in% names(res_crn)) res_crn[[v_p_name]] else rep(0, nrow(res_crn))
  v_n_tr <- if (v_n_name %in% names(res_crn)) res_crn[[v_n_name]] else rep(0, nrow(res_crn))
  u_p_tr <- if (u_p_name %in% names(res_crn)) res_crn[[u_p_name]] else rep(0, nrow(res_crn))
  u_n_tr <- if (u_n_name %in% names(res_crn)) res_crn[[u_n_name]] else rep(0, nrow(res_crn))
  I_p_tr <- if (I_p_name %in% names(res_crn)) res_crn[[I_p_name]] else rep(0, nrow(res_crn))
  I_n_tr <- if (I_n_name %in% names(res_crn)) res_crn[[I_n_name]] else rep(0, nrow(res_crn))
  spike_tr <- if (spike_name %in% names(res_crn)) res_crn[[spike_name]] else rep(0, nrow(res_crn))
  comp_tr <- if (comp_name %in% names(res_crn)) res_crn[[comp_name]] else rep(0, nrow(res_crn))

  v_crn <- v_p_tr - v_n_tr
  u_crn <- u_p_tr - u_n_tr
  I_crn <- I_p_tr - I_n_tr

  # --- 6) Build result data.frame for Plot_behavior
  # Plot_behavior expects a data.frame like your result_crn — put CRN traces as main species and simulation traces as dotted species
  result <- data.frame(time = timing)
  # CRN model outputs (solid)
  result[['v_crn']] <- v_crn
  result[['u_crn']] <- u_crn
  result[['I_crn']] <- I_crn
  result[['izh_spike']] <- spike_tr
  result[['izh_comp']] <- comp_tr

  # ODE simulation outputs (dotted lines)
  # Align lengths: simulate_izhikevich already used same timing vector, so lengths should match
  sim_v <- sim$v
  sim_u <- sim$u
  # If lengths mismatch, interpolate
  if (length(sim_v) != nrow(result)) sim_v <- approx(sim$time, sim$v, xout = timing)$y
  if (length(sim_u) != nrow(result)) sim_u <- approx(sim$time, sim$u, xout = timing)$y

  result[['V_sim']] <- sim_v
  result[['U_sim']] <- sim_u

  # Set rownames to timing (some plotting helpers expect them)
  rownames(result) <- as.character(timing)

  # --- 7) Call your Plot_behavior:
  # Choose which species to plot solid (model) and which dotted (sim). Adjust names if you prefer different labels.
  plot_species_model <- c('v_crn', 'u_crn', 'I_crn', 'izh_spike')
  plot_species_sim   <- c('V_sim', 'U_sim')   # these will be plotted dotted

  Plot_behavior(
    result = result,
    circuit = circuit,
    gate_numbers = NULL,
    min = -90,
    max = 40,
    plot_species = plot_species_model,
    plot_species_dotted = plot_species_sim,
    chart_title = sprintf("%s — CRN (solid) vs ODE sim (dotted) — %s", name, title),
    timing = timing
  )

  # optional pause to inspect each plot
  readline(prompt = "Press <enter> to continue to next behaviour...")
}
