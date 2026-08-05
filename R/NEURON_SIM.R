

# Izhikevich neuron simulator and plotting helpers

# simulate_izhikevich:
# - timing: numeric vector of time points (in ms or seconds; consistent units required)
# - I: scalar, numeric vector same length as timing, or function(timing) returning vector
# - params: list(a,b,c,d, v0=-65, u0=NULL, v_thresh=30)
# - returns: list(time=timing, v, u, spike_times, spike_idx, params)
simulate_izhikevich <- function(timing, I = 10, params = list(a=0.02, b=0.2, c=-65, d=8, v0=-65, u0=NULL, v_thresh = 30)) {
  n <- length(timing)
  if (n < 2) stop("timing must have at least 2 points")
  if (any(diff(timing) <= 0)) stop("timing must be strictly increasing")
  # Input current vector
  if (is.function(I)) {
    Ivec <- I(timing)
  } else if (length(I) == 1) {
    Ivec <- rep(I, n)
  } else if (length(I) == n) {
    Ivec <- as.numeric(I)
  } else stop("I must be scalar, vector of same length as timing, or a function")
  # Parameters (avoid using 'c' name directly)
  a <- params$a; b <- params$b; c_reset <- params$c; d <- params$d
  v_thresh <- ifelse(is.null(params$v_thresh), 30, params$v_thresh)
  v <- numeric(n); u <- numeric(n)
  v[1] <- ifelse(is.null(params$v0), -65, params$v0)
  if (is.null(params$u0)) u[1] <- b * v[1] else u[1] <- params$u0
  spike_times <- numeric(0); spike_idx <- integer(0)
  for (i in seq_len(n-1)) {
    dt_i <- timing[i+1] - timing[i]
    dv <- 0.04 * v[i]^2 + 5 * v[i] + 140 - u[i] + Ivec[i]
    du <- a * (b * v[i] - u[i])
    v[i+1] <- v[i] + dt_i * dv
    u[i+1] <- u[i] + dt_i * du
    if (v[i+1] >= v_thresh) {
      v[i] <- v_thresh
      spike_times <- c(spike_times, timing[i])
      spike_idx <- c(spike_idx, i)
      v[i+1] <- c_reset
      u[i+1] <- u[i+1] + d
    }
  }
  # return params with v_thresh for plotting safety
  params$v_thresh <- v_thresh
  params$c <- c_reset
  return(list(time = timing, v = v, u = u, spike_times = spike_times, spike_idx = spike_idx, params = params, I = Ivec))
}


# Example: run multiple behaviours and plot (faceted)
example_run_behaviours <- function() {
# Preset parameter sets (classic Izhikevich 2003 examples)
izh_presets <- list(
  "Regular Spiking (RS)" = list(a=0.02, b=0.2, c=-65, d=8, v0=-65),
  "Intrinsically Bursting (IB)" = list(a=0.02, b=0.2, c=-55, d=4, v0=-65),
  "Chattering (CH)" = list(a=0.02, b=0.2, c=-50, d=2, v0=-60),
  "Fast Spiking (FS)" = list(a=0.1, b=0.2, c=-65, d=2, v0=-65),
  "Low-threshold spiking (LTS)" = list(a=0.02, b=0.25, c=-65, d=2, v0=-65),
  "Phasic Spiking (PS)" = list(a=0.02, b=0.25, c=-65, d=6, v0=-64),
  "Tonic Spiking (TS)" = list(a=0.02, b=0.2, c=-65, d=6, v0=-65)
)
  # timing in ms: use a fine dt for neuron sims (0.1 ms recommended)
  timing <- seq(0, 200, by = 0.1)  # 200 ms at 0.1 ms resolution
  # constant input current (you can instead pass a vector or function)
  I_inj <- function(t) { ifelse(t >= 20 & t <= 180, 10, 0) } # pulse from 20 to 180 ms
  
  sims <- lapply(names(izh_presets), function(name) {
    sim <- simulate_izhikevich(timing, I = I_inj, params = izh_presets[[name]])
    data.frame(time = sim$time, v = sim$v, u = sim$u, behaviour = name, spike = 0) %>%
      mutate(spike = ifelse(row_number() %in% sim$spike_idx, 1, 0))
  })
  combined <- bind_rows(sims)
  
  # Plot membrane potentials faceted by behaviour
  p <- ggplot(combined, aes(x=time, y=v)) +
    geom_line() +
    geom_point(data = combined %>% filter(spike==1), aes(x=time, y=rep(30, n())), shape = 4, size = 1.5) +
    facet_wrap(~ behaviour, ncol = 1, scales = "free_y") +
    labs(x = "time (ms)", y = "v", title = "Izhikevich behaviours (example input)") +
    theme_minimal()
  print(p)
  invisible(list(timing = timing, combined = combined))
}

# Wrapper to use Plot_behavior instead of plot_izhikevich
plot_izhikevich_with_Plot_behavior <- function(
  timing,
  I = 10,
  params = list(a=0.02, b=0.2, c=-65, d=8, v0=-65, u0=NULL, v_thresh = 30),
  circuit = NULL,          # can be NULL because we pass plot_species explicitly
  gate_numbers = NULL,
  y_min = NULL,            # horizontal green line (min) or NULL
  y_max = NULL,            # horizontal red line (max) or NULL
  plot_species = c("v","u","I"),
  plot_species_dotted = c("I"),
  chart_title = "Izhikevich neuron"
) {
  # run simulation (expects simulate_izhikevich to be defined)
  sim <- simulate_izhikevich(timing = timing, I = I, params = params)
  
  # Build a result data.frame similar to your result_crn
  result <- data.frame(
    time = sim$time,
    v    = sim$v,
    u    = sim$u,
    I    = sim$I
  )
  # also set rownames to timing (many plotting helpers read rownames/time)
  rownames(result) <- as.character(sim$time)
  
  # If you want spike markers as a species column (optional)
  if (length(sim$spike_idx) > 0) {
    spikes <- rep(0, nrow(result))
    spikes[sim$spike_idx] <- 1
    result$spike <- spikes
    # If user didn't request 'spike' in plot_species, we won't add it automatically
  }
  
  # Call your Plot_behavior wrapper
  # Note: Plot_behavior signature: Plot_behavior(result, circuit, gate_numbers, min, max, plot_species, plot_species_dotted, chart_title, timing)
  # We pass circuit (can be NULL) and gate_numbers (NULL), and timing as last arg
  Plot_behavior(
    result = result,
    circuit = circuit,
    gate_numbers = gate_numbers,
    min = y_min,
    max = y_max,
    plot_species = plot_species,
    plot_species_dotted = plot_species_dotted,
    chart_title = chart_title,
    timing = timing
  )
}

# Ideal Hebbian-style reference simulator for the DSD neuron example.
# The model keeps the input pools fixed and uses a simple deterministic
# ODE approximation for B, E and the Hebbian weights.
resolve_neuron_trace_input <- function(value, timing, label) {
  if (is.function(value)) {
    return(as.numeric(value(timing)))
  }
  if (length(value) == 1) {
    return(rep(as.numeric(value), length(timing)))
  }
  if (length(value) == length(timing)) {
    return(as.numeric(value))
  }
  stop(sprintf("%s must be scalar, a vector with the same length as timing, or a function", label))
}

simulate_hebbian_ideal <- function(
  timing,
  A1 = 0.05,
  A2 = 0.02,
  A3 = 0.01,
  H1 = 0.60,
  H2 = 0.70,
  H3 = 0.80,
  B0 = 0,
  E0 = 0,
  rates = list(
    kSM1 = 0.80,
    kSM2 = 0.70,
    kSM3 = 0.60,
    kAF = 0.90,
    kWA1 = 0.010,
    kWA2 = 0.010,
    kWA3 = 0.010,
    kdegB = 0.15,
    kdegE = 0.08,
    kdegH1 = 0.005,
    kdegH2 = 0.005,
    kdegH3 = 0.005
  )
) {
  if (length(timing) < 2) stop("timing must have at least 2 points")
  if (any(diff(timing) <= 0)) stop("timing must be strictly increasing")

  A1_vec <- resolve_neuron_trace_input(A1, timing, "A1")
  A2_vec <- resolve_neuron_trace_input(A2, timing, "A2")
  A3_vec <- resolve_neuron_trace_input(A3, timing, "A3")

  n <- length(timing)
  H1_vec <- numeric(n)
  H2_vec <- numeric(n)
  H3_vec <- numeric(n)
  B_vec <- numeric(n)
  E_vec <- numeric(n)
  waste_vec <- numeric(n)

  H1_vec[1] <- as.numeric(H1)
  H2_vec[1] <- as.numeric(H2)
  H3_vec[1] <- as.numeric(H3)
  B_vec[1] <- as.numeric(B0)
  E_vec[1] <- as.numeric(E0)

  for (i in seq_len(n - 1)) {
    dt <- timing[i + 1] - timing[i]

    drive_B <- rates$kSM1 * A1_vec[i] * H1_vec[i] +
      rates$kSM2 * A2_vec[i] * H2_vec[i] +
      rates$kSM3 * A3_vec[i] * H3_vec[i]
    dB <- drive_B - rates$kdegB * B_vec[i]
    dE <- rates$kAF * B_vec[i] - rates$kdegE * E_vec[i]
    dH1 <- rates$kWA1 * A1_vec[i] * E_vec[i] - rates$kdegH1 * H1_vec[i]
    dH2 <- rates$kWA2 * A2_vec[i] * E_vec[i] - rates$kdegH2 * H2_vec[i]
    dH3 <- rates$kWA3 * A3_vec[i] * E_vec[i] - rates$kdegH3 * H3_vec[i]
    dW <- rates$kdegB * B_vec[i] + rates$kdegE * E_vec[i] +
      rates$kdegH1 * H1_vec[i] + rates$kdegH2 * H2_vec[i] + rates$kdegH3 * H3_vec[i]

    B_vec[i + 1] <- max(0, B_vec[i] + dt * dB)
    E_vec[i + 1] <- max(0, E_vec[i] + dt * dE)
    H1_vec[i + 1] <- max(0, H1_vec[i] + dt * dH1)
    H2_vec[i + 1] <- max(0, H2_vec[i] + dt * dH2)
    H3_vec[i + 1] <- max(0, H3_vec[i] + dt * dH3)
    waste_vec[i + 1] <- max(0, waste_vec[i] + dt * dW)
  }

  data.frame(
    time = timing,
    A1 = A1_vec,
    A2 = A2_vec,
    A3 = A3_vec,
    H1 = H1_vec,
    H2 = H2_vec,
    H3 = H3_vec,
    B = B_vec,
    E = E_vec,
    waste = waste_vec
  )
}

plot_hebbian_ideal_traces <- function(
  simulation,
  species = c("A1", "A2", "A3", "B", "E"),
  species_dotted = c("H1", "H2", "H3"),
  chart_title = "Ideal Hebbian neuron traces"
) {
  behavior <- if (is.list(simulation) && !is.data.frame(simulation)) {
    if (!is.null(simulation$data)) simulation$data else simulation
  } else {
    simulation
  }

  if (!is.data.frame(behavior)) {
    stop("simulation must be a data frame or a list containing a data frame in $data")
  }

  solid_species <- intersect(species, names(behavior))
  dotted_species <- intersect(species_dotted, names(behavior))

  if (!length(solid_species) && !length(dotted_species)) {
    stop("No requested species were found in the simulation data")
  }

  plot_df <- data.frame(
    time = rep(behavior$time, times = length(solid_species)),
    species = rep(solid_species, each = nrow(behavior)),
    value = unlist(behavior[solid_species], use.names = FALSE)
  )

  dotted_df <- NULL
  if (length(dotted_species)) {
    dotted_df <- data.frame(
      time = rep(behavior$time, times = length(dotted_species)),
      species = rep(dotted_species, each = nrow(behavior)),
      value = unlist(behavior[dotted_species], use.names = FALSE)
    )
  }

  g <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = plot_df,
      ggplot2::aes(x = time, y = value, color = species),
      linewidth = 1.1,
      linetype = "solid"
    ) +
    ggplot2::labs(
      title = chart_title,
      x = "Time",
      y = "Concentration",
      color = "Species"
    ) +
    ggplot2::theme_minimal(base_size = 14)

  if (!is.null(dotted_df) && nrow(dotted_df) > 0) {
    g <- g + ggplot2::geom_line(
      data = dotted_df,
      ggplot2::aes(x = time, y = value, color = species),
      linewidth = 1.1,
      linetype = "dotted"
    )
  }

  print(g)
  invisible(g)
}

# Diffusive noisy input helper (Noisy input model)
# Returns a deterministic interpolation function I(t) built from sampled Gaussian noise.
make_diffusive_noisy_input <- function(timing, I0 = 0, sigma = 0, seed = NULL) {
  if(!is.null(seed)) {
    set.seed(seed)
  }
  n <- length(timing)
  if(n < 2) {
    stop("timing must contain at least 2 points")
  }
  dt <- c(diff(timing), tail(diff(timing), 1))
  noise <- rnorm(n, mean = 0, sd = 1)
  # Diffusive scaling: sigma * sqrt(dt) * xi
  Ivec <- I0 + sigma * sqrt(pmax(dt, 1e-12)) * noise
  approxfun(timing, Ivec, rule = 2)
}

# Add intrinsic-noise forcing compatible with react_stochastic().
# This approximates a diffusion-like perturbation by coupling a birth/death
# noise pool into an input species.
add_intrinsic_noise_input <- function(
  species,
  ci,
  reactions,
  ki,
  target_input,
  noise_name = NULL,
  c_noise = 0,
  k_noise_birth = 0,
  k_noise_death = 0,
  k_noise_couple = 0
) {
  if(is.null(noise_name) || !nzchar(noise_name)) {
    noise_name <- paste0(target_input, '_noise')
  }
  if(noise_name %in% species) {
    stop(sprintf('noise_name already exists in species: %s', noise_name))
  }
  if(!(target_input %in% species)) {
    stop(sprintf('target_input not found in species: %s', target_input))
  }

  species2 <- c(species, noise_name)
  ci2 <- c(ci, as.numeric(c_noise))
  reactions2 <- reactions
  ki2 <- ki

  if(as.numeric(k_noise_birth) > 0) {
    reactions2 <- c(reactions2, paste0('0 -> ', noise_name))
    ki2 <- c(ki2, as.numeric(k_noise_birth))
  }
  if(as.numeric(k_noise_death) > 0) {
    reactions2 <- c(reactions2, paste0(noise_name, ' -> 0'))
    ki2 <- c(ki2, as.numeric(k_noise_death))
  }
  if(as.numeric(k_noise_couple) > 0) {
    reactions2 <- c(reactions2, paste0(noise_name, ' -> ', noise_name, ' + ', target_input))
    ki2 <- c(ki2, as.numeric(k_noise_couple))
  }

  list(
    species = species2,
    ci = ci2,
    reactions = reactions2,
    ki = ki2,
    noise_species = noise_name,
    target_input = target_input
  )
}

# LIF simulator (base ODE model) with optional Julia diffeqr backend.
# Model: dV/dt = (-(V - V_rest) + R * I(t)) / tau_m
# Reset rule: if V >= V_th, register spike and set V <- V_reset.
simulate_lif <- function(
  timing,
  I = 0,
  params = list(
    tau_m = 10,
    R = 1,
    v_rest = 0,
    v_reset = 0,
    v_th = 1,
    v0 = 0
  ),
  solver = c("diffeqr", "desolve"),
  noise_sigma = 0,
  noise_seed = NULL
) {
  solver <- match.arg(solver)
  if(length(timing) < 2) stop("timing must have at least 2 points")
  if(any(diff(timing) <= 0)) stop("timing must be strictly increasing")

  if(is.function(I)) {
    I_base_fun <- I
  } else if(length(I) == 1) {
    I_base_fun <- function(t) I
  } else if(length(I) == length(timing)) {
    I_base_fun <- approxfun(timing, as.numeric(I), rule = 2)
  } else {
    stop("I must be scalar, vector of same length as timing, or function")
  }

  if(noise_sigma > 0) {
    noisy_fun <- make_diffusive_noisy_input(
      timing = timing,
      I0 = 0,
      sigma = noise_sigma,
      seed = noise_seed
    )
    I_fun <- function(t) I_base_fun(t) + noisy_fun(t)
  } else {
    I_fun <- I_base_fun
  }

  tau_m <- params$tau_m
  Rm <- params$R
  v_rest <- params$v_rest
  v_reset <- params$v_reset
  v_th <- params$v_th
  v0 <- params$v0

  n <- length(timing)
  V <- numeric(n)
  V[1] <- v0
  spike_idx <- integer(0)

  # Prefer Julia solver for smooth trajectory, then apply threshold/reset rule discretely.
  if(solver == "diffeqr") {
    if(!requireNamespace("diffeqr", quietly = FALSE) || !"solve_ode" %in% getNamespaceExports("diffeqr")) {
      solver <- "deSolve"
    }
  }

  if(solver == "diffeqr") {
    ode_fun <- function(u, p, t) {
      I_t <- I_fun(t)
      dV <- (-(u[1] - v_rest) + Rm * I_t) / tau_m
      c(dV)
    }
    sol <- diffeqr::solve_ode(
      y = c(V[1]),
      times = timing,
      func = ode_fun,
      method = "Tsit5"
    )
    V_cont <- as.numeric(sol$u)
    V[1] <- V_cont[1]
    for(i in 2:n) {
      if(V_cont[i] >= v_th) {
        spike_idx <- c(spike_idx, i)
        V[i] <- v_reset
      } else {
        V[i] <- V_cont[i]
      }
    }
  } else {
    for(i in seq_len(n - 1)) {
      dt_i <- timing[i + 1] - timing[i]
      I_t <- I_fun(timing[i])
      dV <- (-(V[i] - v_rest) + Rm * I_t) / tau_m
      V[i + 1] <- V[i] + dt_i * dV
      if(V[i + 1] >= v_th) {
        spike_idx <- c(spike_idx, i + 1)
        V[i + 1] <- v_reset
      }
    }
  }

  Ivec <- sapply(timing, I_fun)
  spike_times <- timing[spike_idx]

  return(list(
    time = timing,
    v = V,
    I = Ivec,
    spike_idx = spike_idx,
    spike_times = spike_times,
    params = params,
    noise_sigma = noise_sigma
  ))
}

plot_lif_with_Plot_behavior <- function(
  timing,
  I = 0,
  params = list(tau_m = 10, R = 1, v_rest = 0, v_reset = 0, v_th = 1, v0 = 0),
  solver = "diffeqr",
  noise_sigma = 0,
  noise_seed = NULL,
  circuit = NULL,
  gate_numbers = NULL,
  y_min = NULL,
  y_max = NULL,
  plot_species = c("v", "I", "spike"),
  plot_species_dotted = c("I"),
  chart_title = "LIF neuron"
) {
  sim <- simulate_lif(
    timing = timing,
    I = I,
    params = params,
    solver = solver,
    noise_sigma = noise_sigma,
    noise_seed = noise_seed
  )

  result <- data.frame(
    time = sim$time,
    v = sim$v,
    I = sim$I,
    spike = 0
  )
  if(length(sim$spike_idx) > 0) {
    result$spike[sim$spike_idx] <- 1
  }
  rownames(result) <- as.character(sim$time)

  Plot_behavior(
    result = result,
    circuit = circuit,
    gate_numbers = gate_numbers,
    min = y_min,
    max = y_max,
    plot_species = plot_species,
    plot_species_dotted = plot_species_dotted,
    chart_title = chart_title,
    timing = timing
  )

  invisible(sim)
}

heav <- function(x) {
  as.numeric(x >= 0)
}


morris_lecar <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    I_total <- i + input_func(t)
    
    # --- INTERMEDIATE KINETICS ---
    # Calcium (Instantaneous)
    minf <- 0.5 * (1 + tanh((v - v1) / v2))
    
    # Potassium (Time-dependent)
    winf <- 0.5 * (1 + tanh((v - v3) / v4))
    tauw <- 1 / cosh((v - v3) / (2 * v4))
    
    # --- DIFFERENTIAL EQUATIONS ---
    dv <- (I_total - gca * minf * (v - vca) - gk * w * (v - vk) - gl * (v - vl)) / c
    dw <- phi * (winf - w) / tauw
    
    # --- MAGIC TRICK: RETURN EVERYTHING ---
    # deSolve expects the first element to be the vector of rates of change.circuit <- c()# S: Substrate, A: Activator/Product, A2: Cooperative Dimer Complexcircuit$species <- c("S", "A", "A2")# Initial conditions: High substrate, small primer of A to start the switchcircuit$ci <- c(100, 1, 0)# 1. Activation: S produces A 
    # Any subsequent named elements are saved directly into the output data frame!
    return(list(
      c(dv, dw),           # The required derivatives
      
      I_ext    = I_total,  # Total injected current
      XM_frac  = minf,     # Fraction of open Ca2+ channels
      XMc_frac = 1 - minf, # Fraction of closed Ca2+ channels
      XW_frac  = w,        # Fraction of open K+ channels (just w)
      XWc_frac = 1 - w     # Fraction of closed K+ channels
    ))
  })
}

state_class1 <- c(v = -50.0, w = 0.15)
pars_class1  <- list(
  vk = -84, vl = -60, vca = 120,
  i = 0, gk = 8, gl = 2, gca = 4.4, c = 20,
  v1 = -1.2, v2 = 18, 
  v3 = 2, v4 = 30, phi = 0.04
)

state_type1_tangentBif <- c(v = -50.0, w = 0)
pars_type1_tangentBif  <- list(
  c = 5,
  gk = 8, gl = 2,
  vk = -80, vl = -60, vca = 120,
  v1 = -1.2, v2 = 18, 
  
  i = 0,  gca = 4, 
  v3 = 12, v4 = 17.4, phi = 1/15
)

state_type1_homoclinicBif <- c(v = -50.0, w = 0)
pars_type1_homoclinicBif  <- list(
  c = 5,
  gk = 8, gl = 2,
  vk = -80, vl = -60, vca = 120,
  v1 = -1.2, v2 = 18, 
  
  i = 0,  gca = 4, 
  v3 = 2, v4 = 17.4, phi = 1/15
)


translate_ml_to_crn_params <- function(state, pars, Mtot = 40, Wtot = 40, R_m = 10, v_eval = 0) {
  p <- as.list(pars)
  s <- as.list(state)
  
  # ============================================================
  # 1. Reconstruct ML Gating Rate Functions
  # ============================================================
  
  # Potassium (W) gating rates
  w_inf <- function(v) 0.5 * (1 + tanh((v - p$v3) / p$v4))
  tau_w <- function(v) 1 / cosh((v - p$v3) / (2 * p$v4))
  alpha_w <- function(v) p$phi * w_inf(v) / tau_w(v)
  beta_w  <- function(v) p$phi * (1 - w_inf(v)) / tau_w(v)
  
  # Calcium (M) gating is instantaneous in standard ML. 
  # We use R_m to act as a fast time-constant multiplier to simulate this in the CRN.
  m_inf <- function(v) 0.5 * (1 + tanh((v - p$v1) / p$v2))
  alpha_m <- function(v) R_m * m_inf(v)
  beta_m  <- function(v) R_m * (1 - m_inf(v))
  
  # ============================================================
  # 2. Linearize Non-Linear Rates for the CRN
  # ============================================================
  # We use a numerical derivative around 'v_eval' to fit the CRN's linear structure
  dv <- 0.001
  
  # Spontaneous rates evaluate exactly at our evaluation voltage (usually 0mV)]
  k_w_spont_open  <- alpha_w(v_eval)
  k_w_spont_close <- beta_w(v_eval)
  k_m_spont_open  <- alpha_m(v_eval)
  k_m_spont_close <- beta_m(v_eval)
  
  # Voltage-dependent rates use the slope. 
  # Vm activates when V < 0, so we step backwards for the closing rate.
  k_w_open  <- max(0, (alpha_w(v_eval + dv) - alpha_w(v_eval)) / dv)
  k_w_close <- max(0, (beta_w(v_eval - dv) - beta_w(v_eval)) / dv) 
  
  k_m_open  <- max(0, (alpha_m(v_eval + dv) - alpha_m(v_eval)) / dv)
  k_m_close <- max(0, (beta_m(v_eval - dv) - beta_m(v_eval)) / dv)
  
  # ============================================================
  # 3. Direct Variable Mappings
  # ============================================================
  
  # Current injection mapping to Rails
  Ip0 <- if(p$i >= 0) p$i else 0
  Im0 <- if(p$i <  0) abs(p$i) else 0
  
  # Return the named list expected by create_morris_lecar_crn
  return(list(
    C   = p$c,
    gCa = p$gca,
    gK  = p$gk,
    gL  = p$gl,
    VCa = p$vca,
    VK  = p$vk,
    VL  = p$vl,
    
    Mtot = Mtot,
   
    k_m_open  = k_m_open,
    k_m_close = k_m_close,
    k_w_open  = k_w_open,
    k_w_close = k_w_close,
    
    k_m_spont_open  = k_m_spont_open,
    k_m_spont_close = k_m_spont_close,
    k_w_spont_open  = k_w_spont_open,
    k_w_spont_close = k_w_spont_close,
    
    k_ann = 1, 
    
    V0 = s$v,
    M0 = round(m_inf(s$v) * Mtot),
    W0 = round(s$w * Wtot),
    
    Ip0 = Ip0,
    Im0 = Im0
  ))
}


build_tuned_crn <- function(opt_par) {
  
  # Call your base CRN builder, injecting the optimized values 
  # and hardcoding the fixed physical constants.
  tuned_ml_circuit <- create_morris_lecar_crn(
    
    # ==========================================
    # 1. OPTIMIZED PARAMETERS (from tuned_results$par)
    # ==========================================
    rate = opt_par[["rate"]],
    C    = opt_par[["C"]],
    
    gCa = opt_par[["gCa"]],
    gK  = opt_par[["gK"]],
    gL  = opt_par[["gL"]],
    
    k_m_open  = opt_par[["k_m_open"]],
    k_m_close = opt_par[["k_m_close"]],
    k_w_open  = opt_par[["k_w_open"]],
    k_w_close = opt_par[["k_w_close"]],
    
    k_m_spont_open  = opt_par[["k_m_spont_open"]],
    k_m_spont_close = opt_par[["k_m_spont_close"]],
    k_w_spont_open  = opt_par[["k_w_spont_open"]],
    k_w_spont_close = opt_par[["k_w_spont_close"]],
    
    k_ann = opt_par[["k_ann"]],
    
    # ==========================================
    # 2. FIXED PHYSICAL CONSTANTS
    # ==========================================
    VCa  = 120, 
    VK   = -84, 
    VL   = -60,
    Mtot = 40, 
    Wtot = 40,
    
    # ==========================================
    # 3. INITIAL CONDITIONS
    # ==========================================
    V0   = -60,
    M0   = 0, 
    W0   = 20, # ceiling(Wtot/2)
    Ip0  = 0, 
    Im0  = 0
  )
  
  tuned_ml_circuit$t <- timing
  
  return(tuned_ml_circuit)
}
  # ml <- do.call(create_morris_lecar_crn, crn_params)
  # ml <- create_morris_lecar_crn()
# final_crn_circuit <- build_tuned_crn(opt_par = tuned_results$par, base_state = mlstate)