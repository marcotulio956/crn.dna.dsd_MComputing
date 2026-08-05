jn <- function(...) { paste(..., sep = '') }

# ============================================================
# STOCHASTIC CRN SPIKING NEURON
# ------------------------------------------------------------
# A more biologically-inspired neuron model implemented as a
# Chemical Reaction Network 
# Features:
# - weighted synaptic integration
# - nonlinear firing
# - membrane leakage
# - refractory dynamics
# - Hebbian plasticity
# - inhibitory reset
# - stochastic-friendly reactions
# ============================================================

Make_Spiking_CRN_Neuron <- function(
    name,
    rate,
    cA1 = 20,
    cA2 = 20,
    cA3 = 30,
    cW1 = 1,
    cW2 = 1,
    cW3 = 1,
    cV = 0,      # membrane potential
    cS = 0,      # spike/output
    cR = 0,      # refractory state
    kIn1 = 1,
    kIn2 = 1,
    kIn3 = 1,
    kFire = 0.5,
    kLeakV = 0.1,
    kLeakS = 0.05,
    kRefCreate = 0.3,
    kRefDecay  = 0.05,
    kReset     = 1,
    kLearn1 = 0.01,
    kLearn2 = 0.01,
    kLearn3 = 0.01,
    kWDecay1 = 0.001,
    kWDecay2 = 0.001,
    kWDecay3 = 0.001
)
{
  species <- list(
    A1 = jn(name, "_A1"),
    A2 = jn(name, "_A2"),
    A3 = jn(name, "_A3"),
    W1 = jn(name, "_W1"),
    W2 = jn(name, "_W2"),
    W3 = jn(name, "_W3"),
    V = jn(name, "_V"),
    S = jn(name, "_S"),
    R = jn(name, "_R"),
    waste = jn(name, "_waste")
  )
  ci <- c(
    # Inputs
    cA1,
    cA2,
    cA3,
    # Weights
    cW1,
    cW2,
    cW3,
    # Membrane state
    cV,
    # Output spike
    cS,
    # Refractory state
    cR,
    # Waste
    0
  )
  r1 <- jn(
    species$A1, "+", species$W1,
    "->",
    species$W1, "+", species$V, "+", species$A1
  )
  r2 <- jn(
    species$A2, "+", species$W2,
    "->",
    species$W2, "+", species$V, "+", species$A2
  )
  r3 <- jn(
    species$A3, "+", species$W3,
    "->",
    species$W3, "+", species$V, "+", species$A3
  )
  r4 <- jn(
    species$V,
    "->",
    species$waste
  )
  r5 <- jn(
    "2", species$V,
    "->",
    "2", species$V, "+", species$S
  )
  r6 <- jn(
    species$S,
    "->",
    species$waste
  )
  r7 <- jn(
    species$S,
    "->",
    species$S, "+", species$R
  )
  r8 <- jn(
    species$R, "+", species$V,
    "->",
    species$R
  )
  r9 <- jn(
    species$R,
    "->",
    species$waste
  )
  r10 <- jn(
    species$A1, "+", species$S,
    "->",
    species$S, "+", species$W1
  )
  r11 <- jn(
    species$A2, "+", species$S,
    "->",
    species$S, "+", species$W2
  )
  r12 <- jn(
    species$A3, "+", species$S,
    "->",
    species$S, "+", species$W3
  )
  r13 <- jn(
    species$W1,
    "->",
    species$waste
  )
  r14 <- jn(
    species$W2,
    "->",
    species$waste
  )
  r15 <- jn(
    species$W3,
    "->",
    species$waste
  )
  reactions <- c(
    # integration
    r1, r2, r3,
    # membrane leak
    r4,
    # firing
    r5,
    # spike decay
    r6,
    # refractory
    r7, r8, r9,
    # learning
    r10, r11, r12,
    # weight decay
    r13, r14, r15
  )
  ki <- c(
    kIn1,
    kIn2,
    kIn3,
    kLeakV,
    kFire,
    kLeakS,
    kRefCreate,
    kReset,
    kRefDecay,
    kLearn1,
    kLearn2,
    kLearn3,
    kWDecay1,
    kWDecay2,
    kWDecay3
  )
  neuron <- list(
    name = name,
    species = species,
    reactions = reactions,
    ci = ci,
    ki = rate*ki
  )
  return(neuron)
}

ml_preset_params <- function(preset = "tonic_spiking") {
  # ----------------------------------------------------------
  # Reference ML constants (shared across all presets)
  # phi_m=0.4, Va=-1.2, Vb=18, phi_n=0.4, Vc=2, Vd=30
  # ----------------------------------------------------------
  phi_m <- 0.4;  Va <- -1.2;  Vb <- 18
  phi_n <- 0.4;  Vc <-  2.0;  Vd <- 30
  
  minf  <- function(V) 0.5 * (1 + tanh((V - Va) / Vb))
  ninf  <- function(V) 0.5 * (1 + tanh((V - Vc) / Vd))
  tau_m <- function(V) 1 / (phi_m * cosh((V - Va) / (2 * Vb)))
  tau_n <- function(V) 1 / (phi_n * cosh((V - Vc) / (2 * Vd)))
  
  alpha_m <- function(V) minf(V)  / tau_m(V)
  beta_m  <- function(V) (1 - minf(V))  / tau_m(V)
  alpha_n <- function(V) ninf(V)  / tau_n(V)
  beta_n  <- function(V) (1 - ninf(V))  / tau_n(V)
  
  # ----------------------------------------------------------
  # Regime definitions:
  #   V_gate  = representative voltage for gating calibration
  #             (approx midpoint between rest and threshold)
  #   Iapp    = applied current  (molecules/ms in CRN rail units)
  #   V0      = initial voltage
  #   W0_frac = initial fraction of open K channels
  # ----------------------------------------------------------
 regimes <- list(
    low_drive_quiescent = list(
      Iapp = 60, V_gate = -50, V0 = -55,
      W0_frac = 0.15, Mtot = 40, Wtot = 40
    ),
    near_threshold_irregular = list(
      Iapp = 80, V_gate = -35, V0 = -50,
      W0_frac = 0.25, Mtot = 40, Wtot = 40
    ),
    tonic_spiking = list(
      Iapp = 120, V_gate = -20, V0 = -50,
      W0_frac = 0.35, Mtot = 40, Wtot = 40
    ),
    high_drive_fast_spiking = list(
      Iapp = 130, V_gate = -10, V0 = -50,
      W0_frac = 0.40, Mtot = 40, Wtot = 40
    ),
    channel_noise_dominant = list(
      Iapp = 100, V_gate = -20, V0 = -50,
      W0_frac = 0.35, Mtot = 20, Wtot = 20
    )
  )
  
  if (!preset %in% names(regimes))
    stop(sprintf(
      "Unknown preset '%s'. Choose from: %s",
      preset, paste(names(regimes), collapse = ", ")
    ))
  
  p <- regimes[[preset]]
  Vg <- p$V_gate
  
  # ----------------------------------------------------------
  # Calibrate voltage-dependent gating rates at V_gate.
  #
  # In the reference model the per-channel propensity to open is
  #   alpha_m(V) [1/ms]
  # and to close is
  #   beta_m(V)  [1/ms].
  #
  # In the CRN the propensity for one closed channel to open is
  #   k_m_open * Vp   (bimolecular, linear in rail count)
  #
  # At the calibration voltage, Vp ≈ max(V_gate, 0) if V_gate>0,
  # or Vm ≈ |V_gate| if V_gate<0.  We want:
  #   k_m_open  * |Vg|  ≈  alpha_m(Vg)
  #   k_m_close * |Vg|  ≈  beta_m(Vg)   (Vm drives closing)
  #
  # For negative Vg the opening is spontaneous-dominated, so we
  # set k_m_open from alpha_m(Vg) directly (unimolecular fallback).
  # ----------------------------------------------------------
  Vabs <- max(abs(Vg), 5)   # floor to avoid divide-by-zero
  
  k_m_open  <- alpha_m(Vg) / Vabs
  k_m_close <- beta_m(Vg)  / Vabs
  k_w_open  <- alpha_n(Vg) / Vabs
  k_w_close <- beta_n(Vg)  / Vabs
  
  # Small spontaneous floor (prevents absorbing boundaries)
  spont_floor <- 1e-4
  
  list(
    Iapp          = p$Iapp,
    Mtot          = p$Mtot,
    Wtot          = p$Wtot,
    V0            = p$V0,
    M0            = 0L,
    W0            = as.integer(ceiling(p$Wtot * p$W0_frac)),
    k_m_open      = k_m_open,
    k_m_close     = k_m_close,
    k_w_open      = k_w_open,
    k_w_close     = k_w_close,
    k_m_spont_open  = max(spont_floor, alpha_m(Vg) * 0.01),
    k_m_spont_close = max(spont_floor, beta_m(Vg)  * 0.01),
    k_w_spont_open  = max(spont_floor, alpha_n(Vg) * 0.01),
    k_w_spont_close = max(spont_floor, beta_n(Vg)  * 0.01)
  )
}

# low_drive_quiescent near_threshold_irregular tonic_spiking high_drive_fast_spiking channel_noise_dominant

create_morris_lecar_crn_massaction_preset <- function(
    preset = NULL,          # if set, loads regime defaults (see ml_preset_params)
    rate   = 1,
    # ── Membrane parameters ───────────────────────────────────
    C   = 20,
    gCa = 4.4,
    gK  = 8,
    gL  = 2,
    # ── Reversal potentials ───────────────────────────────────
    VCa = 120,
    VK  = -84,
    VL  = -60,
    # ── Channel populations ───────────────────────────────────
    Mtot = 40,
    Wtot = 40,
    # ── Voltage-dependent gating rates ───────────────────────
    # (calibrated per regime when preset is used)
    k_m_open  = 0.08,
    k_m_close = 0.04,
    k_w_open  = 0.01,
    k_w_close = 0.005,
    # ── Basal spontaneous gating ─────────────────────────────
    k_m_spont_open  = 1e-4,
    k_m_spont_close = 1e-4,
    k_w_spont_open  = 1e-4,
    k_w_spont_close = 1e-4,
    # ── Rail annihilation ────────────────────────────────────
    k_ann = 1,
    # ── Applied current (in rail molecules / ms) ─────────────
    Iapp = 100,
    # ── Initial conditions ───────────────────────────────────
    V0 = -50,
    M0 = 0,
    W0 = NULL,              # defaults to ceiling(Wtot/2)
    # ── External current rail initial populations ─────────────
    Ip0 = 0,
    Im0 = 0
) {
  
  # ── Load preset if requested ─────────────────────────────
  if (!is.null(preset)) {
    p <- ml_preset_params(preset)
    Iapp            <- p$Iapp
    Mtot            <- p$Mtot
    Wtot            <- p$Wtot
    V0              <- p$V0
    M0              <- p$M0
    W0              <- p$W0
    k_m_open        <- p$k_m_open
    k_m_close       <- p$k_m_close
    k_w_open        <- p$k_w_open
    k_w_close       <- p$k_w_close
    k_m_spont_open  <- p$k_m_spont_open
    k_m_spont_close <- p$k_m_spont_close
    k_w_spont_open  <- p$k_w_spont_open
    k_w_spont_close <- p$k_w_spont_close
  }
  
  if (is.null(W0)) W0 <- ceiling(Wtot / 2)

  if (Iapp >= 0) {
    Ip0 <- Iapp
    Im0 <- 0
  } else {
    Ip0 <- 0
    Im0 <- abs(Iapp)
  }
  
  # ── Dual-rail voltage initialization ─────────────────────
  if (V0 >= 0) {
    Vp0 <- V0; Vm0 <- 0
  } else {
    Vp0 <- 0;  Vm0 <- abs(V0)
  }
  
  ml <- list()
  
  # ── Species ───────────────────────────────────────────────
  ml$species <- c(
    "Ip",   # positive current rail
    "Im",   # negative current rail
    "Vp",   # positive voltage rail
    "Vm",   # negative voltage rail
    "XM",   # open  Ca2+ channels
    "XMc",  # closed Ca2+ channels
    "XW",   # open  K+  channels
    "XWc"   # closed K+  channels
  )
  
  # ── Initial conditions ────────────────────────────────────
  ml$ci <- c(Ip0, Im0, Vp0, Vm0, M0, Mtot - M0, W0, Wtot - W0)
  
  reactions <- list()
  ki        <- numeric(0)
  
  add_rxn <- function(rxn_str, rate_val) {
    reactions[[length(reactions) + 1]] <<- rxn_str
    ki <<- c(ki, rate_val)
  }
  
  # ── 1. External current injection ────────────────────────
  # Ip/Im are catalytic: each molecule produces one Vp/Vm per ms
  # so the net injection rate = Iapp / C  (mV/ms per molecule).
  add_rxn("Ip -> Ip + Vp", 1 / C)
  add_rxn("Im -> Im + Vm", 1 / C)
  
  # ── 2. Rail annihilation ──────────────────────────────────
  add_rxn("Vp + Vm -> 0", k_ann)
  
  # ── 3. Leak current  [ I_L = gL*(V - VL) ] ───────────────
  # Leak drives V toward VL.
  # Production term: pushes rail toward VL sign.
  if (VL >= 0) {
    add_rxn("0 -> Vp", gL * abs(VL) / C)   # source at +VL
  } else {
    add_rxn("0 -> Vm", gL * abs(VL) / C)   # source at -|VL|
  }
  # Decay terms: both rails drain (net effect: V -> VL)
  add_rxn("Vp -> 0", gL / C)
  add_rxn("Vm -> 0", gL / C)
  
  # ── 4. Calcium channel gating ─────────────────────────────
  # Opening driven by depolarisation (Vp) — bimolecular
  add_rxn("XMc + Vp -> XM + Vp", k_m_open)
  # Closing driven by hyperpolarisation (Vm) — bimolecular
  add_rxn("XM  + Vm -> XMc + Vm", k_m_close)
  # Spontaneous (voltage-independent) floor
  add_rxn("XMc -> XM",  k_m_spont_open)
  add_rxn("XM  -> XMc", k_m_spont_close)
  
  # ── 5. Potassium channel gating ───────────────────────────
  add_rxn("XWc + Vp -> XW + Vp", k_w_open)
  add_rxn("XW  + Vm -> XWc + Vm", k_w_close)
  add_rxn("XWc -> XW",  k_w_spont_open)
  add_rxn("XW  -> XWc", k_w_spont_close)
  
  # ── 6. Calcium current  [ I_Ca = gCa*(M/Mtot)*(V - VCa) ] ─
  # Production: open Ca channels push V toward VCa
  if (VCa >= 0) {
    add_rxn("XM -> XM + Vp", gCa * abs(VCa) / (C * Mtot))
  } else {
    add_rxn("XM -> XM + Vm", gCa * abs(VCa) / (C * Mtot))
  }
  # Removal: open Ca channels drain the rail (current flows out when V > VCa)
  add_rxn("Vp + XM -> XM", gCa / (C * Mtot))
  add_rxn("Vm + XM -> XM", gCa / (C * Mtot))
  
  # ── 7. Potassium current  [ I_K = gK*(N/Ntot)*(V - VK) ] ──
  if (VK >= 0) {
    add_rxn("XW -> XW + Vp", gK * abs(VK) / (C * Wtot))
  } else {
    add_rxn("XW -> XW + Vm", gK * abs(VK) / (C * Wtot))
  }
  add_rxn("Vp + XW -> XW", gK / (C * Wtot))
  add_rxn("Vm + XW -> XW", gK / (C * Wtot))
  
  ml$reactions <- reactions
  ml$ki        <- ki * rate
  
  # ── Attach metadata for inspection ───────────────────────
  ml$params <- list(
    Iapp=Iapp,C = C, gCa = gCa, gK = gK, gL = gL,
    VCa = VCa, VK = VK, VL = VL,
    Mtot = Mtot, Wtot = Wtot,
    Iapp = Iapp, V0 = V0, M0 = M0, W0 = W0,
    k_m_open = k_m_open, k_m_close = k_m_close,
    k_w_open = k_w_open, k_w_close = k_w_close,
    k_m_spont_open  = k_m_spont_open,
    k_m_spont_close = k_m_spont_close,
    k_w_spont_open  = k_w_spont_open,
    k_w_spont_close = k_w_spont_close,
    k_ann = k_ann, rate = rate
  )
  
  # ── Named reaction/rate table for diagnostics ─────────────
  ml$rate_table <- data.frame(
    reaction = unlist(ml$reactions),
    rate     = ml$ki,
    stringsAsFactors = FALSE
  )
  
  return(ml)
}


create_ml_crn_varyingRates <- function(
    rate   = 1,
    # ── Membrane parameters ───────────────────────────────────
    # C   = 20, gCa = 4.4, gK  = 8, # gL = 2,
    C =    100, gCa = 4.4, gK  = 8,   gL = 2,
    # ── Reversal potentials ───────────────────────────────────
    # VCa = 120, VK = -84, VL = -60,
    VCa =   120, VK = -84, VL = -60,
    # ── Gating Parameters ─────────────────────────────────────
    # v1 = -1.2, v2 = 18, v3 = 2, v4 = 30,
    v1 =   -1.2, v2 = 18, v3 = 2, v4 = 30,
    # phi_w = 0.04, # Potassium recovery rate
    phi_w =   0.04,
    # ── 3D Calcium Kinetics Toggle ────────────────────────────
    use_3d_m = TRUE, # FALSE = 2D instantaneous M, TRUE = 3D delayed M
    # phi_m = 0.4,   # Calcium recovery rate (only active if use_3d_m = TRUE)
    phi_m =   0.4,
    # ── Channel populations ───────────────────────────────────
    Mtot = 40,
    Wtot = 40,
    # ── Rail annihilation ─────────────────────────────────────
    k_ann = 0.01,
    # ── Applied current (in rail molecules / ms) ──────────────
    Iapp = 100,
    # ── Initial conditions ────────────────────────────────────
    V0 = -50,
    M0 = 0,
    W0 = NULL,
    volume
) {
  if (is.null(W0)) W0 <- ceiling(Wtot / 2)
  # ── Derive external current rail populations ──────────────
  if (Iapp >= 0) {
    Ip0 <- Iapp; Im0 <- 0
  } else {
    Ip0 <- 0;    Im0 <- abs(Iapp)
  }
  # ── Dual-rail voltage initialization ──────────────────────
  if (V0 >= 0) {
    Vp0 <- V0; Vm0 <- 0
  } else {
    Vp0 <- 0;  Vm0 <- abs(V0)
  }
  ml <- list()
  # ── Species ───────────────────────────────────────────────
  ml$species <- c(
    "Ip",   "Im",   # current rails
    "Vp",   "Vm",   # voltage rails
    "XM",   "XMc",  # Ca2+ channels
    "XW",   "XWc"   # K+ channels
  )
  ml$ci <- c(Ip0, Im0, Vp0, Vm0, M0, Mtot - M0, W0, Wtot - W0)
  reactions <- list()
  ki        <- list()
  # Helper to safely bind reactions to their rates
  add_rxn <- function(rxn_str, rate_val) {
    reactions[[length(reactions) + 1]] <<- rxn_str
    ki[[length(ki) + 1]] <<- rate_val
  }
  # ──────────────────────────────────────────────────────────
  # ── KINETIC RATE FUNCTIONS FOR REACT2 ─────────────────────
  # ──────────────────────────────────────────────────────────
  # Helper to extract voltage
  get_V <- function(y) y["Vp"] - y["Vm"]
  # Potassium (W) Rates - Always 3D
  k_w_open_dyn <- function(t, y, species) {
    v <- get_V(y)
    winf <- 0.5 * (1 + tanh((v - v3) / v4))
    tauw <- 1 / cosh((v - v3) / (2 * v4))
    return(phi_w * winf / tauw) # alpha_w
  }
  k_w_close_dyn <- function(t, y, species) {
    v <- get_V(y)
    winf <- 0.5 * (1 + tanh((v - v3) / v4))
    tauw <- 1 / cosh((v - v3) / (2 * v4))
    return(phi_w * (1 - winf) / tauw) # beta_w
  }
  # Calcium (M) Rates - Selectable 2D/3D
  k_m_open_dyn <- function(t, y, species) {
    v <- get_V(y)
    minf <- 0.5 * (1 + tanh((v - v1) / v2))
    if (use_3d_m) {
      tau_m <- 1 / (phi_m * cosh((v - v1) / (2 * v2)))
    } else {
      tau_m <- 0.001 # Instantaneous 2D approximation
    }
    return(minf / tau_m) # alpha_m
  }
  k_m_close_dyn <- function(t, y, species) {
    v <- get_V(y)
    minf <- 0.5 * (1 + tanh((v - v1) / v2))
    if (use_3d_m) {
      tau_m <- 1 / (phi_m * cosh((v - v1) / (2 * v2)))
    } else {
      tau_m <- 0.001 # Instantaneous 2D approximation
    }
    return((1 - minf) / tau_m) # beta_m
  }
  # current
  add_rxn("Ip -> Ip + Vp", 1 / C)
  add_rxn("Im -> Im + Vm", 1 / C)
  add_rxn("Vp + Vm -> 0", k_ann)
  # [ I_L = gL*(V - VL) ]
  if (VL >= 0) {
    add_rxn("0 -> Vp", gL * abs(VL) / C)   
  } else {
    add_rxn("0 -> Vm", gL * abs(VL) / C)   
  }
  add_rxn("Vp -> 0", gL / C)
  add_rxn("Vm -> 0", gL / C)
  # Calcium channel gating
  add_rxn("XMc -> XM", k_m_open_dyn)
  add_rxn("XM -> XMc", k_m_close_dyn)
  # Potassium channel gating
  add_rxn("XWc -> XW", k_w_open_dyn)
  add_rxn("XW -> XWc", k_w_close_dyn)
  # Calcium current [ I_Ca = gCa*(M/Mtot)*(V - VCa) ]
  if (VCa >= 0) {
    add_rxn("XM -> XM + Vp", gCa * abs(VCa) / (C * Mtot))
  } else {
    add_rxn("XM -> XM + Vm", gCa * abs(VCa) / (C * Mtot))
  }
  add_rxn("Vp + XM -> XM", gCa / (C * Mtot))
  add_rxn("Vm + XM -> XM", gCa / (C * Mtot))
  # Potassium current [ I_K = gK*(W/Wtot)*(V - VK) ] 
  if (VK >= 0) {
    add_rxn("XW -> XW + Vp", gK * abs(VK) / (C * Wtot))
  } else {
    add_rxn("XW -> XW + Vm", gK * abs(VK) / (C * Wtot))
  }
  add_rxn("Vp + XW -> XW", gK / (C * Wtot))
  add_rxn("Vm + XW -> XW", gK / (C * Wtot))
  ml$reactions <- reactions
  # Apply global rate scaling to scalar values, leave functions intact
  ml$ki <- lapply(ki, function(k) if(is.numeric(k)) k * rate else k)
  ml$params <- list(
    Iapp = Iapp, Mtot = Mtot, Wtot = Wtot, 
    use_3d_m = use_3d_m, phi_m = phi_m, phi_w = phi_w
  )
  return(ml)
}
