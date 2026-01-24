jn <- function(...) { paste(..., sep = '') }

# Make_Comparator
# name: base name for gate
# nameInput_p/nameInput_n: differential input species (v_p, v_n)
# nameThreshold: optional species name for threshold pool; if NA, we create jn(name, '_thresh')
# nameOutput: comparator control species produced when input > threshold
# c_threshold: initial concentration for the threshold pool (higher => harder to trigger)
# rate: base rate scalar (used for ki entries)

  # Reactions
  # 1) catalytic production of rectified pools from rails
  #    input_p -> input_p + x_pos
  #    input_n -> input_n + x_neg
  # 2) annihilation (rectify)
  #    x_pos + x_neg -> 0
  # 3) threshold absorption (finite capacity): x_pos + thresh -> thresh_bound
  #    thresh_bound -> 0   (slow decay to avoid permanent binding)
  # 4) amplification path: x_pos -> x_pos + ctrl_seed  (seeded production)
  # 5) autocatalytic amplification of seed: ctrl_seed + x_pos -> 2 ctrl_seed  (fast amplify)
  # 6) seed produces control species: ctrl_seed -> ctrl_seed + control
  # 7) control decay: control -> 0
  # 8) optional decay of x_pos,x_neg to avoid runaway: x_pos -> 0 ; x_neg -> 0

# --------------------------
# Make_Comparator (reworked)
# signature: (name, nameInput_p, nameInput_n, nameOutput, c_threshold, rate)
# species order in ci: input_p, input_n, thresh, thresh_bound, x_pos, x_neg, ctrl_seed, control
# --------------------------
Make_Comparator <- function(name,
                            nameInput_p,
                            nameInput_n,
                            nameOutput,
                            c_threshold = 10.0,
                            rate = 1.0) {

  # species (in the expected canonical order)
  species <- list(
    input_p     = nameInput_p,
    input_n     = nameInput_n,
    thresh      = jn(name, '_thresh'),
    thresh_bound= jn(name, '_thresh_b'),
    x_pos       = jn(name, '_xpos'),
    x_neg       = jn(name, '_xneg'),
    ctrl_seed   = jn(name, '_seed'),
    control     = nameOutput
  )

  # reactions (strings) - keep same logical order as ki below
  reactions <- c(
    # catalytic production from rails
    jn(species$input_p, " -> ", species$input_p, " + ", species$x_pos),
    jn(species$input_n, " -> ", species$input_n, " + ", species$x_neg),
    # annihilation (rectify)
    jn(species$x_pos, " + ", species$x_neg, " -> 0"),
    # threshold absorption (finite capacity)
    jn(species$x_pos, " + ", species$thresh, " -> ", species$thresh_bound),
    jn(species$thresh_bound, " -> 0"),
    # weak seed production and autocatalysis
    jn(species$x_pos, " -> ", species$x_pos, " + ", species$ctrl_seed),
    jn(species$ctrl_seed, " + ", species$x_pos, " -> 2 ", species$ctrl_seed),
    # seed produces control (driver of a short pulse)
    jn(species$ctrl_seed, " -> ", species$ctrl_seed, " + ", species$control),
    # control decay (makes pulses short)
    jn(species$control, " -> 0"),
    # leak/decay of rectified pools to avoid runaway
    jn(species$x_pos, " -> 0"),
    jn(species$x_neg, " -> 0")
  )

  # ci must be numeric and match species order exactly
  # order: input_p (usually 0), input_n (0), thresh (numeric capacity), thresh_bound (0), x_pos(0), x_neg(0), ctrl_seed(0), control(0)
  ci <- c(
    0.0,                # input_p initial concentration (catalytic source should be driven externally)
    0.0,                # input_n
    as.numeric(c_threshold), # thresh capacity (numeric)
    0.0,                # thresh_bound
    0.0,                # x_pos
    0.0,                # x_neg
    0.0,                # ctrl_seed
    0.0                 # control (output)
  )

  # ki: one rate per reaction — tuneable
  ki <- c(
    rate * 1.0,   # input_p -> +x_pos
    rate * 1.0,   # input_n -> +x_neg
    rate * 5.0,   # xpos + xneg -> 0
    rate * 2.0,   # xpos + thresh -> thresh_bound
    rate * 0.1,   # thresh_bound -> 0
    rate * 1.0,   # xpos -> xpos + seed
    rate * 5.0,   # seed + xpos -> 2 seed
    rate * 2.0,   # seed -> seed + control
    rate * 10.0,  # control -> 0
    rate * 0.5,   # xpos -> 0
    rate * 0.5    # xneg -> 0
  )

  gate <- list(
    name = name,
    species = species,
    reactions = reactions,
    ci = ci,
    ki = ki
  )

   # validate_gate_gate(gate)
  return(gate)
}

# -------------------------
# Make_PulseAdder
# Simple pulse-to-step converter:
# - trigger species + const_d -> trigger + add_token   (consumes const_d gradually while trigger present)
# - add_token -> add_token + target_pos   (each token produces target increment)
# - add_token -> 0  (decay)
# If target_neg provided, the gate will add to target_neg instead (useful for negative increments).
# -------------------------
# name: base name
# trigger: species that triggers the conversion (e.g. comparator output)
# nameConstD: name of constant resource species holding amount 'd' (if NULL will be created)
# nameTargetPos: species to increment (u_p)
# nameTargetNeg: optional negative rail species if you want to subtract instead
# c_amount: initial concentration of the const pool (d)
# rate: base rate

# --------------------------
# Make_PulseAdder (reworked)
# signature: (name, trigger, nameConstD, nameTargetPos, nameTargetNeg=NULL, c_amount, rate)
# species order in ci: trigger, const_d, add_token
# Note: target species (nameTargetPos / nameTargetNeg) are external and should NOT be included in gate$ci.
# --------------------------
Make_PulseAdder <- function(name,
                            trigger,
                            nameConstD = NULL,
                            nameTargetPos,
                            nameTargetNeg = NULL,
                            c_amount = 10.0,
                            rate = 1.0) {

  if (is.null(nameConstD) || nchar(nameConstD) == 0) {
    nameConstD <- jn(name, '_const_d')
  }

  # species owned by the gate (in this order)
  # We purposely do NOT include external target species in the owned list for ci (they are provided by circuit)
  species <- list(
    trigger   = jn(name, trigger),
    const_d   = jn(name, nameConstD),
    add_token = jn(name, '_addtok')
  )

  # Reactions
  reactions <- c(
    # while trigger present, convert const_d -> add_token
    jn(species$trigger, " + ", species$const_d, " -> ", species$trigger, " + ", species$add_token),
    # each token produces a bit of target (target is external - we produce it by using target name in the reaction)
    jn(species$add_token, " -> ", species$add_token, " + ", nameTargetPos),
    # token decay
    jn(species$add_token, " -> 0")
  )

  # # If negative rail target is provided, add reaction to also produce it
  # if (!is.null(nameTargetNeg) && nchar(nameTargetNeg) > 0) {
  #   reactions <- c(reactions, jn(species$add_token, " -> ", species$add_token, " + ", nameTargetNeg))
  # }

  # ci: one numeric per owned species (trigger typically 0, const_d is the resource amount, add_token 0)
  ci <- c(
    0.0,                     # trigger (driven externally)
    as.numeric(c_amount),    # const_d (initial resource amount to be consumed)
    0.0                      # add_token
  )

  # ki: one per reaction
  ki <- c(
    rate * 5.0,  # conversion while trigger present
    rate * 2.0,  # add_token -> add target
    rate * 1.0   # add_token decay
  )
  if (!is.null(nameTargetNeg) && nchar(nameTargetNeg) > 0) {
    ki <- c(ki, rate * 2.0)
  }

  gate <- list(
    name = name,
    species = species,
    reactions = reactions,
    ci = ci,
    ki = ki
  )

   # validate_gate_gate(gate)
  return(gate)
}

# ----------------------------
# Make_Izhikevich_Component (factory for species mapping)
# ----------------------------
Make_Izhikevich_Component <- function(a = 0.02, b = 0.2, c = -65, d = 8, v_thresh = 30) {
  comp <- list()
  comp$name <- jn('izh')
  comp$ic <- list(a = a, b = b, c = c, d = d, v_thresh = v_thresh)
  # input current differential pair (attach driver to these)
  comp$il <- list(I_p = jn(comp$name, '_I_p'), I_n = jn(comp$name, '_I_n'))
  # exposed internal rails for v and u (differential)
  comp$ol <- list(
    v_p = jn(comp$name, '_v_p'),
    v_n = jn(comp$name, '_v_n'),
    u_p = jn(comp$name, '_u_p'),
    u_n = jn(comp$name, '_u_n'),
    comp_out = jn(comp$name, '_comp'),
    spike = jn(comp$name, '_spk')
  )
  return(comp)
}

# ----------------------------
# Make_Circuit_Izhikevich_Full
# Builds the full gate list for the Izhikevich neuron using:
#  - Make_Mul2In_Wang
#  - Make_Add3In
#  - Make_Integrator_OishiYordanov
#  - Make_Comparator (user provided)
#  - Make_PulseAdder (user provided)
# Also creates light constant gates for const_140, const_c, const_0, const_d
# p: named list of rate scalars (optional)
# ----------------------------
Make_Circuit_Izhikevich_Full <- function(name, species_input, species_output, ic, p = NULL) {
  # default rate parameters (tunable)
  default_p <- list(
    rate_base = 1,
    rate_mul_sq = 1,
    rate_mul_lin = 1,
    rate_add = 1,
    rate_int_v = 1,
    rate_int_u = 1,
    rate_mul_b = 1,
    rate_a_scale = 1,
    mux_rate = 1,
    comp_rate = 1,
    const_gate_rate = 0.0
  )
  if (is.null(p)) p <- default_p
  for (nm in names(default_p)) if (is.null(p[[nm]])) p[[nm]] <- default_p[[nm]]

  gates <- list()
  # species rails/names
  v_p <- species_output$v_p; v_n <- species_output$v_n
  u_p <- species_output$u_p; u_n <- species_output$u_n
  I_p <- species_input$I_p; I_n <- species_input$I_n
  comp_out <- species_output$comp_out
  spike <- species_output$spike

  # constant species names (we will expose initial concentrations via ci)
  const_0   <- jn(name, '_const0')
  const_140 <- jn(name, '_const140')
  const_c   <- jn(name, '_const_c')  # reset voltage 'c'
  const_d   <- jn(name, '_const_d')  # pulse-adder resource (amount d)

  # # --- constant gates: provide species names and ci values so caller can include them
  # g_const0 <- list(name = jn(name, '_const0_gate'),
  #                  species = list(const0 = const_0),
  #                  reactions = c(),
  #                  ci = c(0),
  #                  ki = c(p$const_gate_rate))
  # gates[[length(gates)+1]] <- g_const0

  # g_const140 <- list(name = jn(name, '_const140_gate'),
  #                    species = list(const140 = const_140),
  #                    reactions = c(),
  #                    ci = c(140),   # NOTE: scale this if you use concentration scaling
  #                    ki = c(p$const_gate_rate))
  # gates[[length(gates)+1]] <- g_const140

  # g_constc <- list(name = jn(name, '_constc_gate'),
  #                  species = list(constc = const_c),
  #                  reactions = c(),
  #                  ci = c(ic$c),   # initial reset value c
  #                  ki = c(p$const_gate_rate))
  # gates[[length(gates)+1]] <- g_constc

  # g_constd <- list(name = jn(name, '_constd_gate'),
  #                  species = list(constd = const_d),
  #                  reactions = c(),
  #                  ci = c(ic$d),   # initial d amount (pulse adder resource)
  #                  ki = c(p$const_gate_rate))
  # gates[[length(gates)+1]] <- g_constd

  # --- multiplier outputs and intermediates
  mul_vp_vp <- jn(name, '_mul_vp_vp')
  mul_vn_vn <- jn(name, '_mul_vn_vn')
  mul_vp_vn <- jn(name, '_mul_vp_vn')
  mul_5vp   <- jn(name, '_mul_5vp')
  mul_5vn   <- jn(name, '_mul_5vn')
  mul_bvp   <- jn(name, '_mul_bvp')
  mul_bvn   <- jn(name, '_mul_bvn')

  add_dv_p <- jn(name, '_add_dv_p')
  add_dv_n <- jn(name, '_add_dv_n')
  add_du_p <- jn(name, '_add_du_p')
  add_du_n <- jn(name, '_add_du_n')

  # --- 1) quadratic terms: v^2 pieces (scaled by 0.04)
  g_mul_vp_vp <- Make_Mul2In_Wang(jn(name, 'mul_vp_vp'), v_p, v_p, mul_vp_vp, 0, 0.04, p$rate_mul_sq)
  gates[[length(gates)+1]] <- g_mul_vp_vp
  g_mul_vn_vn <- Make_Mul2In_Wang(jn(name, 'mul_vn_vn'), v_n, v_n, mul_vn_vn, 0, 0.04, p$rate_mul_sq)
  gates[[length(gates)+1]] <- g_mul_vn_vn
  # cross-term scaled by 2*0.04 routed to negative aggregator
  g_mul_vp_vn <- Make_Mul2In_Wang(jn(name, 'mul_vp_vn'), v_p, v_n, mul_vp_vn, 0, 2 * 0.04, p$rate_mul_sq)
  gates[[length(gates)+1]] <- g_mul_vp_vn

  # --- 2) linear 5 * v split rails
  g_mul_5vp <- Make_Mul2In_Wang(jn(name, 'mul_5vp'), v_p, const_0, mul_5vp, 5, 0, p$rate_mul_lin)
  gates[[length(gates)+1]] <- g_mul_5vp
  g_mul_5vn <- Make_Mul2In_Wang(jn(name, 'mul_5vn'), v_n, const_0, mul_5vn, 5, 0, p$rate_mul_lin)
  gates[[length(gates)+1]] <- g_mul_5vn

  # --- 3) b * v for du/dt
  g_mul_bvp <- Make_Mul2In_Wang(jn(name, 'mul_bvp'), v_p, const_0, mul_bvp, ic$b, 0, p$rate_mul_b)
  gates[[length(gates)+1]] <- g_mul_bvp
  g_mul_bvn <- Make_Mul2In_Wang(jn(name, 'mul_bvn'), v_n, const_0, mul_bvn, ic$b, 0, p$rate_mul_b)
  gates[[length(gates)+1]] <- g_mul_bvn

  # --- 4) bias 140 route into dv positive aggregator
  g_bias <- Make_Add3In(jn(name, 'bias_add'), const_140, const_0, const_0, add_dv_p, 140, 0, 0, p$rate_add)
  gates[[length(gates)+1]] <- g_bias

  # --- 5) aggregators for dv positive and negative
  # dv positive: mul_vp_vp + mul_vn_vn + mul_5vp + u_n + I_p + bias
  temp1_p <- jn(name, '_temp1_p')
  g_add1p <- Make_Add3In(jn(name, 'add1p'), mul_vp_vp, mul_vn_vn, mul_5vp, temp1_p, 0, 0, 0, p$rate_add)
  gates[[length(gates)+1]] <- g_add1p
  g_add2p <- Make_Add3In(jn(name, 'add2p'), temp1_p, u_n, I_p, add_dv_p, 0, 0, 0, p$rate_add)
  gates[[length(gates)+1]] <- g_add2p

  # dv negative: cross-term + mul_5vn + u_p + I_n
  g_add1n <- Make_Add3In(jn(name, 'add1n'), mul_vp_vn, mul_5vn, u_p, add_dv_n, 0, 0, 0, p$rate_add)
  gates[[length(gates)+1]] <- g_add1n
  g_add2n <- Make_Add3In(jn(name, 'add2n'), add_dv_n, I_n, const_0, add_dv_n, 0, 0, 0, p$rate_add) # keeps same name
  gates[[length(gates)+1]] <- g_add2n

  # --- 6) du/dt branch: build adders then scale by a
  g_addu_p <- Make_Add3In(jn(name, 'addu_p'), mul_bvp, const_0, const_0, add_du_p, 0, 0, 0, p$rate_add)
  gates[[length(gates)+1]] <- g_addu_p
  g_addu_n <- Make_Add3In(jn(name, 'addu_n'), u_p, const_0, const_0, add_du_n, 0, 0, 0, p$rate_add)
  gates[[length(gates)+1]] <- g_addu_n

  scaled_du_p <- jn(name, '_scaled_du_p')
  scaled_du_n <- jn(name, '_scaled_du_n')
  g_mul_du_p <- Make_Mul2In_Wang(scaled_du_p, add_du_p, const_0, scaled_du_p, ic$a, 0, p$rate_a_scale)
  gates[[length(gates)+1]] <- g_mul_du_p
  g_mul_du_n <- Make_Mul2In_Wang(scaled_du_n, add_du_n, const_0, scaled_du_n, ic$a, 0, p$rate_a_scale)
  gates[[length(gates)+1]] <- g_mul_du_n

  # --- 7) Integrators (dv -> v_p/v_n ; du -> u_p/u_n)
  g_int_v <- Make_Integrator_OishiYordanov(jn(name, 'int_v'), add_dv_p, add_dv_n, v_p, v_n, 0, 0, p$rate_int_v)
  gates[[length(gates)+1]] <- g_int_v
  g_int_u <- Make_Integrator_OishiYordanov(jn(name, 'int_u'), scaled_du_p, scaled_du_n, u_p, u_n, 0, 0, p$rate_int_u)
  gates[[length(gates)+1]] <- g_int_u

  # --- 8) Comparator: detect v >= v_thresh, produce control pulse comp_out
  g_cmp <- Make_Comparator(
    jn(name, 'cmp'),    # gate base name
    v_p,                 # input positive rail (v_p)
    v_n,                 # input negative rail (v_n)
    comp_out,            # comparator output species
    c_threshold = abs(ic$v_thresh) * 1.0,  # threshold capacity (numeric)
    rate = p$comp_rate
  )
  gates[[length(gates)+1]] <- g_cmp

  # # --- 9) Spike marker (optional): route comp_out into spike species quickly via a tiny gate
  # # Here we make a tiny catalytic production so comp_out -> comp_out + spike (for plotting)
  # g_spk_marker <- list(
  #   name = jn(name, 'spk_marker'),
  #   species = list(trigger = comp_out, spike = spike),
  #   reactions = c(jn(comp_out, " -> ", comp_out, " + ", spike)),
  #   ci = c(0, 0),
  #   ki = c(p$comp_rate * 2.0)
  # )
  # gates[[length(gates)+1]] <- g_spk_marker

  # --- 10) PulseAdder: consume const_d resource when comp_out present and add to u_p
  g_padd <- Make_PulseAdder(jn(name, 'padd_u'), trigger = comp_out, nameConstD = const_d, nameTargetPos = u_p, nameTargetNeg = NULL, c_amount = ic$d, rate = p$comp_rate)
  gates[[length(gates)+1]] <- g_padd

  # Return gates list (caller will add to circuit and must include ci for constants if needed)
  return(gates)
}
