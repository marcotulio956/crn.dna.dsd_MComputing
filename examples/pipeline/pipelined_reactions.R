rm(list = ls())

source('R/parser.R')
source('R/util_functions.R')
source('R/crn_reactor.R')
source('R/4domain_reactor.R')
source('R/io.R')


source('R/forced_concentrations.R')

timing <- seq(0, 100, by = 0.05)


# Generate the noisy input (e.g., an external fluctuating nutrient 'S')
# We set mean-reversion theta=0.5, aiming for a target mean mu=100, with noise sigma=15
ou_data <- simulate_ou_process(
    t = timing, 
    x0 = 100, 
    theta = 0.5, 
    mu = 100, 
    sigma = 15,
    seed = 42
)

# Create an interpolation function from the generated SDE trajectory
# rule = 2 ensures it holds the last value if the SSA slightly overshoots the max time
noisy_nutrient_func <- approxfun(x = ou_data$time, y = ou_data$value, rule = 2)


## ================================================================
## A clocked, pipelined extension of the paper's own example circuit:
##
##   circuit$reactions <- c('S -> A', 'A -> 0')
##
## We reprogram the FIRST reaction so it only fires on the RISING
## EDGE of the clock, starting from the 3rd clock cycle onward, and
## we add a chemical zero-order-hold "register" that snapshots A at
## an arbitrary wall-clock time and holds that value forever after.
##
## Every sub-module below is either taken verbatim from the paper or
## is a direct, explicitly-noted specialization of one of its
## templates:
##
##   - CLK / REDGE ......... clock generator (new; see test_clock*.R)
##   - *_ab ................ absence indicator, Reactions 6-8
##   - BUDGET3/BUDGET3_ab ... a countdown-to-threshold specialization
##                            of the absence-indicator idea, used as
##                            a permanent LEVEL ENABLE (see note below)
##   - S + FIRE -> A + FIRE
##     FIRE + S_ab -> 0 ..... the generic g-triggered transfer
##                            template, Reactions 9-12, with g=FIRE
##   - A/A'/A_hold/SNAP ..... the Copier construct, Reactions 26-31,
##                            with g=SNAP, used as a sample-and-hold
## ================================================================

SLOW <- 1
FAST <- 1000

circuit <- c()

circuit$species <- c(
  # --- clock ---
  "CLK", "REDGE",
  # --- cycle counter / cycle>=3 latch ---
  "BUDGET3", "BUDGET3_ab", "CYCLE",
  # --- edge-AND-level gate for the target reaction ---
  "FIRE", "FIRE_ab", "EDGE_POST3",
  # --- target computation: S -> A, now edge-and-cycle gated ---
  "S", "S_ab", "A",
  # --- zero-order-hold snapshot register for A ---
  "SNAP", "A_ab", "Ap", "SNAP_ab", "A_hold"
)

circuit$ci <- setNames(rep(0, length(circuit$species)), circuit$species)
circuit$ci["BUDGET3"] <- 3      # "fire only after the 3rd clock cycle"

circuit$reactions <- c(
  ## ---- clock generator ------------------------------------------
  "0 -> CLK",                                   # 1  slow charge
  "8 CLK -> REDGE",                             # 2  fast idealized reset (theta=8)

  ## ---- absence indicator on BUDGET3 (Reactions 6-8 template) ----
  "0 -> BUDGET3_ab",                            # 3  slow
  "BUDGET3 + BUDGET3_ab -> BUDGET3",            # 4  fast
  "2 BUDGET3_ab -> BUDGET3_ab",                 # 5  fast

  ## ---- countdown-latch + cycle counter ---------------------------
  ## while budget remains: each rising edge spends one budget token
  ## and counts one cycle (no FIRE yet -- gate still closed)
  "REDGE + BUDGET3 -> CYCLE",                   # 6  slow (pacing)
  ## once budget is exhausted (BUDGET3_ab asserted = "cycle>=3"):
  ## every subsequent rising edge counts the cycle AND marks that a
  ## FIRE pulse is OWED (EDGE_POST3) -- counting itself is never
  ## blocked by anything downstream
  "REDGE + BUDGET3_ab -> CYCLE + BUDGET3_ab + EDGE_POST3",  # 7  slow (pacing)

  ## ---- target module: S -> A, triggered by FIRE (g=FIRE) ---------
  ## generic template (paper's Reactions 9-12): a -> b becomes
  ## S + g -> A + g while g present; g cleared via absence(S)
  "0 -> S",                                     # 8  slow continuous supply (backlog)
  "0 -> S_ab",                                  # 9  slow   (absence indicator of S)
  "S + S_ab -> S",                              # 10 fast
  "2 S_ab -> S_ab",                             # 11 fast
  "S + FIRE -> A + FIRE",                       # 12 fast   (drains S while FIRE present)
  "FIRE + S_ab -> 0",                           # 13 slow   (clears FIRE once S=0)

  ## ---- mutual exclusion, side 1: absence indicator of FIRE (6-8) ----
  ## "is the S->A writer currently idle?" -- stops the ZOH copier from
  ## reading A while FIRE is mid-computation
  "0 -> FIRE_ab",                               # 14 slow
  "FIRE + FIRE_ab -> FIRE",                     # 15 fast
  "2 FIRE_ab -> FIRE_ab",                       # 16 fast

  ## ---- mutual exclusion, side 2: EDGE_POST3 -> FIRE, gated on -------
  ## "no snapshot currently pending" (SNAP_ab, defined below). A one-
  ## sided guard is NOT enough: gating only the copier's read (side 1)
  ## still lets a *later* edge write fresh material into A while an
  ## in-flight snapshot transaction hasn't yet cleared, contaminating
  ## the latched value. Making FIRE emission itself wait for "no
  ## snapshot pending" closes that hole -- true two-sided exclusion,
  ## built entirely from the paper's own absence-indicator idiom.
  "EDGE_POST3 + SNAP_ab -> SNAP_ab + FIRE",     # 17 pace  (deferred if a SNAP is in flight)

  ## ---- zero-order hold: Copier construct (Reactions 26-31) -------
  ## a = A, a' = Ap, b = A_hold, g = SNAP.
  "0 -> A_ab",                                  # 18 slow   (absence indicator of A)
  "A + A_ab -> A",                              # 19 fast
  "2 A_ab -> A_ab",                             # 20 fast
  "SNAP + FIRE_ab + A -> SNAP + FIRE_ab + Ap",  # 21 slow   (transfer A -> A', writer must be idle)
  "SNAP + A_ab -> 0",                           # 22 slow   (clear g once A exhausted)
  "0 -> SNAP_ab",                               # 23 slow   (absence indicator of SNAP)
  "SNAP + SNAP_ab -> SNAP",                     # 24 fast
  "2 SNAP_ab -> SNAP_ab",                       # 25 fast
  "SNAP_ab + Ap -> A + A_hold",                 # 26 slow   (restore A, latch A_hold)

  ## ---- second reaction from the original example, unmodified ----
  "A -> 0"                                      # 27 slow-ish drain (kept from the example)
)

## PACE = the rate used for reactions that must complete comfortably
## within one clock period. It sits BETWEEN the clock's own pacing
## (kc ~ 0.8, period ~ 10) and FAST (1000): the design needs a
## *hierarchy* fast >> PACE >> 1/period, not just fast >> slow -- see
## write-up. All three tiers still differ by >=2 orders of magnitude,
## matching the paper's own "rate separation" criterion.
PACE <- 4

circuit$ki <- c(
  0.8, 2000,                     # clock:      kc, k_reset
  SLOW, FAST, FAST,              # BUDGET3 absence indicator
  PACE, PACE,                    # counter / gate (must clear within ~1 period)
  0.6, SLOW, FAST, FAST, FAST, PACE,  # S -> A module (kS=0.6, then absence-ind + transfer)
  SLOW, FAST, FAST,              # FIRE absence indicator (mutex side 1)
  PACE,                           # EDGE_POST3 -> FIRE, mutex side 2
  SLOW, FAST, FAST, 3*SLOW, 3*SLOW, SLOW, FAST, FAST, 3*SLOW,  # copier / ZOH (sped up)
  0.15                            # A -> 0 (slow decay, from original example)
)

stopifnot(length(circuit$ki) == length(circuit$reactions))

## ---- exogenous event: an experimenter samples A at an ARBITRARY time ----
snap_time <- 47
events <- data.frame(time = snap_time, species = "SNAP", amount = 1)

## ================================================================
## HOW TO GENERALIZE
##
## Different threshold N ("only after the Nth cycle"):
##   circuit$ci["BUDGET3"] <- N              # rename BUDGET3->BUDGETn if you like
##
## Falling-edge instead of rising-edge sensitivity:
##   add a second idealized reset that fires at the HALF-charge point,
##   e.g. "4 CLK -> FEDGE" (theta/2, if theta=8) in addition to
##   "8 CLK -> REDGE". Everything downstream (BUDGET/CYCLE/FIRE/EDGE_POST3)
##   is identical, just built from FEDGE instead of REDGE.
##
## Gating a DIFFERENT existing reaction "X -> Y" on the same clock:
##   apply the generic template (paper's Reactions 9-12) with g=FIRE:
##     "X + FIRE -> Y + FIRE"
##     "FIRE + X_ab -> 0"        (X_ab = absence indicator of X)
##   i.e. literally the same pattern used above for S->A.
##
## Shortcut for quick prototyping (no extra species at all): since the
## engine supports ki as a function(t, x), the whole cycle-3 + rising-
## edge gate collapses to one line if you don't need it to be "real"
## chemistry -- useful for testing the target logic before building
## the full circuit:
##   circuit$ki[reaction_index] <- function(t, x) {
##     if (x["CYCLE"] >= 3 && x["REDGE"] > 0) FAST else 0
##   }
## This is mathematically equivalent in spirit but is NOT itself a
## valid elementary mass-action term (rate depends on a second
## species' history, not just current reactant counts) -- fine for
## quick iteration, but the explicit-species version above is the one
## that is actually a legitimate CRN.
## ================================================================

# result_crn <- react2(
#   species   = circuit$species,
#   ci        = circuit$ci,
#   reactions = circuit$reactions,
#   ki        = circuit$ki,
#   t         = timing,
#   #engine = 'diffeqr',
#   verbose = FALSE,
#   forced_concentrations =  
#     list(
#       S = function(t) square_input(t, amplitude = 100)
#     )
# )

result_sto <- react_stochastic_frates_events(
  species = circuit$species,
  ci = circuit$ci,
  reactions = circuit$reactions,
  ki = circuit$ki,
  t = timing,
  forced_concentrations = 
    list(
      S = function(t) square_input(t, amplitude = 100)
    )
)

# Pass it directly into sto react
# results_tau <- react_tau_leap_forced(
#   circuit$species,
#   circuit$ci,
#   circuit$reactions,
#   circuit$ki,
#   circuit$t,
#   forced_concentrations = list(S = noisy_nutrient_func) # Inject the OU process here
# )
# 
# results_gssa <- react_stochastic_forced(
#   circuit$species,
#   circuit$ci,
#   circuit$reactions,
#   circuit$ki,
#   circuit$t,
#   forced_concentrations = list(S = noisy_nutrient_func), # Inject the OU process here
#   verbose = TRUE,
#   volume = 1
# )

plot_behavior(result_crn)

# g1 <- plot_behavior(results_tau,  circuit$species)
# g1
# g2 <- plot_behavior(results_gssa, circuit$species)
# g2

# circuit_dsd <- update_crn_4domain(circuit)
# results_dsd <- React_4domain_circuit(circuit_dsd)
# 
# 
# g3 <- plot_behavior(results_dsd$behavior, circuit_dsd$species)
# g3

