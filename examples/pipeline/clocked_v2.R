source("R/crn_reactor.R")
source("R/sto_reactor.R")
source("R/parser.R")
source("R/util_functions.R")
source("R/io.R")


## ================================================================
## Clocked / pipelined CRN, v2 -- built for react_stochastic_frates()
## instead of raw mass-action reactions.
##
## KEY INSIGHT vs. the pure-mass-action version:
##
##   The paper's absence-indicator machinery exists ONLY because pure
##   mass-action chemistry has no way to "read" a species' value and
##   branch on it -- the only way to make one reaction's rate depend
##   on "is species X present/absent" is to have a SEPARATE sub-CRN
##   continuously producing a real "X is absent" signal molecule that
##   can act catalytically. That's real, valuable physics if you
##   actually intend to build this in wet chemistry -- but it is pure
##   overhead if you are simulating with an engine whose `ki` can
##   already be an arbitrary function(t, y, species).  With that hook,
##   "gate this rate on species X being absent" is just:
##       ki = function(t, y, species) if (y["X"] == 0) k else 0
##   directly -- no extra species, no extra reactions, and (bonus) no
##   residual error from the indicator's own finite fast/slow ratio.
##
##   The ONE thing you still cannot get from a ki-closure is a genuine
##   EDGE (an exact instant). Propensities are only re-evaluated when
##   SOME reaction fires anywhere in the system -- there is no
##   guarantee anything will happen to be evaluated exactly at the
##   moment a watched quantity crosses a threshold. So anything that
##   must be a one-shot pulse tied to a crossing (the clock's own
##   tick) has to be a REAL, SELF-TRIGGERING reaction, whose own
##   propensity is what becomes nonzero at the crossing -- that's what
##   forces the SSA to notice and draw an event right then.
##
## Net effect: 9 species / 10 reactions here, vs. 14 species / 27
## reactions in the pure-mass-action version -- same behaviour.
## ================================================================

SLOW <- 1
FAST <- 1000
PACE <- 4

species <- c(
  "CLK", "REDGE",      # self-triggering clock (must be real chemistry)
  "CYCLE", "FIRE",      # cycle counter + edge-and-cycle-gated trigger
  "S", "A",             # the target pipeline stage (S -> A on FIRE)
  "SNAP", "Ap", "A_hold" # zero-order-hold snapshot register for A
)

ci <- setNames(rep(0, length(species)), species)

reactions <- c(
  ## ---- clock: self-triggering, must be real reactions -----------
  "0 -> CLK",                # slow charge
  "8 CLK -> 2 REDGE",        # fast idealized threshold reset (theta=8);
                              # 2 REDGE per tick: one for counting
                              # (always active), one for firing
                              # (conditionally active) -- see below

  ## ---- counting: always active, unconditional --------------------
  "REDGE -> CYCLE",

  ## ---- edge-AND-cycle-AND-mutex gate: level conditions read -------
  ## directly off real species via ki, no absence indicator needed
  "REDGE -> FIRE",

  ## ---- target module: S -> A while FIRE present -------------------
  "0 -> S",
  "S + FIRE -> A + FIRE",
  "FIRE -> 0",                # cleared once S==0 (read directly)

  ## ---- ZOH snapshot register for A --------------------------------
  "A -> Ap",                  # transfer while SNAP requested & FIRE idle
  "SNAP -> 0",                # cleared once A==0 (transfer confirmed done)
  "Ap -> A + A_hold"          # restore + latch, once SNAP confirmed clear
)

ki <- list(
  0.8,                                                  # 0->CLK
  2000,                                                 # 8CLK->2REDGE
  PACE,                                                 # REDGE->CYCLE (always)
  function(t, y, species)                               # REDGE->FIRE
    if (y["CYCLE"] >= 3 && y["SNAP"] == 0) PACE else 0, #   cycle-gate + mutex(2)
  0.6,                                                  # 0->S
  FAST,                                                 # S+FIRE->A+FIRE
  function(t, y, species) if (y["S"] == 0) SLOW else 0, # FIRE->0
  function(t, y, species)                               # A->Ap
    if (y["SNAP"] > 0 && y["FIRE"] == 0) SLOW else 0,   #   mutex(1): writer idle
  function(t, y, species) if (y["A"] == 0) SLOW else 0, # SNAP->0
  function(t, y, species) if (y["SNAP"] == 0) SLOW else 0  # Ap->A+A_hold
)

## An arbitrary-time snapshot request, using the newly-patched
## `events` argument (definite-time orchestration):
snap_time <- 60
events <- data.frame(time = snap_time, species = "SNAP", amount = 100)

## ================================================================
## HOW TO GENERALIZE (mirrors the pure-CRN version's notes)
##
## Different threshold N:            change the `3` inside the FIRE ki
## Falling edge instead of rising:   add "4 CLK -> 2 FEDGE" (half theta)
##                                   and build a second FIRE-like gate
##                                   from FEDGE instead of REDGE
## Gate an unrelated reaction "X->Y" the same way:
##   "X + FIRE -> Y + FIRE"  with FIRE as produced above, OR even
##   simpler, since ki can read ANY species directly:
##   ki = function(t,y,species) if (SOME_CONDITION(y)) k else 0
##   applied straight to "X -> Y" -- no FIRE/mutex machinery needed
##   at all unless X is *also* written by another clocked module.
## Back-to-back snapshot requests (an edge case not exercised here):
##   add `&& y["Ap"] == 0` to the "SNAP->0"-gated restore's *sibling*
##   transfer condition to block a second SNAP from being accepted
##   before the first has fully drained Ap -- a busy-flag guard.
## ================================================================


# det <- react3(
#   species = species,
#   ci = ci,
#   reactions = reactions,
#   ki = ki,
#   t = seq(0, 100, by = 0.1),
#   forced_concentrations = NULL,
#   verbose = FALSE
# )

sto <- react_stochastic_frates_events(
  species = species,
  ci = ci,
  reactions = reactions,
  ki = ki,
  t = seq(0, 50, by = 0.1),
  forced_concentrations = NULL,
  seed = 1231,
  verbose = FALSE,
  volume = 100,
  events = events
)

plot_behavior(sto, species = c("CLK", "CYCLE", "FIRE", "S"), title = "Sto Clocked / pipelined CRN v2")
