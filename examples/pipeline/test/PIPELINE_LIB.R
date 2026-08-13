## ================================================================
## PIPELINE_LIB.R
##
## Modelling/construction functions for building clocked, N-stage
## pipelined CRNs -- meant to sit alongside ANALOG_GATE.LIB-style gate
## constructors (Make_Oscillator_Dalchau, etc.) and to feed their
## output straight into your existing simulation methods (react2 /
## react_stochastic_frates or their patched variants). Nothing in
## here runs a simulation; it only builds species/reactions/ki lists
## in the same shape as your gate library already uses:
##     list(name=, species=, reactions=, ci=, ki=)
##
## Requires `jn <- function(...) paste0(...)` to already be defined,
## exactly as in ANALOG_GATE.LIB.
##
## CONTENTS
##   1. Make_NPhase_Oscillator   -- the phased clock itself
##   2. Gate_Ki / Phase_Condition -- ki-level orchestration combinators
##   3. Gate_Stage_On_Phase      -- apply a phase gate to a whole stage
##   4. Combine_Circuits         -- merge stages (+ clock) into one CRN
##   5. Make_ZOH_Register        -- a reusable sample-and-hold module
##   6. Make_Periodic_Events     -- repeating sample-trigger schedule
##   7. Make_Pipeline            -- top-level convenience constructor
## ================================================================

if (!exists("jn")) jn <- function(...) paste0(...)


## ================================================================
## 1. THE PHASED CLOCK
## ================================================================
## Generalizes Make_Oscillator_Dalchau's 3-species cyclic-dominance
## ring ("rock-paper-scissors": phase_i + phase_{i-1} -> 2*phase_i)
## to N phases. This is NOT the idealized-threshold-reset clock from
## clocked_v2.R -- it's a genuine, non-idealized, smooth mass-action
## oscillator (same reaction TEMPLATE as your existing gate library),
## and it gives you N phases for free from a single N-species ring
## instead of needing separate counter/comparator machinery per stage.
##
## IMPORTANT CALIBRATION NOTE (found empirically -- see conversation):
## this system has NO self-limiting/death term, so phase_1+...+phase_N
## is exactly conserved and the orbit's SHAPE depends entirely on how
## far `ci` sits from the symmetric center (total/N, total/N, ...):
##   - ci close to the center  -> small, fast, nearly-sinusoidal
##     wobble; phases are never clearly "dominant" -- bad clock.
##   - ci strongly biased toward one phase (e.g. 90% in phase 1,
##     the rest split thin) -> large-amplitude orbit that swings
##     close to each pure-phase corner in turn, giving sharp,
##     well-separated dominance windows -- good clock.
## `rate` scales the period roughly like 1/rate for a fixed `ci`
## shape; period does NOT depend on `ci`'s absolute scale, only its
## shape (ratios), so tune amplitude/sharpness via `ci`, tune speed
## via `rate`.
##
## Returns a gate-style object; `total <- sum(ci)` is stashed on it
## so downstream phase-gating can pick a sensible default threshold.
## ================================================================

## IMPORTANT SECOND CALIBRATION NOTE, SPECIFIC TO STOCHASTIC (SSA)
## USE -- found empirically while building the pipeline demo:
##
## In DISCRETE/stochastic simulation, "phase_i = 0" is an ABSORBING
## state for that species: its only production route,
## `phase_i + phase_{i-1} -> 2*phase_i`, requires phase_i >= 1 as a
## REACTANT, so once it hits exactly zero it can never fire again --
## the whole ring collapses onto whichever phase(s) remain. This is
## the well-known extinction/fixation failure mode of rock-paper-
## scissors-type cyclic-competition systems under demographic noise.
## It is NOT visible in the continuous/ODE engine (a continuous
## trajectory approaches 0 but never exactly touches it) -- it only
## bites in react_stochastic_frates, which is exactly the engine you
## want for pipeline stages that need to "run to completion".
##
## The deeper reason a small patch doesn't fully fix this: the
## un-modified ring (as in Make_Oscillator_Dalchau) conserves
## phase_1+...+phase_N exactly, so its orbits are NEUTRALLY stable --
## like a frictionless pendulum, there is no restoring force pulling
## a noise-perturbed trajectory back to a "safe" orbit. Demographic
## noise then does a slow unbounded random walk across orbits of
## different amplitude, and since larger-amplitude orbits swing
## closer to the (absorbing) zero boundary, collapse becomes an
## eventual near-certainty given enough time -- a bigger `ci` total
## delays this, it does not prevent it (confirmed empirically: even
## total=500 collapsed within ~60 time units in testing).
##
## The actual fix (also standard in synthetic-biology oscillator
## design, e.g. "dilution"-stabilized designs): add uniform uniform
## decay (phase_i -> 0) and uniform production (0 -> phase_i) to
## every phase. This breaks exact conservation and turns the neutral
## orbit family into a genuinely DISSIPATIVE, self-correcting
## oscillation that gets pulled back after a noise-driven excursion,
## instead of drifting further from center. Verified empirically to
## survive 150+ time units with multiple recoveries from near-zero
## dips, vs. permanent collapse within a handful of time units
## without it.
##
## `decay` defaults to a small fraction of `rate`; `source_rate` is
## auto-chosen so the stationary total equals sum(ci) (solve
## N*source_rate/decay = total), unless you override it. Set both to
## 0 to recover the exact literature/deterministic-only oscillator
## (matches Make_Oscillator_Dalchau's un-modified reactions).
## ================================================================

Make_NPhase_Oscillator <- function(name, phase_names, ci, rate,
                                    decay = rate / 4, source_rate = NULL) {
  n <- length(phase_names)
  stopifnot(n >= 3, length(ci) == n, all(ci >= 0))
  total <- sum(ci)
  if (is.null(source_rate)) source_rate <- if (decay > 0) decay * total / n else 0

  reactions <- character(n)
  for (i in seq_len(n)) {
    prev <- phase_names[((i - 2) %% n) + 1]
    cur  <- phase_names[i]
    reactions[i] <- jn(cur, ' + ', prev, ' -> ', cur, ' + ', cur)
  }
  ki <- as.list(rep(rate, n))

  if (decay > 0) {
    for (i in seq_len(n)) { reactions <- c(reactions, jn(phase_names[i], ' -> 0')); ki <- c(ki, decay) }
  }
  if (source_rate > 0) {
    for (i in seq_len(n)) { reactions <- c(reactions, jn('0 -> ', phase_names[i])); ki <- c(ki, source_rate) }
  }

  list(
    name      = name,
    species   = as.list(setNames(phase_names, phase_names)),
    reactions = reactions,
    ci        = ci,
    ki        = ki,
    total     = total,
    phases    = phase_names   # convenience: ordered phase names
  )
}


## Suggested initial condition for a sharp N-phase clock: phase 1
## gets `dominant_frac` of the total, the rest split evenly.
Suggest_Oscillator_CI <- function(total, n, dominant_frac = 0.9) {
  c(total * dominant_frac, rep(total * (1 - dominant_frac) / (n - 1), n - 1))
}


## ================================================================
## 2. ORCHESTRATION COMBINATORS (the general "when should this
##    reaction run" toolkit -- phases are one instance of this, not
##    a special case)
## ================================================================

## Gate_Ki: AND-compose an arbitrary condition(t, y, species) -> TRUE
## /FALSE with an existing ki entry (numeric constant OR function, any
## of the function(t) / function(t,y) / function(t,y,species) shapes
## your engines already accept). Returns a function(t,y,species).
Gate_Ki <- function(base_ki, condition) {
  base_fn <-
    if (is.function(base_ki)) {
      nargs <- length(formals(base_ki))
      if (nargs <= 1) function(t, y, species) base_ki(t)
      else if (nargs == 2) function(t, y, species) base_ki(t, y)
      else function(t, y, species) base_ki(t, y, species)
    } else {
      function(t, y, species) base_ki
    }
  function(t, y, species) if (isTRUE(condition(t, y, species))) base_fn(t, y, species) else 0
}

## Phase_Condition: "is this phase currently active" as a reusable
## condition(t,y,species) for Gate_Ki. Default threshold = total/N,
## a safe "clearly above average" cutoff for a well-calibrated
## oscillator (see notes above).
Phase_Condition <- function(phase_name, threshold) {
  function(t, y, species) y[phase_name] > threshold
}

## After_Time / While_Condition / Not_Busy: the other common
## orchestration primitives from this conversation, packaged for
## reuse with Gate_Ki.
After_Time      <- function(T) function(t, y, species) t >= T
While_Level     <- function(sp, op = `>=`, value) function(t, y, species) op(y[sp], value)
Species_Idle    <- function(sp) function(t, y, species) y[sp] == 0
All_Of          <- function(...) { conds <- list(...); function(t, y, species) all(vapply(conds, function(f) isTRUE(f(t,y,species)), TRUE)) }


## ================================================================
## 3. GATE A WHOLE STAGE ON A PHASE
## ================================================================
## Takes a gate-style stage object (list(species=,reactions=,ci=,ki=),
## e.g. anything built with your ANALOG_GATE.LIB constructors) and
## returns a COPY whose every ki entry additionally requires the given
## phase to be active. Reactions that should run continuously
## regardless of phase just shouldn't be passed through this.
## ================================================================

Gate_Stage_On_Phase <- function(stage, phase_name, threshold, extra_condition = NULL) {
  cond <- if (is.null(extra_condition)) Phase_Condition(phase_name, threshold)
          else All_Of(Phase_Condition(phase_name, threshold), extra_condition)

  stage$ki <- lapply(stage$ki, function(k) Gate_Ki(k, cond))
  stage
}


## ================================================================
## 4. COMBINE STAGES (+ CLOCK) INTO ONE FINAL CIRCUIT
## ================================================================
## Accepts any number of gate-style objects (species as a named list
## OR a flat character vector -- both are auto-detected, matching
## ANALOG_GATE.LIB's convention of species=list(role=actual_name,...))
## and merges them into one species/ci/reactions/ki circuit, ready for
## react2(...)/react_stochastic_frates(...).
##
## Species that appear in more than one stage (deliberate hand-off
## species between pipeline stages, e.g. stage 1 WRITES what stage 2
## READS, using the same real name) are merged, not duplicated. If two
## stages both specify a *nonzero* ci for the same species, that's
## almost always a mistake (which stage's initial value should win is
## ambiguous) and Combine_Circuits stops with an error naming the
## conflict, rather than silently picking one.
## ================================================================

Combine_Circuits <- function(...) {
  gates <- list(...)
  if (length(gates) == 1 && is.list(gates[[1]]) && is.null(names(gates[[1]])["species"]) == FALSE &&
      is.null(gates[[1]]$reactions)) {
    gates <- gates[[1]]   # allow passing a single list-of-gates too
  }

  all_species <- character(0)
  all_ci      <- c()
  all_reactions <- character(0)
  all_ki        <- list()

  for (g in gates) {
    sp <- if (is.list(g$species)) unlist(g$species, use.names = FALSE) else g$species
    ci_named <- setNames(as.numeric(g$ci), sp)

    new_sp <- setdiff(sp, all_species)
    all_species <- c(all_species, new_sp)
    all_ci[new_sp] <- ci_named[new_sp]

    shared_sp <- intersect(sp, names(all_ci))
    shared_sp <- setdiff(shared_sp, new_sp)  # species seen before this gate
    conflicts <- shared_sp[ci_named[shared_sp] != all_ci[shared_sp]]
    if (length(conflicts) > 0) {
      stop(sprintf(
        "Combine_Circuits: conflicting initial conditions for shared species: %s (existing vs. this gate: %s)",
        paste(conflicts, collapse = ", "),
        paste(sprintf("%s=%g vs %g", conflicts, all_ci[conflicts], ci_named[conflicts]), collapse = "; ")
      ))
    }

    all_reactions <- c(all_reactions, g$reactions)
    all_ki        <- c(all_ki, g$ki)
  }

  list(
    species   = all_species,
    ci        = unname(all_ci[all_species]),
    reactions = all_reactions,
    ki        = all_ki
  )
}


## ================================================================
## 5. ZERO-ORDER-HOLD REGISTER (reusable module)
## ================================================================
## Non-destructively latches the current value of `source` into
## `hold` every time `trigger` receives a pulse (via events=, or via
## a phase-edge/other reaction elsewhere in the circuit that produces
## `trigger`). Each new trigger OVERWRITES (not accumulates onto) the
## previous latch -- see the write-up for why the clear-first step is
## needed for that.
##
## Mutual exclusion: if `writer_idle_species` is given, the transfer
## step additionally waits for that species to be zero before reading
## `source` -- use this whenever `source` is also written by another
## clocked module (the same hazard as clocked_v2.R's FIRE/A example).
## Leave NULL if `source` is not concurrently written by anything else.
##
## Species created: <trigger> (consumed), <source>_zoh_tmp,
## <trigger>_done (internal), <hold>.
## ================================================================

Make_ZOH_Register <- function(source, hold, trigger,
                               writer_idle_species = NULL,
                               rate_slow = 1) {
  tmp  <- jn(source, "_zoh_tmp")
  done <- jn(trigger, "_done")   # absence-of-trigger flag, i.e. "not mid-transaction"

  writer_ok <- if (is.null(writer_idle_species)) {
    function(t, y, species) TRUE
  } else {
    Species_Idle(writer_idle_species)
  }

  species   <- c(source, trigger, tmp, done, hold)
  ci        <- c(0, 0, 0, 0, 0)

  reactions <- c(
    jn(hold, ' + ', trigger, ' -> ', trigger),                 # fast clear-on-new-trigger (overwrite semantics)
    jn(source, ' -> ', tmp),                                    # transfer while triggered & writer idle
    jn(trigger, ' -> 0'),                                       # clear trigger once source==0
    jn(tmp, ' -> ', source, ' + ', hold)                        # restore + latch once trigger==0
  )

  ki <- list(
    1000,                                                       # fast: old hold clears almost immediately
    Gate_Ki(rate_slow, All_Of(While_Level(trigger, `>`, 0), writer_ok)),
    Gate_Ki(rate_slow, Species_Idle(source)),
    Gate_Ki(rate_slow, Species_Idle(trigger))
  )

  list(name = jn(hold, "_zoh"), species = as.list(setNames(species, species)),
       reactions = reactions, ci = ci, ki = ki)
}

## ================================================================
## 6. PERIODIC (instead of one-shot) SAMPLING SCHEDULE
## ================================================================
## The direct answer to "define a sampling PERIOD instead of a
## unique time": builds the repeating events= data.frame that drives
## a Make_ZOH_Register's `trigger` species on a fixed cadence.
##
## NOTE: choose `period` comfortably larger than the ZOH register's
## own transfer+clear+restore completion time (governed by
## rate_slow/rate_fast above and how much `source` typically holds),
## or back-to-back triggers can overlap -- see write-up for the
## general busy-flag fix if you need periods shorter than that.
## ================================================================

Make_Periodic_Events <- function(trigger_species, period, t_start, t_end, amount = 1) {
  data.frame(
    time    = seq(t_start, t_end, by = period),
    species = trigger_species,
    amount  = amount
  )
}


## ================================================================
## 7. TOP-LEVEL CONVENIENCE: WIRE N STAGES TO AN N-PHASE CLOCK
## ================================================================
## `stages` is a list of gate-style stage objects, in pipeline order.
## `phase_names` must have the same length as `stages` -- stage i is
## gated on phase_names[i]. The clock is generated for you (or pass
## your own via `oscillator`). Returns a single combined circuit.
## ================================================================

Make_Pipeline <- function(stages, phase_names = NULL,
                           oscillator = NULL, oscillator_rate = 0.02,
                           oscillator_total = 150,
                           oscillator_dominant_frac = 0.9,
                           threshold = NULL, clock_name = "pclk") {
  n <- length(stages)
  if (is.null(phase_names)) phase_names <- paste0("PHASE", seq_len(n))
  stopifnot(length(phase_names) == n)

  if (is.null(oscillator)) {
    ci <- Suggest_Oscillator_CI(oscillator_total, n, oscillator_dominant_frac)
    oscillator <- Make_NPhase_Oscillator(clock_name, phase_names, ci, oscillator_rate)
  }
  if (is.null(threshold)) threshold <- oscillator$total / n

  gated_stages <- lapply(seq_len(n), function(i) {
    Gate_Stage_On_Phase(stages[[i]], phase_names[i], threshold)
  })

  circuit <- do.call(Combine_Circuits, c(list(oscillator), gated_stages))
  circuit$oscillator <- oscillator
  circuit$threshold  <- threshold
  circuit
}
