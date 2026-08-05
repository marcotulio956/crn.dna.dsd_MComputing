# Helper function to concat strings locally for this module
pipeline_jn <- function(...) { paste(..., sep = '') }

# Internal helper to create a DNAr gate/module structure
pipeline_make_module <- function(name, species = list(), ci = list(), reactions = list(), ki = list()) {
  list(
    name = name,
    species = species,
    ci = ci,
    reactions = reactions,
    ki = ki
  )
}

#' Absence indicator module
#'
#' Implements the pattern:
#' fuel -slow-> x_ab
#' x + x_ab -fast-> x
#' 2x_ab -fast-> x_ab
#'
#' @export
make_absence_indicator_module <- function(name, monitored_species, fuel_ci = 1, slow = 1e-4, fast = 1e-2) {
  xab <- pipeline_jn(name, '_ab')
  fuel <- pipeline_jn(name, '_fuel')

  pipeline_make_module(
    name = name,
    species = list(fuel = fuel, absence = xab),
    ci = list(fuel_ci, 0),
    reactions = list(
      pipeline_jn(fuel, ' -> ', xab),
      pipeline_jn(monitored_species, ' + ', xab, ' -> ', monitored_species),
      pipeline_jn('2', xab, ' -> ', xab)
    ),
    ki = list(slow, fast, fast)
  )
}

#' Oscillating pipeline wave with absence outputs
#'
#' Uses the proven three-species cyclic oscillator as the timing core and adds
#' absence indicators over the exported `value1` and `value0` phases. The
#' oscillatory core gives the visible clock-like motion; the absence channels are
#' the future pipeline guards that can gate execution when one phase has faded.
#'
#' @export
make_absence_wave_module <- function(name, crange = 1, rate = 3e-3,
                                     fuel_ci = 1, absence_slow = 5e-4,
                                     absence_fast = 2e-2) {
  value1 <- pipeline_jn(name, '_1')
  value0 <- pipeline_jn(name, '_0')
  aux <- pipeline_jn(name, '_aux')

  core <- pipeline_make_module(
    name = pipeline_jn(name, '_osc'),
    species = list(value1 = value1, value0 = value0, aux = aux),
    ci = list(crange * 0.9, crange * 0.5, crange * 0.3),
    reactions = list(
      pipeline_jn(value0, ' + ', value1, ' -> ', value0, ' + ', value0),
      pipeline_jn(aux, ' + ', value0, ' -> ', aux, ' + ', aux),
      pipeline_jn(value1, ' + ', aux, ' -> ', value1, ' + ', value1)
    ),
    ki = list(rate, rate, rate)
  )

  value1_abs <- make_absence_indicator_module(
    pipeline_jn(name, '_1mon'),
    value1,
    fuel_ci = fuel_ci,
    slow = absence_slow,
    fast = absence_fast
  )
  value0_abs <- make_absence_indicator_module(
    pipeline_jn(name, '_0mon'),
    value0,
    fuel_ci = fuel_ci,
    slow = absence_slow,
    fast = absence_fast
  )

  all_species <- c(
    unlist(core$species, use.names = FALSE),
    unlist(value1_abs$species, use.names = FALSE),
    unlist(value0_abs$species, use.names = FALSE)
  )
  all_ci <- c(
    unlist(core$ci, use.names = FALSE),
    unlist(value1_abs$ci, use.names = FALSE),
    unlist(value0_abs$ci, use.names = FALSE)
  )

  list(
    modules = list(core, value1_abs, value0_abs),
    species = list(
      value1 = value1,
      value0 = value0,
      aux = aux,
      value1_abs = value1_abs$species$absence,
      value0_abs = value0_abs$species$absence,
      value1_abs_fuel = value1_abs$species$fuel,
      value0_abs_fuel = value0_abs$species$fuel
    ),
    all_species = all_species,
    ci = all_ci,
    reactions = c(
      unlist(core$reactions, use.names = FALSE),
      unlist(value1_abs$reactions, use.names = FALSE),
      unlist(value0_abs$reactions, use.names = FALSE)
    ),
    ki = c(
      unlist(core$ki, use.names = FALSE),
      unlist(value1_abs$ki, use.names = FALSE),
      unlist(value0_abs$ki, use.names = FALSE)
    )
  )
}

#' Copier module (single-shot copy with completion by absence)
#'
#' Implements:
#' g + a -slow-> g + a_copy
#' g + a_ab -slow-> 0
#' g_ab + a_copy -slow-> a + out
#'
#' @export
make_copier_module <- function(name, source_species, output_species, start_ci = 1,
                               slow = 1e-4, fast = 1e-2) {
  g <- pipeline_jn(name, '_g')
  a_copy <- pipeline_jn(name, '_copy')

  g_abs <- make_absence_indicator_module(pipeline_jn(name, '_g'), g, fuel_ci = 1, slow = slow, fast = fast)
  a_abs <- make_absence_indicator_module(pipeline_jn(name, '_src'), source_species, fuel_ci = 1, slow = slow, fast = fast)

  mod <- pipeline_make_module(
    name = name,
    species = list(g = g, a_copy = a_copy),
    ci = list(start_ci, 0),
    reactions = list(
      pipeline_jn(g, ' + ', source_species, ' -> ', g, ' + ', a_copy),
      pipeline_jn(g, ' + ', a_abs$species$absence, ' -> 0'),
      pipeline_jn(g_abs$species$absence, ' + ', a_copy, ' -> ', source_species, ' + ', output_species)
    ),
    ki = list(slow, slow, slow)
  )

  list(
    modules = list(mod, g_abs, a_abs),
    control = list(g = g, g_ab = g_abs$species$absence, src_ab = a_abs$species$absence, copy = a_copy)
  )
}

#' Comparator module (greater-than)
#'
#' Produces token `gt` when `left_species > right_species` using copied species.
#' The core annihilation is: left_copy + right_copy -> 0
#'
#' @export
make_gt_comparator_module <- function(name, left_species, right_species, gt_species,
                                      slow = 1e-4, fast = 1e-2) {
  lcopy <- pipeline_jn(name, '_lcopy')
  rcopy <- pipeline_jn(name, '_rcopy')
  guard <- pipeline_jn(name, '_guard')
  fuel <- pipeline_jn(name, '_fuel')

  l_abs <- make_absence_indicator_module(pipeline_jn(name, '_lcopy'), lcopy, fuel_ci = 1, slow = slow, fast = fast)
  r_abs <- make_absence_indicator_module(pipeline_jn(name, '_rcopy'), rcopy, fuel_ci = 1, slow = slow, fast = fast)

  mod <- pipeline_make_module(
    name = name,
    species = list(lcopy = lcopy, rcopy = rcopy, guard = guard, fuel = fuel),
    ci = list(0, 0, 0, 1),
    reactions = list(
      pipeline_jn(left_species, ' -> ', lcopy),
      pipeline_jn(right_species, ' -> ', rcopy),
      pipeline_jn(lcopy, ' + ', rcopy, ' -> 0'),
      pipeline_jn(r_abs$species$absence, ' + ', fuel, ' -> ', r_abs$species$absence, ' + ', guard),
      pipeline_jn(guard, ' + ', lcopy, ' -> ', guard, ' + ', gt_species),
      pipeline_jn(l_abs$species$absence, ' + ', gt_species, ' -> ', l_abs$species$absence),
      pipeline_jn(guard, ' -> 0')
    ),
    ki = list(slow, slow, fast, slow, slow, fast, slow)
  )

  list(
    modules = list(mod, l_abs, r_abs),
    control = list(left_copy = lcopy, right_copy = rcopy, left_ab = l_abs$species$absence, right_ab = r_abs$species$absence)
  )
}

#' Decrement module (x := x - 1)
#'
#' Inspired by ideas.txt Eq. (3): transfer to temporary pool then bring all but one back.
#'
#' @export
make_decrement_module <- function(name, x_species, start_species,
                                  slow = 1e-4, fast = 1e-2) {
  x_tmp <- pipeline_jn(name, '_tmp')
  x_rx <- pipeline_jn(name, '_rx')
  xfer <- pipeline_jn(name, '_xfer')

  x_abs <- make_absence_indicator_module(pipeline_jn(name, '_x'), x_species, fuel_ci = 1, slow = slow, fast = fast)
  rx_abs <- make_absence_indicator_module(pipeline_jn(name, '_rx'), x_rx, fuel_ci = 1, slow = slow, fast = fast)
  tmp_abs <- make_absence_indicator_module(pipeline_jn(name, '_tmp'), x_tmp, fuel_ci = 1, slow = slow, fast = fast)

  mod <- pipeline_make_module(
    name = name,
    species = list(x_tmp = x_tmp, x_rx = x_rx, xfer = xfer),
    ci = list(0, 0, 0),
    reactions = list(
      pipeline_jn(start_species, ' + ', x_species, ' -> ', start_species, ' + ', x_tmp),
      pipeline_jn(start_species, ' + ', x_abs$species$absence, ' -> ', xfer),
      pipeline_jn(xfer, ' + 2', x_tmp, ' -> ', xfer, ' + ', x_species, ' + ', x_rx),
      pipeline_jn(x_rx, ' -> 0'),
      pipeline_jn(rx_abs$species$absence, ' + ', x_tmp, ' -> 0'),
      pipeline_jn(tmp_abs$species$absence, ' + ', xfer, ' -> 0')
    ),
    ki = list(slow, slow, fast, slow, slow, slow)
  )

  list(
    modules = list(mod, x_abs, rx_abs, tmp_abs),
    control = list(tmp = x_tmp, x_abs = x_abs$species$absence, tmp_abs = tmp_abs$species$absence)
  )
}

#' Phase-controlled subtraction kernel
#'
#' For each molecule in `i_species`, consume one molecule in `b_species`
#' while `phase_species` is active.
#'
#' @export
make_subtract_kernel_module <- function(name, phase_species, i_species, b_species,
                                        slow = 1e-4, fast = 1e-2) {
  i_work <- pipeline_jn(name, '_iwork')
  i_ret <- pipeline_jn(name, '_iret')
  b_sink <- pipeline_jn(name, '_bsink')

  iwork_abs <- make_absence_indicator_module(pipeline_jn(name, '_iwork'), i_work, fuel_ci = 1, slow = slow, fast = fast)

  mod <- pipeline_make_module(
    name = name,
    species = list(i_work = i_work, i_ret = i_ret, b_sink = b_sink),
    ci = list(0, 0, 0),
    reactions = list(
      pipeline_jn(phase_species, ' + ', i_species, ' -> ', phase_species, ' + ', i_work),
      pipeline_jn(i_work, ' + ', b_species, ' -> ', i_ret, ' + ', b_sink),
      pipeline_jn(i_ret, ' -> ', i_species)
    ),
    ki = list(slow, fast, slow)
  )

  list(
    modules = list(mod, iwork_abs),
    control = list(i_work = i_work, i_work_ab = iwork_abs$species$absence, b_sink = b_sink)
  )
}

#' Build a phased while-loop CRN program
#'
#' Program shape implemented:
#' while (i > a) {
#'   b = b - i
#'   i = i - 1
#' }
#'
#' @export
make_pipeline_while_gt_subtract_circuit <- function(name, timing,
                                                    i_init = 10, a_init = 3, b_init = 80,
                                                    slow = 1e-4, fast = 1e-2) {
  circuit <- make_circuit(timing)

  i <- pipeline_jn(name, '_i')
  a <- pipeline_jn(name, '_a')
  b <- pipeline_jn(name, '_b')

  phase_cmp <- pipeline_jn(name, '_Pcmp')
  phase_sub <- pipeline_jn(name, '_Psub')
  phase_dec <- pipeline_jn(name, '_Pdec')
  gt <- pipeline_jn(name, '_gt')
  halt <- pipeline_jn(name, '_halt')
  sub_done <- pipeline_jn(name, '_sub_done')

  regs <- pipeline_make_module(
    name = pipeline_jn(name, '_registers'),
    species = list(i = i, a = a, b = b, phase_cmp = phase_cmp, phase_sub = phase_sub,
                   phase_dec = phase_dec, gt = gt, halt = halt, sub_done = sub_done),
    ci = list(i_init, a_init, b_init, 1, 0, 0, 0, 0, 0),
    reactions = list(),
    ki = list()
  )

  comparator <- make_gt_comparator_module(pipeline_jn(name, '_cmp'), i, a, gt, slow = slow, fast = fast)
  subtractor <- make_subtract_kernel_module(pipeline_jn(name, '_sub'), phase_sub, i, b, slow = slow, fast = fast)
  decr <- make_decrement_module(pipeline_jn(name, '_dec'), i, phase_dec, slow = slow, fast = fast)

  phase_logic <- pipeline_make_module(
    name = pipeline_jn(name, '_phase_logic'),
    species = list(),
    ci = list(),
    reactions = list(
      pipeline_jn(phase_cmp, ' + ', gt, ' -> ', phase_sub, ' + ', gt),
      pipeline_jn(phase_cmp, ' + ', comparator$control$left_ab, ' -> ', halt),
      pipeline_jn(subtractor$control$i_work_ab, ' + ', gt, ' -> ', sub_done),
      pipeline_jn(phase_sub, ' + ', sub_done, ' -> ', phase_dec),
      pipeline_jn(phase_dec, ' + ', decr$control$tmp_abs, ' -> ', phase_cmp),
      pipeline_jn(gt, ' -> 0'),
      pipeline_jn(sub_done, ' -> 0')
    ),
    ki = list(slow, slow, slow, fast, slow, slow, slow)
  )

  for (m in c(list(regs), comparator$modules, subtractor$modules, decr$modules, list(phase_logic))) {
    circuit <- circuit_insert_gate(circuit, m)
  }

  circuit <- circuit_compile(circuit)
  circuit$registers <- list(i = i, a = a, b = b, halt = halt,
                            phase_cmp = phase_cmp, phase_sub = phase_sub, phase_dec = phase_dec)

  return(circuit)
}

#' Convenience simulation wrapper for phased pipeline circuits
#'
#' @export
simulate_pipeline_circuit <- function(circuit, ...) {
  react(
    species = circuit$species,
    ci = circuit$ci,
    reactions = circuit$reactions,
    ki = circuit$ki,
    t = circuit$t,
    ...
  )
}

# ----------------------------------------------------------------------------
# Analog blocks adapted for pipeline control (copied from ANALOG_GATE_LIB style)
# ----------------------------------------------------------------------------

#' Pipeline-adapted Song 2-input adder
#'
#' @export
pipeline_make_adder2in_song <- function(name, nameInput1, nameInput2, nameOutput,
                                        cinput1, cinput2, crange, rate) {
  species <- list(
    input1 = nameInput1,
    input2 = nameInput2,
    intermediate1 = pipeline_jn(name, '_A1'),
    intermediate2 = pipeline_jn(name, '_A2'),
    output = nameOutput
  )

  pipeline_make_module(
    name = name,
    species = species,
    ci = c(cinput1, cinput2, crange, crange, 0),
    reactions = c(
      pipeline_jn(species$input1, ' + ', species$intermediate1, ' -> ', species$output),
      pipeline_jn(species$input2, ' + ', species$intermediate2, ' -> ', species$output)
    ),
    ki = c(rate, rate)
  )
}

#' Pipeline-adapted Song 2-input subtractor
#'
#' @export
pipeline_make_sub2in_song <- function(name, nameInput1, nameInput2,
                                      cinput1, cinput2, crange, rate) {
  species <- list(
    input1 = nameInput1,
    input2 = nameInput2,
    intermediate1 = pipeline_jn(name, '_S'),
    intermediate2 = pipeline_jn(name, '_Sx')
  )

  pipeline_make_module(
    name = name,
    species = species,
    ci = c(cinput1, cinput2, crange, 0),
    reactions = c(
      pipeline_jn(species$input2, ' + ', species$intermediate1, ' -> ', species$intermediate2),
      pipeline_jn(species$input1, ' + ', species$intermediate2, ' -> 0')
    ),
    ki = c(rate, rate)
  )
}

#' Pipeline-adapted Dalchau oscillator
#'
#' @export
pipeline_make_oscillator_dalchau <- function(name, nameInput1, nameInput2, nameInput3,
                                             cinput1, cinput2, cinput3, rate) {
  species <- list(
    input1 = nameInput1,
    input2 = nameInput2,
    input3 = nameInput3
  )

  pipeline_make_module(
    name = name,
    species = species,
    ci = c(cinput1, cinput2, cinput3),
    reactions = c(
      pipeline_jn(species$input2, ' + ', species$input1, ' -> ', species$input2, ' + ', species$input2),
      pipeline_jn(species$input3, ' + ', species$input2, ' -> ', species$input3, ' + ', species$input3),
      pipeline_jn(species$input1, ' + ', species$input3, ' -> ', species$input1, ' + ', species$input1)
    ),
    ki = c(rate, rate, rate)
  )
}

pipeline_add_catalyst_to_reaction <- function(reaction, catalysts) {
  parts <- strsplit(reaction, '->', fixed = TRUE)[[1]]
  lhs <- trimws(parts[1])
  rhs <- trimws(parts[2])

  catstr <- paste(catalysts, collapse = ' + ')

  lhs_new <- if (lhs == '0') catstr else paste(catstr, lhs, sep = ' + ')
  rhs_new <- if (rhs == '0') catstr else paste(catstr, rhs, sep = ' + ')

  pipeline_jn(lhs_new, ' -> ', rhs_new)
}

#' Gate a module by phase species and optional absence species
#'
#' @export
pipeline_gate_module <- function(module, phase_species, absence_species = NULL,
                                 base_rate = 1e-3, fast_factor = 20,
                                 fast_idx = integer(0)) {
  catalysts <- c(phase_species)
  if (!is.null(absence_species)) {
    catalysts <- c(catalysts, absence_species)
  }

  module$reactions <- vapply(
    module$reactions,
    function(r) pipeline_add_catalyst_to_reaction(r, catalysts),
    character(1)
  )

  module$ki <- rep(base_rate, length(module$reactions))
  if (length(fast_idx) > 0) {
    module$ki[fast_idx] <- base_rate * fast_factor
  }

  module
}

#' Build a single-react-call pipelined circuit demo
#'
#' This follows the same integration style as stepRLC: assemble gates into one
#' circuit, compile, then call react() once.
#'
#' @export
make_singlecall_pipeline_demo_circuit <- function(name, timing,
                                                  base_rate = 1e-3,
                                                  fast_factor = 20,
                                                  crange = 10,
                                                  i_init = 6,
                                                  a_init = 2,
                                                  b_init = 8) {
  circuit <- make_circuit(timing)

  phase1 <- pipeline_jn(name, '_phase1')
  phase2 <- pipeline_jn(name, '_phase2')
  phase3 <- pipeline_jn(name, '_phase3')

  # Clock core (oscillator)
  osc <- pipeline_make_oscillator_dalchau(
    pipeline_jn(name, '_osc'),
    phase1,
    phase2,
    phase3,
    cinput1 = 0.9,
    cinput2 = 0.5,
    cinput3 = 0.3,
    rate = base_rate
  )

  # Absence indicators on phases for end-of-phase guards
  p1_abs <- make_absence_indicator_module(
    pipeline_jn(name, '_p1'),
    phase1,
    fuel_ci = 1,
    slow = base_rate,
    fast = base_rate * fast_factor
  )
  p2_abs <- make_absence_indicator_module(
    pipeline_jn(name, '_p2'),
    phase2,
    fuel_ci = 1,
    slow = base_rate,
    fast = base_rate * fast_factor
  )

  i <- pipeline_jn(name, '_i')
  a <- pipeline_jn(name, '_a')
  b <- pipeline_jn(name, '_b')
  b_next <- pipeline_jn(name, '_b_next')
  i_next <- pipeline_jn(name, '_i_next')

  regs <- pipeline_make_module(
    name = pipeline_jn(name, '_registers'),
    species = list(i = i, a = a, b = b, b_next = b_next, i_next = i_next),
    ci = c(i_init, a_init, b_init, 0, 0),
    reactions = c(),
    ki = c()
  )

  # Stage 1 (phase1): compute b_next := b - i (Song subtractor)
  stage_sub <- pipeline_make_sub2in_song(
    pipeline_jn(name, '_sub_stage'),
    nameInput1 = b,
    nameInput2 = i,
    cinput1 = 0,
    cinput2 = 0,
    crange = crange,
    rate = base_rate
  )
  # Route subtractor intermediate to b_next only when phase1 is active
  sub_route <- pipeline_make_module(
    name = pipeline_jn(name, '_sub_route'),
    species = list(),
    ci = c(),
    reactions = c(
      pipeline_jn(stage_sub$species$intermediate2, ' -> ', b_next)
    ),
    ki = c(base_rate)
  )

  # Stage 2 (phase2): compute i_next := i - a (as a decrement-like op with constant a)
  stage_dec <- pipeline_make_sub2in_song(
    pipeline_jn(name, '_dec_stage'),
    nameInput1 = i,
    nameInput2 = a,
    cinput1 = 0,
    cinput2 = 0,
    crange = crange,
    rate = base_rate
  )
  dec_route <- pipeline_make_module(
    name = pipeline_jn(name, '_dec_route'),
    species = list(),
    ci = c(),
    reactions = c(
      pipeline_jn(stage_dec$species$intermediate2, ' -> ', i_next)
    ),
    ki = c(base_rate)
  )

  # Commit phase: when phase3 dominates and absence of phase1/phase2 has built,
  # move staged values back into registers.
  commit <- pipeline_make_module(
    name = pipeline_jn(name, '_commit'),
    species = list(),
    ci = c(),
    reactions = c(
      pipeline_jn(phase3, ' + ', p1_abs$species$absence, ' + ', b_next, ' -> ', phase3, ' + ', p1_abs$species$absence, ' + ', b),
      pipeline_jn(phase3, ' + ', p2_abs$species$absence, ' + ', i_next, ' -> ', phase3, ' + ', p2_abs$species$absence, ' + ', i)
    ),
    ki = c(base_rate * fast_factor, base_rate * fast_factor)
  )

  stage_sub <- pipeline_gate_module(stage_sub, phase1, p2_abs$species$absence,
                                    base_rate = base_rate, fast_factor = fast_factor,
                                    fast_idx = integer(0))
  sub_route <- pipeline_gate_module(sub_route, phase1, p2_abs$species$absence,
                                    base_rate = base_rate, fast_factor = fast_factor)

  stage_dec <- pipeline_gate_module(stage_dec, phase2, p1_abs$species$absence,
                                    base_rate = base_rate, fast_factor = fast_factor,
                                    fast_idx = integer(0))
  dec_route <- pipeline_gate_module(dec_route, phase2, p1_abs$species$absence,
                                    base_rate = base_rate, fast_factor = fast_factor)

  for (m in list(osc, p1_abs, p2_abs, regs, stage_sub, sub_route, stage_dec, dec_route, commit)) {
    circuit <- circuit_insert_gate(circuit, m)
  }

  circuit <- circuit_compile(circuit)
  circuit$registers <- list(i = i, a = a, b = b, b_next = b_next, i_next = i_next)
  circuit$phases <- list(phase1 = phase1, phase2 = phase2, phase3 = phase3,
                         phase1_abs = p1_abs$species$absence,
                         phase2_abs = p2_abs$species$absence)

  return(circuit)
}