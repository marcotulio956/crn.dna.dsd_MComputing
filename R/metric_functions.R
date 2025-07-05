library(pracma)   # for signal processing utilities
library(xtable)   # for generating LaTeX tables


# ' Estimate time constants for capacitor voltage response
#'#' @param time                  numeric vector of time points
#' @param voltage_input         numeric vector of input voltage (excitation)
#' @param capacitor_voltage_output numeric vector of capacitor voltage output 
#' @return list with:
#'  - tau_63: time at which capacitor voltage reaches 63.2%
#'  - tau_95: time at which capacitor voltage reaches 95%
#'  - tau_d36.8: time at which capacitor voltage discharges to 36.8% of its peak value
#'  - t_threshold: time when input voltage first exceeds 0.01 V

estimate_tau_capacitor_voltage <- function(time, voltage_input, capacitor_voltage_output) {
  # Ensure inputs are the same length
  if (!all(length(time) == length(voltage_input),
           length(time) == length(capacitor_voltage_output))) {
    stop("time, voltage_input and capacitor_voltage_output must be the same length")
  }

  # Find first point where excitation exceeds 0.01 V
  i0 <- which(voltage_input > 0.01)[1]
  if (is.na(i0)) stop("voltage_input never exceeds 0.01 V")

  # Reference time at threshold crossing
  t0 <- time[i0]

  # Truncate vectors from i0 onward
  t_full <- time[i0:length(time)]
  Vin     <- voltage_input[i0:length(time)]
  Vc      <- capacitor_voltage_output[i0:length(time)]

  # Compute step levels
  V0 <- min(Vin)
  V1 <- max(Vin)
  ΔV <- V1 - V0

  # Helper: linear interpolation of crossing time
  get_t_at <- function(t, y, target) {
    idx <- which(diff(sign(y - target)) != 0)
    if (length(idx) == 0) return(NA_real_)
    i <- idx[1]
    t1 <- t[i]; t2 <- t[i+1]
    y1 <- y[i]; y2 <- y[i+1]
    return(t1 + (target - y1) * (t2 - t1) / (y2 - y1))
  }

  # Charge targets (63.2% and 95% of ΔV)
  target_63  <- V0 + 0.632 * ΔV
  target_95  <- V0 + 0.95  * ΔV

  # Find relative times of crossings within truncated data
  t_rel_63 <- get_t_at(t_full - t0, Vc, target_63)
  t_rel_95 <- get_t_at(t_full - t0, Vc, target_95)

  # Convert to absolute times
  t_abs_63 <- if (!is.na(t_rel_63)) t0 + t_rel_63 else NA
  t_abs_95 <- if (!is.na(t_rel_95)) t0 + t_rel_95 else NA

  # DISCHARGE: identify peak index in Vin
  i_peak <- which.max(Vin)
  t_peak <- t_full[i_peak]
  Vp     <- Vc[i_peak]
  Vfinal <- Vc[length(Vc)]
  ΔVd    <- Vp - Vfinal
  target_36.8 <- Vfinal + 0.368 * ΔVd

  # Post-peak segment
  t_post_rel <- (t_full - t_peak)[i_peak:length(t_full)]
  Vc_post    <- Vc[i_peak:length(Vc)]
  t_rel_36.8 <- get_t_at(t_post_rel, Vc_post, target_36.8)

  # Absolute discharge time
  t_abs_36.8 <- if (!is.na(t_rel_36.8)) t_peak + t_rel_36.8 else NA

  list(
    tau_63     = t_rel_63,
    tau_95     = t_rel_95,
    tau_d36.8  = t_rel_36.8,
    t_threshold = t0
  )
}

#' Estimate the fundamental AC period of two waveforms
#'
#' @param timing        numeric vector of time points
#' @param source_input  not used here (placeholder for your input signal)
#' @param model_vals    numeric vector of model signal values
#' @param sim_vals      numeric vector of simulation signal values
#' @param start_time    time after which to begin analysis (defaults to 10)
#' @return              list(period_model, period_sim)
estimate_ac_cycle <- function(timing, source_input, model_vals, sim_vals, start_time = 10) {
  # slice to times >= start_time
  idx <- which(timing >= start_time)
  t  <- timing[idx]
  m  <- model_vals[idx]
  s  <- sim_vals[idx]
  
  # helper: find upward mean‐crossings
  find_period <- function(t, y) {
    ym <- mean(y)
    # indices where we cross from below to above the mean
    cross_up <- which(diff(y < ym) == 1)
    times   <- t[cross_up]
    # periods between successive crossings
    per     <- diff(times)
    mean(per)
  }
  
  period_model <- find_period(t, m)
  period_sim   <- find_period(t, s)
  
  list(period_model = period_model,
       period_sim   = period_sim)
}


#' Compute phase shift between current and voltage for model & sim
#'
#' @param timing    numeric vector of time points
#' @param curr_vals numeric vector of current values
#' @param volt_vals numeric vector of voltage values
#' @param start_time time after which to begin analysis (defaults to 10)
#' @return          list(phase_model, phase_sim) in degrees
compute_phase_shift <- function(timing, 
                                model_current, model_voltage, 
                                sim_current,   sim_voltage, 
                                start_time = 10) {
  idx <- which(timing >= start_time)
  t   <- timing[idx]
  
  # helper: compute phase shift of y2 relative to y1
  # by comparing their first upward zero-crossing times
  phase_deg <- function(t, y1, y2) {
    th1 <- mean(y1) 
    th2 <- mean(y2)
    # find first up-crossing after start
    i1 <- which(diff(y1 < th1) == 1)[1]
    i2 <- which(diff(y2 < th2) == 1)[1]
    dt  <- t[i2] - t[i1]
    # estimate period from y1
    per <- 1e-9
    if (length(which(diff(y1 < th1) == 1)) >= 2) {
      times <- t[which(diff(y1 < th1) == 1)]
      per   <- mean(diff(times))
    }
    (dt / per) * 360
  }
  
  phase_model <- phase_deg(t, model_current[idx], model_voltage[idx])
  phase_sim   <- phase_deg(t, sim_current[idx],   sim_voltage[idx])
  
  list(phase_model = phase_model,
       phase_sim   = phase_sim)
}


#' Compare sim vs model for phase and amplitude
#'
#' @param timing        numeric vector of time points
#' @param model_current numeric vector of model current
#' @param model_voltage numeric vector of model voltage
#' @param sim_current   numeric vector of sim current
#' @param sim_voltage   numeric vector of sim voltage
#' @param start_time    time after which to begin analysis (defaults to 10)
#' @return              list with:
#'   - phase_diff_current:   phase(model_current vs sim_current)
#'   - phase_diff_voltage:   phase(model_voltage vs sim_voltage)
#'   - amp_diff_current:     max(sim_current)-max(model_current), and same for minima
#'   - amp_diff_voltage:     likewise for voltage
compare_model_sim <- function(timing, 
                              model_current, model_voltage, 
                              sim_current,   sim_voltage, 
                              start_time = 10) {
  idx <- which(timing >= start_time)
  t   <- timing[idx]
  
  # reuse phase shift helper (y2 relative to y1)
  phase_deg <- function(t, y1, y2) {
    th1 <- mean(y1); th2 <- mean(y2)
    i1  <- which(diff(y1 < th1) == 1)[1]
    i2  <- which(diff(y2 < th2) == 1)[1]
    dt  <- t[i2] - t[i1]
    per <- 1e-9
    ups <- which(diff(y1 < th1) == 1)
    if (length(ups) >= 2) per <- mean(diff(t[ups]))
    (dt / per) * 360
  }
  
  # phase diffs
  phase_diff_current <- phase_deg(t, model_current[idx], sim_current[idx])
  phase_diff_voltage <- phase_deg(t, model_voltage[idx], sim_voltage[idx])
  
  # amplitude diffs
  amp_diff_current <- c(
    max_diff = max(sim_current[idx]) - max(model_current[idx]),
    min_diff = min(sim_current[idx]) - min(model_current[idx])
  )
  amp_diff_voltage <- c(
    max_diff = max(sim_voltage[idx]) - max(model_voltage[idx]),
    min_diff = min(sim_voltage[idx]) - min(model_voltage[idx])
  )
  
  list(
    phase_diff_current = phase_diff_current,
    phase_diff_voltage = phase_diff_voltage,
    amp_diff_current   = amp_diff_current,
    amp_diff_voltage   = amp_diff_voltage
  )
}

analyze_transient_metrics <- function(timing, vc_model, vc_sim, t0, t1) {

  idx <- which(timing >= t0 & timing <= t1)
  t_slice <- timing[idx]
  v_model <- vc_model[idx]
  v_sim   <- vc_sim[idx]

  compute_metrics <- function(t, v) {
    v_final <- tail(v, 1)
    v_peak <- max(v)
    peak_index <- which.max(v)
    t_peak <- t[peak_index]
    overshoot <- (v_peak - v_final) / abs(v_final) * 100

    t_10 <- tryCatch(approx(v, t, xout = 0.1 * v_final)$y, error = function(e) NA)
    t_90 <- tryCatch(approx(v, t, xout = 0.9 * v_final)$y, error = function(e) NA)
    rise_time <- t_90 - t_10

    within_bounds <- abs(v - v_final) <= 0.02 * abs(v_final)
    settling_time <- NA
    for (i in seq_along(t)) {
      if (t[i] > t_peak && all(within_bounds[i:length(v)])) {
        settling_time <- t[i]
        break
      }
    }

    zeta <- if (v_peak > v_final && v_final != 0) {
      log_dec <- log(v_peak / v_final)
      log_dec / sqrt(pi^2 + log_dec^2)
    } else {
      NA
    }

    return(list(
      peak_time = t_peak,
      overshoot = overshoot,
      rise_time = rise_time,
      settling_time = settling_time,
      damping_ratio = zeta,
      final_value = v_final
    ))
  }

  m_model <- compute_metrics(t_slice, v_model)
  m_sim   <- compute_metrics(t_slice, v_sim)

  steady_state_error <- abs(m_model$final_value - m_sim$final_value) / abs(m_sim$final_value) * 100

  result <- data.frame(
    Metric = c("Peak Time", "Max Overshoot (%)", "Rise Time", "Settling Time", "Damping Ratio", "Steady-State Error (%)"),
    Model  = c(m_model$peak_time, m_model$overshoot, m_model$rise_time, m_model$settling_time, m_model$damping_ratio, NA),
    Sim    = c(m_sim$peak_time,   m_sim$overshoot,   m_sim$rise_time,   m_sim$settling_time,   m_sim$damping_ratio, NA),
    AbsError = c(
      abs(m_model$peak_time - m_sim$peak_time),
      abs(m_model$overshoot - m_sim$overshoot),
      abs(m_model$rise_time - m_sim$rise_time),
      abs(m_model$settling_time - m_sim$settling_time),
      abs(m_model$damping_ratio - m_sim$damping_ratio),
      NA
    ),
    RelError = c(
      abs(m_model$peak_time - m_sim$peak_time) / abs(m_sim$peak_time) * 100,
      abs(m_model$overshoot - m_sim$overshoot) / abs(m_sim$overshoot) * 100,
      abs(m_model$rise_time - m_sim$rise_time) / abs(m_sim$rise_time) * 100,
      abs(m_model$settling_time - m_sim$settling_time) / abs(m_sim$settling_time) * 100,
      abs(m_model$damping_ratio - m_sim$damping_ratio) / abs(m_sim$damping_ratio) * 100,
      steady_state_error
    )
  )

  return(result)
}
