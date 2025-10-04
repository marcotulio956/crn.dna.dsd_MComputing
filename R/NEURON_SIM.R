# Izhikevich neuron simulator and plotting helpers
library(ggplot2)
library(dplyr)

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