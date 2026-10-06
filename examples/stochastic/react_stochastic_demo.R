rm(list = ls())

source('R/parser.R')
source('R/util_functions.R')
source('R/crn_reactor.R')

library(GillespieSSA2)
library(ggplot2)
library(tidyr)

# =========================
# CRN definition
# =========================

crn <- c()
crn$species <- c("X", "Y")

crn$reactions <- c(
  "X -> 2X",
  "X + Y -> 2Y",
  "Y -> 0"
)

crn$ki <- c(1.5, 1.0, 3.0)

crn$ci <- c(10, 5)

times <- seq(0, 20, by = 0.1)
crn$t <- times

# =========================
# Run stochastic simulation
# =========================

stoch_result <- React_stochastic(
  crn, volume = 100
)


plot(stoch_result$time, stoch_result$X,
     type = "l", col = "blue",
     ylim = range(stoch_result[, -1]),
     xlab = "Time", ylab = "Population",
     main = "Stochastic Lotka–Volterra (SSA)")

# Add
lines(stoch_result$time, stoch_result$Y,
      col = "red")

legend("topright",
       legend = c("Prey (SSA)", "Predator (SSA)"),
       col = c("blue", "red"),
       lty = 1)