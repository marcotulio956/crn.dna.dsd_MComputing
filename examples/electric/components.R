rm(list = ls())

source('R/4domain_reactor.R')
source('R/ANALOG_GATE_LIB.R')
source('R/analysis.R')
source('R/crn_reactor.R')
source('R/dsd.R')
source('R/ELECTRO_LIB.R')
source('R/ELECTRO_SIM.R')
source('R/GATE_LIB.R')
source('R/io.R')
source('R/neuron_hjelmfelt.R')
source('R/parser.R')
source('R/util_functions.R')
source('R/metric_functions.R')
source('R/forced_concentrations.R')

jn <- function(...) { paste(..., sep = '') }

# ============================================================
# RLC COMPONENT TEST
#
# Assumes the following are already implemented:
#
#   Make_Capacitor_mermaid()
#   Make_Inductor_mermaid()
#   Make_Mul2In_Wang()
#   Make_Integrator_OishiYordanov()
#   Make_Derivative()
#   Make_Signed_Sum()
#
# And the circuit framework:
#
#   make_circuit()
#   circuit_add_gate()
#   React_circuit()
#
# ============================================================

library(ggplot2)
library(tidyr)
library(dplyr)


# ============================================================
# PARAMETERS
# ============================================================

rate        <- 1
C           <- 1
L           <- 1
R           <- 1

t_end       <- 50
timing      <- seq(0, t_end, by = 0.01)


# ============================================================
# HELPER: INITIAL CONDITIONS
# ============================================================

make_cap_ic <- function(C,
                        voltage_positive = 0,
                        voltage_negative = 0) {

  list(
    capacitance = C,

    voltage_positive = voltage_positive,
    voltage_negative = voltage_negative,

    # Q0 = C V0
    charge_positive = C * voltage_positive,
    charge_negative = C * voltage_negative,

    # Kept for compatibility with older gate interfaces
    current_positive = 0,
    current_negative = 0
  )
}


make_ind_ic <- function(L,
                        current_positive = 0,
                        current_negative = 0) {

  list(
    inductance = L,

    current_positive = current_positive,
    current_negative = current_negative,

    voltage_positive = 0,
    voltage_negative = 0,

    resistance = R
  )
}


# ============================================================
# 1. CAPACITOR ONLY
#
# Voltage is externally imposed:
#
#     v_C,in(t) = V0 sin(w t)
#
# and the component reconstructs
#
#     i_C = C dv/dt
#     q_C = integral(i dt)
#     v_C = q/C
#
# ============================================================

cap_name <- "C1"

cap_input <- list(
  voltage_positive = "V_C_in_p",
  voltage_negative = "V_C_in_n"
)

cap_output <- list(
  voltage_positive = "V_C_p",
  voltage_negative = "V_C_n",

  current_positive = "I_C_p",
  current_negative = "I_C_n"
)

cap_ic <- make_cap_ic(
  C = C,
  voltage_positive = 0,
  voltage_negative = 0
)


cap_gates <- Make_Capacitor_mermaid(
  name = cap_name,
  species_input = cap_input,
  species_output = cap_output,
  ic = cap_ic,
  rate = rate
)


cap_circuit <- make_compiled_circuit_from_gates(timing, cap_gates)


# ------------------------------------------------------------
# Capacitor voltage excitation
# ------------------------------------------------------------
forced_cap <- list(
  V_C_in_p = function(t) square_input(t, pulse_width = 30, period = 60, amplitude = 10)
)


cap_behavior <- React_circuit(
  cap_circuit,
  forced_concentrations = forced_cap
)

cap_behavior["V_C"] <- cap_behavior[, "V_C_p"] - cap_behavior[, "V_C_n"]
cap_behavior["I_C"] <- cap_behavior[, "I_C_p"] - cap_behavior[, "I_C_n"]

# ------------------------------------------------------------
# Plot capacitor
# ------------------------------------------------------------

plot_behavior(
  cap_behavior,
  species = c("V_C_in_p", "V_C", "I_C"),
)


# ============================================================
# 2. INDUCTOR ONLY
#
# Voltage is externally imposed through the source term so the
# component demonstrates:
#
#     v_L = L di/dt
#
# while the KVL block reconstructs the required inductor drop.
#
# ============================================================

ind_name <- "L1"

circuit <- make_circuit(timing)

ind_input <- list(
  voltage_source_positive = "V_SRC_p",
  voltage_source_negative = "V_SRC_n",

  voltage_capacitor_positive = "V_CAP_p",
  voltage_capacitor_negative = "V_CAP_n"
)

ind_output <- list(
  voltage_positive = "V_L_p",
  voltage_negative = "V_L_n",

  current_positive = "I_L_p",
  current_negative = "I_L_n"
)


ind_ic <- make_ind_ic(
  L = L,
  current_positive = 0,
  current_negative = 0
)


ind_gates <- Make_Inductor_mermaid(
  name = ind_name,
  species_input = ind_input,
  species_output = ind_output,
  ic = ind_ic,
  rate = rate
)


ind_circuit <- circuit_add_compile_gates(timing, ind_gates)


# ------------------------------------------------------------
# For an isolated inductor:
#
# v_L = L di/dt
#
# Choose a source waveform and keep the capacitor leg at zero.
# ------------------------------------------------------------

forced_ind <- list(

  V_SRC_p = function(t) {
    pmax(
      V0 * sin(w * t),
      0
    )
  },

  V_SRC_n = function(t) {
    pmax(
      -V0 * sin(w * t),
      0
    )
  },

  V_CAP_p = function(t) {
    0
  },

  V_CAP_n = function(t) {
    0
  }
)

ind_behavior <- React_circuit(
  ind_circuit,
  forced_concentrations = forced_ind
)

plot_behavior(
  ind_behavior,
  species = c("V_SRC_p", "V_SRC_n", "V_CAP_p", "V_CAP_n", "V_L_p", "V_L_n", "I_L_p", "I_L_n"),
)

# ============================================================
# 3. SERIES RLC
#
#             R
#     +---/\\/\\/\\/---+
#     |              |
#   Vin             C
#     |              |
#     +------ L -----+
#
# Series current:
#
#       i_R = i_C = i_L = i
#
# KVL:
#
#       Vin = VR + VC + VL
#
# VR = R i
#
# VC = 1/C integral(i dt)
#
# VL = L di/dt
#
# therefore:
#
#       Vin = R i + (1/C) integral(i dt) + L di/dt
#
# ============================================================


# ------------------------------------------------------------
# Shared species
# ------------------------------------------------------------

rlc_current <- list(
  positive = "I_RLC_p",
  negative = "I_RLC_n"
)

rlc_voltage_cap <- list(
  positive = "V_C_RLC_p",
  negative = "V_C_RLC_n"
)

rlc_voltage_ind <- list(
  positive = "V_L_RLC_p",
  negative = "V_L_RLC_n"
)


# ------------------------------------------------------------
# CAPACITOR
# ------------------------------------------------------------

rlc_cap_input <- list(
  voltage_positive = "V_IN_p",
  voltage_negative = "V_IN_n"
)

rlc_cap_output <- list(
  voltage_positive = rlc_voltage_cap$positive,
  voltage_negative = rlc_voltage_cap$negative,

  current_positive = rlc_current$positive,
  current_negative = rlc_current$negative
)


rlc_cap_ic <- make_cap_ic(
  C = C,
  voltage_positive = 0,
  voltage_negative = 0
)


rlc_cap_gates <- Make_Capacitor_mermaid(
  name = "RLC_C",
  species_input = rlc_cap_input,
  species_output = rlc_cap_output,
  ic = rlc_cap_ic,
  rate = rate
)


# ------------------------------------------------------------
# INDUCTOR
#
# The inductor sees:
#
#   Vin
#   VC
#   VR
#
# and determines the required VL through KVL.
# ------------------------------------------------------------

rlc_ind_input <- list(

  voltage_source_positive = "V_IN_p",
  voltage_source_negative = "V_IN_n",

  voltage_capacitor_positive = rlc_voltage_cap$positive,
  voltage_capacitor_negative = rlc_voltage_cap$negative,

  current_positive = rlc_current$positive,
  current_negative = rlc_current$negative
)


rlc_ind_output <- list(

  voltage_positive = rlc_voltage_ind$positive,
  voltage_negative = rlc_voltage_ind$negative,

  current_positive = rlc_current$positive,
  current_negative = rlc_current$negative
)


rlc_ind_ic <- make_ind_ic(
  L = L,
  current_positive = 0,
  current_negative = 0
)


rlc_ind_gates <- Make_Inductor_mermaid(
  name = "RLC_L",
  species_input = rlc_ind_input,
  species_output = rlc_ind_output,
  ic = rlc_ind_ic,
  rate = rate
)



# ------------------------------------------------------------
# RLC CIRCUIT
# ------------------------------------------------------------

rlc_circuit <- make_compiled_circuit_from_gates(
  timing,
  c(rlc_cap_gates, rlc_ind_gates)
)



# ------------------------------------------------------------
# INPUT VOLTAGE
#
# Only external input:
#
#     Vin(t)
#
# Everything else is generated internally.
# ------------------------------------------------------------

V0 <- 1
w_rlc <- 0.5

forced_rlc <- list(

  V_IN_p = function(t) {

    pmax(
      V0 * sin(w_rlc * t),
      0
    )

  },

  V_IN_n = function(t) {

    pmax(
      -V0 * sin(w_rlc * t),
      0
    )

  }
)



# ------------------------------------------------------------
# SIMULATION
# ------------------------------------------------------------

rlc_behavior <- React_circuit(
  rlc_circuit,
  forced_concentrations = forced_rlc
)


# ============================================================
# PROCESS RESULTS
# ============================================================

rlc_results <- as.data.frame(rlc_behavior$data)


rlc_results$I <- (
  rlc_results$I_RLC_p -
  rlc_results$I_RLC_n
)


rlc_results$V_C <- (
  rlc_results$V_C_RLC_p -
  rlc_results$V_C_RLC_n
)


rlc_results$V_L <- (
  rlc_results$V_L_RLC_p -
  rlc_results$V_L_RLC_n
)


rlc_results$V_IN <- (
  rlc_results$V_IN_p -
  rlc_results$V_IN_n
)


# Reconstruct resistor voltage
rlc_results$V_R <- R * rlc_results$I


# KVL residual
rlc_results$KVL_error <-
  rlc_results$V_IN -
  (
    rlc_results$V_R +
    rlc_results$V_C +
    rlc_results$V_L
  )


# ============================================================
# RLC PLOT
# ============================================================

p_rlc <- ggplot(
  rlc_results,
  aes(x = time)
) +

  geom_line(
    aes(y = V_IN),
    linewidth = 1
  ) +

  geom_line(
    aes(y = V_R),
    linewidth = 0.8,
    linetype = "dashed"
  ) +

  geom_line(
    aes(y = V_C),
    linewidth = 0.8,
    linetype = "dotted"
  ) +

  geom_line(
    aes(y = V_L),
    linewidth = 0.8
  ) +

  labs(
    title = "Series RLC Circuit",
    subtitle = "v_in = v_R + v_C + v_L",
    x = "Time",
    y = "Voltage"
  ) +

  theme_minimal()

print(p_rlc)



# ============================================================
# RLC CURRENT
# ============================================================

p_current <- ggplot(
  rlc_results,
  aes(
    x = time,
    y = I
  )
) +

  geom_line(
    linewidth = 1
  ) +

  labs(
    title = "Series RLC Current",
    x = "Time",
    y = "Current"
  ) +

  theme_minimal()

print(p_current)



# ============================================================
# KVL ERROR
#
# This should converge toward zero if the CRN composition is
# implementing the circuit equations correctly.
# ============================================================

p_kvl <- ggplot(
  rlc_results,
  aes(
    x = time,
    y = KVL_error
  )
) +

  geom_line(
    linewidth = 1
  ) +

  labs(
    title = "RLC KVL Residual",
    subtitle = "Vin - (VR + VC + VL)",
    x = "Time",
    y = "KVL error"
  ) +

  theme_minimal()

print(p_kvl)