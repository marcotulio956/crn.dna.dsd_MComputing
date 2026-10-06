# =============================================================================
# Example: Generalized Time-Varying Reaction Rates
# =============================================================================

rm(list = ls())

source('R/crn_reactor.R')
source('R/parser.R')
source('R/io.R')

library(ggplot2)
library(tidyr)
library(dplyr)

# =============================================================================
# Example 1: Sinusoidally Varying Reaction Rate
# =============================================================================

cat("Example 1: Sinusoidally varying reaction rate\n")
cat("==============================================\n\n")

species_ex1 <- c('A', 'B')

ci_ex1 <- c(
  A = 10,
  B = 0
)

reactions_ex1 <- c(
  'A -> B'
)

# k(t)
k0 <- 0.1
period <- 20

rate_function_sin <- function(t, y, species) {
  
  k0 * (1 + 0.5 * sin(2 * pi * t / period))
}

ki_ex1 <- list(
  rate_function_sin
)

t_ex1 <- seq(0, 100, length.out = 500)

behavior_ex1 <- react2(
  species = species_ex1,
  ci = ci_ex1,
  reactions = reactions_ex1,
  ki = ki_ex1,
  t = t_ex1
)

plot_df_ex1 <- behavior_ex1 %>%
  pivot_longer(
    cols = -time,
    names_to = "species",
    values_to = "concentration"
  )

rate_values <- sapply(t_ex1, function(tt) {
  rate_function_sin(tt, NULL, NULL)
})

rate_df <- data.frame(
  time = t_ex1,
  species = "Rate k(t)",
  concentration = rate_values * 50
)

plot_df_ex1_combined <- rbind(
  plot_df_ex1,
  rate_df
)

p1 <- ggplot(
  plot_df_ex1_combined,
  aes(x = time, y = concentration, color = species)
) +
  geom_line(linewidth = 1) +
  labs(
    title = "Time-Varying Reaction Rate: A -> B",
    subtitle = "k(t) = 0.1 * (1 + 0.5 * sin(2πt/20))",
    x = "Time",
    y = "Concentration / Rate (scaled)"
  ) +
  theme_minimal()

print(p1)

# =============================================================================
# Example 2: Step Change in Reaction Rate
# =============================================================================

cat("Example 2: Step change in reaction rate\n")
cat("========================================\n\n")

species_ex2 <- c('A', 'B')

ci_ex2 <- c(
  A = 10,
  B = 0
)

reactions_ex2 <- c(
  'A -> B'
)

rate_function_step <- function(t, y, species) {
  
  if(t < 50) {
    0.05
  } else {
    0.15
  }
}

ki_ex2 <- list(
  rate_function_step
)

t_ex2 <- seq(0, 100, length.out = 500)

behavior_ex2 <- react2(
  species = species_ex2,
  ci = ci_ex2,
  reactions = reactions_ex2,
  ki = ki_ex2,
  t = t_ex2
)

plot_df_ex2 <- behavior_ex2 %>%
  pivot_longer(
    cols = -time,
    names_to = "species",
    values_to = "concentration"
  )

p2 <- ggplot(
  plot_df_ex2,
  aes(x = time, y = concentration, color = species)
) +
  geom_line(linewidth = 1) +
  geom_vline(
    xintercept = 50,
    linetype = "dashed",
    alpha = 0.5
  ) +
  labs(
    title = "Step Change in Reaction Rate",
    subtitle = "k(t): 0.05 -> 0.15 at t = 50",
    x = "Time",
    y = "Concentration"
  ) +
  theme_minimal()

print(p2)

# =============================================================================
# Example 3: Mixed Constant + Dynamic Rates
# =============================================================================

cat("Example 3: Mixed constant and dynamic rates\n")
cat("===========================================\n\n")

species_ex3 <- c(
  'A',
  'B',
  'C',
  'D'
)

ci_ex3 <- c(
  A = 5,
  B = 5,
  C = 0,
  D = 0
)

reactions_ex3 <- c(
  'A + B -> C',
  'C -> D'
)

# Linearly increasing kinetic coefficient
rate_varying <- function(t, y, species) {
  
  0.01 * (1 + 0.01 * t)
}

ki_ex3 <- list(
  rate_varying,
  0.05
)

t_ex3 <- seq(0, 200, length.out = 500)

behavior_ex3 <- react2(
  species = species_ex3,
  ci = ci_ex3,
  reactions = reactions_ex3,
  ki = ki_ex3,
  t = t_ex3
)

plot_df_ex3 <- behavior_ex3 %>%
  pivot_longer(
    cols = -time,
    names_to = "species",
    values_to = "concentration"
  )

p3 <- ggplot(
  plot_df_ex3,
  aes(x = time, y = concentration, color = species)
) +
  geom_line(linewidth = 1) +
  labs(
    title = "Mixed Constant and Dynamic Rates",
    subtitle = "A+B->C uses dynamic k(t), C->D constant",
    x = "Time",
    y = "Concentration"
  ) +
  theme_minimal()

print(p3)

# =============================================================================
# Example 4: State-Dependent Kinetics
# =============================================================================

cat("Example 4: State-dependent kinetics\n")
cat("===================================\n\n")

# Autocatalytic reaction:
#
# A + B -> 2B
#
# with rate:
#
# k(B) = 0.01 + 0.2 * B/(1+B)
#
# demonstrating generalized kinetics.

species_ex4 <- c(
  'A',
  'B'
)

ci_ex4 <- c(
  A = 10,
  B = 0.1
)

reactions_ex4 <- c(
  'A + B -> 2 B'
)

rate_autocat <- function(t, y, species) {
  
  B <- y["B"]
  
  0.01 + 0.2 * B / (1 + B)
}

ki_ex4 <- list(
  rate_autocat
)

t_ex4 <- seq(0, 100, length.out = 500)

behavior_ex4 <- react2(
  species = species_ex4,
  ci = ci_ex4,
  reactions = reactions_ex4,
  ki = ki_ex4,
  t = t_ex4
)

plot_df_ex4 <- behavior_ex4  %>%
  pivot_longer(
    cols = -time,
    names_to = "species",
    values_to = "concentration"
  )

p4 <- ggplot(
  plot_df_ex4,
  aes(x = time, y = concentration, color = species)
) +
  geom_line(linewidth = 1) +
  labs(
    title = "State-Dependent Kinetics",
    subtitle = "Autocatalytic rate depends on B concentration",
    x = "Time",
    y = "Concentration"
  ) +
  theme_minimal()

print(p4)

# =============================================================================
# Example 5: Morris-Lecar Style Voltage Dependence
# =============================================================================

cat("Example 5: Voltage-dependent neuron kinetics\n")
cat("============================================\n\n")

species_ex5 <- c(
  'Vp',
  'Vm'
)

ci_ex5 <- c(
  Vp = 2,
  Vm = 0
)

reactions_ex5 <- c(
  'Vp -> Vm'
)

M_inf <- function(V) {
  
  0.5 * (1 + tanh((V - 1)/15))
}

rate_voltage <- function(t, y, species) {
  
  V <- y["Vp"] - y["Vm"]
  
  0.1 * M_inf(V)
}

ki_ex5 <- list(
  rate_voltage
)

t_ex5 <- seq(0, 100, length.out = 500)

behavior_ex5 <- react2(
  species = species_ex5,
  ci = ci_ex5,
  reactions = reactions_ex5,
  ki = ki_ex5,
  t = t_ex5
)

plot_df_ex5 <- behavior_ex5 %>%
  pivot_longer(
    cols = -time,
    names_to = "species",
    values_to = "concentration"
  )

p5 <- ggplot(
  plot_df_ex5,
  aes(x = time, y = concentration, color = species)
) +
  geom_line(linewidth = 1) +
  labs(
    title = "Voltage-Dependent Kinetics",
    subtitle = "Rate depends on V = Vp - Vm",
    x = "Time",
    y = "Concentration"
  ) +
  theme_minimal()

print(p5)

# =============================================================================
# Summary
# =============================================================================

cat("\n========================================================\n")
cat("Generalized Kinetics Summary\n")
cat("========================================================\n\n")

cat("Supported kinetic forms:\n\n")

cat("1. Constant:\n")
cat("   ki = list(0.1)\n\n")

cat("2. Time-dependent:\n")
cat("   function(t, y, species) 0.1 * sin(t)\n\n")

cat("3. State-dependent:\n")
cat("   function(t, y, species) y['A']/(1+y['A'])\n\n")

cat("4. Hybrid:\n")
cat("   function(t, y, species) sin(t)*y['A']\n\n")

cat("All are automatically combined with mass-action kinetics.\n\n")