# =============================================================================
# Example: Time-Varying Reaction Rates
# =============================================================================
# This example demonstrates reactions with time-varying rates, where the 
# rate constant changes over time according to a function f(t).
#
# Example: A + B -f(t)-> C
# where f(t) determines how many moles per second react at time t
# =============================================================================

# Load required libraries
rm(list = ls())

source('R/crn_reactor.R')
source('R/parser.R')
source('R/io.R')

library(ggplot2)
library(tidyr)

# =============================================================================
# Example 1: Sinusoidally Varying Reaction Rate
# =============================================================================
cat("Example 1: Sinusoidally varying reaction rate\n")
cat("==============================================\n\n")

# Simple reaction: A -> B with time-varying rate
# Rate oscillates: k(t) = k0 * (1 + 0.5 * sin(2*pi*t/period))

species_ex1 <- c('A', 'B')
ci_ex1 <- c(10, 0)
reactions_ex1 <- c('A -> B')

# Define time-varying rate function
k0 <- 0.1
period <- 20
rate_function_sin <- function(t) {
  k0 * (1 + 0.5 * sin(2 * pi * t / period))
}

# Can pass as a list to allow mixing constants and functions
ki_ex1 <- list(rate_function_sin)

t_ex1 <- seq(0, 100, length.out = 500)

# Run simulation
behavior_ex1 <- react(
  species = species_ex1,
  ci = ci_ex1,
  reactions = reactions_ex1,
  ki = ki_ex1,
  t = t_ex1
)

# Plot results
plot_df_ex1 <- behavior_ex1 %>%
  pivot_longer(cols = -time, names_to = "species", values_to = "concentration")

# Also compute and plot the rate over time for visualization
rate_values <- sapply(t_ex1, rate_function_sin)
rate_df <- data.frame(time = t_ex1, species = "Rate k(t)", concentration = rate_values * 50) # Scaled for visibility

plot_df_ex1_combined <- rbind(plot_df_ex1, rate_df)

p1 <- ggplot(plot_df_ex1_combined, aes(x = time, y = concentration, color = species)) +
  geom_line(linewidth = 1) +
  labs(title = "Time-Varying Reaction Rate: A -> B",
       subtitle = "Rate k(t) = 0.1 * (1 + 0.5 * sin(2πt/20))",
       x = "Time", y = "Concentration (M) / Rate (scaled)") +
  theme_minimal()

print(p1)

cat("\nNote: Rate oscillates between 0.05 and 0.15\n")
cat("A decays faster when rate is high, slower when rate is low\n\n")

# =============================================================================
# Example 2: Step Change in Reaction Rate
# =============================================================================
cat("Example 2: Step change in reaction rate\n")
cat("========================================\n\n")

# Reaction: A -> B with rate that increases at t = 50

species_ex2 <- c('A', 'B')
ci_ex2 <- c(10, 0)
reactions_ex2 <- c('A -> B')

# Rate doubles at t = 50
rate_function_step <- function(t) {
  if (t < 50) {
    return(0.05)
  } else {
    return(0.15)
  }
}

ki_ex2 <- list(rate_function_step)
t_ex2 <- seq(0, 100, length.out = 500)

behavior_ex2 <- react(
  species = species_ex2,
  ci = ci_ex2,
  reactions = reactions_ex2,
  ki = ki_ex2,
  t = t_ex2
)

plot_df_ex2 <- behavior_ex2 %>%
  pivot_longer(cols = -time, names_to = "species", values_to = "concentration")

p2 <- ggplot(plot_df_ex2, aes(x = time, y = concentration, color = species)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 50, linetype = "dashed", alpha = 0.5) +
  annotate("text", x = 50, y = 8, label = "Rate change", hjust = -0.1) +
  labs(title = "Step Change in Reaction Rate: A -> B",
       subtitle = "Rate increases from 0.05 to 0.15 at t = 50",
       x = "Time", y = "Concentration (M)") +
  theme_minimal()

print(p2)

cat("\nNote: Steeper decay of A after t = 50 due to higher rate\n\n")

# =============================================================================
# Example 3: Multiple Reactions with Mixed Constant and Time-Varying Rates
# =============================================================================
cat("Example 3: Mixed constant and time-varying rates\n")
cat("================================================\n\n")

# Network: A + B -> C (time-varying)
#          C -> D (constant)

species_ex3 <- c('A', 'B', 'C', 'D')
ci_ex3 <- c(5, 5, 0, 0)
reactions_ex3 <- c('A + B -> C', 'C -> D')

# First reaction has time-varying rate (increases linearly)
# Second reaction has constant rate
rate_varying <- function(t) {
  0.01 * (1 + 0.01 * t)  # Gradually increases
}

ki_ex3 <- list(
  rate_varying,  # Time-varying for A + B -> C
  0.05           # Constant for C -> D
)

t_ex3 <- seq(0, 200, length.out = 500)

behavior_ex3 <- react(
  species = species_ex3,
  ci = ci_ex3,
  reactions = reactions_ex3,
  ki = ki_ex3,
  t = t_ex3
)

plot_df_ex3 <- behavior_ex3 %>%
  pivot_longer(cols = -time, names_to = "species", values_to = "concentration")

p3 <- ggplot(plot_df_ex3, aes(x = time, y = concentration, color = species)) +
  geom_line(linewidth = 1) +
  labs(title = "Mixed Rate Types: A + B -> C -> D",
       subtitle = "A+B->C has linearly increasing rate, C->D has constant rate",
       x = "Time", y = "Concentration (M)") +
  theme_minimal()

print(p3)

cat("\nNote: C production accelerates over time due to increasing rate of A+B->C\n\n")

# =============================================================================
# Example 4: Light-Dependent Reaction (Photocatalysis)
# =============================================================================
cat("Example 4: Light-dependent photocatalytic reaction\n")
cat("===================================================\n\n")

# Simulating a photocatalytic reaction where rate depends on light intensity
# Light follows a day/night cycle

species_ex4 <- c('Reactant', 'Product')
ci_ex4 <- c(10, 0)
reactions_ex4 <- c('Reactant -> Product')

# Rate varies with day/night cycle (period = 24 hours)
# During day: high rate, during night: low rate
day_length <- 24
rate_photocat <- function(t) {
  # Time of day (0-24 hours)
  time_of_day <- (t %% day_length)
  
  # Light intensity follows a sine curve (day: high, night: low)
  # Day from 6h to 18h (12 hours of light)
  if (time_of_day >= 6 && time_of_day <= 18) {
    # Day time - use sine for smooth transition
    light_intensity <- sin(pi * (time_of_day - 6) / 12)
  } else {
    # Night time - minimal activity
    light_intensity <- 0.1
  }
  
  # Rate proportional to light intensity
  k_max <- 0.2
  k_min <- 0.01
  return(k_min + (k_max - k_min) * light_intensity)
}

ki_ex4 <- list(rate_photocat)
t_ex4 <- seq(0, 72, length.out = 500)  # 3 days

behavior_ex4 <- react(
  species = species_ex4,
  ci = ci_ex4,
  reactions = reactions_ex4,
  ki = ki_ex4,
  t = t_ex4
)

plot_df_ex4 <- behavior_ex4 %>%
  pivot_longer(cols = -time, names_to = "species", values_to = "concentration")

# Add light intensity curve for reference
light_values <- sapply(t_ex4, function(t) {
  rate_photocat(t) * 30  # Scaled for visibility
})
light_df <- data.frame(time = t_ex4, species = "Light Rate (scaled)", concentration = light_values)

plot_df_ex4_combined <- rbind(plot_df_ex4, light_df)

p4 <- ggplot(plot_df_ex4_combined, aes(x = time, y = concentration, color = species)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = c(0, 24, 48, 72), linetype = "dotted", alpha = 0.3) +
  labs(title = "Photocatalytic Reaction with Day/Night Cycles",
       subtitle = "Reaction rate varies with light intensity (24h period)",
       x = "Time (hours)", y = "Concentration (M) / Rate (scaled)") +
  theme_minimal()

print(p4)

cat("\nNote: Product formation accelerates during day, slows during night\n")
cat("Shows clear day/night pattern over 3 days\n\n")

# =============================================================================
# Summary
# =============================================================================
cat("=============================================================================\n")
cat("Time-Varying Reaction Rates Summary\n")
cat("=============================================================================\n\n")

cat("Time-varying rates enable modeling of:\n")
cat("1. Oscillating catalytic activity (Example 1)\n")
cat("2. Step changes in conditions (Example 2)\n")
cat("3. Complex networks with different rate behaviors (Example 3)\n")
cat("4. Environmental dependencies like light/temperature (Example 4)\n\n")

cat("Usage: Pass functions instead of constants for ki parameter:\n")
cat("  ki = list(function(t) k0 * (1 + sin(t)), 0.5)  # Mixed\n")
cat("  ki = c(0.1, 0.2)                               # All constant (traditional)\n")
cat("  ki = list(function(t) 0.1 * t)                 # All time-varying\n\n")
