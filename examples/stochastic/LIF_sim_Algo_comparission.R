rm(list = ls())


library(ggplot2)


simulate_gillespie <- function(Tmax, params, V0, s0) {
  with(params, {
    t <- 0
    V <- V0
    s <- s0
    
    out <- data.frame(t = t, V = V, s = s)
    
    while (t < Tmax) {
      
      # Propensities (CRN-inspired)
      a1 <- k_rest                  # baseline production
      a2 <- k_leak * V             # leak
      a3 <- beta * V               # spike activation (phi(V) ≈ V)
      a4 <- gamma * s              # decay of s
      a5 <- alpha * s * V          # nonlinear feedback term
      
      a0 <- a1 + a2 + a3 + a4 + a5
      if (a0 <= 0) break
      
      # Time step
      tau <- rexp(1, rate = a0)
      t <- t + tau
      
      # Reaction selection
      r <- sample(1:5, size = 1, prob = c(a1,a2,a3,a4,a5))
      
      # State updates
      if (r == 1) V <- V + 1
      if (r == 2) V <- max(V - 1, 0)
      if (r == 3) s <- s + 1
      if (r == 4) s <- max(s - 1, 0)
      if (r == 5) V <- max(V - 1, 0)  # feedback reduces V
      
      out <- rbind(out, data.frame(t = t, V = V, s = s))
    }
    
    return(out)
  })
}

simulate_tau <- function(Tmax, dt, params, V0, s0) {
  with(params, {
    t <- seq(0, Tmax, by = dt)
    V <- numeric(length(t))
    s <- numeric(length(t))
    
    V[1] <- V0
    s[1] <- s0
    
    for (i in 1:(length(t)-1)) {
      
      # Propensities
      a1 <- k_rest
      a2 <- k_leak * V[i]
      a3 <- beta * V[i]
      a4 <- gamma * s[i]
      a5 <- alpha * s[i] * V[i]
      
      # Poisson sampling
      K1 <- rpois(1, a1 * dt)
      K2 <- rpois(1, a2 * dt)
      K3 <- rpois(1, a3 * dt)
      K4 <- rpois(1, a4 * dt)
      K5 <- rpois(1, a5 * dt)
      
      # Updates
      V[i+1] <- max(V[i] + K1 - K2 - K5, 0)
      s[i+1] <- max(s[i] + K3 - K4, 0)
    }
    
    return(data.frame(t = t, V = V, s = s))
  })
}

simulate_ou <- function(Tmax, dt, params, V0, s0) {
  with(params, {
    t <- seq(0, Tmax, by = dt)
    V <- numeric(length(t))
    s <- numeric(length(t))
    
    V[1] <- V0
    s[1] <- s0
    
    for (i in 1:(length(t)-1)) {
      
      # Drift terms (your equations)
      dV_det <- (-(V[i] - V_rest) + R * I)/tau_m -
                alpha * s[i] * (V[i] - V_reset)
      
      ds_det <- beta * V[i] - gamma * s[i]
      
      # Noise (diffusion approximation)
      dV_stoch <- sigma_V * rnorm(1, 0, sqrt(dt))
      ds_stoch <- sigma_s * rnorm(1, 0, sqrt(dt))
      
      # Update
      V[i+1] <- V[i] + dV_det * dt + dV_stoch
      s[i+1] <- s[i] + ds_det * dt + ds_stoch
    }
    
    return(data.frame(t = t, V = V, s = s))
  })
}

params <- list(
  k_rest = 2,
  k_leak = 0.1,
  beta = 0.05,
  gamma = 0.1,
  alpha = 0.01,
  V_rest = 10,
  V_reset = 5,
  R = 1,
  I = 2,
  tau_m = 10,
  sigma_V = 0.5,
  sigma_s = 0.3
)



# Run simulations first (using previous functions)
g <- simulate_gillespie(50, params, 10, 0)
tau <- simulate_tau(50, 0.1, params, 10, 0)
ou <- simulate_ou(50, 0.01, params, 10, 0)

g$method <- "Gillespie"
tau$method <- "Tau-leaping"
ou$method <- "OU"

# Combine
df <- rbind(g, tau, ou)

# Plot V
ggplot(df, aes(x = t, y = V, linetype = method)) +
  geom_line() +
  labs(title = "Comparison of V(t) across simulation methods",
       x = "Time", y = "Membrane potential V") +
  theme_minimal()

# Plot s
ggplot(df, aes(x = t, y = s, linetype = method)) +
  geom_line() +
  labs(title = "Comparison of s(t) across simulation methods",
       x = "Time", y = "Spike variable s") +
  theme_minimal()