library(diffeqr)
library(JuliaCall)
julia_setup()

de <- diffeqr::diffeq_setup() 

# Lotka-Volterra equations
lv_model <- function(t, y, parms) {
  x <- y[1]  # prey
  y2 <- y[2] # predator
  
  alpha <- 1.5
  beta  <- 1.0
  delta <- 1.0
  gamma <- 3.0
  
  dx <- alpha * x - beta * x * y2
  dy <- delta * x * y2 - gamma * y2
  
  list(c(dx, dy))
}

# (u, p, t) instead of (t, y, parms)
lv_diffeqr <- function(u, p, t) {
  result <- lv_model(t, u, NULL)
  return(unlist(result))
}


y0 <- c(10, 5)             # initial populations
times <- seq(0, 20, by=0.1)

sol <- de::solve_ode(
  times = times,
  y = y0,
  func = lv_diffeqr,
  method = "Tsit5"  # fast Julia solver
)
library(diffeqr)

# Setup Julia bridge
de <- diffeq_setup()

# Lotka–Volterra (Julia-compatible signature)
lv_diffeqr <- function(u, p, t) {
  x <- u[1]  # prey
  y <- u[2]  # predator
  
  alpha <- 1.5
  beta  <- 1.0
  delta <- 1.0
  gamma <- 3.0
  
  dx <- alpha * x - beta * x * y
  dy <- delta * x * y - gamma * y
  
  c(dx, dy)
}

# Initial condition
u0 <- c(10.0, 5.0)
tspan <- c(0.0, 20.0)

# Solve ODE
prob <- de$ODEProblem(lv_diffeqr, u0, tspan)
sol  <- de$solve(prob, de$Tsit5(), saveat = 0.1)

# Extract solution
times  <- sol$t
values <- do.call(rbind, sol$u)

det_result <- data.frame(
  time = times,
  prey = values[,1],
  predator = values[,2]
)

# Plot deterministic
plot(det_result$time, det_result$prey, type="l", col="blue",
     ylim=range(det_result[,2:3]),
     xlab="Time", ylab="Population",
     main="Lotka–Volterra: Deterministic vs Stochastic")

lines(det_result$time, det_result$predator, col="red")


result <- cbind(time = sol$t, do.call(rbind, sol$u))
colnames(result) <- c("time", "prey", "predator")

plot(result[, "time"], result[, "prey"], type = "l", col = "blue",
     ylim = range(result[, 2:3]),
     xlab = "Time", ylab = "Population",
     main = "Lotka-Volterra (Julia via diffeqr)")

lines(result[, "time"], result[, "predator"], col = "red")

legend("topright", legend = c("Prey", "Predator"),
       col = c("blue", "red"), lty = 1)