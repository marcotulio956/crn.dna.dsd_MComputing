benchmark_solver <- function(engine, repetitions = 20) {
  
  times <- numeric(repetitions)
  
  for (i in seq_len(repetitions)) {
    
    gc()
    
    t <- system.time({
      
      react2(
        species = circuit$species,
        ci = circuit$ci,
        reactions = circuit$reactions,
        ki = circuit$ki,
        t = circuit$t,
        engine = engine,
        verbose = FALSE
      )
      
    })
    
    times[i] <- t["elapsed"]
  }
  
  data.frame(
    solver = engine,
    mean = mean(times),
    median = median(times),
    min = min(times),
    max = max(times),
    sd = sd(times)
  )
}

# Warm-up for diffeqr
react2(
  species = circuit$species,
  ci = circuit$ci,
  reactions = circuit$reactions,
  ki = circuit$ki,
  t = circuit$t,
  engine = "diffeqr",
  verbose = FALSE
)

rbind(
  benchmark_solver("desolve"),
  benchmark_solver("diffeqr")
)

## result

# solver    mean median   min   max       sd
# 1 desolve 61.1945  61.47 48.89 73.81 6.885816
# 2 diffeqr  9.4205   9.28  7.42 14.67 1.541651