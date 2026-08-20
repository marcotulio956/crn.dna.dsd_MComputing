# Load the libraries
# DO NOT USE library(DNAr)
# DO NOT USE library(DNArLogic)
# DO NOT USE library(DNArAnalog)

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

jn <- function(...) { paste(..., sep = '') }


Make_Generic <- function(timing) {
  circuit <- make_circuit(timing)
  
  g_dalchau <- Make_Oscillator_Dalchau('osc', 'y', 'z', 'c1il_v1p', 9, 8, 5, 10e-2)
  
  c1 <- Make_Capacitor_Component(1, L * 1e-2)
  
  c1$il$voltage_positive <- 'c1il_v1p'
  
  # - Electro
  e1_gates <- Make_Circuit_Capacitor(c1$name, c1$il, c1$ol, c1$ic, 10000)
  
  # add2circuit
  circuit <- circuit_add_gate(circuit, g_dalchau)
  circuit <- circuit_add_compile_gates(circuit, e1_gates)
  
  return (circuit)
}

t0 = 0
t1 = 20
points = (t1 - t0) * 50 # Using 50 time points
time_grid  <- seq(t0, t1, length.out = points) # Using 50 time points

df_opt <- React_circuit({
  circuit <- DNArLogic::make_circuit(time_grid)
  circuit <- circuit_add_gate(
    circuit,
    Make_Oscillator_Dalchau('osc', 'v3p', 'v2p', 'v1p', 9, 8, 5, 10e-2)
  )
  circuit
})

x1_sim_opt <- df_opt[['v1p']]
x2_sim_opt <- df_opt[['v2p']]
x3_sim_opt <- df_opt[['v3p']]

v_target <- simulate_sin(time_grid)

plot(time_grid, v_target, type = "l", col = "green", lwd=2, lty = 2,
     xlab="Time (S)", ylab="Concentratin (M)",
     main="Generating a Voltage Source for Sinusoidal inputs DSD Target=2.5sin(2π/5 t+5)+7.5[V]", xlim=c(0,15), ylim=c(5,10))
lines(time_grid, x1_sim_opt, col = "blue", lwd=3, lty=1)
lines(time_grid, x2_sim_opt, col = "red", lwd=1, lty=1)
lines(time_grid, x3_sim_opt, col = "yellow", lwd=1, lty=1)

legend("bottomright",
       legend=c("V(S)","v1p","v2p", "v3p"),
       col=c("green","blue", 'red', 'yellow'), lwd=2, lty=c(2,1, 1, 1)) # c("purple","red", 'green', 'blue'), lwd=2, lty=c(2,1, 1, 1))




