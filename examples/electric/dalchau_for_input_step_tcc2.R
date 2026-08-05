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

jn <- function(...) { paste(..., sep = '') }

library(ggplot2) # plot()
library(dplyr) # mutate()

t0 <- 0
t1 <- 40
npoints <- (t1 - t0) * 100
time_grid <- seq(t0, t1, length.out = npoints)


error_for_params <- function(p) {
  p1 <- p[1:4]
  p2 <- p[5:10]
  
  circuit <- DNArLogic::make_circuit(time_grid)
  
  g_dalchau <- Make_Oscillator_Dalchau(
    'sin', 'x', 'v1p', 'z',
    p1[1], p1[2], p1[3], p1[4]
  )
  c_comp <- Make_Mux2_balanced(
    'mux1',
    'x', 'v1p',
    'low', 'high',
    'comp_out',
    p2[1], p2[2],
    p2[3], p2[4],
    p2[5], p2[6]
  )
  
  circuit <- circuit_add_gate(circuit, g_dalchau)
  circuit <- circuit_add_gate(circuit, c_comp)
  
  # (b) Simula pelo CRN
  df <- React_circuit(circuit)
  # Extrai v1p (assumindo que `React_circuit` já tem coluna `v1p`)
  v_sim <- df[['v1p']]
  
  # (c) Erro SSE
  # (caso os comprimentos diferirem, ajuste por interp. mas aqui assumimos mesmo grid)
  err <- sum((v_sim - v_target)^2)
  print(err)
  return(err)
}

# ---------------------------------------------------------------------
# 3) Rodar otimização via optim()
# ---------------------------------------------------------------------
# Chute inicial (ajuste conforme seu conhecimento)

cat("=== Otimização Iniciada ===\n")

init_p <- c(
  1e-3, 1e-3, 15, 4e-1,   # p1
  0, 0, 3.5, 8.5, 0, 6.75e-1                  # p2
)

#cat("Guess:", init_p)

#res <- optim(
#  par    = init_p,
#  fn     = error_for_params,
#  method = "L-BFGS-B",
#  lower  = c(0,0,0,0,   0,0,0,0,0,0),
#  upper  = c(10,10,20,1,  10,10,10,10,10,10),
#  control= list(trace = 1, maxit = 50)
#)

#cat("=== Otimização concluída ===\n")
#cat("Parâmetros finais p1:", res$par[1:4], "\n")
#cat("Parâmetros finais p2:", res$par[5:10], "\n")
#cat("Erro final SSE =", res$value, "\n")

# ---------------------------------------------------------------------
# 4) Visualizar o resultado com o p* encontrado
# ---------------------------------------------------------------------
p_opt <- init_p # res$par
df_opt <- React_circuit({
  circuit <- DNArLogic::make_circuit(time_grid)
  circuit <- circuit_add_gate(
    circuit,
    Make_Oscillator_Dalchau('sin','v2p','v1p','v3p', p_opt[1],p_opt[2],p_opt[3],p_opt[4])
  )
  circuit <- circuit_add_gate(
    circuit,
    Make_Mux2_balanced('mux1','x','v1p','low','high','comp_out',
                       p_opt[5],p_opt[6],p_opt[7],p_opt[8], p_opt[9],p_opt[10])
  )
  circuit
})

x1_sim_opt <- df_opt[['v1p']]
x2_sim_opt <- df_opt[['v2p']]
x3_sim_opt <- df_opt[['v3p']]

v_target <- simulate_Vcc(time_grid)

plot(time_grid, v_target, type = "l", col = "green", lwd=2, lty = 2,
     xlab="Time (S)", ylab="Concentratin (M)",
     main="Generating a Voltage Source for Step inputs DSD Target=10step(t-10)[V]", xlim=c(0,40), ylim=c(1,15))
lines(time_grid, x1_sim_opt, col = "blue", lwd=3, lty=1)
lines(time_grid, x2_sim_opt, col = "red", lwd=1, lty=1)
lines(time_grid, x3_sim_opt, col = "yellow", lwd=1, lty=1)

legend("bottomright",
       legend=c("V(S)","v1p","v2p", "v3p"),
       col=c("green","blue", 'red', 'yellow'), lwd=2, lty=c(2,1, 1, 1))