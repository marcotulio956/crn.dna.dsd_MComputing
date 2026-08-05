# -------------------------------------------------------------------
# Avalia amortecimento de um ou vários trios (R, L, C)
# -------------------------------------------------------------------
evaluate_RLC <- function(R, L, C, tol = 1e-3) {
  # R, L, C  : vetores numéricos de mesma extensão, em que cada tripla
  #            (R[i], L[i], C[i]) será avaliada.
  # tol      : tolerância para decidir zeta == 1 (amortecimento crítico).
  #
  # Retorna um data.frame com colunas: R, L, C, zeta e behavior.
  
  # Checa comprimentos
  nR <- length(R); nL <- length(L); nC <- length(C)
  if (!(nR == nL && nR == nC)) {
    stop("R, L e C devem ter o mesmo comprimento.")
  }
  
  # Calcula zeta = (R / 2L) / (1/sqrt(LC)) = R/2 * sqrt(C/L)
  zeta <- (R / 2) * sqrt(C / L)
  
  # Classifica
  behavior <- ifelse(
    abs(zeta - 1) <= tol, 
    "critically_damped",
    ifelse(zeta < 1, "underdamped", "overdamped")
  )
  
  # Monta data.frame de saída
  df <- data.frame(
    R        = R,
    L        = L,
    C        = C,
    zeta     = zeta,
    behavior = behavior,
    stringsAsFactors = FALSE
  )
  return(df)
}


# 2) Avaliar vários trios de uma só vez:
R_vals <- seq(0.1, 3, 0.01)
L_vals <- rep(1, length(R_vals))
C_vals <- rep(1, length(R_vals))
grid <- evaluate_RLC(R = R_vals, L = L_vals, C = C_vals)
print(grid)
