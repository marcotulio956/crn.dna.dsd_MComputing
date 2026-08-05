# ==============================================================================
# 1. SETUP & CONFIGURATION (DEINE STELLSCHRAUBEN)
# ==============================================================================

# Definiere die Anzahl der Replikationen (Wiederholungen pro Versuchsbedingung)
NUM_REPLICATES <- 5  

# DEIN SIMULATIONS-KNOB: Ersetze den Inhalt dieser Funktion mit deiner Logik.
# Sie MUSS einen einzelnen numerischen Wert (deine Zielmetrik) zurückgeben.
my_simulation_knob <- function(volatility_level, reversion_level, volume_level, rep_id) {
  
  # HIER MAPPS DU DIE LEVELS (-1 und +1) AUF DEINE ECHTEN PARAMETERWERTE:
  # Beispielhaft für dein OU-Modell und Systemvolumen:
  sig  <- if (volatility_level == -1) 0.05 else 0.40
  th   <- if (reversion_level == -1) 0.1  else 5.0
  omega <- if (volume_level == -1)   10   else 1000
  
  # ----------------------------------------------------------------------------
  # Platziere hier deinen Aufruf für 'simulate_ou_process' und den Tau-Leaper!
  # ----------------------------------------------------------------------------
  # Beispiel: 
  # ou_data <- simulate_ou_process(t=1:100, x0=1, theta=th, mu=1, sigma=sig)
  # result  <- run_tau_leaping(ou_data, volume=omega)
  # ----------------------------------------------------------------------------
  
  # DUMMY-METRIK FÜR DIESES BEISPIEL (Simuliert ein Messergebnis mit Rauschen):
  # Ersetze dies durch deine echte Metrik (z. B. Spike-Frequenz, Fehler, etc.)
  base_metric <- 12.5 + (3.2 * volatility_level) - (1.5 * reversion_level) + 
                 (4.0 * volume_level) + (2.1 * volatility_level * volume_level)
  random_noise <- rnorm(1, mean = 0, sd = 0.5) 
  
  metric_result <- base_metric + random_noise
  return(metric_result)
}

# ==============================================================================
# 2. GENERIERUNG DER ANOVA-DESIGN-MATRIX (-1 / +1 Coded)
# ==============================================================================

# Erstelle das Basis-Design für 3 Faktoren (2^3 = 8 Kombinationen)
base_design <- expand.grid(
  X1_Volatility = c(-1, 1),
  X2_Reversion  = c(-1, 1),
  X3_Volume     = c(-1, 1)
)

# Erweitere das Design um die gewünschten Replikationen
factorial_design <- base_design[rep(seq_len(nrow(base_design)), each = NUM_REPLICATES), ]
factorial_design$Replicate <- rep(1:NUM_REPLICATES, times = nrow(base_design))
rownames(factorial_design) <- NULL

# ==============================================================================
# 3. AUSFÜHRUNG DER SIMULATIONEN
# ==============================================================================
cat("Starte", nrow(factorial_design), "Simulationen...\n")

# Loop über alle Zeilen der Design-Matrix und füttere den "Knob"
factorial_design$Y_Metric <- sapply(1:nrow(factorial_design), function(i) {
  my_simulation_knob(
    volatility_level = factorial_design$X1_Volatility[i],
    reversion_level  = factorial_design$X2_Reversion[i],
    volume_level     = factorial_design$X3_Volume[i],
    rep_id           = factorial_design$Replicate[i]
  )
})

# ==============================================================================
# 4. LINEARE REGRESSION & ANOVA (Inklusive Interaktionen)
# ==============================================================================

# Definiere das Modell mit allen Haupteffekten und Interaktionen bis zur 3. Ordnung
# In R sorgt das Zeichen '*' automatisch für die Berechnung der Interaktionen
linear_model <- lm(Y_Metric ~ X1_Volatility * X2_Reversion * X3_Volume, data = factorial_design)

# Berechne die ANOVA-Tabelle (Sum of Squares)
anova_results <- anova(linear_model)

# ==============================================================================
# 5. AUSGABE DER ERGEBNISSE
# ==============================================================================

cat("\n==================================================\n")
cat("1. REGRESSIONS-KOEFFIZIENTEN (Effekt-Größen):\n")
cat("==================================================\n")
print(summary(linear_model)$coefficients)

cat("\n==================================================\n")
cat("2. ANOVA TABELLE (Sum of Squares & Signifikanz):\n")
cat("==================================================\n")
print(anova_results)

# ==============================================================================
# 6. ANNAHMEN-PRÜFUNG: NORMALVERTEILUNG DER RESIDUEN
# ==============================================================================

# 1. Extrahiere die Residuen aus deinem linearen Modell
model_residuals <- residuals(linear_model)

# 2. Erstelle eine Grafik mit zwei Diagnose-Plots nebeneinander
par(mfrow = c(1, 2))

# Plot A: Histogramm der Residuen
hist(model_residuals, 
     main = "Histogramm der Residuen", 
     xlab = "Residuen (Modellfehler)", 
     col = "lightblue", 
     breaks = 10)

# Plot B: Q-Q Plot (Quantile-Quantile Plot)
# Wenn die Punkte eng an der diagonalen Linie liegen, ist alles perfekt!
qqnorm(model_residuals, main = "Normal Q-Q Plot")
qqline(model_residuals, col = "red", lwd = 2)

# Setze das Grafikfenster wieder zurück
par(mfrow = c(1, 1))

# 3. Statistischer Test: Shapiro-Wilk-Test
# Ein p-Wert > 0.05 bedeutet: Keine signifikante Abweichung von der Normalverteilung.
shapiro_test <- shapiro.test(model_residuals)
cat("\n==================================================\n")
cat("3. SHAPIRO-WILK-TEST AUF NORMALVERTEILUNG:\n")
cat("==================================================\n")
print(shapiro_test)
