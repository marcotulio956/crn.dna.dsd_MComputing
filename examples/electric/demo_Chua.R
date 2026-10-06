source('R/parser.R')
source('R/util_functions.R')
source('R/crn_reactor.R')
source('R/4domain_reactor.R')
source('R/io.R')
source('R/GATE_LIB.R')
source('R/ANALOG_GATE_LIB.R')
source('R/ELECTRO_LIB.R')
source('R/ELECTRO_SIM.R')

timing <- seq(0, 20, by = 0.001)
rate <- 1e3
c1 <- Make_Capacitor_Component(1, capacitance = 10)
c2 <- Make_Capacitor_Component(2, capacitance = 1)
l1 <- Make_Inductor_Component(1, inductance = 1.4, resistance = 1)

diode_linear <- -1.4
c1vp <- 1.5
diode_cubic <- 1.33

circuit <- Make_Circuit_Chua_Composited(make_circuit(timing), c1, c2, l1, rate = rate,
                             diode_linear = diode_linear, diode_cubic = diode_cubic)
circuit$ci[match(c1$ol$voltage_positive, circuit$species)] <- c1vp

behavior <- c()
behavior <- react4(circuit$species, circuit$ci, circuit$reactions, circuit$ki,
                  circuit$t, engine = 'diffeqr')
reference <- simulate_Chua(timing, capacitance1 = 1, capacitance2 = 1,
                           inductance = 1, resistance = 1,
                           diode_linear = diode_linear, diode_cubic = diode_cubic,
                           initial_state = c(c1vp, 0, 0)
                           )
behavior$chua_v1 <- behavior[[c1$ol$voltage_positive]] - behavior[[c1$ol$voltage_negative]]
behavior$chua_v2 <- behavior[[c2$ol$voltage_positive]] - behavior[[c2$ol$voltage_negative]]
behavior$chua_il <- behavior[[l1$ol$current_positive]] - behavior[[l1$ol$current_negative]]
behavior[['ref_v1']] <- reference$v1
behavior[['ref_v2']] <- reference$v2
behavior[['ref_il']] <- reference$il

plot_behavior(behavior, title = 'Chua oscillator: CRN and reference',
              species = c('chua_v1', 'chua_v2', 'chua_il'),
              species_dotted = c('ref_v1', 'ref_v2', 'ref_il'))

plot_chua_custom <- function(data_frame, 
                             col_x = "ref_v1", 
                             col_y = "ref_v2", 
                             col_z = "ref_il", 
                             type = "xy") {
  
  # Verifica se as colunas existem no dataframe fornecido
  required_cols <- c(col_x, col_y, col_z)
  if (!all(required_cols %in% names(data_frame))) {
    stop("Erro: Alguma das colunas especificadas não foi encontrada no dataframe.")
  }
  
  # Extrai as variáveis para vetores limpos
  x <- data_frame[[col_x]]
  y <- data_frame[[col_y]]
  z <- data_frame[[col_z]]
  
  # Cria um vetor de tempo/índice caso não exista uma coluna de tempo formal
  time_steps <- 1:nrow(data_frame)
  if ("time" %in% names(data_frame)) {
    time_steps <- data_frame$time
  }
  
  # Renderiza o gráfico baseado no tipo escolhido
  if (type == "xy") {
    plot(x, y, type = "l", col = "firebrick", lwd = 0.6,
         xlab = paste("V1 (", col_x, ")", sep=""), 
         ylab = paste("V2 (", col_y, ")", sep=""), 
         main = "Oscilador de Chua: Plano V1 - V2")
    
  } else if (type == "xz") {
    plot(x, z, type = "l", col = "forestgreen", lwd = 0.6,
         xlab = paste("V1 (", col_x, ")", sep=""), 
         ylab = paste("IL (", col_z, ")", sep=""), 
         main = "Oscilador de Chua: Plano V1 - IL")
    
  } else if (type == "yz") {
    plot(y, z, type = "l", col = "purple", lwd = 0.6,
         xlab = paste("V2 (", col_y, ")", sep=""), 
         ylab = paste("IL (", col_z, ")", sep=""), 
         main = "Oscilador de Chua: Plano V2 - IL")
    
  } else if (type == "time") {
    # Layout de 3 painéis para evolução temporal das variáveis
    old_par <- par(mfrow = c(3, 1), mar = c(3, 4, 2, 1))
    plot(time_steps, x, type = "l", col = "blue", ylab = "V1 (t)", main = "Evolução das Variáveis de Chua")
    plot(time_steps, y, type = "l", col = "red", ylab = "V2 (t)")
    plot(time_steps, z, type = "l", col = "darkgreen", ylab = "IL (t)")
    par(old_par) # Restaura configuração original de layout
    
  } else if (type == "3d") {
    # Caso queira uma perspectiva estática básica em 3D usando Base R
    if (!requireNamespace("scatterplot3d", quietly = TRUE)) {
      message("Dica: Instale o pacote 'scatterplot3d' para uma visualização 3D melhor.")
      # Alternativa simples usando projeção simples se o pacote não existir
      plot(x, y + z*0.3, type = "l", col = "royalblue", lwd = 0.5,
           xlab = "V1", ylab = "V2 + Projeção IL", main = "Projeção Espacial Chua")
    } else {
      scatterplot3d::scatterplot3d(x, y, z, type = "l", color = "royalblue",
                                   xlab = "V1", ylab = "V2", zlab = "IL",
                                   main = "Espaço de Estados 3D - Chua")
    }
  } else {
    stop("Tipo inválido. Escolha entre: 'xy', 'xz', 'yz', 'time' ou '3d'.")
  }
}

behavior <- as.data.frame(behavior)
plot_chua_custom(behavior, col_x = 'ref_v1', col_y = 'ref_v2', col_z = 'ref_il', type = 'yz')
plot_chua_custom(behavior, col_x = 'chua_v1', col_y = 'chua_v2', col_z = 'chua_il', type = 'yz')


# 
# timing <- seq(0, 100, by = 0.01)
# rate <- 1
# 
# c1 <- Make_Capacitor_Component(1, capacitance = 0.1)
# c2 <- Make_Capacitor_Component(2, capacitance = 1.0)
# l1 <- Make_Inductor_Component(1, inductance = 0.0714, resistance = 1.0)
# 
# circuit <- make_circuit(timing)
# circuit <- Make_Circuit_Chua(circuit, c1, c2, l1, resistance = 1.0, rate = rate,
#                              diode_linear = -1.2, diode_cubic = 0.2)
# 
# # Set non-zero initial condition for C1 voltage
# circuit$ci[match(c1$ol$voltage_positive, circuit$species)] <- 0.10
# 
# behavior <- react4(circuit$species, circuit$ci, circuit$reactions, circuit$ki,
#                    circuit$t, engine = 'diffeqr')
