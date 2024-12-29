library(ggplot2) # plot()
library(dplyr) # mutate()

Init_RLC_Structures <- function() {
  r1 = c()
  r1$name <- 'r1'
  r1$il$current <- 'r1il_current'
  r1$il$voltage <- 'r1il_voltage'
  r1$ol$current <- 'r1ol_current'
  r1$ol$voltage <- 'r1ol_voltage'
  r1$ic$resistence <- 3
  r1$ic$current <- 5
  r1$ic$voltage <- 7

  vcc1 = c()
  vcc1$name <- 'vcc1'
  vcc1$ol$voltage <- 'vcc1_vcc'
  vcc1$ol$current <- 'vcc1_i'
  vcc1$ic$voltage <- 10
  vcc1$ic$current <- 0

  r2 = c()
  r2$name <- 'r2'
  r2$il$current <- 'vcc1_i'
  r2$il$voltage <- 'vcc1_vcc'
  r2$ol$current <- 'r2ol_current'
  r2$ol$voltage <- 'r2ol_voltage'
  r2$ic$resistence <- 2
  r2$ic$current <- vcc1$ic$current
  r2$ic$voltage <- vcc1$ic$voltage

  c1 = c()
  c1$name <- 'c1'
  c1$il$current <- 'vcc1_i'
  c1$il$voltage <- 'vcc1_vcc'
  c1$il$capacitance <- 'c1il_capacitance'
  c1$il$charge <- 'c1il_charge'
  c1$ol$current <- 'c1ol_current'
  c1$ol$voltage <- 'c1ol_voltage'
  c1$ol$charge <- 'c1ol_charge'
  c1$ic$charge <- 0
  c1$ic$capacitance <- 5 # q/V=C[farad]
  c1$ic$current <- vcc1$ic$current
  c1$ic$voltage <- vcc1$ic$voltage
  #print(c1)
  
  l1 = c()
  l1$name <- 'l1'
  l1$il$current <- 'vcc1_i'
  l1$il$voltage <- 'vcc1_vcc'
  l1$il$inductance <- 'l1ol_inductance'
  l1$ol$current <- 'l1ol_current'
  l1$ol$voltage <- 'l1ol_voltage'
  l1$ol$flux <- 'l1ol_flux'
  l1$ic$flux <- 0
  l1$ic$inductance <- 5 # phi/I=L[henry]
  l1$ic$current <- vcc1$ic$current
  l1$ic$voltage <- vcc1$ic$voltage
}

React_circuit <- function(circuit) {
  return(react(
    species   = circuit$species,
    ci        = circuit$ci,
    reactions = circuit$reactions,
    ki        = circuit$ki,
    t         = circuit$t
  ))
}

React_4domain_circuit <- function(circuit) {
  return(react_4domain(
    species   = circuit$species,
    ci        = circuit$ci,
    reactions = circuit$reactions,
    ki        = circuit$ki,
    qmax      = 1e6, # maximum strand displacement rate constant
    cmax      = 1e7, # 1e-4  # starting concentration of auxiliary complexes Gi and Ti
    alpha     = 1,   # DSD timescale versus CRN
    beta      = 1,   # DSD concentration scale versus CRN
    t         = circuit$t
  ))
}

Plot_behavior <- function(
  result, circuit, gate_number, min, max, 
  plot_species=FALSE, 
  timing
) {

  if(!length(plot_species)){
    species_to_plot = c(
      circuit$gates[[gate_number]]$species$input1,
      circuit$gates[[gate_number]]$species$input2,
      circuit$gates[[gate_number]]$species$input3,
      circuit$gates[[gate_number]]$species$input4,
      circuit$gates[[gate_number]]$species$output,
      circuit$gates[[gate_number]]$species$output1,
      circuit$gates[[gate_number]]$species$output2,
      circuit$gates[[gate_number]]$species$output3
    )
  }else{
    species_to_plot = plot_species 
  }

  print("Species to plot:")
  print(species_to_plot)
  g <- plot_behavior(result, species = species_to_plot,
                     x_label     = 'Time (s)',
                     y_label     = 'Concentration (M)',
                     legend_name = 'Species',
                     geom_list   = c('line', 'point'),
                     variable_line_type = FALSE,
                     variable_point_type = TRUE
  )

  #if (!length(integrator)){
  #  g <- g + geom_hline(yintercept=min, linetype="dashed", color = "green", size=1)
  #  g <- g + geom_hline(yintercept=max, linetype="dashed", color = "red", size=1)
  #}
  
  # if (add_capacitor) {
  #   if (is.null(R) || is.null(C) || is.null(V_max)) {
  #     stop("R, C, and V_max must be provided to add capacitor curves.")
  #   }
    
  #   t <- result['time']

  #   # Compute the charging and discharging curves
  #   V_t_charging <- V_max * (1 - exp(-t / (R * C)))
  #   V_t_discharging <- V_max * exp(-t / (R * C))
    
  #   g + ggplot(data.frame(x=c(0, 0.01)), aes(x)) + 
  #     stat_function(fun=function(x) 10*(1-exp(-x/(R*C))))
  # }
  print(g)
}