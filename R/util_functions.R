library(ggplot2) # plot()
library(dplyr) # mutate()

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
  result, circuit, gate_numbers, min, max, 
  plot_species=FALSE, chart_title="test",
  timing
) {

  species_to_plot = c()
  if(!length(plot_species)){
      if(!length(gate_numbers)){
          gate_numbers <- 1:length(circuit$gates)
      }
      print(jn(gate_numbers, " gate numbers", 1:length(circuit$gates)))
      for(i in gate_numbers){
          print(circuit$gates[i]$species)
          for(j in circuit$gates[i]$species) {
              species_to_plot <- append(species_to_plot, j)
          }
      }
  }else{
    species_to_plot = plot_species 
  }

  print("Species to plot:")
  print(species_to_plot)
  g <- plot_behavior(result, 
                     species = species_to_plot,
                     chart_title = chart_title, 
                     x_label     = 'Time (s)',
                     y_label     = 'Concentration (M)',
                     legend_name = 'Species',
                     geom_list   = c('line', 'point'),
                     variable_line_type = FALSE,
                     variable_point_type = TRUE
  )

  if (min || max){
    g <- g + geom_hline(yintercept=min, linetype="dashed", color = "green", size=1)
    g <- g + geom_hline(yintercept=max, linetype="dashed", color = "red", size=1)
  }
  
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