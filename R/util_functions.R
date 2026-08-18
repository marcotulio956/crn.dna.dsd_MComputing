library(ggplot2) # plot()
library(dplyr) # mutate()

React_circuit <- function(circuit, forced_concentrations, engine = 'desolve') {
  return(react2(
    species   = circuit$species,
    ci        = circuit$ci,
    reactions = circuit$reactions,
    ki        = circuit$ki,
    t         = circuit$t,
    engine = engine,
    # engine = 'diffeqr',
    verbose = FALSE,
    forced_concentrations = forced_concentrations
      # list(
      # #  Ip = Ip_crn_ml_input
      # )
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

# mu_target_func1 <- function(t) {100}
# mu_target_func2 <- function(t) {
#     pulse_input(t, time = 20, width = 40, amplitude = 50)
# }
# fuzzy_pulse_data <- simulate_ou_process(
  # t = mu_target_func2, 
  # x0 = 50, 
  # theta = 0.5, 
  # mu = 100, 
  # sigma = 15,
  # seed = 42ss
# )
# fuzzy_input_func <- approxfun(x = fuzzy_pulse_data$time, y = fuzzy_pulse_data$value, rule = 2)



React_stochastic <-function(circuit, volume = 10, seed = NULL) {
  return(react_stochastic_frates(
    species = circuit$species,
    ci = circuit$ci,
    reactions = circuit$reactions,
    ki = circuit$ki,
    t = circuit$t,
    forced_concentrations =  
      list(
       #Ip = fuzzy_input_func 
      ),
    verbose = FALSE,
    volume = volume,
    seed = seed
  ))
}


Plot_behavior <- function(result, circuit, species = c(), intercept=FALSE, min, max) {
  if(length(species) == 0) {
    species <- circuit$species
  }
  g <- plot_behavior(result, title = 'test',
                     species = species,
                     species_dotted = c(),
                     x_label     = 'Time (s)',
                     y_label     = 'Concentration (M)',
                     legend_name = 'Species'
  )
  
  if (intercept){
    g <- g + geom_hline(yintercept=min, linetype="dashed", color = "green", size=1)
    g <- g + geom_hline(yintercept=max, linetype="dashed", color = "red", size=1)
    
  }
  
  print(g)
  
}

Plot_behavior_circuit <- function(
  result, circuit, gate_numbers, min, max, 
  plot_species=FALSE, plot_species_dotted=FALSE, chart_title="test",
  timing
) {

  species_to_plot = c()
    dotted_species_to_plot = NULL
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

  if(length(plot_species_dotted) && !isFALSE(plot_species_dotted)) {
    dotted_species_to_plot <- plot_species_dotted
  }

  print("Species to plot:")
  g <- plot_behavior(result, 
                     species = species_to_plot,
                     species_dotted = dotted_species_to_plot,
                     chart_title = chart_title, 
                     x_label     = 'Time (s)',
                     y_label     = 'Concentration (M)',
                     legend_name = 'Species',
                     geom_list   = c('line', 'point'),
                     variable_line_type = FALSE,
                     variable_point_type = TRUE
  )

  if (!is.null(min)) {
    g <- g + geom_hline(yintercept = min, linetype = "dashed", color = "green", linewidth = 1)
  }

  if (!is.null(max)) {
    g <- g + geom_hline(yintercept = max, linetype = "dashed", color = "red", linewidth = 1)
  }

  print(g)
}

# reactions <- append_reaction(reactions, "a -_. b")
append_reaction <- function (reactions, new_reaction) {
  reactions[[length(reactions)+1]] <- new_reaction
  return (reactions)
}