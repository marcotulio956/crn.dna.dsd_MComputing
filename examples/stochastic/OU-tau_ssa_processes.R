rm(list = ls())

source('R/parser.R')
source('R/util_functions.R')
source('R/crn_reactor.R')
source('R/4domain_reactor.R')
source('R/io.R')


timing <- seq(0, 100, by = 0.05)


# Generate the noisy input (e.g., an external fluctuating nutrient 'S')
# We set mean-reversion theta=0.5, aiming for a target mean mu=100, with noise sigma=15
ou_data <- simulate_ou_process(
    t = timing, 
    x0 = 100, 
    theta = 0.5, 
    mu = 100, 
    sigma = 15,
    seed = 42
)

# Create an interpolation function from the generated SDE trajectory
# rule = 2 ensures it holds the last value if the SSA slightly overshoots the max time
noisy_nutrient_func <- approxfun(x = ou_data$time, y = ou_data$value, rule = 2)


circuit <- c()
circuit$ki <- circuit$ki * 10 

result_crn <- react2(
  species   = circuit$species,
  ci        = circuit$ci,
  reactions = circuit$reactions,
  ki        = circuit$ki,
  t         = timing,
  #engine = 'diffeqr',
  verbose = FALSE,
  forced_concentrations =  
    list(
      # S = noisy_nutrient_func
    )
)

result_crn['V'] = (result_crn['Vp'] - result_crn['Vm']) 
result_crn['M'] = (result_crn['XM'] / result_crn['XMc']) 
result_crn['W'] = (result_crn['XW'] / result_crn['XWc']) 


plot_behavior(result_crn, species=c('V'))

# g1 <- plot_behavior(results_tau,  circuit$species)
# g1
# g2 <- plot_behavior(results_gssa, circuit$species)
# g2

# circuit_dsd <- update_crn_4domain(circuit)
# results_dsd <- React_4domain_circuit(circuit_dsd)
# 
# 
# g3 <- plot_behavior(results_dsd$behavior, circuit_dsd$species)
# g3

