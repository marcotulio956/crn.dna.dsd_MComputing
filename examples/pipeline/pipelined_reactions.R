rm(list = ls())

source('R/parser.R')
source('R/util_functions.R')
source('R/crn_reactor.R')
source('R/4domain_reactor.R')
source('R/io.R')


source('R/forced_concentrations.R')

timing <- seq(0, 25, by = 0.05)


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
circuit$species <- c(
'S','A','B',
'CLKup','CLKdown',
'C0','C1','C2',
'E0','E1','E2'
)

circuit$ci <- c(
100,0,0,
0,0,
1,0,0,
0,0,0
)

circuit$reactions <- c(

# cycle transitions
'C0 + CLKup -> C1',
'C1 + CLKup -> C2',

# enable generators
'C0 + CLKup -> E0',
'C1 + CLKup -> E1',
'C2 + CLKup -> E2',

# scheduled reactions
'S + E0 -> A + E0',
'A + E1 -> B + E1',
'B + E2 -> 0 + E2'
)

circuit$ki <- c(
100,
100,

100,
100,
100,

1,
1,
1
)


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
      S = function(t) square_input(t, amplitude = 100)
    )
)

# Pass it directly into sto react
# results_tau <- react_tau_leap_forced(
#   circuit$species,
#   circuit$ci,
#   circuit$reactions,
#   circuit$ki,
#   circuit$t,
#   forced_concentrations = list(S = noisy_nutrient_func) # Inject the OU process here
# )
# 
# results_gssa <- react_stochastic_forced(
#   circuit$species,
#   circuit$ci,
#   circuit$reactions,
#   circuit$ki,
#   circuit$t,
#   forced_concentrations = list(S = noisy_nutrient_func), # Inject the OU process here
#   verbose = TRUE,
#   volume = 1
# )

plot_behavior(result_crn)

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

