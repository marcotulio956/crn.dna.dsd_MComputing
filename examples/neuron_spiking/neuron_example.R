rm(list = ls())

source('R/parser.R')
source('R/util_functions.R')
source('R/crn_reactor.R')
source('R/4domain_reactor.R')
source('R/NEURON_LIB.R')
source('R/io.R')

library(ggplot2)

flatten_neuron_model <- function(neuron) {
  list(
    species = unname(unlist(neuron$species, use.names = FALSE)),
    species_map = unlist(neuron$species, use.names = TRUE),
    ci = as.numeric(neuron$ci),
    reactions = as.character(neuron$reactions),
    ki = neuron$ki
  )
}

rename_model_species <- function(model, rename_map) {
  species <- model$species
  reactions <- model$reactions

  for(old_name in names(rename_map)) {
    new_name <- rename_map[[old_name]]
    species[species == old_name] <- new_name
    reactions <- gsub(
      paste0('(?<![[:alnum:]_])', old_name, '(?![[:alnum:]_])'),
      new_name,
      reactions,
      perl = TRUE
    )
  }

  list(
    species = species,
    ci = model$ci,
    reactions = reactions,
    ki = model$ki,
    species_map = model$species_map
  )
}

safe_interpolate <- function(df, column_name, target_time) {
  approx(
    x = df$time,
    y = df[[column_name]],
    xout = target_time,
    rule = 2,
    ties = mean
  )$y
}

build_comparison_frame <- function(reference_df, translated_df, specs) {
  comparison <- data.frame(time = reference_df$time)

  for(label in names(specs)) {
    ref_name <- specs[[label]]$ref
    translated_name <- specs[[label]]$translated

    comparison[[paste0(label, '_crn')]] <- reference_df[[ref_name]]
    comparison[[paste0(label, '_dsd')]] <- safe_interpolate(
      translated_df,
      translated_name,
      reference_df$time
    )
  }

  comparison
}

plot_comparison <- function(behavior, species, species_dotted, chart_title) {
  g <- plot_behavior(
    behavior = behavior,
    species = species,
    species_dotted = species_dotted,
    chart_title = chart_title,
    x_label = 'Time',
    y_label = 'Concentration',
    legend_name = 'Species'
  )
  print(g)
  invisible(g)
}

rate = 1000

cat('Building spiking neuron model...\n')
neuron <- Make_Spiking_CRN_Neuron(
  name = 'spiking_demo',
  rate = 1,
  cA1 = 0,
  cA2 = 0,
  cA3 = 0,
  cW1 = 1,
  cW2 = 1,
  cW3 = 1,
  cV = 0,
  cS = 0,
  cR = 0,
  kIn1 = 1.25*rate,
  kIn2 = 0.80*rate,
  kIn3 = 0.65*rate,
  kFire = 0.75*rate,
  kLeakV = 0.12*rate,
  kLeakS = 0.05*rate,
  kRefCreate = 0.25*rate,
  kRefDecay = 0.04*rate,
  kReset = 0.90*rate,
  kLearn1 = 0.008*rate,
  kLearn2 = 0.008*rate,
  kLearn3 = 0.008*rate,
  kWDecay1 = 0.0008*rate,
  kWDecay2 = 0.0008*rate,
  kWDecay3 = 0.0008*rate
)

formal_model <- flatten_neuron_model(neuron)

# react_4domain() reserves W-style auxiliary names, so the translated model
# uses safe aliases for the weight species only.
rename_map <- c(W1 = 'Q1', W2 = 'Q2', W3 = 'Q3')
translated_model <- rename_model_species(formal_model, rename_map)

timing <- seq(0, 80, by = 0.05)

input_current <- function(t) {
	if (t>0){
		10
	} else {
		0
	}
#   if(t < 4) {
#     0
#   } else if(t < 18) {
#     6
#   } else if(t < 28) {
#     2
#   } else {
#     0
#   }
}

forced_concentrations <- list()
forced_concentrations[[formal_model$species_map[['A1']]]] <- input_current

cat('Running formal CRN with forced input...\n')
result_crn <- react(
  species = formal_model$species,
  ci = formal_model$ci,
  reactions = formal_model$reactions,
  ki = formal_model$ki,
  t = timing,
  forced_concentrations = forced_concentrations,
  method = 'rk4',
  engine = 'desolve',
  verbose = FALSE
)

cat('Translating to 4-domain DNA reactions...\n')
result_translation <- react_4domain(
  species = translated_model$species,
  ci = translated_model$ci,
  reactions = translated_model$reactions,
  ki = translated_model$ki,
  qmax = 1e4,
  cmax = 100,
  alpha = 1,
  beta = 1,
  t = timing,
  auto_buffer = FALSE,
  method = 'rk4',
  verbose = FALSE
)

result_dsd <- result_translation$behavior

crn_input <- formal_model$species_map[['A1']]
crn_membrane <- formal_model$species_map[['V']]
crn_spike <- formal_model$species_map[['S']]
crn_refractory <- formal_model$species_map[['R']]
crn_weight <- formal_model$species_map[['W1']]

dsd_input <- formal_model$species_map[['A1']]
dsd_membrane <- formal_model$species_map[['V']]
dsd_spike <- formal_model$species_map[['S']]
dsd_refractory <- formal_model$species_map[['R']]
dsd_weight <- rename_map[['W1']]

input_frame <- build_comparison_frame(
  reference_df = result_crn,
  translated_df = result_dsd,
  specs = list(
    input = list(ref = crn_input, translated = dsd_input),
    membrane = list(ref = crn_membrane, translated = dsd_membrane)
  )
)

activity_frame <- build_comparison_frame(
  reference_df = result_crn,
  translated_df = result_dsd,
  specs = list(
    spike = list(ref = crn_spike, translated = dsd_spike),
    refractory = list(ref = crn_refractory, translated = dsd_refractory)
  )
)

weight_frame <- build_comparison_frame(
  reference_df = result_crn,
  translated_df = result_dsd,
  specs = list(
    weight1 = list(ref = crn_weight, translated = dsd_weight)
  )
)

cat('Plotting comparisons...\n')
plot_comparison(
  behavior = input_frame,
  species = c('input_crn', 'membrane_crn'),
  species_dotted = c(),
  chart_title = 'Spiking CRN vs 4-domain DSD - input current and membrane potential'
)

plot_comparison(
  behavior = activity_frame,
  species = c('spike_crn', 'refractory_crn'),
  species_dotted = c(),
  chart_title = 'Spiking CRN vs 4-domain DSD - spike and refractory state'
)

plot_comparison(
  behavior = weight_frame,
  species = c('weight1_crn'),
  species_dotted = c(),
  chart_title = 'Spiking CRN vs 4-domain DSD - synaptic weight W1'
)

cat('Done.\n')