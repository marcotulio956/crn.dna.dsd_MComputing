# Load the libraries
# DO NOT USE library(DNAr)
# DO NOT USE library(DNArLogic)
# DO NOT USE library(DNArAnalog)

rm(list = ls())
# setwd("~/MEGAsync/_CEFET/tcc/dnar")

source('R/4domain_reactor.R')
source('R/analysis.R')
source('R/crn_reactor.R')
source('R/dsd.R')
source('R/GATE_LIB.R')
source('R/io.R')
source('R/parser.R')

source('R/util_functions.R')

source('R/forced_concentrations.R')
source('R/NEURON_LIB.R')
source('R/NEURON_SIM.R')

jn <- function(...) { paste(..., sep = '') }

tend = 800
timing <-  seq(0, tend, length.out = tend + 1)

volume = 5

crn1 <- create_ml_crn_varyingRates(rate=1,Iapp=60, Mtot=40, Wtot=40, use_3d_m = TRUE, volume = volume)
crn1$t <- timing

# Ip_ode_ml_input <- function(timing) {crn1$params$Iapp}
# mls1 <- state_class1
# mlp1 <- pars_class1
# mlp1$input_func <- Ip_ode_ml_input
# library(deSolve)
# out1 <- ode(y = mls1, times = timing, func = morris_lecar, parms = mlp1)
# 
# result_ode <- as.data.frame(out1)
# result_ode['Vref_ode'] <- result_ode['v']

result_sto <- React_stochastic(crn1, volume = volume)
result_sto['Vsto'] = (result_sto['Vp'] - result_sto['Vm'])

crn1 <- create_ml_crn_varyingRates(rate=10,Iapp=140, Mtot=40, Wtot=40, use_3d_m = FALSE, volume = volume)
crn1$t <- timing
result_crn <- React_circuit(crn1)
result_crn['Vdet'] = (result_crn['Vp'] - result_crn['Vm'])
result_crn['M'] <- result_crn['XM'] / (result_crn['XM'] + result_crn['XMc'])
result_crn['W'] <- result_crn['XW'] / (result_crn['XW'] + result_crn['XWc'])

# result_dsd <- React_4domain_circuit(crn1)
# result_sto_dsd <- React_4domain_stochastic_circuit(crn1)


# result_stocsv <- read.csv("./crn_stochastic_metrics.csv")

# Iapp = 60
# Mtot = 60
# Wtot = 60
# 
# seed = 12345
# filename <- sprintf("ml_single_Iapp%d_Mtot%d_Ntot%d_seed%d_tmax%d.csv", 
#                      # crn1$params$Iapp, crn1$params$Mtot, crn1$params$Wtot, seed, tend)
#                     Iapp, Mtot, Wtot, seed, tend)
# result_sto_ref <-  read.csv(file.path("./matlab/data", filename))
# result_sto_ref <- subset(result_sto_ref, time <= tend)
# # result_sto_ref <- result_sto_ref_filtered[order(result_sto_ref_filtered$time), ]
# result_sto_ref['Vref_sto'] <- result_sto_ref['V']

# df_list <- list(result_ode, result_crn, result_sto, result_sto_ref)
df_list <- list(result_sto, result_crn)
merged_df <- Reduce(function(x, y) merge(x, y, by = "time", all = TRUE), df_list)
                    
# plot_behavior(merged_df, species = c('Vcrn', 'Vsto', 'Vref_ode', 'Vref_sto'))
plot_behavior(merged_df, species = c('Vsto', 'Vdet'))
# plot_behavior(joined_df, species = c('M','W'),
              # species_dotted = c('w'))
              

# result_sto <- React_stochastic(circuit)
# result_sto['V'] = result_sto['Vp'] - result_sto['Vm']
# Plot_behavior(result_sto, circuit, intercept , minimum, maximum)


#result_4dom <- React_4domain_circuit(circuit)
#Plot_behavior(result_4dom, circuit, intercept , minimum, maximum)

#result_sto <- React_stochastic(circuit)
# result_sto['Vt'] = result_sto['V_plus'] - result_sto['V_minus']
# result_sto['S1'] = result_sto['S1_plus'] - result_sto['S1_minus']
# result_sto['W1'] = result_sto['W1_plus'] - result_sto['W1_minus']
# result_sto['U'] = result_sto['U_plus'] - result_sto['U_minus']
#Plot_behavior(result_sto, circuit)
#
#
#Plot_behavior(resultado_4dom$behavior, circuit, num_gate, minimum, maximum, TRUE)
#
#resultado_comb <- Compare_behaviors(resultado_crn, resultado_4dom, circuit, num_gate)
#
#p1 <- Plot_behavior_comb(resultado_comb, circuit, num_gate, minimum, maximum, TRUE)

