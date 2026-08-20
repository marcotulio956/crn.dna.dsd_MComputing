# Load the libraries
# DO NOT USE library(DNAr)
# DO NOT USE library(DNArLogic)
# DO NOT USE library(DNArAnalog)

rm(list = ls())

source('R/4domain_reactor.R')
source('R/ANALOG_GATE_LIB.R')
source('R/analysis.R')
source('R/crn_reactor.R')
source('R/dsd.R')
source('R/ELECTRO_LIB.R')
source('R/GATE_LIB.R')
source('R/io.R')
source('R/neuron_hjelmfelt.R')
source('R/parser.R')
source('R/util_functions.R')

jn <- function(...) { paste(..., sep = '') }

library(ggplot2) # plot()
library(dplyr) # mutate()

Make_Circuit_RLC_dualRail_SS <- function(comp, rate) {
  # 'comp' is the structure from Make_RLC_Component_dualRail.
  # It contains:
  #   comp$in: v_inp, v_inn
  #   comp$st: iLp, iLn, vCp, vCn
  #   comp$out: vRp, vRn, vLp, vLn
  #   comp$ic: initial conditions for iLp, iLn, vCp, vCn
  #   comp$params: R, L, C
  name <- comp$name
  
  gates <- list()
  R <- comp$params$R
  L <- comp$params$L
  C <- comp$params$C

  fuel <- 1e3
  
  ## === Compute derivatives for inductor current (i_L) ===
  ## Using the splitting:
  ## iLp' = (1/L)*v_inp + (1/L)*vCn + (R/L)*iLn
  ## iLn' = (1/L)*v_inn + (1/L)*vCp + (R/L)*iLp
  
  # (1) Multiply v_inp by 1/L:
  g_mul_vinp <- Make_Mul2In_Wang(
    jn(name, "g_mul_vinp"),
    comp$input$v_inp,
    jn(name, "const_1oL"),   # constant representing 1/L
    jn(name, "term_vinp"),
    0, 1/L, rate
  )
  gates[[length(gates)+1]] <- g_mul_vinp
  
  # (2) Multiply vCn by 1/L:
  g_mul_vCn <- Make_Mul2In_Wang(
    jn(name, "g_mul_vCn"),
    comp$st$vCn,
    jn(name, "const_1oL"),
    jn(name, "term_vCn"),
    0, 1/L, rate
  )
  gates[[length(gates)+1]] <- g_mul_vCn
  
  # (3) Multiply iLn by R/L:
  g_mul_iLn <- Make_Mul2In_Wang(
    jn(name, "g_mul_iLn"),
    comp$st$ineg,
    jn(name, "const_RoL"),   # constant representing R/L
    jn(name, "term_iLn"),
    0, R/L, rate
  )
  gates[[length(gates)+1]] <- g_mul_iLn
  
  # (4) Sum these three terms to get derivative for iLp (d_iLp):
  g_add_iLp1 <- Make_Adder2In_Wang(
    jn(name, "g_add_iLp1"),
    jn(name, "term_vinp"),
    jn(name, "term_vCn"),
    jn(name, "sum_iLp_temp"),
    0, 0, fuel, rate
  )
  gates[[length(gates)+1]] <- g_add_iLp1
  
  g_add_iLp2 <- Make_Adder2In_Wang(
    jn(name, "g_add_iLp2"),
    jn(name, "sum_iLp_temp"),
    jn(name, "term_iLn"),
    jn(name, "d_iLp"),
    0, 0, fuel, rate
  )
  gates[[length(gates)+1]] <- g_add_iLp2
  
  # (5) Multiply v_inn by 1/L for iLn':
  g_mul_vinn <- Make_Mul2In_Wang(
    jn(name, "g_mul_vinn"),
    comp$input$v_inn,
    jn(name, "const_1oL"),
    jn(name, "term_vinn"),
    0, 1/L, rate
  )
  gates[[length(gates)+1]] <- g_mul_vinn
  
  # (6) Multiply vCp by 1/L:
  g_mul_vCp <- Make_Mul2In_Wang(
    jn(name, "g_mul_vCp"),
    comp$st$vCp,
    jn(name, "const_1oL"),
    jn(name, "term_vCp"),
    0, 1/L, rate
  )
  gates[[length(gates)+1]] <- g_mul_vCp
  
  # (7) Multiply iLp by R/L:
  g_mul_iLp <- Make_Mul2In_Wang(
    jn(name, "g_mul_iLp"),
    comp$st$ipos,
    jn(name, "const_RoL"),
    jn(name, "term_iLp"),
    0, R/L, rate
  )
  gates[[length(gates)+1]] <- g_mul_iLp
  
  # (8) Sum terms for iLn' (d_iLn):
  g_add_iLn1 <- Make_Adder2In_Wang(
    jn(name, "g_add_iLn1"),
    jn(name, "term_vinn"),
    jn(name, "term_vCp"),
    jn(name, "sum_iLn_temp"),
    0, 0, fuel, rate
  )
  gates[[length(gates)+1]] <- g_add_iLn1
  
  g_add_iLn2 <- Make_Adder2In_Wang(
    jn(name, "g_add_iLn2"),
    jn(name, "sum_iLn_temp"),
    jn(name, "term_iLp"),
    jn(name, "d_iLn"),
    0, 0, fuel, rate
  )
  gates[[length(gates)+1]] <- g_add_iLn2
  
  ## === Compute derivatives for capacitor voltage (v_C) ===
  ## vCp' = (1/C)*iLp,   vCn' = (1/C)*iLn.
  g_mul_vCp_deriv <- Make_Mul2In_Wang(
    jn(name, "g_mul_vCp_deriv"),
    comp$st$ipos,
    jn(name, "const_1oC"),   # constant representing 1/C
    jn(name, "d_vCp"),
    0, 1/C, rate
  )
  gates[[length(gates)+1]] <- g_mul_vCp_deriv
  
  g_mul_vCn_deriv <- Make_Mul2In_Wang(
    jn(name, "g_mul_vCn_deriv"),
    comp$st$ineg,
    jn(name, "const_1oC"),
    jn(name, "d_vCn"),
    0, 1/C, rate
  )
  gates[[length(gates)+1]] <- g_mul_vCn_deriv
  
  ## === Integrate the derivatives to update the states ===
  # Integrate d_iLp to update iLp.
  # Integrate d_iLn to update iLn.
  g_int_iLpn <- Make_Integrator_OishiYordanov(
    jn(name, "g_int_iLp"),
    jn(name, "d_iLp"),
    jn(name, "d_iLn"),
    comp$st$ipos,
    comp$st$ineg,
    comp$ic$ipos, comp$ic$iLn,
    rate
  )
  gates[[length(gates)+1]] <- g_int_iLpn
    
  # Integrate d_vCp to update vCp.
  # Integrate d_vCn to update vCn.
  g_int_vCpn <- Make_Integrator_OishiYordanov(
    jn(name, "g_int_vCp"),
    jn(name, "d_vCp"),
    jn(name, "d_vCn"),
    comp$st$vCp,
    comp$st$vCn,
    comp$ic$vCp, comp$ic$vCn,
    rate
  )
  gates[[length(gates)+1]] <- g_int_vCpn
  
  ## === Compute output variables ===
  # Resistor voltage: v_R = R * i_L, where i_L = iLp - iLn.
  # Inductor voltage: v_L = L * (d(i_L)/dt).
  # Capacitor current: i_C = i_C
  return(gates)
}

Make_Generic <- function(timing) {
  circuit <- DNArLogic::make_circuit(timing)

   # RLC component with id = 1, R = 10 ohm, L = 5 H, C = 0.01 F,
   rlc1 <- Make_RLC_Component_dualRail(
     id = 1,
     R = 1,    # resistor value in ohms
     L = 10,     # inductor value in henries
     C = 10,  # capacitor value in farads
     init_ip = 10,    # initial current positive rail
     init_in = 0,    # initial current negative rail
     init_vCp = 10,   # initial capacitor voltage positive rail
     init_vCn = 0    # initial capacitor voltage negative rail
   )

  rlc_gates <- Make_Circuit_RLC_dualRail_SS(rlc1, 1e3)

  #rlc_crn <- Make_RLC_crn_simple('rlc', 1, 1, 1, 'vp', 'vn', 10, 0, 1)
  # add2circuit
  circuit <- circuit_add_compile_gates(circuit, rlc_gates)
  # circuit <- circuit_add_gate(circuit, rlc_crn)
  
  print(circuit)

  return (circuit)
}

t0 = 0
t1 = 10
points = (t1 - t0) * 100 # Using 50 time points
timing  <- seq(t0, t1, length.out = points) # Using 50 time points
circuit <- Make_Generic(timing)
result_crn <- React_circuit(circuit)

expected_value = 0
minimum = expected_value * 0.95
maximum = expected_value * 1.05
gate_number = 0

title = 'RLC Circuit'

Plot_behavior(
  result_crn, circuit, gate_number, minimum, maximum,
  #species=c( 'iLp', 'iLn'),
  #species=c('x', 'vCp', 'vCn', 'vLp', 'vLn'),
  species=c(),
  chart_title = title,
  timing
)

# Plot_behavior(result_crn, circuit, gate_number, minimum, maximum, specify_species = TRUE, species=c('c1ol_current', 'c1sub1_C', 'c1_l_dv_out'))

#resultado_4dom <- React_4domain_circuit(circuit)
#
#Plot_behavior(resultado_4dom$behavior, circuit, gate_number, minimum, maximum, TRUE)
#
#resultado_comb <- Compare_behaviors(resultado_crn, resultado_4dom, circuit, gate_number)
#
#p1 <- Plot_behavior_comb(resultado_comb, circuit, gate_number, minimum, maximum, TRUE)

# save <- save_behavior_csv(result_crn, 'save_behaviors/rlc_bigplot')

