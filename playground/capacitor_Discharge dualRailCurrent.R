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

Make_Capacitor_ <- function(name, species_input, species_output, ic) {
  rate <- 2e3
  gates <- list()

   # V_in lag
  l_v_in_lag = jn(name, '_l_v_in_lag')
  g1_1 <- Make_Mul2In_Wang(jn(name, 'mul2_'),
    species_input$voltage, jn(name, '_1cte'), l_v_in_lag,
    ic$voltage, 1,
    rate*0.46
  )
  gates[[length(gates)+1]] <- g1_1
  
  # dV_in = V_in - V_in_lag
  l_dv =  jn(name, '_l_dv')
  g1_2 <- Make_Sub2In_Wang(jn(name, 'sub1_1'),
    species_input$voltage, l_v_in_lag, l_dv,
    0, 0,
    1e-2, rate*1e3
  )
  gates[[length(gates)+1]] <- g1_2


  # dQ(v) = C * dV_in
  l_dQv = jn(name, '_l_dQv')
  g1 <- Make_Mul2In_Wang(jn(name, '_mul1'),
    species_input$capacitance, l_dv, l_dQv,
    ic$capacitance, 0,
    rate
  )
  gates[[length(gates)+1]] <- g1 
  
  #Q_total = Q_init + dQ(v)
  # degratate species_input$charge
  # 
  g1_3 <- Make_Adder_apBeC(jn(name, 'add1_2'),
    species_input$charge, l_dQv, species_output$charge,
    ic$charge, 0,
    10, rate
  )
  gates[[length(gates)+1]] <- g1_3

  # V_out rate 1=[1/C]*Q_total
  l_invC = jn(name, '_l_invC')
  l_v_rate1 = jn(name, '_l_v_rate1')
  g2 <- Make_Mul2In_Wang(jn(name, 'mul2'),
    l_invC, species_output$charge, l_v_rate1,
    1/ic$capacitance, 0,
    rate*0.46
  )
  gates[[length(gates)+1]] <- g2
  
  # V_out rate 2=[1/C]*Q_c
  # l_v_rate2 = species_output$voltage,
  g3 <- Make_Mul2In_Wang(jn(name, 'mul3'),
     l_invC, species_output$charge, species_output$voltage,
     1/ic$capacitance, 0,
     rate
  )
  gates[[length(gates)+1]] <- g3
  
  # dv_out =  species_output$voltage - l_v_rate1
  l_dv = jn(name, '_l_dv_out')
  g4 <- Make_Sub2In_Wang(jn(name, 'sub1'),
    species_output$voltage, l_v_rate1, l_dv,
    0, 0,
    1e-2, rate*1e3
  )
  gates[[length(gates)+1]] <- g4
  
  # dIn_out=C*dv_out
  g5 <- Make_Mul2In_Wang(jn(name, 'mul4'),
   species_input$capacitance, l_dv, species_output$current,
   ic$capacitance, 0,
   rate*1e3
  )
  gates[[length(gates)+1]] <- g5

  return (gates)

  # I_total = I_init + dT_out
}

Make_Generic <- function(timing) {
  circuit <- DNArLogic::make_circuit(timing)
  
  vcc1 = c()
  vcc1$name <- 'vcc1'
  vcc1$ol$voltage <- 'vcc1_vcc'
  vcc1$ol$current <- 'vcc1_i'
  vcc1$ic$voltage <- 0
  vcc1$ic$current <- 0

  c1 = c()
  c1$name <- 'c1'
  c1$il$capacitance <- 'c1il_capacitance'
  c1$ic$capacitance <- 5 # q/V=C[farad]
  # Dual rail species for charge, voltage, and current
  # c1$il$charge_positive <- 'c1il_charge_positive'
  # c1$il$charge_negative <- 'c1il_charge_negative'
  # c1$il$voltage_positive <- 'c1il_voltage_positive'
  # c1$il$voltage_negative <- 'c1il_voltage_negative'
  # c1$il$current_positive <- 'c1il_current_positive'
  # c1$il$current_negative <- 'c1il_current_negative'
  # c1$ol$charge_positive <- 'c1ol_charge_positive'
  # c1$ol$charge_negative <- 'c1ol_charge_negative'
  # c1$ol$voltage_positive <- 'c1ol_voltage_positive'
  # c1$ol$voltage_negative <- 'c1ol_voltage_negative'
  # c1$ol$current_positive <- 'c1ol_current_positive'
  # c1$ol$current_negative <- 'c1ol_current_negative'
  # c1$ic$charge_positive <- 50
  # c1$ic$charge_negative <- 0
  # c1$ic$voltage_positive <- vcc1$ic$voltage
  # c1$ic$voltage_negative <- 0
  # c1$ic$current_positive <- vcc1$ic$current
  # c1$ic$current_negative <- 0
  
  c1$il$charge <- 'c1il_charge'
  c1$il$voltage <- 'c1il_voltage'
  c1$il$current <- 'c1il_current'
  c1$ol$charge <- 'c1ol_charge'
  c1$ol$voltage <- 'c1ol_voltage'
  c1$ol$current <- 'c1ol_current'
  c1$ic$charge <- 50
  c1$ic$voltage <- vcc1$ic$voltage
  c1$ic$current <- vcc1$ic$current

  # - Electro
  # e1_gates <- Make_Resistor(r1$name, r1$il, r1$ol, r1$ic)
  # e1_gates <- Make_Resistor(r2$name, r2$il, r2$ol, r2$ic)
  e1_gates <- Make_Capacitor_(c1$name, c1$il, c1$ol, c1$ic)
  print(e1_gates)
  # e2_gates <- Make_Inductor(l1$name, l1$il, l1$ol, l1$ic)
  # add2circuit
  circuit <- circuit_add_electro_gates(circuit, e1_gates)
  #circuit <- circuit_add_electro_gates(circuit, e2_gates)

  # - Dig and Analog
  #g1 <- Make_Mul2In_Wang(r1$name, r1$il$current, jn(r1$name, '_R'), r1$ol$voltage, r1$ic$current, r1$ic$resistence, 2e3)
  #g2 <- Make_Div2In_Wang(r1$name, r1$il$voltage, jn(r1$name, '_R'), r1$ol$current, r1$ic$voltage, r1$ic$resistence, 1e3, 2e3)
  #g1 <- make_latchd('latch1',2, 1)
  #g2 <- make_flipflopd('ffd1', 2, 1)
  #g1 <- Make_Div2In_Wang('div1w', 'Xd4', 'Yd4', 'Zd4', 1, 5, 0.5, 1e3)
  # add2circuit
  #circuit <- DNArLogic::circuit_add_gate(circuit, g1)
  #circuit <- DNArLogic::circuit_add_gate(circuit, g2)

  return (circuit)
}

timing  <- seq(0, 0.01, length.out = 50) # Using 50 time points
circuit <- Make_Generic(timing)
result_crn <- React_circuit(circuit)

expected_value = 0
minimum = expected_value * 0.95
maximum = expected_value * 1.05
gate_number = 1

Plot_behavior(
  result_crn, circuit, gate_number, minimum, maximum,
  #plot_species=c('c1ol_voltage', 'c1_l_v_rate1','c1ol_current','c1ol_charge'),
  plot_species=c('c1il_charge', 'c1ol_charge', 'c1ol_voltage', 'c1ol_current'),
  #plot_species=c(),
  timing
)
# Plot_behavior(result_crn, circuit, gate_number, minimum, maximum, specify_species = TRUE, plot_species=c('c1ol_current', 'c1sub1_C', 'c1_l_dv_out'))

#resultado_4dom <- React_4domain_circuit(circuit)
#
#Plot_behavior(resultado_4dom$behavior, circuit, gate_number, minimum, maximum, TRUE)
#
#resultado_comb <- Compare_behaviors(resultado_crn, resultado_4dom, circuit, gate_number)
#
#p1 <- Plot_behavior_comb(resultado_comb, circuit, gate_number, minimum, maximum, TRUE)

