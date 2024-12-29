#' @export
#' @title Make_Resistor
#' @description Analog Ohmic resistor, Electrical Impedance Z = R, Based on Wang Divider
#' @usage Make_Resistor(name, nameInput1, nameInput2, nameOutput,
#' @usage cinput1, cinput2, crange, rate)
#' @param name The analog gate/unit name
#' @param nameInput1 The input 1 name
#' @param nameInput2 The input 2 name
#' @param nameOutput The output name
#' @param cinput1 The initial concentration of input 1
#' @param cinput2 The initial concentration of input 2
#' @param crange The initial concentration of the gate/unit operation range
#' @param rate The reaction rate of the gate/unit
#' @return A analog gate/unit created with its name, species, CRN reactions, initial concentrations (ci), and reaction rates (ki) constants
#' @examples Make_Adder2In_Song('add1s','Ia1','Ia2','Oaa', 5, 3, 10, 2e-3)

jn <- function(...) { paste(..., sep = '') }

circuit_add_electro_gates <- function(circuit, gates) {
  # Merge gates into the circuit
  for (gate in gates) {
    circuit$gates <- append(circuit$gates, list(gate))
    circuit <- circuit_compile(circuit)
  }
  return(circuit)
}

Make_Inductor <- function(name, species_input, species_output, ic) {
  rate <- 2e3
  gates <- list()

  # F_l = L * I_in
  g1 <- Make_Mul2In_Wang(jn(name, 'mul1'),
    species_input$inductance, species_input$current, species_output$flux,
    ic$inductance, ic$current,
    rate
  )
  gates[[1]] <- g1
  # I_out = F_l / L
  g2 <- Make_Div2In_Wang(jn(name,'div1'),
    species_output$flux, species_input$inductance, species_output$current,
    ic$flux, ic$inductance,
    1e3, rate
  )
  # gates[[2]] <- g2
  # V_out = V_in
  g3 <- Make_Buffer_Lakin(jn(name, 'buf1'),
    species_input$voltage, species_output$voltage,
    ic$voltage,
    1e3, 1e3, 1e-1 
  )
  gates[[3]] <- g3
  return (gates)
}

Make_Capacitor <- function(name, species_input, species_output, ic) {
  rate <- 2e3
  gates <- list()

  # C_cte = 1 * ic$capacitance
  # l_C_cte = jn(name, '_C_cte') 
  # g6 <- Make_Mul2In_Wang(jn(name,'mul3'),
  #  'int1', species_input$capacitance, l_C_cte,
  #   1, ic$capacitance,
  #   rate
  # )
  # gates[[6]] <- g6
  
  # Q(v) = C * V_in
  Qv = jn(name, '_Q(v)')
  g1 <- Make_Mul2In_Wang(jn(name, '_mul1'),
    species_input$capacitance, species_input$voltage, Qv,
    # l_C_cte, species_input$voltage, Qv
    ic$capacitance, ic$voltage,
    rate
  )
  gates[[1]] <- g1
  print(species_input$charge)
  print(species_output$charge)
  print(ic$charge)
  g5 <- Make_Buffer_Lakin(jn(name, '_buff2'),
    species_input$charge, species_output$charge,
    ic$charge, 
    1e3, 1e3,
    rate
  )
  gates[[5]] <- g5

  # Q_c = Q(v) + Q_init
  g2 <- Make_Adder2In_Wang(jn(name, '_add1'),
    Qv, species_input$charge, species_output$charge,
    0, 0,
    1e3, rate
  )
  gates[[2]] <- g2

  # V_out=[1/C]*Q_c
  C <- ic$capacitance
  print(C)
  ic_elastance <- 1 / C
  g3 <- Make_Mul2In_Wang(jn(name, 'mul2'),
    jn(name, '_l_invC'), species_output$charge, species_output$voltage,
    ic_elastance, 0,
    # l_invC, species_output$charge, species_output$voltage,
    # 0, 0,
    rate
  )
  gates[[3]] <- g3

  # I_out=I_in
  g4 <- Make_Buffer_Lakin(jn(name, 'buf1'),
    species_input$current, species_output$current,
    0,
    1e3, 1e3, 1e-1 
  )
  gates[[4]] <- g4

  return (gates)
}

Make_Wire <- function(name, li_input1, lo_output1, ic_value){
  rate <- 2e3
  gates <- list()
  g1 <- Make_Mul2In_Wang(jn(name, 'wire1'),
    li_input1, jn(li_input1,'_'), lo_output1,
    ic_value, 1,
    rate
  )
  gates[[1]] <- g1
  return(gates)
}


Make_Resistor <- function(name, species_input, species_output, ic) {
  rate <- 2e3
  gates <- list()

  # V_out=R*I_in
  g1 <- Make_Mul2In_Wang(jn(name, 'mul1'),
    species_input$current, jn(name,'_R'), species_output$voltage,
    ic$resistence, ic$current,
    rate
  )
  gates[[1]] <- g1

  # # I_out=I_in
  # g2 <- Make_Buffer_Lakin(jn(name, 'buf1'),
  #   species_input$current, jn(species_input$current,'_waste'),
  #   ic$current,
  #   1e3, 1e3, 1e-1 # ex1: 1e-1 computacao de V em funcao i(t)
  # )
  # gates[[2]] <- g2

  g3 <- Make_Wire(jn(name, "wire1"), species_input$current, species_output$current, ic$current)
  gates[[3]] <- g3

  return (gates)
}