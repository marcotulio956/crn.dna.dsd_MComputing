# _____________________________________________________________________________
# Function created to concatenate strings

jn <- function(...) { paste(..., sep = '') }

# _____________________________________________________________________________

# Reminder: "The species names L[0-9], H[0-9], W[0-9], O[0-9], T[0-9],
# G[0-9], LS[0-9], HS[0-9], WS[0-9]* are not supported by DNAr."

#
#' @export
#' @title Make_Adder2In_Song
#' @description Analog adder with two inputs proposed by Song et al. (2016)
#' @usage Make_Adder2In_Song(name, nameInput1, nameInput2, nameOutput,
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

Make_Adder2In_Song <- function(name, nameInput1, nameInput2, nameOutput, cinput1,
                                cinput2, crange, rate) {
  species <- list(
    input1 = nameInput1,
    input2 = nameInput2,
    intermediate1 = jn(name, '_A1'),
    intermediate2 = jn(name, '_A2'),
    output = nameOutput
  )

  ci <- c(cinput1, cinput2, crange, crange, 0)

  reactions <- c(
    # 'Ia1 + A1 -> Oa'
    jn(species$input1, ' + ', species$intermediate1, ' -> ', species$output),
    # 'Ia2 + A2 -> Oa'
    jn(species$input2, ' + ', species$intermediate2, ' -> ', species$output)
  )

  ki <- c(rate, rate)

  add_gate_s <- list(
    name      = name,
    species   = species,
    reactions = reactions,
    ci        = ci,
    ki        = ki
  )

  return(add_gate_s)
}

# _____________________________________________________________________________

#' @export
#' @title Make_Adder3In_Song
#' @description Analog adder with three inputs proposed by Song et al. (2016) and
#' Oliveira et al. (2020)
#' @usage Make_Adder3In_Song(name, nameInput1, nameInput2, nameInput3,
#' nameOutput, cinput1, cinput2, cinput3, crange, rate)
#' @param name The analog gate/unit name
#' @param nameInput1 The input 1 name
#' @param nameInput2 The input 2 name
#' @param nameInput3 The input 3 name
#' @param nameOutput The output name
#' @param cinput1 The initial concentration of input 1
#' @param cinput2 The initial concentration of input 2
#' @param cinput3 The initial concentration of input 3
#' @param crange The initial concentration of the gate/unit operation range
#' @param rate The reaction rate of the gate/unit
#' @return A analog gate/unit created with its name, species, CRN reactions,
#' initial concentrations (ci), and reaction rates (ki) constants
#' @examples Make_Adder3In_Song('add2s', 'Ia1', 'Ia2', 'Ia3', 'Oa', 5, 3, 1, 10, 2e-3)

Make_Adder3In_Song <- function(name, nameInput1, nameInput2, nameInput3,
                               nameOutput, cinput1, cinput2, cinput3, crange, rate) {
  species <- list(
    input1 = nameInput1,
    input2 = nameInput2,
    input3 = nameInput3,
    intermediate1 = jn(name, '_A1'),
    intermediate2 = jn(name, '_A2'),
    intermediate3 = jn(name, '_A3'),
    output = nameOutput
  )

  ci <- c(cinput1, cinput2, cinput3, crange, crange, crange, 0)

  reactions <- c(
    # 'Ia1 + A1 -> Oa'
    jn(species$input1, ' + ', species$intermediate1, ' -> ', species$output),
    # 'Ia2 + A2 -> Oa'
    jn(species$input2, ' + ', species$intermediate2, ' -> ', species$output),
    # 'Ia3 + A3 -> Oa'
    jn(species$input3, ' + ', species$intermediate3, ' -> ', species$output)
  )

  ki        <- c(rate, rate, rate)

  addthree_gate_s <- list(
    name      = name,
    species   = species,
    reactions = reactions,
    ci        = ci,
    ki        = ki
  )

  return(addthree_gate_s)
}

# _____________________________________________________________________________

#' @export
#' @title Make_Adder4In_Song
#' @description Analog adder with four inputs proposed by Song et al. (2016) and
#' Oliveira et al. (2020)
#' @usage Make_Adder4In_Song(name, nameInput1, nameInput2, nameInput3,
#' nameInput4, nameOutput, cinput1, cinput2, cinput3, cinput4, crange, rate)
#' @param name The analog gate/unit name
#' @param nameInput1 The input 1 name
#' @param nameInput2 The input 2 name
#' @param nameInput3 The input 3 name
#' @param nameInput4 The input 4 name
#' @param nameOutput The output name
#' @param cinput1 The initial concentration of input 1
#' @param cinput2 The initial concentration of input 2
#' @param cinput3 The initial concentration of input 3
#' @param cinput4 The initial concentration of input 4
#' @param crange The initial concentration of the gate/unit operation range
#' @param rate The reaction rate of the gate/unit
#' @return A analog gate/unit created with its name, species, CRN reactions,
#' initial concentrations (ci), and reaction rates (ki) constants
#' @examples Make_Adder4In_Song('add3s', 'Ia1', 'Ia2', 'Ia3', 'Ia4, 'Oa', 5, 3, 1, 2, 10, 2e-3)

Make_Adder4In_Song <- function(name, nameInput1, nameInput2, nameInput3,
                               nameInput4, nameOutput, cinput1, cinput2,
                               cinput3, cinput4, crange, rate) {
  species <- list(
    input1 = nameInput1,
    input2 = nameInput2,
    input3 = nameInput3,
    input4 = nameInput4,
    intermediate1 = jn(name, '_A1'),
    intermediate2 = jn(name, '_A2'),
    intermediate3 = jn(name, '_A3'),
    intermediate4 = jn(name, '_A4'),
    output = nameOutput
  )

  ci <- c(cinput1, cinput2, cinput3, cinput4, crange, crange, crange, crange, 0)

  reactions <- c(
    # 'Ia1 + A1 -> Oa'
    jn(species$input1, ' + ', species$intermediate1, ' -> ', species$output),
    # 'Ia2 + A2 -> Oa'
    jn(species$input2, ' + ', species$intermediate2, ' -> ', species$output),
    # 'Ia3 + A3 -> Oa'
    jn(species$input3, ' + ', species$intermediate3, ' -> ', species$output),
    # 'Ia4 + A4 -> Oa'
    jn(species$input4, ' + ', species$intermediate4, ' -> ', species$output)
  )

  ki        <- c(rate, rate, rate, rate)

  addfour_gate_s <- list(
    name      = name,
    species   = species,
    reactions = reactions,
    ci        = ci,
    ki        = ki
  )

  return(addfour_gate_s)
}

# _____________________________________________________________________________

#' @export
#' @title Make_Sub2In_Song
#' @description Analog subtractor with two inputs proposed by Song et al. (2016)
#' @usage Make_Sub2In_Song(name, nameInput1, nameInput2, cinput1, cinput2,
#' crange, rate)
#' @param name The analog gate/unit name
#' @param nameInput1 The input 1 name
#' @param nameInput2 The input 2 name
#' @param cinput1 The initial concentration of input 1
#' @param cinput2 The initial concentration of input 2
#' @param crange The initial concentration of the gate/unit operation range
#' @param rate The reaction rate of the gate/unit
#' @return A analog gate/unit created with its name, species, CRN reactions,
#' initial concentrations (ci), and reaction rates (ki) constants
#' @examples Make_Sub2In_Song('sub1s', 'Is1', 'Is2', 5, 3, 10, 2e-3)


Make_Sub2In_Song <- function(name, nameInput1, nameInput2, cinput1, cinput2,
                              crange, rate) {
  species <- list(
    input1 = nameInput1,
    input2 = nameInput2,
    intermediate1 = jn(name, '_S'),
    intermediate2 = jn(name, '_Sx')
  )

  ci <- c(cinput1, cinput2, crange, 0)

  reactions <- c(
    # 'Is2 + S -> Sx'
    jn(species$input2, ' + ', species$intermediate1, ' -> ',
       species$intermediate2),
    # 'Is1 + Sx -> 0'
    jn(species$input1, ' + ', species$intermediate2, ' -> 0')
  )

  ki        <- c(rate, rate)

  sub_gate_s <- list(
    name      = name,
    species   = species,
    reactions = reactions,
    ci        = ci,
    ki        = ki
  )

  return(sub_gate_s)
}

# _____________________________________________________________________________

#' @export
#' @title Make_Mult2In_Song
#' @description Analog multiplier with two inputs proposed by Song et al. (2016)
#' @usage Make_Mult2In_Song(name, nameInput1, nameInput2, nameOutput, cinput1,
#' cinput2, crange, rate)
#' @param name The analog gate/unit name
#' @param nameInput1 The input 1 name
#' @param nameInput2 The input 2 name
#' @param nameOutput The output name
#' @param cinput1 The initial concentration of input 1
#' @param cinput2 The initial concentration of input 2
#' @param crange The initial concentration of the gate/unit operation range, only power of 2
#' @param rate The reaction rate of the gate/unit
#' @return A analog gate/unit created with its name, species, CRN reactions,
#' initial concentrations (ci), and reaction rates (ki) constants
#' @examples Make_Mult2In_Song('mul1s', 'Im1', 'Im2', 'Om', 5, 3, 8, 2e-3)

Make_Mult2In_Song <- function(name, nameInput1, nameInput2, nameOutput, cinput1,
                              cinput2, crange, rate) {
  species <- list(
    input1 = nameInput1,
    input2 = nameInput2,
    intermediate1 = jn(name, '_M1'),
    intermediate2 = jn(name, '_Im1a'),
    intermediate3 = jn(name, '_M2'),
    intermediate4 = jn(name, '_Im2a'),
    intermediate5 = jn(name, '_Im2b'),
    intermediate6 = jn(name, '_M3'),
    intermediate7 = jn(name, '_Gm3'),
    intermediate8 = jn(name, '_Gm4'),
    intermediateOutput = jn(name, '_Om1'),
    amplify1 = jn(name, '_Am1'),
    output = nameOutput
  )

  ci <- c(cinput1, cinput2, crange, 0, crange, 0, 0, crange, 0, crange, 0,
          crange, 0)

  reactions <- c(
    # 'Im1 + M1 -> Im1a'
    jn(species$input1, ' + ', species$intermediate1, ' -> ',
       species$intermediate2),
    # 'Im2 + M2 -> Im2a + Im2b'
    jn(species$input2, ' + ', species$intermediate3, ' -> ',
       species$intermediate4, ' + ', species$intermediate5),
    # 'Im2a + M3 -> Gm3'
    jn(species$intermediate4, ' + ', species$intermediate6, ' -> ',
       species$intermediate7),
    # 'Im2b + Gm4 -> 0'
    jn(species$intermediate5, ' + ', species$intermediate8, ' -> 0'),
    # 'Im1a + Gm4 -> 0'
    jn(species$intermediate2, ' + ', species$intermediate8, ' -> 0'),
    # 'Im1a + Gm3 -> Om1'
    jn(species$intermediate2, ' + ', species$intermediate7, ' -> ',
       species$intermediateOutput),
    # 'Om1 + A1 -> Om + ... + Om' -> 8*Om, se range =8
    jn(species$intermediateOutput, ' + ', species$amplify1, ' -> ',
       species$output, ' + ', species$output, ' + ', species$output, ' + ',
       species$output, ' + ', species$output, ' + ', species$output, ' + ',
       species$output, ' + ', species$output)
  )

  ki       <- c(rate*0.01, rate, rate, rate, rate*0.01, rate*0.01, rate)

  mult_gate_s <- list(
    name      = name,
    species   = species,
    reactions = reactions,
    ci        = ci,
    ki        = ki
  )

  return(mult_gate_s)
}

# _____________________________________________________________________________

#' @export
#' @title Make_Buffer_Lakin
#' @description Analog buffer/amplifier/divider proposed by Lakin et al. (2016)
#' @usage Make_Buffer_Lakin(name, nameInput1, nameOutput, cinput1, cint1,
#' cint2, rate)
#' @param name The analog gate/unit name
#' @param nameInput1 The input 1 name
#' @param nameOutput The output name
#' @param cinput1 The initial concentration of input 1
#' @param cint1 The initial concentration of B
#' @param cint2 The initial concentration of B'
#' @param rate The reaction rate of the gate/unit
#' @return A analog gate/unit created with its name, species, CRN reactions,
#' initial concentrations (ci), and reaction rates (ki) constants
#' @examples Make_Buffer_Lakin('buf1l', 'IBx', 'OBy', 5, 10, 10, 2e-3)


Make_Buffer_Lakin <- function(name, nameInput1, nameOutput, cinput1, cint1,
                              cint2, rate) {
  species <- list(
    input1 = nameInput1, #X
    output = nameOutput, #Y
    intermediate1 = jn(name, '_B'),
    intermediate2 = jn(name, '_BB')
  )

  ci <- c(cinput1, 0, cint1, cint2)

  reactions <- c(
    # 'B + X -> X + Y + B'
    jn(species$intermediate1, ' + ', species$input1, ' -> ', species$input1,
       ' + ', species$output, ' + ', species$intermediate1),
    # 'BB + X -> BB'
    jn(species$intermediate2, ' + ', species$input1, ' -> ', species$intermediate2)
  )

  ki        <- c(rate, rate)

  buffer_gate <- list(
    name      = name,
    species   = species,
    reactions = reactions,
    ci        = ci,
    ki        = ki
  )

  return(buffer_gate)
}

# _____________________________________________________________________________

#' @export
#' @title Make_Adder2In_Wang
#' @description Analog adder with two inputs proposed by Wang et al. (2022)
#' @usage Make_Adder2In_Wang(name, nameInput1, nameInput2, nameOutput,
#' cinput1, cinput2, cfuel, rate)
#' @param name The analog gate/unit name
#' @param nameInput1 The input 1 name
#' @param nameInput2 The input 2 name
#' @param nameOutput The output name
#' @param cinput1 The initial concentration of input 1
#' @param cinput2 The initial concentration of input 2
#' @param cfuel The initial concentration of the gate/unit operation fuel
#' @param rate The reaction rate of the gate/unit
#' @return A analog gate/unit created with its name, species, CRN reactions,
#' initial concentrations (ci), and reaction rates (ki) constants
#' @examples Make_Adder2In_Wang('add1w', 'Xa1', 'Ya1', 'Za1', 5, 3, 2, 1e-3)

Make_Adder2In_Wang <- function(name, nameInput1, nameInput2, nameOutput,
                             cinput1, cinput2, cfuel, rate) {
  species <- list(
    input1 = nameInput1,
    input2 = nameInput2,
    output = nameOutput,
    intermediate1 = jn(name, '_B'),
    intermediate2 = jn(name, '_waste')
  )

  ci <- c(cinput1, cinput2, 0, cfuel, 0)

  reactions <- c(
    # 'x1 -> x1 + z1'
    jn(species$input1, ' -> ', species$input1, ' + ', species$output),
    # 'z1 + B -> z1 + z1'
    jn(species$output, ' + ', species$intermediate1, ' -> ',
       species$output, ' + ', species$output),
    # 'y1 -> y1 + B'
    jn(species$input2, ' -> ', species$input2, ' + ', species$intermediate1),
    # 'z1 -> waste'
    jn(species$output, ' -> ', species$intermediate2)
  )

  ki        <- c(rate, rate, rate, rate)

  add_gate_w <- list(
    name      = name,
    species   = species,
    reactions = reactions,
    ci        = ci,
    ki        = ki
  )

  return(add_gate_w)
}

# _____________________________________________________________________________

#' @export
#' @title Make_Sub2In_Wang
#' @description Analog subtractor with two inputs proposed by Wang et al. (2022)
#' @usage Make_Sub2In_Wang(name, nameInput1, nameInput2, nameOutput,
#' cinput1, cinput2, cfuel, rate)
#' @param name The analog gate/unit name
#' @param nameInput1 The input 1 name
#' @param nameInput2 The input 2 name
#' @param nameOutput The output name
#' @param cinput1 The initial concentration of input 1
#' @param cinput2 The initial concentration of input 2
#' @param cfuel The initial concentration of the gate/unit operation fuel
#' @param rate The reaction rate of the gate/unit
#' @return A analog gate/unit created with its name, species, CRN reactions,
#' initial concentrations (ci), and reaction rates (ki) constants
#' @examples Make_Sub2In_Wang('sub1w', 'Xs2', 'Ys2', 'Zs2', 5, 3, 3, 1e-3)

Make_Sub2In_Wang <- function(name, nameInput1, nameInput2, nameOutput,
                             cinput1, cinput2, cfuel, rate) {
  species <- list(
    input1 = nameInput1,
    input2 = nameInput2,
    output = nameOutput,
    intermediate1 = jn(name, '_C'),
    intermediate2 = jn(name, '_waste')
  )

  ci <- c(cinput1, cinput2, 0, cfuel, 0)

  reactions <- c(
    # 'x2 -> x2 + z2'
    jn(species$input1, ' -> ', species$input1, ' + ', species$output),
    # 'z2 + C -> waste'
    jn(species$output, ' + ', species$intermediate1, ' -> ',
       species$intermediate2),
    # 'y2 -> y2 + C'
    jn(species$input2, ' -> ', species$input2, ' + ', species$intermediate1),
    # 'z2 -> waste'
    jn(species$output, ' -> ', species$intermediate2)
  )

  ki        <- c(rate, rate, rate, rate)

  sub_gate_w <- list(
    name      = name,
    species   = species,
    reactions = reactions,
    ci        = ci,
    ki        = ki
  )

  return(sub_gate_w)
}

# _____________________________________________________________________________

#' @export
#' @title Make_Mul2In_Wang
#' @description Analog multiplier with two inputs proposed by Wang et al. (2022)
#' @usage Make_Mul2In_Wang(name, nameInput1, nameInput2, nameOutput, cinput1, cinput2, rate)
#' @param name The analog gate/unit name
#' @param nameInput1 The input 1 name
#' @param nameInput2 The input 2 name
#' @param nameOutput The output name
#' @param cinput1 The initial concentration of input 1
#' @param cinput2 The initial concentration of input 2
#' @param rate The reaction rate of the gate/unit
#' @return A analog gate/unit created with its name, species, CRN reactions,
#' initial concentrations (ci), and reaction rates (ki) constants
#' @examples Make_Mul2In_Wang('mul1w', 'Xm3', 'Ym3', 'Zm3', 5, 3, 1e-3)

Make_Mul2In_Wang <- function(name, nameInput1, nameInput2, nameOutput,
                             cinput1, cinput2, rate) {
  species <- list(
    input1 = nameInput1,
    input2 = nameInput2,
    output = nameOutput,
    intermediate1 = jn(name, '_waste')
  )

  ci <- c(cinput1, cinput2, 0, 0)

  reactions <- c(
    # 'x3 + y3 -> x3 + y3 + z3'
    jn(species$input1, ' + ', species$input2, ' -> ', species$input1,
       ' + ', species$input2, ' + ', species$output),
    # 'z3 -> waste'
    jn(species$output, ' -> ', species$intermediate1)
  )

  ki        <- c(rate, rate)

  mul_gate_w <- list(
    name      = name,
    species   = species,
    reactions = reactions,
    ci        = ci,
    ki        = ki
  )

  return(mul_gate_w)
}

# _____________________________________________________________________________

#' @export
#' @title Make_Div2In_Wang
#' @description Analog divider with two inputs proposed by Wang et al. (2022)
#' @usage Make_Div2In_Wang(name, nameInput1, nameInput2, nameOutput,
#' cinput1, cinput2, cfuel, rate)
#' @param name The analog gate/unit name
#' @param nameInput1 The input 1 name
#' @param nameInput2 The input 2 name
#' @param nameOutput The output name
#' @param cinput1 The initial concentration of input 1
#' @param cinput2 The initial concentration of input 2
#' @param cfuel The initial concentration of the gate/unit operation fuel
#' @param rate The reaction rate of the gate/unit
#' @return A analog gate/unit created with its name, species, CRN reactions,
#' initial concentrations (ci), and reaction rates (ki) constants
#' @examples Make_Div2In_Wang('div1w', 'Xd4', 'Yd4', 'Zd4', 10, 2, 10, 1e-3)

Make_Div2In_Wang <- function(name, nameInput1, nameInput2, nameOutput,
                             cinput1, cinput2, cfuel, rate) {
  species <- list(
    input1 = nameInput1,
    input2 = nameInput2,
    output = nameOutput,
    intermediate1 = jn(name, '_D'),
    intermediate2 = jn(name, '_DD')
  )

  ci <- c(cinput1, cinput2, 0, cfuel, cfuel)

  reactions <- c(

    # 'z4 + y4 + D -> y4 + D + D' ### DNAr does not support 3-reagent reactions

    #jn(species$output, ' + ', species$input2, ' + ', species$intermediate1, ' -> ',
    #   species$input2, ' + ', species$intermediate1, ' + ',
    #   species$intermediate1),


    # Breakdown of the trimolecular reaction into two bimolecular reactions
    # 'y4 + D -> DD'
    #jn(species$input2, ' + ', species$intermediate1, ' -> ', species$intermediate2),

    # 'DD + z4 -> y4 + D + D'
    #jn(species$intermediate2, ' + ', species$output, ' -> ',
    #   species$input2, ' + ', species$intermediate1, ' + ',
    #   species$intermediate1),


    # Breakdown of the trimolecular reaction into three bimolecular reactions
    # 'z4 + Dw -> Dw + D'
    jn(species$output, ' + ', species$intermediate2, ' -> ',
       species$intermediate2, ' + ', species$intermediate1),

    # 'y4 + D -> Dw'
    jn(species$input2, ' + ', species$intermediate1, ' -> ',
       species$intermediate2),

    # 'Dw -> y4 + D'
    jn(species$intermediate2, ' -> ',
       species$input2, ' + ', species$intermediate1),

    # 'D + x4 -> z4 + x4'
    jn(species$intermediate1, ' + ', species$input1, ' -> ',
       species$output, ' + ', species$input1)

  )

  ki        <- c(rate, rate, rate, rate)

  div_gate_w <- list(
    name      = name,
    species   = species,
    reactions = reactions,
    ci        = ci,
    ki        = ki
  )

  return(div_gate_w)
}

# _____________________________________________________________________________

#' @export
#' @title Make_Exp2_Wang
#' @description Analog exponential order two proposed by Wang et al. (2022)
#' @usage Make_Exp2_Wang(name, nameInput1, nameOutput, cinput1, rate)
#' @param name The analog gate/unit name
#' @param nameInput1 The input 1 name
#' @param nameOutput The output name
#' @param cinput1 The initial concentration of input 1
#' @param rate The reaction rate of the gate/unit
#' @return A analog gate/unit created with its name, species, CRN reactions,
#' initial concentrations (ci), and reaction rates (ki) constants
#' @examples Make_Exp2_Wang('exp1w', 'Xe5', 'Ze5', 3, 1e-3)

Make_Exp2_Wang <- function(name, nameInput1, nameOutput, cinput1, rate) {
  species <- list(
    input1 = nameInput1,
    output = nameOutput,
    intermediate1 = jn(name, '_E'),
    intermediate2 = jn(name, '_waste')
  )

  ci <- c(cinput1, 0, 2, 0)

  reactions <- c(
    # 'x5 + E -> x5 + z5 + E'
    jn(species$input1, ' + ', species$intermediate1, ' -> ', species$input1,
       ' + ', species$output, ' + ', species$intermediate1),
    # 'Z5 -> waste'
    jn(species$output, ' -> ', species$intermediate2),
    # '(n-1)x5 -> (n-1)x5 + E'
    jn(species$input1, ' -> ', species$input1,
       ' + ', species$intermediate1),
    # 'E -> waste'
    jn(species$intermediate1, ' -> ', species$intermediate2)
  )

  ki        <- c(rate, rate, rate, rate)

  exp2_gate <- list(
    name      = name,
    species   = species,
    reactions = reactions,
    ci        = ci,
    ki        = ki
  )

  return(exp2_gate)
}

# _____________________________________________________________________________

#' @export
#' @title Make_Sqrt2_Wang
#' @description Analog exponential 1/n order two proposed by Wang et al. (2022)
#' @usage Make_Sqrt2_Wang(name, nameInput1, nameOutput, cinput1, rate)
#' @param name The analog gate/unit name
#' @param nameInput1 The input 1 name
#' @param nameOutput The output name
#' @param cinput1 The initial concentration of input 1
#' @param rate The reaction rate of the gate/unit
#' @return A analog gate/unit created with its name, species, CRN reactions,
#' initial concentrations (ci), and reaction rates (ki) constants
#' @examples Make_Sqrt2_Wang('sqr1w', 'Xq6', 'Zq6', 9, 1e-3)

Make_Sqrt2_Wang <- function(name, nameInput1, nameOutput, cinput1, rate) {
  species <- list(
    input1 = nameInput1,
    output = nameOutput,
    intermediate1 = jn(name, '_F'),
    intermediate2 = jn(name, '_waste')
  )

  ci <- c(cinput1, 0, 4, 0)

  reactions <- c(
    # 'x6 -> x6 + z6'
    jn(species$input1, ' -> ', species$input1, ' + ', species$output),
    # 'Z6 + F -> F'
    jn(species$output, ' + ', species$intermediate1, ' -> ', species$intermediate1),
    # 'F -> waste'
    jn(species$intermediate1, ' -> ', species$intermediate2),
    # 'z6 -> z6 + F'
    jn(species$output, ' -> ', species$output, ' + ', species$intermediate1)
  )

  ki        <- c(rate, rate, rate, rate)

  sqrt2_gate <- list(
    name      = name,
    species   = species,
    reactions = reactions,
    ci        = ci,
    ki        = ki
  )

  return(sqrt2_gate)
}

# _____________________________________________________________________________

#' @export
#' @title Make_Integrator_OishiYordanov
#' @description Analog integrator proposed by Oishi et al. (2011)
#' and Yordanov et al. (2014)
#' @usage Make_Integrator_OishiYordanov(name, nameInput1, nameInput2,
#' nameOutput1, nameOutput2, cinput1, cinput2, rate)
#' @param name The analog gate/unit name
#' @param nameInput1 The input 1 name
#' @param nameInput2 The input 2 name
#' @param nameOutput1 The output 1 name
#' @param nameOutput2 The output 2 name
#' @param cinput1 The initial concentration of input 1
#' @param cinput2 The initial concentration of input 2
#' @param rate The reaction rate of the gate/unit
#' @return A analog gate/unit created with its name, species, CRN reactions,
#' initial concentrations (ci), and reaction rates (ki) constants
#' @examples Make_Integrator_OishiYordanov('int11o', 'Up', 'Un', 'Yp', 'Yn', 5, 3, 1e-3)

Make_Integrator_OishiYordanov <- function(name, nameInput1, nameInput2,
                    nameOutput1, nameOutput2, cinput1, cinput2, rate) {

  species <- list(
    input1 = nameInput1,   #U+
    input2 = nameInput2,   #U-
    output1 = nameOutput1, #Y+
    output2 = nameOutput2  #Y-
  )

  ci <- c(cinput1, cinput2, 0, 0)

  reactions <- c(
    # 'U+ -> U+ + Y+'
    jn(species$input1, ' -> ', species$input1, ' + ', species$output1),
    # 'U- -> U- + Y-'
    jn(species$input2, ' -> ', species$input2, ' + ', species$output2),
    # 'Y+ + Y- -> 0'
    jn(species$output1, ' + ', species$output2, ' -> 0')
  )

  ki        <- c(rate, rate, rate*10)

  integrator_gate <- list(
    name      = name,
    species   = species,
    reactions = reactions,
    ci        = ci,
    ki        = ki
  )

  return(integrator_gate)
}

# _____________________________________________________________________________
# FUNÇÃO: Make_Mux2
# DESCRIÇÃO: O multiplexador consiste em duas entradas (E1 e E2) e dois
# sinais de controle (Control1 e Control2), os quais são habilitados (Gate1 e
# Gate2) e desabilitados (Gate1' e Gate2') por gates.

#' @export
#' @title Make_Mux2
#' @description Analog multiplexer with two inputs and two controls
#' @usage Make_Mux2(name, nameInput1, nameInput2, nameControl1, nameControl2,
#' nameOutput, cinput1, cinput2, control1, control2, crange, rate)
#' @param name The analog gate/unit name
#' @param nameInput1 The input 1 name
#' @param nameInput2 The input 2 name
#' @param nameControl1 The control 1 name
#' @param nameControl2 The control 2 name
#' @param nameOutput The output name
#' @param cinput1 The initial concentration of input 1
#' @param cinput2 The initial concentration of input 2
#' @param control1 The initial concentration of control 1
#' @param control2 The initial concentration of control 2
#' @param crange The initial concentration of the gate/unit operation range
#' @param rate The reaction rate of the gate/unit
#' @return A analog gate/unit created with its name, species, CRN reactions,
#' initial concentrations (ci), and reaction rates (ki) constants
#' @examples Make_Mux2('mux11','In1','In2','Ctr1','Ctr2','Out', 5, 1, 10, 0, 10, 2e-3)

Make_Mux2 <- function(name, nameInput1, nameInput2, nameControl1, nameControl2,
                      nameOutput, cinput1, cinput2, control1, control2, crange,
                      rate) {
  species <- list(
    input1 = nameInput1, #E1
    input2 = nameInput2, #E2
    output1 = nameControl1,  #C1
    output2 = nameControl2,  #C2
    gate1E = jn(name, '_GEn1'), #G1E
    gate2E = jn(name, '_GEn2'), #G2E
    gate1U = jn(name, '_GUn1'), #G1U
    gate2U = jn(name, '_GUn2'), #G2U
    output = nameOutput  #Output
  )

  ci <- c(cinput1, cinput2, control1, control2, control1, control2, crange, crange, 0)

  reactions <- c(
    # 'G1U + C1 -> G1E'
    jn(species$gate1U, ' + ', species$output1, ' -> ', species$output1, species$gate1E),
    # 'G1E + C2 -> G1U'
    jn(species$gate1E, ' + ', species$output2, ' -> ', species$output2, species$gate1U),
    # 'G2U + C2 -> G2'
    jn(species$gate2U, ' + ', species$output2, ' -> ', species$output2, species$gate2E),
    # 'G2E + C1 -> G2U'
    jn(species$gate2E, ' + ', species$output1, ' -> ', species$output1, species$gate2U),
    # 'E1 + G1 -> Output'
    jn(species$input1, ' + ', species$gate1E, ' -> ', species$output),
    # 'E2 + G2 -> Output'
    jn(species$input2, ' + ', species$gate2E, ' -> ', species$output)
  )

  ki <- c(rate, rate, rate, rate, rate, rate)

  mux2_gate <- list(
    name      = name,
    species   = species,
    reactions = reactions,
    ci        = ci,
    ki        = ki
  )

  return(mux2_gate)
}

# _____________________________________________________________________________

#' @export
#' @title Make_Oscillator_Dalchau
#' @description Analog oscillator with three inputs proposed by Dalchau et al. (2018)
#' @usage Make_Oscillator_Dalchau(name, nameInput1, nameInput2, nameInput3,
#' cinput1, cinput2, cinput3, rate)
#' @param name The analog gate/unit name
#' @param nameInput1 The input 1 name
#' @param nameInput2 The input 2 name
#' @param nameInput3 The input 3 name
#' @param cinput1 The initial concentration of input 1
#' @param cinput2 The initial concentration of input 2
#' @param cinput3 The initial concentration of input 3
#' @param rate The reaction rate of the gate/unit
#' @return A analog gate/unit created with its name, species, CRN reactions,
#' initial concentrations (ci), and reaction rates (ki) constants
#' @examples Make_Oscillator_Dalchau('clk', 'clk1', 'clk2', 'clk3', 15, 5, 10, 2e-5)

Make_Oscillator_Dalchau <- function(name, nameInput1, nameInput2, nameInput3,
                                    cinput1, cinput2, cinput3, rate) {
  species <- list(
    input1 = nameInput1, #A
    input2 = nameInput2, #B
    input3 = nameInput3  #C
  )

  ci <- c(cinput1, cinput2, cinput3)

  reactions <- c(
    # 'B + A -> B + B'
    jn(species$input2, ' + ', species$input1, ' -> ', species$input2,
       ' + ', species$input2),
    # 'C + B -> C + C'
    jn(species$input3, ' + ', species$input2, ' -> ', species$input3,
       ' + ', species$input3),
    # 'A + C -> A + A'
    jn(species$input1, ' + ', species$input3, ' -> ', species$input1,
       ' + ', species$input1)
  )

  ki        <- c(rate, rate, rate)

  oscillator_gate <- list(
    name      = name,
    species   = species,
    reactions = reactions,
    ci        = ci,
    ki        = ki
  )

  return(oscillator_gate)
}

# _____________________________________________________________________________

#' @export
#' @title Make_Adder2In_Buisman
#' @description Analog adder with two inputs proposed by Buisman et al. (2009)
#' @usage Make_Adder2In_Buisman(name, nameInput1, nameInput2, nameOutput1, cinput1,
#' cinput2, rate)
#' @param name The analog gate/unit name
#' @param nameInput1 The input 1 name
#' @param nameInput2 The input 2 name
#' @param nameOutput The output name
#' @param cinput1 The initial concentration of input 1
#' @param cinput2 The initial concentration of input 2
#' @param rate The reaction rate of the gate/unit
#' @return A analog gate/unit created with its name, species, CRN reactions,
#' initial concentrations (ci), and reaction rates (ki) constants
#' @examples Make_Adder2In_Buisman('N0', 'Y1', 'Y3', 'NH0', 0, 0, 2e-3)

Make_Adder2In_Buisman <- function(name, nameInput1, nameInput2, nameOutput1, cinput1,
                                  cinput2, rate) {
  species <- list(
    input1 = nameInput1,
    input2 = nameInput2,
    output1 = nameOutput1
  )

  ci <- c(cinput1, cinput2, 0)

  reactions <- c(
    # 'A -> A + X'
    jn(species$input1, ' -> ', species$input1, ' + ', species$output1),
    # 'B -> B + X'
    jn(species$input2, ' -> ', species$input2, ' + ', species$output1),
    # 'X -> 0'
    jn(species$output1, ' -> 0')
  )

  ki        <- c(rate, rate, rate)

  add_gate_b <- list(
    name      = name,
    species   = species,
    reactions = reactions,
    ci        = ci,
    ki        = ki
  )

  return(add_gate_b)
}

# _____________________________________________________________________________

#' @export
#' @title Make_Weighted_Signal_Arredondo
#' @description Analog computing weighted sums multiplying two inputs proposed
#'  by Arredondo et al. (2022)
#' @usage Make_Weighted_Signal_Arredondo(name, nameInput1, nameWeight1, nameOutput1,
#' cinput1, cweight1, rate)
#' @param name The analog gate/unit name
#' @param nameInput1 The input 1 name
#' @param nameInput2 The weight 1 name
#' @param nameOutput The output name
#' @param cinput1 The initial concentration of input 1
#' @param cinput2 The initial concentration of weight 1
#' @param rate The reaction rate of the gate/unit
#' @return A analog gate/unit created with its name, species, CRN reactions,
#' initial concentrations (ci), and reaction rates (ki) constants
#' @examples Make_Weighted_Signal_Arredondo('I1W11', 'I1', 'W11', 'Y1', cinput1, cweight1, 2e-3)

Make_Weighted_Signal_Arredondo <- function(name, nameInput1, nameWeight1, nameOutput1,
                                           cinput1, cweight1, rate) {
  species <- list(
    input1 = nameInput1,
    weight1 = nameWeight1,
    output1 = nameOutput1
  )

  ci <- c(cinput1, cweight1, 0)

  reactions <- c(
    # 'X1 + W1 -> I1 + W1 + Y1'
    jn(species$input1, ' + ', species$weight1, ' -> ', species$input1, ' + ',
       species$weight1, ' + ', species$output1),
    # 'I1 -> 0'
    jn(species$input1, ' -> 0')
  )

  ki        <- c(rate, rate)

  signal_gate_a <- list(
    name      = name,
    species   = species,
    reactions = reactions,
    ci        = ci,
    ki        = ki
  )

  return(signal_gate_a)
}
