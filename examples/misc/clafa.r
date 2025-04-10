library(DNArLogic)
library(binaryLogic)

# Auxiliary function to join strings
jn <- function(...) { paste(..., sep = '') }

make_FA_maj <- function(name, bit1value, bit2value, bit3value) {
  entity <- make_circuit(0)

  # Instantiate the Full-Adder sub-entities
  # FA input signals
  # FIXME: create a generic gate without output
  interface <- make_generic3to1_gate(name, 'FAMAJ', bit1value, bit2value, bit3value)
  entity$inputs <- list(interface$species$input1,
                        interface$species$input2,
                        interface$species$input3)

  # Maj-gates
  carry <- make_majority_gate(jn(name, '_Cout'), 0, 0, 0)
  sum   <- make_majority_gate(jn(name, '_Sum'), 0, 0, 0)
  aux   <- make_majority_gate(jn(name, '_Aux'), 0, 0, 0)

  # Generate
  gen <- make_and_gate(jn(name, '_gen'), 0, 0)

  # Propagate
  pro <- make_or_gate(jn(name, '_pro'), 0, 0)

  # FA output signals
  entity$outputs <- list(sum$species$output, carry$species$output, gen$species$output, pro$species$output)

  # Inserting all the sub-entities
  entity <- circuit_insert_gate(entity, interface)
  entity <- circuit_insert_gate(entity, carry)
  entity <- circuit_insert_gate(entity, sum)
  entity <- circuit_insert_gate(entity, aux)
  entity <- circuit_insert_gate(entity, gen)
  entity <- circuit_insert_gate(entity, pro)

  # Make the links between sub-entities signals
  entity <- circuit_insert_gate(entity, make_link_gate(carry$species$output, sum$species$input1, negated = TRUE))
  entity <- circuit_insert_gate(entity, make_link_gate(aux$species$output, sum$species$input3))

  # Make the fanouts from the inputs
  entity <- circuit_insert_gate(entity, make_link_gate(interface$species$input1, gen$species$input1))
  entity <- circuit_insert_gate(entity, make_link_gate(interface$species$input2, gen$species$input2))

  entity <- circuit_insert_gate(entity, make_link_gate(interface$species$input1, pro$species$input1))
  entity <- circuit_insert_gate(entity, make_link_gate(interface$species$input2, pro$species$input2))

  entity <- circuit_insert_gate(entity, make_link_gate(interface$species$input1, carry$species$input1))
  entity <- circuit_insert_gate(entity, make_link_gate(interface$species$input2, carry$species$input2))
  entity <- circuit_insert_gate(entity, make_link_gate(interface$species$input3, carry$species$input3))

  entity <- circuit_insert_gate(entity, make_link_gate(interface$species$input3, sum$species$input2))

  entity <- circuit_insert_gate(entity, make_link_gate(interface$species$input1, aux$species$input1))
  entity <- circuit_insert_gate(entity, make_link_gate(interface$species$input2, aux$species$input2))
  entity <- circuit_insert_gate(entity, make_link_gate(interface$species$input3, aux$species$input3, negated = TRUE))

  # Combine all the sub-entities in the final entity
  entity <- circuit_compile(entity)

  return(entity)
}

make_nbit_CLAFA <- function(name, input1value, input2value, carryinvalue, bits = 4) {
  input1value <- as.integer(as.list(as.binary(input1value, n=bits, littleEndian = TRUE)))
  input2value <- as.integer(as.list(as.binary(input2value, n=bits, littleEndian = TRUE)))

  entity <- make_circuit(0)

  # Adders
  fa    <- list(make_FA_maj(jn(name, '_fa1'), input1value[1], input2value[1], carryinvalue))
  entity <- circuit_insert_gate(entity, fa[[1]])
  entity$outputs<- c(fa[[1]]$outputs[[1]]$value1)

  generateOrPropagateCarry <- make_or_gate(jn(name, '_genpropcarry1'), 0, 0)
  propagateAndCarry        <- make_and_gate(jn(name, '_propcarry1'), 0, carryinvalue)

  carryOutputSignal <- generateOrPropagateCarry$species$output
  entity$carryouts <- c(carryOutputSignal$value1)

  entity <- circuit_insert_gate(entity, generateOrPropagateCarry)
  entity <- circuit_insert_gate(entity, propagateAndCarry)

  entity <- circuit_insert_gate(entity, make_link_gate(fa[[1]]$outputs[[4]], propagateAndCarry$species$input1))
  entity <- circuit_insert_gate(entity, make_link_gate(propagateAndCarry$species$output, generateOrPropagateCarry$species$input2))
  entity <- circuit_insert_gate(entity, make_link_gate(fa[[1]]$outputs[[3]], generateOrPropagateCarry$species$input1))

  for (i in seq(2, bits))
  {
    fa <- rlist::list.append(fa, make_FA_maj(jn(name, '_fa', i), input1value[i], input2value[i], 0))
    entity <- circuit_insert_gate(entity, fa[[i]])
    entity$outputs <- c(entity$outputs, fa[[i]]$outputs[[1]]$value1)

    entity <- circuit_insert_gate(entity, make_link_gate(carryOutputSignal, fa[[i]]$inputs[[3]]))

    # Carry Look-a-head
    generateOrPropagateCarry <- make_or_gate(jn(name, '_genpropcarry', i), 0, 0)
    propagateAndCarry        <- make_and_gate(jn(name, '_propcarry', i), 0, 0)

    entity <- circuit_insert_gate(entity, generateOrPropagateCarry)
    entity <- circuit_insert_gate(entity, propagateAndCarry)

    entity <- circuit_insert_gate(entity, make_link_gate(fa[[i]]$outputs[[4]], propagateAndCarry$species$input1))
    entity <- circuit_insert_gate(entity, make_link_gate(carryOutputSignal, propagateAndCarry$species$input2))

    entity <- circuit_insert_gate(entity, make_link_gate(propagateAndCarry$species$output, generateOrPropagateCarry$species$input2))
    entity <- circuit_insert_gate(entity, make_link_gate(fa[[i]]$outputs[[3]], generateOrPropagateCarry$species$input1))

    # Update the carry signal to be used in the next carry
    carryOutputSignal <- generateOrPropagateCarry$species$output
    entity$carryouts <- c(entity$carryouts, carryOutputSignal$value1)
  }

  entity <- DNArLogic::circuit_compile(entity)

  return(entity)
}

plot_circuit <- function (results, signals, newColNames, legend = TRUE) {
  filteredResults <- results[, c('time', signals)]

  if ( is.null(newColNames) == FALSE && length(newColNames) > 0 )
  {
    colnames(filteredResults) <- newColNames
  }

  groupedResults <- reshape2::melt(filteredResults,
                                   id.vars = c('time'),
                                   measure.vars = dimnames(filteredResults)[[2]][-1])

  legend_name <- 'Species'

  g <- ggplot2::ggplot(groupedResults, ggplot2::aes(time, value, color = variable)) +
    ggplot2::theme_minimal(base_size = 18) +
    ggplot2::labs(x = 'Time (a.u.)', y = 'Concentration (a.u.)', color = legend_name) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::theme( strip.background = ggplot2::element_blank(),
                    strip.text.x = ggplot2::element_blank() ) +
    ggplot2::geom_line(size = 1.3) +
    ggplot2::aes(linetype = variable) +
    ggplot2::labs(linetype = legend_name) +
    ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 5))) +
    ggplot2::theme(legend.key.width = ggplot2::unit(4, "line")) +
    ggplot2::scale_y_continuous(breaks=seq(0,1,1))

  if (legend == FALSE)
  {
    g <- g + ggplot2::theme(legend.position = "none")
  }

  return(g)
}

# Carry Look-a-head Adder Simulation -----------------------

# Simulation timing
startTime <- 0
endTime   <- 4000
deltaTime <- 100

# Make new circuit
timing  <- seq(startTime, endTime, deltaTime)
circuit <- DNArLogic::make_circuit(timing)

# Add 3 + 7
carrylookahead <- make_nbit_CLAFA('CLA', 3, 7, 0)
circuit  <- circuit_insert_gate(circuit, carrylookahead)
compiled <- DNArLogic::circuit_compile(circuit)
nreactions_claadder <- length(compiled$reactions)

results <- react(
  species   = compiled$species,
  ci        = compiled$ci,
  reactions = compiled$reactions,
  ki        = compiled$ki,
  t         = compiled$t
)

newColNames <- c('time', 'Bit 0', 'Bit 1', 'Bit 2', 'Bit 3')
adder_g <- plot_circuit(results, carrylookahead$outputs, newColNames, legend = TRUE)
print(adder_g)
