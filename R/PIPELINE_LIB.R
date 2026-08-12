Make_Absence_Indicator<- function(name, rate, fast_factor, nameInput, nameOutput) {
  species <- list(
    input = nameInput,  # x
    output = nameOutput # x_absence
  )

  ci <- c(0, 0)

  reactions <- c(
    jn('0 -> ', species$output),
    jn(species$input, '+', species$output, '->', species$input),
    jn('2 ',species$output, '->', species$output)
  )

  ki        <- c(rate, fast_factor*rate, fast_factor*rate)

  gate <- list(
    name      = name,
    species   = species,
    reactions = reactions,
    ci        = ci,
    ki        = ki
  )

  return(gate)
}
