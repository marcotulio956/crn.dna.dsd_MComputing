# DNAr adapter: nM, seconds, explicit IDs, volume-weighted mass-conserving merges.
suppressPackageStartupMessages(library(jsonlite))
script_arguments <- commandArgs(trailingOnly=FALSE)
script_file <- script_arguments[grepl("^--file=", script_arguments)][1]
script_directory <- if (length(script_file) == 0 || is.na(script_file)) getwd() else dirname(normalizePath(sub("^--file=", "", script_file)))
source(file.path(script_directory, "source_dnar.R"))
args <- commandArgs(trailingOnly=TRUE)
input <- fromJSON(args[1], simplifyVector=FALSE)
ids <- vapply(input$species, function(s) s$id, character(1))
names_species <- vapply(input$species, function(s) s$name, character(1))
name_by_id <- setNames(names_species, ids)
reactions <- character(0)
rates <- numeric(0)
for (r in input$reactions) {
  left <- paste(name_by_id[unlist(r$reactants)], collapse=" + ")
  right <- paste(name_by_id[unlist(r$products)], collapse=" + ")
  reactions <- c(reactions, paste(left, "->", right))
  rates <- c(rates, r$rate)
  if (!is.null(r$reverse_rate)) {
    reactions <- c(reactions, paste(right, "->", left))
    rates <- c(rates, r$reverse_rate)
  }
}
outputs <- list()
finals <- list()
volumes <- list()

`%||%` <- function(value, fallback) if (is.null(value)) fallback else value

resolve_forcing <- function(config, species) {
  if (is.null(config) || identical(config$enabled, FALSE)) return(NULL)
  allowed <- c("saw_wave_input", "sinusoidal_input", "step_input", "pulse_input", "square_input")
  name <- config$name %||% config[["function"]]
  if (is.null(name) || !name %in% allowed) stop("Unsupported forcing function")
  target <- config$species %||% ""
  if (!target %in% ids) stop("Forcing references an unknown species id")
  fn <- get(name, envir=.GlobalEnv)
  params <- config$params %||% list()
  setNames(list(function(t) do.call(fn, c(list(t=t), params))), name_by_id[[target]])
}

simulate_circuit <- function(species, ci, reactions, rates, times, settings, forcing) {
  circuit <- list(species=species, ci=ci, reactions=reactions, ki=rates, t=times)
  if (isTRUE(settings$dna) && isTRUE(settings$stochastic)) {
    return(React_4domain_stochastic(circuit, volume=settings$volume %||% 10,
      seed=settings$seed %||% NULL, forced_concentrations=forcing))
  }
  if (isTRUE(settings$dna)) {
    return(React_4domain(circuit, engine=settings$engine %||% "desolve",
      forced_concentrations=forcing))
  }
  if (isTRUE(settings$stochastic)) {
    return(React_stochastic(circuit, volume=settings$volume %||% 10,
      seed=settings$seed %||% NULL, forced_concentrations=forcing))
  }
  React_circuit(circuit, engine=settings$engine %||% "desolve", forced_concentrations=forcing)
}

settings <- input$settings %||% list()
forcing <- resolve_forcing(settings$forcing, ids)
for (d in input$droplets) {
  ci <- setNames(rep(0, length(ids)), ids)
  if (length(d$parents)) {
    total <- sum(vapply(d$parents, function(p) volumes[[p]], numeric(1)))
    for (p in d$parents) {
      weight <- if (input$settings$mixing == "legacy_sum") 1 else volumes[[p]] / total
      ci <- ci + finals[[p]] * weight
    }
  } else {
    for (id in names(d$concentrations)) ci[id] <- d$concentrations[[id]]
  }
  # Relative time avoids losing tiny but positive intervals in large timestamps.
  dt <- d$end - d$start
  if (dt < 0) stop("Negative chemical lifetime")
  if (dt == 0 || length(reactions) == 0 || length(ids) == 0) {
    times <- unique(c(d$start, d$end))
    frame <- data.frame(time=times)
    for (id in ids) frame[[id]] <- rep(ci[id], length(times))
  } else {
    relative <- unlist(d$offsets)
    result <- simulate_circuit(names_species, unname(ci), reactions, rates, relative,
      settings, forcing)
    frame <- as.data.frame(if (is.list(result) && !is.null(result$behavior)) result$behavior else result)
    frame <- frame[, c("time", names_species), drop=FALSE]
    if (nrow(frame) != length(relative)) stop("Incomplete DNAr integration")
    frame$time <- relative + d$start
    frame$time[1] <- d$start
    frame$time[nrow(frame)] <- d$end
    colnames(frame) <- c("time", ids)
  }
  finals[[d$id]] <- setNames(as.numeric(frame[nrow(frame), ids, drop=FALSE]), ids)
  volumes[[d$id]] <- d$volume
  outputs[[d$id]] <- frame
}
write_json(outputs, args[2], dataframe="rows", auto_unbox=TRUE, digits=NA, na="null")
