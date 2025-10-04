rm(list = ls())

source('R/4domain_reactor.R')
source('R/ANALOG_GATE_LIB.R')
source('R/analysis.R')
source('R/crn_reactor.R')
source('R/dsd.R')
source('R/NEURON_SIM.R')
source('R/GATE_LIB.R')
source('R/io.R')
source('R/parser.R')
source('R/util_functions.R')
source('R/metric_functions.R')
source('examples/izhikevich_neuron/plotspikes.R')

jn <- function(...) { paste(..., sep = '') }

library(ggplot2) # plot()
library(dplyr) # mutate()

# --- presets
izh_presets <- list(
  "Regular Spiking (RS)" = list(a=0.02, b=0.2, c=-65, d=8, v0=-65),
  "Intrinsically Bursting (IB)" = list(a=0.02, b=0.2, c=-55, d=4, v0=-65),
  "Chattering (CH)" = list(a=0.02, b=0.2, c=-50, d=2, v0=-60),
  "Fast Spiking (FS)" = list(a=0.1, b=0.2, c=-65, d=2, v0=-65),
  "Low-threshold spiking (LTS)" = list(a=0.02, b=0.25, c=-65, d=2, v0=-65),
  "Phasic Spiking (PS)" = list(a=0.02, b=0.25, c=-65, d=6, v0=-64),
  "Tonic Spiking (TS)" = list(a=0.02, b=0.2, c=-65, d=6, v0=-65)
)

# --- timing (fine dt recommended)
timing <- seq(0, 200, by = 0.1)  # 200 ms, 0.1 ms steps

# --- define input-current patterns tuned to each behaviour
I_patterns <- list(
  # constant pulse (20-180 ms). amplitude tuned for each behaviour below
  "RS"  = function(t) ifelse(t >= 20 & t <= 180, 10, 0),
  # lower tonic + brief strong kick to reveal bursts
  "IB"  = function(t) {base <- ifelse(t >= 20 & t <= 180, 6, 0);kick <- ifelse(t >= 50 & t <= 55, 20, 0);base + kick},
  # strong tonic current
  "CH"  = function(t) ifelse(t >= 20 & t <= 180, 20, 0),
  "FS"  = function(t) ifelse(t >= 20 & t <= 180, 15, 0),
  "LTS" = function(t) ifelse(t >= 30 & t <= 170, 8, 0),
  # short pulse only
  "PS"  = function(t) ifelse(t >= 30 & t <= 40, 12, 0),
  "TS"  = function(t) ifelse(t >= 20 & t <= 180, 8, 0)
)

# utility to pick I pattern by preset name
pick_I_by_name <- function(name) {
  if (grepl("Regular Spiking", name)) return(I_patterns[["RS"]])
  if (grepl("Intrinsically Bursting", name)) return(I_patterns[["IB"]])
  if (grepl("Chattering", name)) return(I_patterns[["CH"]])
  if (grepl("Fast Spiking", name)) return(I_patterns[["FS"]])
  if (grepl("Low-threshold", name)) return(I_patterns[["LTS"]])
  if (grepl("Phasic", name)) return(I_patterns[["PS"]])
  if (grepl("Tonic", name)) return(I_patterns[["TS"]])
  # fallback: small pulse
  return(function(t) ifelse(t >= 20 & t <= 180, 10, 0))
}

describe_I <- function(I_obj, max_len = 300) {
  # If numeric scalar or vector, summarize
  if (!is.function(I_obj)) {
    if (is.numeric(I_obj)) {
      return(sprintf("numeric vector (len=%d) min=%.3g max=%.3g", length(I_obj), min(I_obj), max(I_obj)))
    }
    return(as.character(I_obj))
  }
  
  body_str <- paste(deparse(body(I_obj)), collapse = " ")
  body_str <- gsub("\\s+", " ", trimws(body_str))
  
  # find all ifelse(...) occurrences
  matches <- gregexpr("ifelse\\s*\\(([^\\)]*)\\)", body_str, perl = TRUE)
  reg_matches <- regmatches(body_str, matches)[[1]]
  
  if (length(reg_matches) == 0) {
    # no ifelse patterns — return collapsed body (truncated)
    return(if (nchar(body_str) <= max_len) body_str else paste0(substr(body_str, 1, max_len), "..."))
  }
  
  parse_one_ifelse <- function(ifelse_text) {
    inner <- sub("^ifelse\\s*\\((.*)\\)$", "\\1", ifelse_text, perl = TRUE)
    # split into parts by comma; assume typical form: condition, value, else
    parts <- strsplit(inner, ",")[[1]]
    parts <- trimws(parts)
    cond <- if (length(parts) >= 1) parts[1] else ""
    val  <- if (length(parts) >= 2) parts[2] else ""
    # try to extract interval numbers from condition like "t >= 20 & t <= 180"
    nums <- regmatches(cond, gregexpr("-?\\d+\\.?\\d*", cond, perl = TRUE))[[1]]
    if (length(nums) >= 2 && grepl("&", cond)) {
      return(paste0("t in [", nums[1], ",", nums[2], "] -> ", val))
    }
    # fallback: show short condition
    cond_short <- if (nchar(cond) > 80) paste0(substr(cond, 1, 80), "...") else cond
    return(paste0("if(", cond_short, ") -> ", val))
  }
  
  summaries <- vapply(reg_matches, parse_one_ifelse, FUN.VALUE = character(1))
  out <- paste(summaries, collapse = "; ")
  if (nchar(out) > max_len) out <- paste0(substr(out, 1, max_len), "...")
  out
}

title_from_I_smart <- function(name, I_fun) {
  paste0(name, " — input: ", describe_I(I_fun))
}

# --- Loop through presets and plot each behaviour
for (name in names(izh_presets)) {
  params <- izh_presets[[name]]
  I_fun <- pick_I_by_name(name)
  title <- title_from_I_smart(name, I_fun) # paste0(name, " — input pattern: ", deparse(body(I_fun)))
  
  cat("Running: ", name, "\n")
  # call the wrapper which builds `result` and calls your Plot_behavior()
  plot_izhikevich_with_Plot_behavior(
    timing = timing,
    I = I_fun,
    params = params,
    circuit = NULL,                # wrapper will use plot_species explicitly
    gate_numbers = NULL,
    y_min = -90,                   # optional horizontal lines to help read scale
    y_max = 40,
    plot_species = c("v","u","I","spike"),  # show v, u, input current, spike markers
    plot_species_dotted = c("I"), 
    chart_title = title
  )
  # pause briefly so you can view each plot interactively; comment out if running non-interactive
  readline(prompt = "Press <enter> to continue to next behaviour...")
}
