source('R/parser.R')
source('R/util_functions.R')
source('R/crn_reactor.R')
source('R/sbml.R')
source('R/io.r')
source('R/util_functions.R')

# #define the list of cases
# case_id <- c() # list containing the 00028
# 
# # 1. Parse the circuit
# crn <- read_sbml_to_dnar("examples/sbml/cases/00028.xml")
# 
# # 2. Define the simulation timeframe
# t_seq <- seq(0, 2, length.out = 50)
# 
# # 3. Simulate using the Julia diffeqr backend for speed on large DSD networks
# sim_results <- react4(
#   species = crn$species, 
#   ci = crn$ci, 
#   reactions = crn$reactions, 
#   ki = crn$ki, 
#   t = t_seq,
#   engine = 'diffeqr'
# )
# Plot_behavior(sim_results, title=jn("SBML CRN: ", case_id))
# # import sbml test case .csv
# reference <- read.csv("C:/Users/Mark/AppData/Roaming/.test-suite/cases/semantic/00028/00028-results.csv")
# Plot_behavior(reference, title=jn("SBML REF: ", case_id))


# library(DNAr) # Ensure required libraries are loaded

# Helper function to parse SBML test case settings
parse_sbml_settings <- function(filepath) {
  lines <- readLines(filepath, warn = FALSE)
  
  # Extracts the numeric value following "key:"
  extract_num <- function(key) {
    match_line <- grep(paste0("^", key, ":"), lines, value = TRUE)
    if (length(match_line) > 0) {
      return(as.numeric(trimws(sub(paste0("^", key, ":"), "", match_line))))
    }
    return(NA)
  }
  
  list(
    start    = extract_num("start"),
    duration = extract_num("duration"),
    steps    = extract_num("steps")
  )
}

# Main simulation wrapper
run_sbml_case <- function(case_id, base_dir, generate_plots = FALSE, save_csv = FALSE) {
  # 1. Construct dynamic file paths
  xml_file      <- file.path(base_dir, paste0(case_id, "-sbml-l3v2.xml"))
  settings_file <- file.path(base_dir, paste0(case_id, "-settings.txt"))
  out_csv_file  <- file.path(base_dir, paste0(case_id, "-results-dnar.csv"))
  ref_csv_file  <- file.path(base_dir, paste0(case_id, "-results.csv"))
  
  # 2. Parse settings to define the timeframe
  settings <- parse_sbml_settings(settings_file)
  # length.out is steps + 1 to account for time = 0
  t_seq <- seq(settings$start, settings$start + settings$duration, length.out = settings$steps + 1)
  
  # 3. Parse the circuit
  crn <- read_sbml_to_dnar(xml_file)
  
  # 4. Simulate using diffeqr
  sim_results <- react4(
    species   = crn$species, 
    ci        = crn$ci, 
    reactions = crn$reactions, 
    ki        = crn$ki, 
    t         = t_seq,
    engine    = 'desolve'
  )
  
  # 5. Optional CSV Output
  if (save_csv) {
    # The SBML test runner generally expects "time" as the first column
    write.csv(sim_results, out_csv_file, row.names = FALSE, quote = FALSE)
  }
  
  # 6. Optional Plotting
  if (generate_plots) {
    Plot_behavior(sim_results, title = paste("SBML CRN:", case_id))
    
    if (file.exists(ref_csv_file)) {
      reference <- read.csv(ref_csv_file)
      Plot_behavior(reference, title = paste("SBML REF:", case_id))
    } else {
      warning(paste("Reference CSV not found for case", case_id))
    }
  }
  
  return(sim_results)
}

# ==========================================
# Execution Block
# ==========================================

# Define the list of cases to iterate through
case_ids <- c("00028") 
base_directory <- "C:/Users/Mark/AppData/Roaming/.test-suite/cases/semantic/00028/" # Update this to your actual directory

# Run all cases
results_list <- list()
for (case in case_ids) {
  cat("Running case:", case, "\n")
  
  # Toggle flags as needed (set save_csv = TRUE for the SBML test runner)
  results_list[[case]] <- run_sbml_case(
    case_id        = case, 
    base_dir       = base_directory, 
    generate_plots = TRUE, 
    save_csv       = TRUE   
  )
}