dnar_root <- Sys.getenv("DNAR_ROOT", unset = "/workspaces/dnar")
dnar_r_root <- file.path(dnar_root, "R")

if (!dir.exists(dnar_r_root)) {
  stop(sprintf("DNAr development source directory does not exist: %s", dnar_r_root))
}

source_order <- c(
  "parser.R",
  "sto_reactor.R",
  "crn_reactor.R",
  "dsd.R",
  "4domain_reactor.R",
  "io.R",
  "util_functions.R",
  "forced_concentrations.R"
)
missing_files <- source_order[!file.exists(file.path(dnar_r_root, source_order))]
if (length(missing_files) > 0) {
  stop(sprintf("DNAr development source is incomplete; missing: %s", paste(missing_files, collapse = ", ")))
}

previous_directory <- getwd()
setwd(dnar_root)
for (source_file in source_order) {
  source(file.path(dnar_r_root, source_file), local = .GlobalEnv)
}
setwd(previous_directory)
