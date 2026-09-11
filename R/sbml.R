library(xml2)

# install.packages(c("V8", "xml2"))

# if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
# remotes::install_github("tomicapretto/latex2r")


read_sbml_to_dnar <- function(xml_file_path) {
  doc <- read_xml(xml_file_path)
  
  # Dynamically extract the default namespace (handles both Level 2 and Level 3)
  ns <- xml_ns(doc) 
  if (length(ns) == 0 || !("d1" %in% names(ns))) {
    # Fallback mapping if xml_ns() assigns a different prefix
    ns <- xml_ns_rename(ns, names(ns)[1], "d1") 
  }
  
  # ---------------------------------------------------------
  # 1. Parse Species and Initial Concentrations
  # ---------------------------------------------------------
  species_nodes <- xml_find_all(doc, ".//d1:species", ns)
  species_ids <- xml_attr(species_nodes, "id")
  
  # Attempt to read amounts, fallback to concentrations, default to 0
  ci <- as.numeric(xml_attr(species_nodes, "initialAmount"))
  ci_conc <- as.numeric(xml_attr(species_nodes, "initialConcentration"))
  
  na_amounts <- is.na(ci)
  ci[na_amounts] <- ci_conc[na_amounts]
  ci[is.na(ci)] <- 0.0 
  
  # Name the vector to align seamlessly with react4's y0 mapping
  names(ci) <- species_ids
  
  # ---------------------------------------------------------
  # 2. Parse Reactions, Stoichiometry, and Kinetics
  # ---------------------------------------------------------
  reaction_nodes <- xml_find_all(doc, ".//d1:reaction", ns)
  reactions <- character(length(reaction_nodes))
  ki <- numeric(length(reaction_nodes))
  
  for (i in seq_along(reaction_nodes)) {
    node <- reaction_nodes[[i]]
    
    # Helper to format stoichiometric strings (e.g., "2X")
    build_side <- function(ref_nodes) {
      if (length(ref_nodes) == 0) return("0")
      
      parts <- sapply(ref_nodes, function(n) {
        stoich <- xml_attr(n, "stoichiometry")
        if (is.na(stoich) || stoich == "1" || stoich == "1.0") {
          stoich_str <- ""
        } else {
          stoich_str <- stoich
        }
        paste0(stoich_str, xml_attr(n, "species"))
      })
      paste(parts, collapse = " + ")
    }
    
    reactants <- xml_find_all(node, ".//d1:listOfReactants/d1:speciesReference", ns)
    products <- xml_find_all(node, ".//d1:listOfProducts/d1:speciesReference", ns)
    
    r_str <- build_side(reactants)
    p_str <- build_side(products)
    reactions[i] <- paste(r_str, "->", p_str)
    
    # Extract rate constant (k). Targets standard mass-action parameter definitions.
    rate_node <- xml_find_first(
      node, 
      ".//d1:kineticLaw//d1:localParameter[@id='k'] | .//d1:kineticLaw//d1:localParameter | .//d1:kineticLaw//d1:parameter", 
      ns
    )
    
    if (!is.na(rate_node)) {
      ki[i] <- as.numeric(xml_attr(rate_node, "value"))
    } else {
      warning(sprintf("No kinetic parameter found for reaction %s. Defaulting to 1.0", xml_attr(node, "id")))
      ki[i] <- 1.0 
    }
  }
  
  # ---------------------------------------------------------
  # 3. Construct DNAr Object
  # ---------------------------------------------------------
  crn <- list(
    species = species_ids,
    ci = ci,
    reactions = reactions,
    ki = ki
  )
  
  return(crn)
}

convert__to_latex <- function(mathml_str) {
  # 1. Ensure required packages are installed
  if (!requireNamespace("V8", quietly = TRUE)) {
    stop("The 'V8' package is required. Run install.packages('V8') first.")
  }
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("The 'xml2' package is required. Run install.packages('xml2') first.")
  }
  
  # 2. Clean the input (remove extra whitespace/newlines that break JS strings)
  clean_mathml <- as.character(xml2::read_xml(mathml_str))
  clean_mathml <- gsub("\n", "", clean_mathml)
  clean_mathml <- gsub("'", "\\'", clean_mathml, fixed = TRUE)
  
  # 3. Initialize V8 context
  ctx <- V8::v8()
  
  # 4. Fetch and load the open-source mathml-to-latex JavaScript library
  # We use a reliable UNPKG CDN link for the minified library
  js_url <- "https://unpkg.com"
  
  tryCatch({
    ctx$source(js_url)
  }, error = function(e) {
    stop("Failed to download the JavaScript converter library. Check your internet connection.")
  })
  
  # 5. Assign the MathML string to the JS environment
  ctx$assign("mml", clean_mathml)
  
  # 6. Execute the library conversion method
  # The library registers a global window object or module exports depending on context.
  # UNPKG bundles usually expose 'MathMLToLaTeX' or an export function directly.
  latex_result <- tryCatch({
    ctx$eval("MathMLToLaTeX.convert(mml)")
  }, error = function(e) {
    # Fallback syntax if the module structure varies across CDN versions
    ctx$eval("typeof MathMLToLaTeX !== 'undefined' ? MathMLToLaTeX.convert(mml) : MathMLToLaTeX.default.convert(mml)")
  })
  
  return(latex_result)
}

solve_latex_equation <- function(latex_str, ...) {
  if (!requireNamespace("latex2r", quietly = TRUE)) {
    stop("The 'latex2r' package is required.")
  }
  
  # 1. Clean the LaTeX string (remove standard equation shells like $ or \[, if any)
  clean_latex <- gsub("^\\$|\\$$|^\\\\\\[|\\\\\\]$", "", trimws(latex_str))
  
  # If the string contains an "=", split it to only take the right side for evaluation
  if (grepl("=", clean_latex)) {
    clean_latex <- unlist(strsplit(clean_latex, "="))[2]
  }

  # 2. Convert the LaTeX math formula directly into an executable R function
  # latex2fun automatically maps variables like x, t, or time as function arguments
  r_math_function <- latex2r::latex2fun(clean_latex)
  
  # 3. Execute the newly built function with the variables passed by the user
  result <- tryCatch({
    r_math_function(...)
  }, error = function(e) {
    stop(paste("Evaluation failed. Make sure you passed all necessary variable names correctly.\nError:", e$message))
  })
  
  return(result)
}

# latex_string <- convert_mathml_to_latex(mathml_input)
# # Internal result: "y=5\\cdot time^{2}+3\\cdot time"

# # 3. Pass that string directly into our solver and define the 'time' argument
# solved_value <- solve_latex_equation(latex_string, time = 4)

# print(solved_value)