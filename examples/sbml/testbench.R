# Testbench para verificar a importacao de casos SBML no DNAr.
#
# Uso:
#   source("examples/sbml/testbench.R")
#   result <- run_sbml_case("00028", "examples/sbml/cases")
#   results <- run_sbml_cases("examples/sbml/cases")
#
# Para a SBML Test Suite, base_dir deve apontar para o diretorio que contem
# os arquivos <id>-sbml-l3v2.xml, <id>-settings.txt e <id>-results.csv.

source(file.path("R", "parser.R"))
source(file.path("R", "util_functions.R"))
source(file.path("R", "crn_reactor.R"))
source(file.path("R", "sbml.R"))

parse_sbml_settings <- function(filepath) {
    if (!file.exists(filepath)) {
        stop("Arquivo de configuracao nao encontrado: ", filepath)
    }

    lines <- readLines(filepath, warn = FALSE)
    read_number <- function(key) {
        line <- grep(paste0("^\\s*", key, "\\s*:"), lines, value = TRUE)
        if (length(line) != 1L) {
            stop("Chave '", key, "' ausente ou repetida em ", filepath)
        }
        value <- as.numeric(trimws(sub(
            paste0("^\\s*", key, "\\s*:"), "", line
        )))
        if (!is.finite(value)) {
            stop("Valor invalido para '", key, "' em ", filepath)
        }
        value
    }

    list(
        start = read_number("start"),
        duration = read_number("duration"),
        steps = as.integer(read_number("steps"))
    )
}

find_sbml_case_file <- function(case_id, base_dir, suffix) {
    candidates <- file.path(
        base_dir,
        c(
            paste0(case_id, suffix),
            case_id,
            paste0(case_id, "-", suffix)
        )
    )
    matches <- candidates[file.exists(candidates)]
    if (length(matches) == 0L) NA_character_ else matches[1L]
}

compare_sbml_results <- function(result, reference,
                                 absolute_tolerance = 1e-6,
                                 relative_tolerance = 1e-4) {
    if (!"time" %in% names(result) || !"time" %in% names(reference)) {
        stop("Resultado e referencia devem conter a coluna 'time'.")
    }
    species <- setdiff(names(result), "time")
    missing <- setdiff(species, names(reference))
    if (length(missing) > 0L) {
        stop("Especies ausentes na referencia: ", paste(missing, collapse = ", "))
    }
    if (nrow(result) != nrow(reference)) {
        stop("Resultado e referencia possuem numero de linhas diferente.")
    }

    values <- do.call(rbind, lapply(species, function(name) {
        difference <- abs(result[[name]] - reference[[name]])
        relative <- difference / pmax(abs(reference[[name]]), 1e-12)
        data.frame(
            species = name,
            max_absolute_error = max(difference, na.rm = TRUE),
            max_relative_error = max(relative, na.rm = TRUE),
            stringsAsFactors = FALSE
        )
    }))
    passed <- all(
        values$max_absolute_error <= absolute_tolerance |
            values$max_relative_error <= relative_tolerance
    )
    list(
        passed = passed,
        by_species = values,
        max_absolute_error = max(values$max_absolute_error),
        max_relative_error = max(values$max_relative_error),
        absolute_tolerance = absolute_tolerance,
        relative_tolerance = relative_tolerance
    )
}

run_sbml_case <- function(case_id, base_dir,
                          absolute_tolerance = 1e-6,
                          relative_tolerance = 1e-4,
                          engine = "desolve") {
    if (!sbml_available()) {
        stop("libSBML nao esta disponivel neste ambiente.")
    }

    xml_file <- find_sbml_case_file(
        case_id, base_dir, "-sbml-l3v2.xml"
    )
    if (is.na(xml_file)) {
        xml_file <- file.path(base_dir, paste0(case_id, ".xml"))
    }
    if (!file.exists(xml_file)) {
        stop("Arquivo SBML nao encontrado para o caso ", case_id)
    }

    validation <- validate_sbml(xml_file)
    model <- read_sbml_to_dnar(xml_file)
    settings_file <- find_sbml_case_file(case_id, base_dir, "-settings.txt")
    reference_file <- find_sbml_case_file(case_id, base_dir, "-results.csv")

    if (is.na(settings_file) || is.na(reference_file)) {
        return(list(
            case_id = case_id,
            xml_file = xml_file,
            validation = validation,
            model = model,
            status = "nao suportado",
            reason = "settings ou referencia nao encontrados"
        ))
    }

    settings <- parse_sbml_settings(settings_file)
    times <- seq(
        settings$start,
        settings$start + settings$duration,
        length.out = settings$steps + 1L
    )
    simulation <- react3(
        species = model$species,
        ci = model$ci,
        reactions = model$reactions,
        ki = model$ki,
        t = times,
        engine = engine
    )
    reference <- read.csv(reference_file, check.names = FALSE)
    comparison <- compare_sbml_results(
        simulation, reference,
        absolute_tolerance, relative_tolerance
    )

    list(
        case_id = case_id,
        xml_file = xml_file,
        validation = validation,
        model = model,
        settings = settings,
        simulation = simulation,
        reference = reference,
        comparison = comparison,
        status = if (comparison$passed) "passou" else "falhou"
    )
}

run_sbml_cases <- function(base_dir, case_ids = NULL, ...) {
    if (is.null(case_ids)) {
        case_ids <- sub(
            "-sbml-l[23].*\\.xml$", "",
            basename(list.files(
                base_dir, pattern = "-sbml-l[23].*\\.xml$",
                full.names = FALSE
            ))
        )
        if (length(case_ids) == 0L) {
            case_ids <- sub(
                "\\.xml$", "",
                basename(list.files(
                    base_dir, pattern = "\\.xml$", full.names = FALSE
                ))
            )
        }
    }
    results <- lapply(case_ids, function(case_id) {
        tryCatch(
            run_sbml_case(case_id, base_dir, ...),
            error = function(error) list(
                case_id = case_id,
                status = "falhou",
                reason = conditionMessage(error)
            )
        )
    })
    summary <- do.call(rbind, lapply(results, function(result) {
        data.frame(
            case_id = result$case_id,
            status = result$status,
            max_absolute_error = if (!is.null(result$comparison)) {
                result$comparison$max_absolute_error
            } else NA_real_,
            max_relative_error = if (!is.null(result$comparison)) {
                result$comparison$max_relative_error
            } else NA_real_,
            reason = if (is.null(result$reason)) "" else result$reason,
            stringsAsFactors = FALSE
        )
    }))
    list(results = results, summary = summary)
}

# Executa o caso local se este arquivo for chamado diretamente.
if (sys.nframe() == 0L) {
    base_dir <- file.path("examples", "sbml", "cases")
    print(run_sbml_cases(base_dir, case_ids = "00028")$summary)
}
