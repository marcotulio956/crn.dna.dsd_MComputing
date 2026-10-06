#' Simulate a CRN using Stochastic Gillespie Algorithm
#'
#' This function performs stochastic simulation of a chemical reaction network
#' using the Gillespie algorithm (via the GillespieSSA2 package). This approach
#' is useful for systems with low molecular counts where discrete stochasticity
#' is important, such as in studying noise-driven dynamics in biological systems
#' and DNA strand displacement reactions.
#'
#' @section Requirements:
#'   \itemize{
#'     \item Initial concentrations (\code{ci}) must be non-negative integers
#'           representing molecule counts. Floating-point values will be converted
#'           to integers via rounding.
#'     \item Reaction rate constants (\code{ki}) must be constant numeric values;
#'           time-varying rates (functions) are not supported in this implementation.
#'     \item The GillespieSSA2 package must be installed.
#'   }
#'
#' @section Known limitations:
#'   \itemize{
#'     \item Only supports uni or bimolecular reactions (same as \code{\link{react}()}).
#'     \item Bidirectional reactions are not supported; use two separate reactions.
#'     \item Time-varying rates and forced concentrations are not supported.
#'     \item For very fast reactions or very large molecular counts, consider
#'           \code{\link{react}()} instead.
#'   }
#'
#' @param species    A vector with the species of the reaction. The order of
#'                   this vector is important and must match the order in \code{ci}.
#' @param ci         A vector specifying the initial molecule counts of each species.
#'                   Values should be non-negative integers; floats will be rounded.
#' @param reactions  A vector with the reactions of the CRN, in the same format as
#'                   for \code{\link{react}()}.
#' @param ki         A vector defining the rate constant (propensity scaling) for each
#'                   reaction, in order. Must be constant numeric values (no time-varying
#'                   functions). For bimolecular reactions, this is the second-order rate
#'                   constant; for unimolecular reactions, the first-order rate constant.
#' @param t          A vector specifying time points for which output should be returned.
#'                   The simulation will run until the maximum time value.
#'                   Only trajectories at specified time points will be recorded.
#' @param seed       Random seed for reproducibility. If NULL, uses a random seed.
#'                   Default is NULL.
#' @param verbose    Be verbose and print information about the simulation process.
#'                   Default value is \code{FALSE}.
#' @param ...        Additional parameters (currently unused, for compatibility with
#'                   \code{\link{react}()}).
#'
#' @return A data frame with each row being a time point and each column being a species.
#'         The first column is named 'time'. Column names correspond to species names.
#'         Note: Due to the discrete nature of Gillespie simulation, the returned values
#'         are always non-negative integers (molecule counts).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Simple decay reaction with stochastic simulation
#' species <- c('A', 'B')
#' ci <- c(100, 0)  # Start with 100 molecules of A
#' reactions <- c('A -> B')
#' ki <- c(0.1)  # First-order decay rate
#' t <- seq(0, 100, by=1)
#'
#' # Run stochastic simulation
#' result <- react_stochastic(species, ci, reactions, ki, t)
#'
#' # Compare with deterministic simulation
#' result_det <- react(species, ci, reactions, ki, t)
#' }
react_stochastic_GSSA2 <- function(species, ci, reactions, ki, t, seed = NULL, verbose = FALSE, ...) {

    # Check the crn specification
    reactions <- check_crn(species, ci, reactions, ki, t)

    # Validate and process ki parameter (must be constants for stochastic)
    ki_values <- numeric(length(ki))
    for(i in seq_along(ki)) {
        if(is.function(ki[[i]])) {
            stop(sprintf("ki[%d] is a function, but react_stochastic() only supports constant rates. ",
                        "Use react() for time-varying rates, or provide a constant numeric value."))
        } else if(is.numeric(ki[[i]])) {
            ki_values[i] <- as.numeric(ki[[i]])
        } else {
            stop(sprintf("ki[%d] must be a numeric constant, not %s", 
                        i, class(ki[[i]])[1]))
        }
    }

    # Convert concentrations to integer molecule counts
    ci_counts <- as.integer(round(ci))
    if(verbose) {
        if(!all.equal(ci, ci_counts)) {
            message("Note: Initial concentrations were rounded to integers for stochastic simulation.")
            message("Original: ", paste(ci, collapse=", "))
            message("Rounded:  ", paste(ci_counts, collapse=", "))
        }
    }

    # Get stoichiometry information for the reaction matrix (used by GillespieSSA2)
    sto_info <- get_M(reactions, species)

    # Stochastic SSA requires integer molecularity per reaction side.
    # Non-integer stoichiometry is supported in deterministic react(), but not here.
    frac_left <- abs(sto_info$react - round(sto_info$react)) > 1e-12
    frac_right <- abs(sto_info$prod - round(sto_info$prod)) > 1e-12
    if(any(frac_left | frac_right)) {
        stop(
            paste(
                'react_stochastic() does not support non-integer stoichiometric coefficients.',
                'Use react() for fractional stoichiometry, or rewrite reactions with integer coefficients.'
            )
        )
    }
    
    # Create the propensity function for GillespieSSA2
    # The propensity function defines the rate at which each reaction occurs
    propensity_func <- function(x, t, ...) {
        # x is the current state (molecule counts)
        # Returns a vector of propensities (expected # of reactions per unit time)
        propensities <- numeric(length(reactions))
        
        for(i in seq_along(reactions)) {
            reactants <- reactants_in_reaction(species, reactions[i])
            stoc_react <- sto_info$react[, i]
            
            if(is.null(reactants) || all(is.na(reactants))) {
                # Formation reaction (0 -> products): constant propensity
                propensities[i] <- ki_values[i]
            } else {
                # Calculate propensity based on reaction order
                # For unimolecular: propensity = k * [A]
                # For bimolecular: propensity = k * [A] * [B]
                propensities[i] <- ki_values[i]
                for(j in reactants) {
                    for(m in 1:stoc_react[j]) {
                        propensities[i] <- propensities[i] * max(0, x[j] - (m - 1))
                    }
                }
            }
        }
        return(propensities)
    }

    # Set seed if provided for reproducibility
    if(!is.null(seed)) {
        set.seed(seed)
    }

    if(verbose) {
        message("Starting stochastic simulation with GillespieSSA2...")
        message(sprintf("Species: %s", paste(species, collapse=", ")))
        message(sprintf("Reactions: %s", paste(reactions, collapse="; ")))
        message(sprintf("Time range: %.4f to %.4f", min(t), max(t)))
    }

    # Build a Gillespie-compatible reaction map once
    reactant_map <- lapply(reactions, function(reaction) {
        r_map <- reactants_in_reaction(species, reaction)
        if(is.null(r_map)) {
            return(NA)
        }
        return(r_map)
    })

    sto_react <- t(sto_info$react)
    v_exp_reactants <- lapply(seq_along(reactant_map), function(i) {
        if(length(reactant_map[[i]]) > 1 || !is.na(reactant_map[[i]])) {
            return(sto_react[reactant_map[[i]], i])
        }
        return(0)
    })

    # Helper to compute one propensity vector at state x
    propensity_vec <- function(x_state) {
        a <- numeric(length(reactions))
        for(i in seq_along(reactions)) {
            react_map <- reactant_map[[i]]
            if(length(react_map) == 1 && is.na(react_map[1])) {
                a[i] <- ki_values[i]
            } else {
                # Mass-action propensity with combinatorial correction
                # product_j prod_{m=0}^{nu_j-1} (x_j - m)
                term <- ki_values[i]
                exps <- v_exp_reactants[[i]]
                for(j_idx in seq_along(react_map)) {
                    s_idx <- react_map[j_idx]
                    nu_j <- exps[j_idx]
                    if(nu_j <= 0) {
                        next
                    }
                    if(x_state[s_idx] < nu_j) {
                        term <- 0
                        break
                    }
                    for(m in 0:(nu_j - 1)) {
                        term <- term * (x_state[s_idx] - m)
                    }
                }
                a[i] <- max(0, term)
            }
        }
        a
    }

    # Try GillespieSSA2 first (if available and compatible), otherwise fallback to built-in direct SSA
    used_fallback <- FALSE
    result_matrix <- matrix(0, nrow = length(t), ncol = length(species))
    result_times <- t

    can_try_gssa2 <- requireNamespace("GillespieSSA2", quietly = TRUE)
    if(can_try_gssa2) {
        ssa_attempt <- tryCatch({
            # Keep this call explicit to expose API mismatch quickly.
            GillespieSSA2::ssa(
                x0 = ci_counts,
                a = function(x, t) propensity_vec(x),
                nu = t(sto_info$M),
                t0 = min(t),
                tf = max(t),
                method = "D"
            )
        }, error = function(e) e)

        if(!inherits(ssa_attempt, "error")) {
            ssa_time <- ssa_attempt$t
            ssa_state <- ssa_attempt$x
            for(i in seq_along(t)) {
                idx <- max(which(ssa_time <= t[i]))
                if(length(idx) == 0 || is.infinite(idx)) {
                    idx <- 1
                }
                result_matrix[i, ] <- ssa_state[idx, ]
            }
        } else {
            if(verbose) {
                msg <- paste0(
                    "GillespieSSA2 API mismatch detected (", ssa_attempt$message,
                    "). Falling back to built-in direct SSA implementation."
                )
                message(msg)
            }
            used_fallback <- TRUE
        }
    } else {
        used_fallback <- TRUE
        if(verbose) {
            message("GillespieSSA2 not installed. Using built-in direct SSA implementation.")
        }
    }

    if(used_fallback) {
        # migrated to 
    }

    # Create output data frame
    df_result <- data.frame(result_matrix)
    df_result <- cbind(time = result_times, df_result)
    names(df_result) <- c('time', species)

    if(verbose) {
        message("Stochastic simulation completed successfully.")
        message(sprintf("Final state: %s", paste(df_result[nrow(df_result), -1], collapse=", ")))
    }

    return(df_result)
}

react_stochastic_forced <- function(
    species,
    ci,
    reactions,
    ki,
    t,
    forced_concentrations = NULL,
    seed = NULL,
    verbose = FALSE,
    max_events = 1e8,
    volume = 1,
    ...) {

    # ============================================================
    # Validate CRN
    # ============================================================

    reactions <- check_crn(
        species,
        ci,
        reactions,
        ki,
        t
    )

    if(!is.null(seed)) {
        set.seed(seed)
    }

    # ============================================================
    # Process kinetic rates
    # ============================================================

    ki_processed <- vector("list", length(ki))

    for(i in seq_along(ki)) {

        if(is.function(ki[[i]])) {

            ki_processed[[i]] <- ki[[i]]

        } else if(is.numeric(ki[[i]])) {

            k_const <- as.numeric(ki[[i]])

            ki_processed[[i]] <- function(tt) k_const

        } else {

            stop(sprintf(
                "ki[%d] must be numeric or function",
                i
            ))
        }
    }

    # ============================================================
    # Validate forcing functions
    # ============================================================

    if(!is.null(forced_concentrations)) {

        if(!is.list(forced_concentrations)) {
            stop("forced_concentrations must be a named list")
        }

        for(nm in names(forced_concentrations)) {

            if(!is.function(forced_concentrations[[nm]])) {

                stop(sprintf(
                    "forced_concentrations[['%s']] must be a function",
                    nm
                ))
            }
        }
    }

    # ============================================================
    # Initial state
    # ============================================================

    # state <- as.numeric(round(ci))
    state <- as.numeric(round(ci)*volume)

    names(state) <- species

    # ============================================================
    # Stoichiometry
    # Convention:
    # rows = species
    # cols = reactions
    # ============================================================

    sto_info <- get_M(reactions, species)

    M <- sto_info$M
    react_sto <- t(sto_info$react)

    # ============================================================
    # Build reactant maps
    # ============================================================

    reactant_map <- lapply(
        reactions,
        function(reaction) {

            r_map <- reactants_in_reaction(
                species,
                reaction
            )

            if(is.null(r_map)) {
                return(NA)
            }

            r_map
        }
    )

    # Reaction orders
    v_exp_reactants <- lapply(
        seq_along(reactant_map),
        function(i) {

            rmap <- reactant_map[[i]]

            if(length(rmap) == 1 && is.na(rmap[1])) {
                return(0)
            }

            react_sto[rmap, i]
        }
    )

    # ============================================================
    # Propensity function
    # ============================================================

    propensity_vec <- function(x_state, current_t) {

        a <- numeric(length(reactions))

        ki_t <- sapply(
            ki_processed,
            function(f) f(current_t)
        )

        for(i in seq_along(reactions)) {

            react_map <- reactant_map[[i]]

            # Zeroth-order reaction
            if(length(react_map) == 1 &&
               is.na(react_map[1])) {

                a[i] <- ki_t[i]
                next
            }

            exps <- v_exp_reactants[[i]]

            reaction_order <- sum(exps)

            k_scaled <- ki_t[i] /
                (volume^(reaction_order - 1))

            term <- k_scaled
            
            #term <- ki_t[i]

            for(j_idx in seq_along(react_map)) {

                s_idx <- react_map[j_idx]

                nu_j <- exps[j_idx]

                if(nu_j <= 0) {
                    next
                }

                if(x_state[s_idx] < nu_j) {

                    term <- 0
                    break
                }

                # Exact CME combinatorial factor
                term <- term *
                    choose(x_state[s_idx], nu_j)
            }

            a[i] <- max(0, term)
        }

        a
    }

    # ============================================================
    # Output allocation
    # ============================================================

    out <- matrix(
        0,
        nrow = length(t),
        ncol = length(species)
    )

    colnames(out) <- species

    # ============================================================
    # Main SSA loop
    # ============================================================

    current_t <- min(t)

    out_i <- 1

    event_count <- 0

    while(out_i <= length(t) &&
          t[out_i] <= current_t) {

        out[out_i, ] <- state
        out_i <- out_i + 1
    }

    while(current_t < max(t) &&
          out_i <= length(t)) {

        event_count <- event_count + 1

        if(event_count > max_events) {

            stop(
                "Maximum number of SSA events exceeded."
            )
        }

        # ========================================================
        # Apply forced concentrations
        # ========================================================

        if(!is.null(forced_concentrations)) {

            for(forced_species in
                names(forced_concentrations)) {

                idx <- match(
                    forced_species,
                    species
                )

                if(is.na(idx)) {
                    next
                }

                forced_value <- tryCatch(

                    forced_concentrations[[forced_species]](current_t),

                    error = function(e) {

                        stop(sprintf(
                            paste0(
                                "Error evaluating forcing ",
                                "function for '%s' at t=%f: %s"
                            ),
                            forced_species,
                            current_t,
                            e$message
                        ))
                    }
                )

                # Clamp directly
                state[idx] <- max(
                    0,
                    # round(forced_value)
                    round(forced_value * volume)
                )
            }
        }

        # ========================================================
        # Propensities
        # ========================================================

        a <- propensity_vec(
            state,
            current_t
        )

        a0 <- sum(a)

        # No more reactions possible
        if(a0 <= 0) {

            while(out_i <= length(t)) {

                out[out_i, ] <- state

                out_i <- out_i + 1
            }

            break
        }

        # ========================================================
        # Sample next reaction time
        # ========================================================

        tau <- rexp(
            1,
            rate = a0
        )

        next_t <- current_t + tau

        # ========================================================
        # Record trajectory before jump
        # ========================================================

        while(out_i <= length(t) &&
              t[out_i] < next_t) {

            out[out_i, ] <- state

            out_i <- out_i + 1
        }

        # ========================================================
        # Select reaction
        # ========================================================

        r <- runif(1) * a0

        mu <- which(cumsum(a) >= r)[1]

        # ========================================================
        # State update
        # IMPORTANT:
        # M is species x reactions
        # ========================================================

        state <- state + M[mu, ]

        current_t <- next_t
    }

    # ============================================================
    # Fill remaining samples
    # ============================================================

    while(out_i <= length(t)) {

        out[out_i, ] <- state

        out_i <- out_i + 1
    }

    # ============================================================
    # Output dataframe
    # ============================================================

    df_result <- data.frame(
        time = t,
        # out
        out / volume
    )

    if(verbose) {

        message(
            sprintf(
                "SSA completed with %d events.",
                event_count
            )
        )

        message(
            sprintf(
                "Final state: %s",
                paste(
                    round(
                        df_result[
                            nrow(df_result),
                            -1
                        ]
                    ),
                    collapse = ", "
                )
            )
        )
    }

    return(df_result)
}

react_stochastic_frates <- function(
    species,
    ci,
    reactions,
    ki,
    t,
    forced_concentrations = NULL,
    seed = NULL,
    verbose = FALSE,
    max_events = 1e7,
    volume = 1,
    ...) {

    # ============================================================
    # Validate engine inputs
    # ============================================================

    reactions <- check_crn(
        species,
        ci,
        reactions,
        ki,
        t
    )

    if(!is.null(seed)) {
        set.seed(seed)
    }

    # ============================================================
    # Build stoichiometry
    # ============================================================

    sto_info <- get_M(reactions, species)

    M <- sto_info$M
    react_sto <- t(sto_info$react)

    # ============================================================
    # Build reactant maps
    # ============================================================

    reactant_map <- lapply(
        reactions,
        function(reaction) {

            r_map <- reactants_in_reaction(
                species,
                reaction
            )

            if(is.null(r_map)) {
                return(NA)
            }

            r_map
        }
    )

    # ============================================================
    # Reaction exponents
    # ============================================================

    v_exp_reactants <- lapply(
        seq_along(reactant_map),
        function(i) {

            rmap <- reactant_map[[i]]

            if(length(rmap) == 1 &&
               is.na(rmap[1])) {

                return(0)
            }

            react_sto[rmap, i]
        }
    )

    # ============================================================
    # Process kinetic functions
    #
    # Supported:
    #
    # 1. numeric constant
    # 2. function(t)
    # 3. function(t, y)
    # 4. function(t, y, species)
    #
    # IMPORTANT:
    # Returned value is interpreted as:
    #
    #     generalized kinetic coefficient
    #
    # and will STILL be multiplied by the
    # combinatorial mass-action term.
    #
    # Therefore:
    #
    #   A + B -> C
    #
    # with:
    #
    #   ki = function(t,y) y["X"]
    #
    # gives:
    #
    #   a = y["X"] * A * B
    #
    # ============================================================

    kinetic_functions <- vector(
        "list",
        length(ki)
    )

    for(i in seq_along(ki)) {

        k_current <- ki[[i]]

        # --------------------------------------------------------
        # Constant
        # --------------------------------------------------------

        if(is.numeric(k_current) &&
           length(k_current) == 1) {

            kinetic_functions[[i]] <- local({

                k_const <- as.numeric(k_current)

                function(t, y, species) {
                    k_const
                }

            })

        # --------------------------------------------------------
        # Functional
        # --------------------------------------------------------

        } else if(is.function(k_current)) {

            kinetic_functions[[i]] <- local({

                k_fun <- k_current

                nargs_k <- length(formals(k_fun))

                # function(t)

                if(nargs_k == 1) {

                    function(t, y, species) {
                        k_fun(t)
                    }

                # function(t, y)

                } else if(nargs_k == 2) {

                    function(t, y, species) {
                        k_fun(t, y)
                    }

                # function(t, y, species)

                } else if(nargs_k >= 3) {

                    function(t, y, species) {
                        k_fun(t, y, species)
                    }

                } else {

                    stop(sprintf(
                        "Unsupported function signature in ki[%d]",
                        i
                    ))
                }

            })

        } else {

            stop(sprintf(
                "ki[%d] must be numeric or function",
                i
            ))
        }
    }

    # ============================================================
    # Validate forcing functions
    # ============================================================

    if(!is.null(forced_concentrations)) {

        if(!is.list(forced_concentrations)) {

            stop(
                "forced_concentrations must be a named list"
            )
        }

        for(nm in names(forced_concentrations)) {

            if(!is.function(
                forced_concentrations[[nm]]
            )) {

                stop(sprintf(
                    paste0(
                        "forced_concentrations[['%s']] ",
                        "must be a function"
                    ),
                    nm
                ))
            }
        }
    }

    # ============================================================
    # Initial state
    # ============================================================

    state <- as.numeric(
        round(ci * volume)
    )

    names(state) <- species

    # ============================================================
    # Propensity function
    # ============================================================

    # propensity_vec <- function(
    #     x_state,
    #     current_t
    # ) {

    #     names(x_state) <- species

    #     a <- numeric(length(reactions))

    #     for(i in seq_along(reactions)) {

    #         react_map <- reactant_map[[i]]

    #         # ----------------------------------------------------
    #         # Evaluate generalized kinetic coefficient
    #         # ----------------------------------------------------

    #         k_val <- tryCatch({

    #             kinetic_functions[[i]](
    #                 current_t,
    #                 x_state,
    #                 species
    #             )

    #         }, error = function(e) {

    #             stop(sprintf(
    #                 paste0(
    #                     "Error evaluating ki[%d] ",
    #                     "at t=%f : %s"
    #                 ),
    #                 i,
    #                 current_t,
    #                 e$message
    #             ))
    #         })

    #         # ----------------------------------------------------
    #         # Zeroth-order reaction
    #         # ----------------------------------------------------

    #         if(length(react_map) == 1 &&
    #            is.na(react_map[1])) {

    #             a[i] <- max(0, k_val)
    #             next
    #         }

    #         # ----------------------------------------------------
    #         # Mass-action combinatorial factor
    #         # ----------------------------------------------------

    #         exps <- v_exp_reactants[[i]]

    #         reaction_order <- sum(exps)

    #         # Volume scaling
    #         #
    #         # CME-consistent scaling:
    #         #
    #         # bimolecular:
    #         #   divide by V
    #         #
    #         # trimolecular:
    #         #   divide by V^2
    #         #

    #         k_scaled <- k_val /
    #             (volume^(reaction_order - 1))

    #         term <- k_scaled

    #         for(j_idx in seq_along(react_map)) {

    #             s_idx <- react_map[j_idx]

    #             nu_j <- exps[j_idx]

    #             if(nu_j <= 0) {
    #                 next
    #             }

    #             # insufficient molecules

    #             if(x_state[s_idx] < nu_j) {

    #                 term <- 0
    #                 break
    #             }

    #             # exact CME combinatorial factor

    #             term <- term *
    #                 choose(
    #                     x_state[s_idx],
    #                     nu_j
    #                 )
    #         }

    #         a[i] <- max(0, term)
    #     }

    #     a
    # }
    # ============================================================
    # ATTEMPT OPTIMIZATION: Propensity function
    # ============================================================
    propensity_vec <- function(
        x_state,
        current_t
    ) {
        # REMOVED: names(x_state) <- species (Memory allocation bottleneck)
        
        a <- numeric(length(reactions))

        for(i in seq_along(reactions)) {
            react_map <- reactant_map[[i]]

            # REMOVED: tryCatch (Performance bottleneck). 
            # We assume your kinetic functions are safe to evaluate fast.
            k_val <- kinetic_functions[[i]](
                current_t,
                x_state,
                species
            )

            # ----------------------------------------------------
            # Zeroth-order reaction
            # ----------------------------------------------------
            if(length(react_map) == 1 && is.na(react_map[1])) {
                a[i] <- max(0, k_val)
                next
            }

            # ----------------------------------------------------
            # Mass-action combinatorial factor
            # ----------------------------------------------------
            exps <- v_exp_reactants[[i]]
            reaction_order <- sum(exps)

            k_scaled <- k_val / (volume^(reaction_order - 1))
            term <- k_scaled

            for(j_idx in seq_along(react_map)) {
                s_idx <- react_map[j_idx]
                nu_j <- exps[j_idx]

                if(nu_j <= 0) next
                
                if(x_state[s_idx] < nu_j) {
                    term <- 0
                    break
                }

                term <- term * choose(x_state[s_idx], nu_j)
            }

            a[i] <- max(0, term)
        }
        return(a)
    }

    # ============================================================
    # Allocate output
    # ============================================================

    out <- matrix(
        0,
        nrow = length(t),
        ncol = length(species)
    )

    colnames(out) <- species

    # ============================================================
    # SSA main loop
    # ============================================================

    current_t <- min(t)

    out_i <- 1

    event_count <- 0

    # initial output fill

    while(out_i <= length(t) &&
          t[out_i] <= current_t) {

        out[out_i, ] <- state

        out_i <- out_i + 1
    }

    while(current_t < max(t) &&
          out_i <= length(t)) {

        event_count <- event_count + 1

        if(event_count > max_events) {

            stop(
                paste0(
                    "Maximum number of SSA events ",
                    "exceeded."
                )
            )
        }

        # ========================================================
        # Forced concentrations
        # ========================================================

        if(!is.null(forced_concentrations)) {

            for(forced_species in
                names(forced_concentrations)) {

                idx <- match(
                    forced_species,
                    species
                )

                if(is.na(idx)) {
                    next
                }

                forced_value <- tryCatch({

                    forced_concentrations[[forced_species]](current_t)

                }, error = function(e) {

                    stop(sprintf(
                        paste0(
                            "Error evaluating forcing ",
                            "function for '%s' ",
                            "at t=%f : %s"
                        ),
                        forced_species,
                        current_t,
                        e$message
                    ))
                })

                state[idx] <- max(
                    0,
                    round(forced_value * volume)
                )
            }
        }

        # ========================================================
        # Compute propensities
        # ========================================================

        a <- propensity_vec(
            state,
            current_t
        )

        a0 <- sum(a)

        # no more reactions possible

        if(a0 <= 0) {

            while(out_i <= length(t)) {

                out[out_i, ] <- state

                out_i <- out_i + 1
            }

            break
        }

        # ========================================================
        # Draw next reaction time
        # ========================================================

        tau <- rexp(
            1,
            rate = a0
        )

        next_t <- current_t + tau

        # ========================================================
        # Record state before jump
        # ========================================================

        while(out_i <= length(t) &&
              t[out_i] < next_t) {

            out[out_i, ] <- state

            out_i <- out_i + 1
        }

        # ========================================================
        # Select reaction channel
        # ========================================================

        r <- runif(1) * a0

        mu <- which(
            cumsum(a) >= r
        )[1]

        # ========================================================
        # State update
        #
        # M:
        # rows = reactions
        # cols = species
        # ========================================================
        # ========================================================
        # State update
        # ========================================================
        # state <- state + M[mu, ]

        # state[state < 0] <- 0

        # current_t <- next_t

        state <- state + M[mu, ]
        state[state < 0] <- 0
        
        # ADDED: Re-apply names here ONCE per event, rather than 
        # inside the propensity loop.
        names(state) <- species 

        current_t <- next_t

    }

    # ============================================================
    # Fill remaining output rows
    # ============================================================

    while(out_i <= length(t)) {

        out[out_i, ] <- state

        out_i <- out_i + 1
    }

    # ============================================================
    # Build output dataframe
    # ============================================================

    df_result <- data.frame(
        time = t,
        out / volume
    )

    # ============================================================
    # Verbose summary
    # ============================================================

    if(verbose) {

        message(sprintf(
            "SSA completed with %d events.",
            event_count
        ))

        message(sprintf(
            "Final state: %s",
            paste(
                round(
                    df_result[
                        nrow(df_result),
                        -1
                    ],
                    4
                ),
                collapse = ", "
            )
        ))
    }

    return(df_result)
}

react_stochastic_frates_events <- function(
    species,
    ci,
    reactions,
    ki,
    t,
    forced_concentrations = NULL,
    seed = NULL,
    verbose = FALSE,
    max_events = 1e7,
    volume = 1,
    events = NULL,     ## >>> PATCH 2: new parameter
                        ## data.frame(time=, species=, amount=)
                        ## instantaneous injections/removals at
                        ## specific times, independent of any reaction
    ...
) {

    reactions <- check_crn(species, ci, reactions, ki, t)

    if(!is.null(seed)) set.seed(seed)

    ## >>> PATCH 2: validate & sort events
    if (!is.null(events)) {
        stopifnot(all(c("time","species","amount") %in% names(events)))
        events <- events[order(events$time), , drop = FALSE]
        ev_i  <- 1L
        ev_n  <- nrow(events)
    } else {
        ev_i <- 1L; ev_n <- 0L
    }
    ## <<< END PATCH 2

    sto_info <- get_M(reactions, species)
    M <- sto_info$M
    react_sto <- t(sto_info$react)

    reactant_map <- lapply(reactions, function(reaction) {
        r_map <- reactants_in_reaction(species, reaction)
        if(is.null(r_map)) return(NA)
        r_map
    })

    v_exp_reactants <- lapply(seq_along(reactant_map), function(i) {
        rmap <- reactant_map[[i]]
        if(length(rmap) == 1 && is.na(rmap[1])) return(0)
        react_sto[rmap, i]
    })

    kinetic_functions <- vector("list", length(ki))
    for(i in seq_along(ki)) {
        k_current <- ki[[i]]
        if(is.numeric(k_current) && length(k_current) == 1) {
            kinetic_functions[[i]] <- local({
                k_const <- as.numeric(k_current)
                function(t, y, species) k_const
            })
        } else if(is.function(k_current)) {
            kinetic_functions[[i]] <- local({
                k_fun <- k_current
                nargs_k <- length(formals(k_fun))
                if(nargs_k == 1) {
                    function(t, y, species) k_fun(t)
                } else if(nargs_k == 2) {
                    function(t, y, species) k_fun(t, y)
                } else if(nargs_k >= 3) {
                    function(t, y, species) k_fun(t, y, species)
                } else {
                    stop(sprintf("Unsupported function signature in ki[%d]", i))
                }
            })
        } else {
            stop(sprintf("ki[%d] must be numeric or function", i))
        }
    }

    if(!is.null(forced_concentrations)) {
        if(!is.list(forced_concentrations)) stop("forced_concentrations must be a named list")
        for(nm in names(forced_concentrations)) {
            if(!is.function(forced_concentrations[[nm]])) {
                stop(sprintf("forced_concentrations[['%s']] must be a function", nm))
            }
        }
    }

    state <- as.numeric(round(ci * volume))
    names(state) <- species

    propensity_vec <- function(x_state, current_t) {
        a <- numeric(length(reactions))
        for(i in seq_along(reactions)) {
            react_map <- reactant_map[[i]]
            k_val <- kinetic_functions[[i]](current_t, x_state, species)

            ## >>> PATCH 1: zeroth-order reactions now fall through to
            ## the SAME volume-scaling formula as every other order,
            ## instead of using max(0, k_val) unscaled. For order r=0,
            ## k_scaled = k_val / volume^(0-1) = k_val * volume, which
            ## is the correct concentration -> count-rate conversion
            ## (matches the deterministic ODE reference exactly; see
            ## test_zeroth_order_bug2.R). The old special-case branch
            ## is removed entirely -- the general code below already
            ## handles "no reactants" correctly via the nu_j<=0 skip,
            ## it just wasn't being allowed to run.
            exps <- v_exp_reactants[[i]]
            reaction_order <- sum(exps)
            k_scaled <- k_val / (volume^(reaction_order - 1))
            term <- k_scaled

            if(!(length(react_map) == 1 && is.na(react_map[1]))) {
                for(j_idx in seq_along(react_map)) {
                    s_idx <- react_map[j_idx]
                    nu_j <- exps[j_idx]
                    if(nu_j <= 0) next
                    if(x_state[s_idx] < nu_j) { term <- 0; break }
                    term <- term * choose(x_state[s_idx], nu_j)
                }
            }
            a[i] <- max(0, term)
            ## <<< END PATCH 1
        }
        return(a)
    }

    out <- matrix(0, nrow = length(t), ncol = length(species))
    colnames(out) <- species

    current_t <- min(t)
    out_i <- 1
    event_count <- 0

    while(out_i <= length(t) && t[out_i] <= current_t) {
        out[out_i, ] <- state
        out_i <- out_i + 1
    }

    while(current_t < max(t) && out_i <= length(t)) {

        event_count <- event_count + 1
        if(event_count > max_events) stop("Maximum number of SSA events exceeded.")

        if(!is.null(forced_concentrations)) {
            for(forced_species in names(forced_concentrations)) {
                idx <- match(forced_species, species)
                if(is.na(idx)) next
                forced_value <- forced_concentrations[[forced_species]](current_t)
                state[idx] <- max(0, round(forced_value * volume))
            }
        }

        a <- propensity_vec(state, current_t)
        a0 <- sum(a)

        if(a0 <= 0) {
            while(out_i <= length(t)) { out[out_i, ] <- state; out_i <- out_i + 1 }
            break
        }

        tau <- rexp(1, rate = a0)
        next_t <- current_t + tau

        ## >>> PATCH 2: an exogenous event between now and the next
        ## reaction takes priority -- apply it and restart the loop
        ## (fresh propensities), exactly like a reaction, but with a
        ## user-specified (not exponentially-drawn) time and effect.
        if (ev_i <= ev_n && events$time[ev_i] <= min(next_t, max(t))) {
            ## record output up to the event time first
            while(out_i <= length(t) && t[out_i] < events$time[ev_i]) {
                out[out_i, ] <- state
                out_i <- out_i + 1
            }
            current_t <- events$time[ev_i]
            sp <- events$species[ev_i]
            state[sp] <- state[sp] + events$amount[ev_i] * volume
            ev_i <- ev_i + 1L
            next
        }
        ## <<< END PATCH 2

        ## >>> PATCH 3: fill output rows in [current_t, next_t) using
        ## the CURRENT reaction-driven state for ordinary species, but
        ## re-evaluate forced species at each row's EXACT time, so a
        ## forced "clock" species reports its true value at every
        ## requested sample point, not just at the last reaction event
        ## (see test_forcing_staleness.R for the failure mode this
        ## fixes).
        while(out_i <= length(t) && t[out_i] < next_t) {
            row <- state
            if (!is.null(forced_concentrations)) {
                for (forced_species in names(forced_concentrations)) {
                    idx <- match(forced_species, species)
                    if (is.na(idx)) next
                    row[idx] <- max(0, round(forced_concentrations[[forced_species]](t[out_i]) * volume))
                }
            }
            out[out_i, ] <- row
            out_i <- out_i + 1
        }
        ## <<< END PATCH 3

        r <- runif(1) * a0
        mu <- which(cumsum(a) >= r)[1]

        state <- state + M[mu, ]
        state[state < 0] <- 0
        names(state) <- species

        current_t <- next_t
    }

    while(out_i <= length(t)) {
        out[out_i, ] <- state
        out_i <- out_i + 1
    }

    df_result <- data.frame(time = t, out / volume)

    if(verbose) {
        message(sprintf("SSA completed with %d events.", event_count))
        message(sprintf("Final state: %s",
                         paste(round(df_result[nrow(df_result), -1], 4), collapse = ", ")))
    }

    return(df_result)
}


tau_leap_sim <- function(ci_counts, t, sto_info, propensity_vec,
                         tau = 0.05, max_steps = 1e6, verbose = FALSE) {
  
  state <- as.numeric(ci_counts)
  current_t <- min(t)
  
  result_matrix <- matrix(0, nrow = length(t), ncol = length(state))
  out_i <- 1
  
  # Fill initial values
  while(out_i <= length(t) && t[out_i] <= current_t) {
    result_matrix[out_i, ] <- state
    out_i <- out_i + 1
  }
  
  steps <- 0
  
  while(current_t < max(t) && out_i <= length(t)) {
    
    a <- propensity_vec(state)
    a0 <- sum(a)
    
    if(a0 <= 0) {
      # no reactions possible
      while(out_i <= length(t)) {
        result_matrix[out_i, ] <- state
        out_i <- out_i + 1
      }
      break
    }
    
    # --- τ selection (simple fixed step for now) ---
    dt <- tau
    
    # --- Poisson sampling ---
    K <- rpois(length(a), lambda = a * dt)
    
    # --- state update ---
    delta <- as.numeric(K %*% sto_info$M)
    new_state <- state + delta
    
    # --- prevent negative populations (critical!) ---
    if(any(new_state < 0)) {
      # fallback to SSA step (one reaction)
      if(verbose) message("Negative detected → fallback SSA step")
      
      tau_ssa <- rexp(1, rate = a0)
      r <- runif(1) * a0
      mu <- which(cumsum(a) >= r)[1]
      
      state <- state + sto_info$M[mu, ]
      state[state < 0] <- 0
      current_t <- current_t + tau_ssa
      
    } else {
      state <- new_state
      current_t <- current_t + dt
    }
    
    # --- record output ---
    while(out_i <= length(t) && t[out_i] <= current_t) {
      result_matrix[out_i, ] <- state
      out_i <- out_i + 1
    }
    
    steps <- steps + 1
    if(steps > max_steps) {
      warning("Max steps reached in tau-leaping")
      break
    }
  }
  
  return(result_matrix)
}

react_tau_leap_forced <- function(
    species,
    ci,
    reactions,
    ki,
    t,
    tau = 0.05,
    forced_concentrations = NULL,
    seed = NULL,
    verbose = FALSE,
    max_steps = 1e6,
    volume = 1,
    ...
) {

    # ============================================================
    # Validate CRN & Set Seed (Assuming check_crn is defined)
    # ============================================================
    # reactions <- check_crn(species, ci, reactions, ki, t)
    
    if(!is.null(seed)) {
        set.seed(seed)
    }

    # ============================================================
    # Process kinetic rates (Time-dependent support)
    # ============================================================
    ki_processed <- vector("list", length(ki))
    for(i in seq_along(ki)) {
        if(is.function(ki[[i]])) {
            ki_processed[[i]] <- ki[[i]]
        } else if(is.numeric(ki[[i]])) {
            k_const <- as.numeric(ki[[i]])
            ki_processed[[i]] <- function(tt) k_const
        } else {
            stop(sprintf("ki[%d] must be numeric or function", i))
        }
    }

    # ============================================================
    # Initial state
    # ============================================================
    state <- as.numeric(round(ci) * volume)
    names(state) <- species

    # ============================================================
    # Stoichiometry & Reactant Maps (Assuming get_M / reactants_in_reaction)
    # ============================================================
    sto_info <- get_M(reactions, species)
    M <- sto_info$M
    react_sto <- t(sto_info$react)

    reactant_map <- lapply(reactions, function(rxn) {
        r_map <- reactants_in_reaction(species, rxn)
        if(is.null(r_map)) return(NA)
        r_map
    })

    v_exp_reactants <- lapply(seq_along(reactant_map), function(i) {
        rmap <- reactant_map[[i]]
        if(length(rmap) == 1 && is.na(rmap[1])) return(0)
        react_sto[rmap, i]
    })

    # ============================================================
    # Propensity function (Matches previous exact CME logic)
    # ============================================================
    propensity_vec <- function(x_state, current_t) {
        a <- numeric(length(reactions))
        ki_t <- sapply(ki_processed, function(f) f(current_t))

        for(i in seq_along(reactions)) {
            react_map <- reactant_map[[i]]

            # Zeroth-order
            if(length(react_map) == 1 && is.na(react_map[1])) {
                a[i] <- ki_t[i] * volume # Scale 0th order production
                next
            }

            exps <- v_exp_reactants[[i]]
            reaction_order <- sum(exps)
            k_scaled <- ki_t[i] / (volume^(reaction_order - 1))
            term <- k_scaled

            for(j_idx in seq_along(react_map)) {
                s_idx <- react_map[j_idx]
                nu_j <- exps[j_idx]
                if(nu_j <= 0) next
                if(x_state[s_idx] < nu_j) {
                    term <- 0
                    break
                }
                term <- term * choose(x_state[s_idx], nu_j)
            }
            a[i] <- max(0, term)
        }
        a
    }

    # ============================================================
    # Output allocation
    # ============================================================
    out <- matrix(0, nrow = length(t), ncol = length(species))
    colnames(out) <- species
    
    current_t <- min(t)
    out_i <- 1
    steps <- 0

    while(out_i <= length(t) && t[out_i] <= current_t) {
        out[out_i, ] <- state
        out_i <- out_i + 1
    }

    # ============================================================
    # Main Tau-Leaping Loop
    # ============================================================
    while(current_t < max(t) && out_i <= length(t)) {
        
        steps <- steps + 1
        if(steps > max_steps) {
            stop("Maximum number of tau-leaping steps exceeded.")
        }

        # --- 1. Apply forced concentrations ---
        if(!is.null(forced_concentrations)) {
            for(forced_species in names(forced_concentrations)) {
                idx <- match(forced_species, species)
                if(is.na(idx)) next
                
                forced_value <- forced_concentrations[[forced_species]](current_t)
                state[idx] <- max(0, round(forced_value * volume))
            }
        }

        # --- 2. Calculate Propensities ---
        a <- propensity_vec(state, current_t)
        a0 <- sum(a)

        if(a0 <= 0) {
            # System is frozen until a forced concentration changes it
            # We advance time by tau to check forced conditions again
            current_t <- current_t + tau
            
            while(out_i <= length(t) && t[out_i] <= current_t) {
                out[out_i, ] <- state
                out_i <- out_i + 1
            }
            next
        }

        # --- 3. Poisson sampling ---
        K <- rpois(length(a), lambda = a * tau)

        # --- 4. State update proposal ---
        delta <- as.numeric(K %*% M)
        new_state <- state + delta

        # --- 5. Collision resolution (Fallback to SSA) ---
        if(any(new_state < 0)) {
            if(verbose) message(sprintf("Negative state detected at t=%.3f. Falling back to SSA.", current_t))
            
            # Draw exact time to next single reaction
            tau_ssa <- rexp(1, rate = a0)
            
            # Select which reaction fired
            r <- runif(1) * a0
            mu <- which(cumsum(a) >= r)[1]
            
            # Update state exactly
            state <- state + M[mu, ]
            current_t <- current_t + tau_ssa
            
        } else {
            # Accept Tau step
            state <- new_state
            current_t <- current_t + tau
        }

        # --- 6. Record trajectory ---
        while(out_i <= length(t) && t[out_i] <= current_t) {
            out[out_i, ] <- state
            out_i <- out_i + 1
        }
    }

    # Fill remaining time points if loop exits early
    while(out_i <= length(t)) {
        out[out_i, ] <- state
        out_i <- out_i + 1
    }

    # Return as discrete molecule counts for factorial testing
    df_result <- data.frame(time = t, out)

    if(verbose) {
        message(sprintf("Tau-leaping completed in %d steps.", steps))
    }

    return(df_result)
}

simulate_ou_process <- function(
    t, 
    x0,    # start value
    theta, # Extrinsic Noise Reversion Rate 
    mu,    # mean
    sigma, # Extrinsic Noise Volatility , l,h: ( 0.05, 0.40 ) * mu  * sqrt(2*theta)
    clamp_zero = TRUE, 
    seed = NULL
) {
    if(!is.null(seed)) {
        set.seed(seed)
    }

    # ============================================================
    # 1. Parameter Processing (Aligning with your previous models)
    # Allows parameters to be either constants or functions of time
    # ============================================================
    make_time_func <- function(param) {
        if (is.function(param)) {
            return(param)
        } else if (is.numeric(param)) {
            return(function(current_t) param)
        } else {
            stop("Parameters must be numeric constants or functions of time.")
        }
    }

    theta_f <- make_time_func(theta)
    mu_f    <- make_time_func(mu)
    sigma_f <- make_time_func(sigma)

    # ============================================================
    # 2. Initialization
    # ============================================================
    N <- length(t)
    x <- numeric(N)
    x[1] <- x0

    # ============================================================
    # 3. Euler-Maruyama Integration Loop
    # ============================================================
    for (i in 1:(N - 1)) {
        dt <- t[i + 1] - t[i]
        current_t <- t[i]

        # Evaluate parameters at the current time step
        th  <- theta_f(current_t)
        m   <- mu_f(current_t)
        sig <- sigma_f(current_t)

        # Generate Wiener process increment
        # Variance scales with dt, standard deviation scales with sqrt(dt)
        dW <- rnorm(1, mean = 0, sd = sqrt(dt))

        # Calculate SDE step
        drift <- th * (m - x[i]) * dt
        diffusion <- sig * dW
        
        x_next <- x[i] + drift + diffusion
        
        # Prevent negative concentrations for chemical/biological validity
        if (clamp_zero && x_next < 0) {
            x_next <- 0
        }
        
        x[i + 1] <- x_next
    }

    # Return exactly matching the discrete grid format of the tau-leaper
    return(data.frame(time = t, value = x))
}