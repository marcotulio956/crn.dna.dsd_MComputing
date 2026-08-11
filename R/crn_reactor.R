# DNAr is a program used to simulate formal Chemical Reaction Networks
# and the ones based on DNA.
# Copyright (C) 2017  Daniel Kneipp <danielv[at]dcc[dot]ufmg[dot]com[dot]br>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


#' Returns the matrix M.
#'
#' The matrix M is used for the calculations made on \code{\link{react}()}.
#' This matrix  has #lines = #reactions and #columns = #species and it
#' represents the stoichiometry of each species at each reaction.
#'
#' @return A list with the named indexes `prod` (stoichiometry matrix of the
#'         products), `react` (stoichiometry matrix of reactants) and `M`
#'         (the M matrix generate from the difference of theses two matrices).
get_M <- function(reactions, species) {
    products <- matrix(data = 0,
                       nrow = length(reactions),
                       ncol = length(species))
    reactants <- matrix(data = 0,
                        nrow = length(reactions),
                        ncol = length(species))

    for(i in 1:length(reactions)) {
        for(j in 1:length(species)) {
            stoc <- get_stoichiometry_onespecies(species[j], reactions[i])
            products[i,j] <- stoc$right_sto
            reactants[i,j] <- stoc$left_sto
        }
    }

    M <- products - reactants
    data <- list(prod = products, react = reactants, M = M)
    return(data)
}

solve_crn_diffeqr <- function(t, ci, Mt, reactant_map, v_exp_reactants, ki) {
    if(!all(vapply(ki, is.numeric, logical(1)))) {
        stop(
            "The 'diffeqr' engine currently supports numeric constant ki values only."
        )
    }

    if(!requireNamespace("diffeqr", quietly = TRUE) ||
       !"diffeq_setup" %in% getNamespaceExports("diffeqr")) {
        stop(
            "The 'diffeqr' engine requires the diffeqr package with diffeq_setup()."
        )
    }

    de <- diffeqr::diffeq_setup(pkg_check = TRUE)

    JuliaCall::julia_assign("crn_M", Mt)
    JuliaCall::julia_assign("crn_k", as.numeric(ki))
    JuliaCall::julia_assign("crn_u0", as.numeric(ci))
    JuliaCall::julia_assign("crn_tspan", c(min(t), max(t)))
    JuliaCall::julia_assign(
        "crn_reactant_map",
        lapply(reactant_map, function(r_map) {
            if(length(r_map) == 1 && is.na(r_map[1])) {
                integer(0)
            } else {
                as.integer(r_map)
            }
        })
    )
    JuliaCall::julia_assign(
        "crn_exp_map",
        lapply(v_exp_reactants, function(exps) as.numeric(exps))
    )

    JuliaCall::julia_command(
        paste0(
            "function crn_ode(u, p, t)\n",
            "    v = zeros(length(crn_k))\n",
            "    for i in eachindex(crn_k)\n",
            "        react_map = crn_reactant_map[i]\n",
            "        if isempty(react_map)\n",
            "            v[i] = crn_k[i]\n",
            "        else\n",
            "            term = crn_k[i]\n",
            "            exps = crn_exp_map[i]\n",
            "            for j in eachindex(react_map)\n",
            "                term *= u[react_map[j]] ^ exps[j]\n",
            "            end\n",
            "            v[i] = term\n",
            "        end\n",
            "    end\n",
            "    return vec(crn_M * v)\n",
            "end"
        )
    )

    prob <- JuliaCall::julia_eval(
        "ODEProblem(crn_ode, crn_u0, crn_tspan)"
    )

    sol <- de$solve(
        prob,
        de$Tsit5(),
        saveat = as.numeric(t),
        abstol = 1e-8,
        reltol = 1e-8
    )

    list(
        time = as.numeric(unlist(as.list(sol$t))),
        values = do.call(rbind, lapply(as.list(sol$u), as.numeric))
    )
}

#' Simulate a CRN
#'
#' This is the function used to actually simulate the chemical reaction network.
#' Given the CRN specifications, it returns the behavior of the reaction.
#'
#' @section Known limitations:
#'   \itemize{
#'     \item It only supports uni or bimolecular reactions;
#'     \item Bidirectional reactions (e.g.: 'A <--> B') are not supported
#'   yet (you have to describe them with two separated reactions).
#'   }
#'
#' @param species    A vector with the species of the reaction. The order of
#'                   this vector is important because it will define the
#'                   column order of the returned behavior.
#' @param ci         A vector specifying the initial concentrations of the
#'                   \code{species} specified, in order.
#' @param reactions  A vector with the reactions of the CRN. If a reaction has
#'                   has only reactants that are non in `species`, this
#'                   reaction will be treated as `0 -> products`. Furthermore,
#'                   this function treats a reaction `non_registered_species
#'                   + registered_species -> products` equally to
#'                   `registered_species -> products`, ignoring the species non
#'                   registered on the `species` vector.
#' @param ki         A vector defining the rate of each reaction in \code{reactions}, 
#'                   in order. Can be either:
#'                   - Numeric values for constant rates (e.g., c(1e-3, 2e-4))
#'                   - Functions of time for time-varying rates (e.g., function(t) 1e-3 * sin(t))
#'                   - A mix of both (e.g., list(1e-3, function(t) 2e-4 * (1 + 0.5*sin(t))))
#' @param t          A vector specifying the time interval. Each value
#'                   would be a specific time point.
#' @param verbose    Be verbose and print information about the integration
#'                   process with `deSolve::diagnostics.deSolve()`. Default
#'                   value is `FALSE`
#' @param engine     ODE solver engine to use. Options are:
#'                   - \code{'desolve'} (default): Uses \code{deSolve::ode()}.
#'                   - \code{'diffeqr'}: Uses Julia-based solver via \code{diffeqr::solve_ode()}.
#'                     Requires Julia and diffeqr R package to be installed.
#' @param forced_concentrations A named list where names are species names and values are
#'                   functions of time that return the forced concentration rate. 
#'                   These functions add a non-homogeneous forcing term to the ODE.
#'                   Example: list(A = function(t) sin(2*pi*t), B = function(t) ifelse(t > 10, 1, 0))
#'                   Note: Currently supported only with engine='desolve'.
#' @param ...        Parameters passed to the ODE solver (deSolve::ode() or diffeqr::solve_ode()).
#'
#' @return A data frame with each line being a specific point in the time
#'         and each column but the first being the concentration of a
#'         species. The first column is the time interval. The column names
#'         are filled with the species's names.
#'
#' @export
#'
#' @example demo/main_crn.R
react <- function(species, ci, reactions, ki, t, verbose = FALSE, engine = 'desolve', forced_concentrations = NULL, ...) {
    # Validate engine parameter
    if(!engine %in% c('desolve', 'diffeqr')) {
        stop(sprintf(
            "Invalid engine: '%s'. Must be either 'desolve' or 'diffeqr'.",
            engine
        ))
    }

    # ------------------------------------------------------------
    # Check CRN
    # ------------------------------------------------------------

    reactions <- check_crn(species, ci, reactions, ki, t)

    # ------------------------------------------------------------
    # Build stoichiometry
    # ------------------------------------------------------------

    sto_info   <- get_M(reactions, species)
    sto_react  <- t(sto_info$react)
    Mt         <- t(sto_info$M)

    # ------------------------------------------------------------
    # Reactant map
    # ------------------------------------------------------------

    reactant_map <- lapply(reactions, function(reaction) {

        r_map <- reactants_in_reaction(species, reaction)

        if(is.null(r_map)) {
            return(NA)
        } else {
            return(r_map)
        }
    })

    # ------------------------------------------------------------
    # Mass-action exponents
    # ------------------------------------------------------------

    v_exp_reactants <- lapply(seq_along(reactant_map), function(i) {

        if(length(reactant_map[[i]]) > 1 || !is.na(reactant_map[[i]])) {
            return(sto_react[reactant_map[[i]], i])
        } else {
            return(0)
        }
    })

    # ------------------------------------------------------------
    # Process kinetics
    # ------------------------------------------------------------

    kinetic_functions <- vector("list", length(ki))

    for(i in seq_along(ki)) {

        k <- ki[[i]]

        # --------------------------------------------------------
        # Constant rate
        # --------------------------------------------------------

        if(is.numeric(k) && length(k) == 1) {

            kinetic_functions[[i]] <- function(t, y, species) {
                k
            }

        # --------------------------------------------------------
        # Function
        # --------------------------------------------------------

        } else if(is.function(k)) {

            nargs_k <- length(formals(k))

            # function(t)
            if(nargs_k == 1) {

                kinetic_functions[[i]] <- function(t, y, species) {
                    k(t)
                }

            # function(t, y)
            } else if(nargs_k == 2) {

                kinetic_functions[[i]] <- function(t, y, species) {
                    k(t, y)
                }

            # function(t, y, species)
            } else if(nargs_k >= 3) {

                kinetic_functions[[i]] <- function(t, y, species) {
                    k(t, y, species)
                }

            } else {

                stop(sprintf(
                    "Unsupported function signature in ki[%d]",
                    i
                ))
            }

        } else {

            stop(sprintf(
                "ki[%d] must be numeric or function",
                i
            ))
        }
    }

    # ------------------------------------------------------------
    # Validate forcing functions
    # ------------------------------------------------------------

    if(!is.null(forced_concentrations)) {

        if(!is.list(forced_concentrations)) {
            stop("forced_concentrations must be a named list")
        }

        for(name in names(forced_concentrations)) {

            if(!is.function(forced_concentrations[[name]])) {

                stop(sprintf(
                    "forced_concentrations['%s'] must be a function",
                    name
                ))
            }
        }
    }

    # ------------------------------------------------------------
    # ODE system
    # ------------------------------------------------------------

    fx <- function(t, y, parms) {

        names(y) <- species

        # --------------------------------------------------------
        # Compute propensities
        # --------------------------------------------------------

        v <- numeric(length(reactions))

        for(i in seq_along(reactions)) {

            react_map <- reactant_map[[i]]

            # Base kinetic coefficient / generalized propensity
            k_val <- tryCatch({

                kinetic_functions[[i]](t, y, species)

            }, error = function(e) {

                stop(sprintf(
                    "Error evaluating ki[%d] at t=%f : %s",
                    i,
                    t,
                    e$message
                ))
            })

            # ----------------------------------------------------
            # Mass-action multiplier
            # ----------------------------------------------------

            if(length(react_map) == 1 && is.na(react_map)) {

                mass_action_term <- 1

            } else {

                mass_action_term <- prod(
                    y[react_map]^v_exp_reactants[[i]]
                )
            }

            v[i] <- k_val * mass_action_term
        }

        # --------------------------------------------------------
        # Dynamics
        # --------------------------------------------------------

        dy <- as.numeric(Mt %*% v)

        names(dy) <- species

        # --------------------------------------------------------
        # Forced concentrations
        # --------------------------------------------------------

        if(!is.null(forced_concentrations)) {

            for(forced_species in names(forced_concentrations)) {

                idx <- match(forced_species, species)

                if(is.na(idx)) {
                    next
                }

                forcing_value <- forced_concentrations[[forced_species]](t)

                tau <- 0.01

                dy[idx] <- (forcing_value - y[idx]) / tau
            }
        }

        list(dy)
    }
    
    # Select and run the appropriate ODE solver
    if(engine == 'desolve') {
        # Use deSolve ODE solver (default)
        result <- deSolve::ode(times = t, y = ci, func = fx, parms = NULL, ...)
        if(verbose) {
            print("= DESOLVE: ")
            print(deSolve::diagnostics.deSolve(result))
        }
        
    } else if(engine == 'diffeqr') {
        if(!is.null(forced_concentrations)) {
            stop(
                "The 'diffeqr' engine currently does not support forced_concentrations. Use 'desolve' for forced systems."
            )
        }

        solved <- solve_crn_diffeqr(
            t = t,
            ci = ci,
            Mt = Mt,
            reactant_map = reactant_map,
            v_exp_reactants = v_exp_reactants,
            ki = ki
        )

        sol_t <- solved$time
        sol_u <- solved$values

        result <- cbind(
            time = sol_t,
            sol_u
        )
    }

    # Convert double matrix to dataframe
    df_result <- data.frame(result)
    names(df_result) <- c('time', species)

    return(df_result)
}

react2 <- function(
    species,
    ci,
    reactions,
    ki,
    t,
    verbose = FALSE,
    engine = 'desolve',
    forced_concentrations = NULL,
    ...
) {
    # ------------------------------------------------------------
    # Validate engine
    # ------------------------------------------------------------
    if(!engine %in% c('desolve', 'diffeqr')) {
        stop(sprintf(
            "Invalid engine: '%s'. Must be either 'desolve' or 'diffeqr'.",
            engine
        ))
    }
    # ------------------------------------------------------------
    # Check CRN
    # ------------------------------------------------------------
    reactions <- check_crn(
        species,
        ci,
        reactions,
        ki,
        t
    )
    # ------------------------------------------------------------
    # Build stoichiometry
    # ------------------------------------------------------------
    sto_info  <- get_M(reactions, species)
    sto_react <- t(sto_info$react)
    Mt <- t(sto_info$M)
    # ------------------------------------------------------------
    # Reactant map
    # ------------------------------------------------------------
    reactant_map <- lapply(reactions, function(reaction) {
        r_map <- reactants_in_reaction(
            species,
            reaction
        )
        if(is.null(r_map)) {
            return(NA)
        } else {
            return(r_map)
        }
    })
    # ------------------------------------------------------------
    # Mass-action exponents
    # ------------------------------------------------------------
    v_exp_reactants <- lapply(
        seq_along(reactant_map),
        function(i) {
            if(length(reactant_map[[i]]) > 1 ||
               !is.na(reactant_map[[i]])) {
                return(
                    sto_react[
                        reactant_map[[i]],
                        i
                    ]
                )
            } else {
                return(0)
            }
        }
    )
    # ------------------------------------------------------------
    # Process kinetics
    # ------------------------------------------------------------
    kinetic_functions <- vector(
        "list",
        length(ki)
    )
    for(i in seq_along(ki)) {
        k_current <- ki[[i]]
        # --------------------------------------------------------
        # Constant kinetics
        # --------------------------------------------------------
        if(is.numeric(k_current) &&
           length(k_current) == 1) {
            kinetic_functions[[i]] <- local({
                k_const <- k_current
                function(t, y, species) {
                    k_const
                }
            })
        # --------------------------------------------------------
        # Functional kinetics
        # --------------------------------------------------------
        } else if(is.function(k_current)) {
            kinetic_functions[[i]] <- local({
                k_fun <- k_current
                nargs_k <- length(
                    formals(k_fun)
                )
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
                        k_fun(
                            t,
                            y,
                            species
                        )
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
    # ------------------------------------------------------------
    # Validate forced concentrations
    # ------------------------------------------------------------
    if(!is.null(forced_concentrations)) {
        if(!is.list(forced_concentrations)) {
            stop(
                "forced_concentrations must be a named list"
            )
        }
        for(name in names(forced_concentrations)) {
            if(!is.function(
                forced_concentrations[[name]]
            )) {
                stop(sprintf(
                    "forced_concentrations['%s'] must be a function",
                    name
                ))
            }
        }
    }
    # ------------------------------------------------------------
    # ODE system
    # ------------------------------------------------------------
    fx <- function(t, y, parms) {
        # IMPORTANT:
        # Never mutate solver state directly
        y_named <- setNames(
            as.numeric(y),
            species
        )
        # --------------------------------------------------------
        # Compute propensities
        # --------------------------------------------------------
        v <- numeric(length(reactions))
        for(i in seq_along(reactions)) {
            react_map <- reactant_map[[i]]
            # ----------------------------------------------------
            # Evaluate generalized kinetic coefficient
            # ----------------------------------------------------
            k_val <- tryCatch({
                kinetic_functions[[i]](
                    t,
                    y_named,
                    species
                )
            }, error = function(e) {
                stop(sprintf(
                    "Error evaluating ki[%d] at t=%f : %s",
                    i,
                    t,
                    e$message
                ))
            })
            # ----------------------------------------------------
            # Mass-action term
            # ----------------------------------------------------
            if(length(react_map) == 1 &&
               is.na(react_map)) {
                mass_action_term <- 1
            } else {
                mass_action_term <- prod(
                    y_named[react_map]^
                    v_exp_reactants[[i]]
                )
            }
            # ----------------------------------------------------
            # Final propensity
            # ----------------------------------------------------
            v[i] <- k_val * mass_action_term
        }
        # --------------------------------------------------------
        # System dynamics
        # --------------------------------------------------------
        dy <- as.numeric(
            Mt %*% v
        )
        names(dy) <- species
        # --------------------------------------------------------
        # Forced concentrations
        # --------------------------------------------------------
        if(!is.null(forced_concentrations)) {
            for(forced_species in names(forced_concentrations)) {
                idx <- match(
                    forced_species,
                    species
                )
                if(is.na(idx)) {
                    next
                }
                forcing_value <- tryCatch({
                    forced_concentrations[[forced_species]](t)
                }, error = function(e) {
                    stop(sprintf(
                        "Error evaluating forcing function for '%s' at t=%f : %s",
                        forced_species,
                        t,
                        e$message
                    ))
                })
                tau <- 0.01
                dy[idx] <- (
                    forcing_value -
                    y_named[idx]
                ) / tau
            }
        }
        list(dy)
    }
    # ------------------------------------------------------------
    # Solve system
    # ------------------------------------------------------------
    if(engine == 'desolve') {
        result <- deSolve::ode(
            times = t,
            y = ci,
            func = fx,
            parms = NULL,
            ...
        )
        if(verbose) {
            print("= DESOLVE:")
            print(
                deSolve::diagnostics.deSolve(
                    result
                )
            )
        }
    } else if(engine == 'diffeqr') {
        if(!is.null(forced_concentrations)) {
            stop(
                "The 'diffeqr' engine currently does not support forced_concentrations. Use 'desolve' for forced systems."
            )
        }

        solved <- solve_crn_diffeqr(
            t = t,
            ci = ci,
            Mt = Mt,
            reactant_map = reactant_map,
            v_exp_reactants = v_exp_reactants,
            ki = ki
        )

        sol_t <- solved$time
        sol_u <- solved$values

        result <- cbind(
            time = sol_t,
            sol_u
        )
    }
    # ------------------------------------------------------------
    # Output
    # ------------------------------------------------------------
    df_result <- data.frame(result)
    names(df_result) <- c(
        'time',
        species
    )
    return(df_result)
}

react2_patched <- function(
    species, ci, reactions, ki, t,
    verbose = FALSE, engine = 'desolve',
    forced_concentrations = NULL, ...
) {
    if(!engine %in% c('desolve', 'diffeqr')) {
        stop(sprintf("Invalid engine: '%s'. Must be either 'desolve' or 'diffeqr'.", engine))
    }
    reactions <- check_crn(species, ci, reactions, ki, t)
    sto_info  <- get_M(reactions, species)
    sto_react <- t(sto_info$react)
    Mt <- t(sto_info$M)

    reactant_map <- lapply(reactions, function(reaction) {
        r_map <- reactants_in_reaction(species, reaction)
        if(is.null(r_map)) return(NA) else return(r_map)
    })

    v_exp_reactants <- lapply(seq_along(reactant_map), function(i) {
        if(length(reactant_map[[i]]) > 1 || !is.na(reactant_map[[i]])) {
            return(sto_react[reactant_map[[i]], i])
        } else return(0)
    })

    kinetic_functions <- vector("list", length(ki))
    for(i in seq_along(ki)) {
        k_current <- ki[[i]]
        if(is.numeric(k_current) && length(k_current) == 1) {
            kinetic_functions[[i]] <- local({
                k_const <- k_current
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

    ## >>> PATCH A: validate every ki once, up front, instead of
    ## wrapping every call in fx() with tryCatch.
    y0_named <- setNames(as.numeric(ci), species)
    for (i in seq_along(kinetic_functions)) {
        val <- tryCatch(
            kinetic_functions[[i]](t[1], y0_named, species),
            error = function(e) {
                stop(sprintf("Error evaluating ki[%d] at t=%f : %s", i, t[1], e$message))
            }
        )
        if (!is.numeric(val) || length(val) != 1 || is.na(val)) {
            stop(sprintf(
                "ki[%d] must return a single finite numeric value; got: %s",
                i, paste(capture.output(print(val)), collapse = " ")
            ))
        }
    }
    ## <<< END PATCH A

    if(!is.null(forced_concentrations)) {
        if(!is.list(forced_concentrations)) stop("forced_concentrations must be a named list")
        for(name in names(forced_concentrations)) {
            if(!is.function(forced_concentrations[[name]])) {
                stop(sprintf("forced_concentrations['%s'] must be a function", name))
            }
        }
    }

    fx <- function(t, y, parms) {
        y_named <- setNames(as.numeric(y), species)

        ## >>> PATCH B: non-negative view used ONLY for propensity
        ## evaluation; the true state `y` handed back to deSolve is
        ## untouched, so this does not mask genuine integration
        ## problems, it only stops transient negative-overshoot noise
        ## from corrupting the computed rates.
        y_eval <- pmax(y_named, 0)
        ## <<< END PATCH B

        v <- numeric(length(reactions))
        for(i in seq_along(reactions)) {
            react_map <- reactant_map[[i]]

            ## >>> PATCH A: no tryCatch here anymore
            k_val <- kinetic_functions[[i]](t, y_named, species)
            ## <<< END PATCH A

            if(length(react_map) == 1 && is.na(react_map)) {
                mass_action_term <- 1
            } else {
                mass_action_term <- prod(y_eval[react_map]^v_exp_reactants[[i]])
            }
            v[i] <- k_val * mass_action_term
        }

        dy <- as.numeric(Mt %*% v)
        names(dy) <- species

        if(!is.null(forced_concentrations)) {
            for(forced_species in names(forced_concentrations)) {
                idx <- match(forced_species, species)
                if(is.na(idx)) next
                forcing_value <- tryCatch({
                    forced_concentrations[[forced_species]](t)
                }, error = function(e) {
                    stop(sprintf(
                        "Error evaluating forcing function for '%s' at t=%f : %s",
                        forced_species, t, e$message
                    ))
                })
                tau <- 0.01
                dy[idx] <- (forcing_value - y_named[idx]) / tau
            }
        }
        list(dy)
    }

    if(engine == 'desolve') {
        result <- deSolve::ode(times = t, y = ci, func = fx, parms = NULL, ...)
        if(verbose) {
            print("= DESOLVE:")
            print(deSolve::diagnostics.deSolve(result))
        }
    } else if(engine == 'diffeqr') {
        if(!is.null(forced_concentrations)) {
            stop("The 'diffeqr' engine currently does not support forced_concentrations. Use 'desolve' for forced systems.")
        }
        solved <- solve_crn_diffeqr(
            t = t, ci = ci, Mt = Mt, reactant_map = reactant_map,
            v_exp_reactants = v_exp_reactants, ki = ki
        )
        result <- cbind(time = solved$time, solved$values)
    }

    df_result <- data.frame(result)
    names(df_result) <- c('time', species)
    return(df_result)
}


#' Combine CRNs into one single larger CRN
#'
#' Use this function to combine multiple crns into a larger crn which
#' has all the reactions, species and other parameters of all other CRNs.
#'
#' @param crns  List of CRNs.
#'
#' @return  Another CRN representing the input CRNs combined
#'
#' @export
combine_crns <- function(crns) {
    # Initialize the parameters
    parms <- list(
        species   = c(),
        ci        = c(),
        reactions = c(),
        ki        = c()
    )

    # Combine the CRNs
    for(crn in crns) {
        parms$species   <- c(parms$species, crn$species)
        parms$ci        <- c(parms$ci, crn$ci)
        parms$reactions <- c(parms$reactions, crn$reactions)
        parms$ki        <- c(parms$ki, crn$ki)
    }

    return(parms)
}
