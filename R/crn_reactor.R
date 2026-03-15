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
#' @param forced_concentrations A named list where names are species names and values are
#'                   functions of time that return the forced concentration rate. 
#'                   These functions add a non-homogeneous forcing term to the ODE.
#'                   Example: list(A = function(t) sin(2*pi*t), B = function(t) ifelse(t > 10, 1, 0))
#' @param ...        Parameters passed to `deSolve::ode()`.
#'
#' @return A data frame with each line being a specific point in the time
#'         and each column but the first being the concentration of a
#'         species. The first column is the time interval. The column names
#'         are filled with the species's names.
#'
#' @export
#'
#' @example demo/main_crn.R
react <- function(species, ci, reactions, ki, t, verbose = FALSE, forced_concentrations = NULL, ...) {
    # Check the crn specification
    reactions <- check_crn(species, ci, reactions, ki, t)

    # Validate and process ki parameter (can be constants or functions)
    ki_processed <- list()
    ki_is_function <- logical(length(ki))
    
    for(i in seq_along(ki)) {
        if(is.function(ki[[i]])) {
            # Time-varying rate
            ki_processed[[i]] <- ki[[i]]
            ki_is_function[i] <- TRUE
        } else if(is.numeric(ki[[i]])) {
            # Constant rate - convert to a constant function for consistency
            k_const <- ki[[i]]
            ki_processed[[i]] <- function(t) k_const
            ki_is_function[i] <- FALSE
        } else {
            stop(sprintf("ki[%d] must be either a numeric constant or a function of time, not %s", 
                        i, class(ki[[i]])[1]))
        }
    }

    # Validate forced_concentrations parameter
    if(!is.null(forced_concentrations)) {
        if(!is.list(forced_concentrations)) {
            stop("forced_concentrations must be a named list of functions")
        }
        if(is.null(names(forced_concentrations)) || any(names(forced_concentrations) == "")) {
            stop("forced_concentrations must be a named list (all elements must have names)")
        }
        # Check that all forcing functions are actually functions
        for(name in names(forced_concentrations)) {
            if(!is.function(forced_concentrations[[name]])) {
                stop(sprintf("forced_concentrations['%s'] must be a function, not %s", 
                           name, class(forced_concentrations[[name]])[1]))
            }
        }
    }

    # Get stoichiometry information
    sto_info <- get_M(reactions, species)
    sto_react <- t(sto_info$react)
    Mt <- t(sto_info$M)

    # Matrix that works as a map, specifying the reactants
    # of each reaction.
    reactant_map <- lapply(reactions, function(reaction) {
        r_map <- reactants_in_reaction(species, reaction)
        if(is.null(r_map)) {
            # Return NA instead of NULL because NA^0 = 1
            return(NA)
        } else {
            return(r_map)
        }
    })

    # If a species A is consumed n times in a reaction,
    # the speed of the reaction will be -k[A]^n
    v_exp_reactants <- lapply(seq_along(reactant_map), function(i) {
        # Get the stoichiometry info of the reactant for each reaction
        if(length(reactant_map[[i]]) > 1 || !is.na(reactant_map[[i]])) {
            return(sto_react[reactant_map[[i]], i])
        } else {
            return(0)
        }
    })
    

    # Define function for deSolve
    fx <- function(t, y, parms) {
        # Evaluate reaction rates at current time t
        # For time-varying rates, evaluate the function; for constant rates, use the value
        ki_t <- sapply(ki_processed, function(k_func) {
            tryCatch({
                k_func(t)
            }, error = function(e) {
                stop(sprintf("Error evaluating rate function at time %f: %s", t, e$message))
            })
        })
        
        # Define the vector, which specifies the impact magnitude of
        # each reaction
        v <- matrix(
            mapply(
                function(react_map, k, v_exp) {
                    vi <- k * prod(y[react_map]^v_exp)
                }, 
                reactant_map,
                ki_t,  # Use time-evaluated rates
                v_exp_reactants
            )
        )

        # Multiply the impact magnitude of each reaction with the stoichiometry
        # of each species to get the new species concentrations
        dy <- Mt %*% v

        # Add forced concentration terms (non-homogeneous forcing)
        if(!is.null(forced_concentrations)) {
            for(forced_species in names(forced_concentrations)) {
                # Find the index of the forced species
                species_idx <- which(species == forced_species)
                if(length(species_idx) == 0) {
                    # Species not found - issue a warning but continue
                    warning(sprintf("Forced species '%s' not found in species list. Ignoring.", forced_species))
                    next
                }
                
                # Evaluate the forcing function at time t
                tryCatch({
                    forcing_value <- forced_concentrations[[forced_species]](t)
                    # Add the forcing term to the derivative
                    dy[species_idx] <- dy[species_idx] + forcing_value
                }, error = function(e) {
                    stop(sprintf("Error evaluating forcing function for species '%s' at time %f: %s", 
                                 forced_species, t, e$message))
                })
            }
        }

        list(dy)
    }
    
    result <- deSolve::ode(times = t, y = ci, func = fx, parms = NULL, ...)
    if(verbose) {
        print("= DESOLVE: ")
        print(deSolve::diagnostics.deSolve(result))
    }

    # Convert double matrix to dataframe
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
