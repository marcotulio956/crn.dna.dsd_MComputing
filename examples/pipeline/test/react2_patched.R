## ============================================================
## react2_patched.R
##
## Two surgical changes from the original, marked "## >>> PATCH":
##
##  PATCH A: the same tryCatch-in-the-hot-loop cost that was already
##           found and removed in react_stochastic_frates is still
##           present in react2's fx() -- every reaction, every solver
##           evaluation. deSolve's stiff integrators call fx() many
##           times per accepted step, so this multiplies fast (see
##           bench_trycatch.R: ~15x overhead per call in isolation).
##           Fix: validate every ki function ONCE, up front, at
##           t=0/y=ci (catches the same errors, same messages) and
##           then call kinetic_functions[[i]] directly with no
##           tryCatch inside fx().
##
##  PATCH B: react2 has no protection against a solver's adaptive
##           step briefly overshooting a concentration slightly below
##           zero -- common with stiff / near-threshold mass-action
##           terms (exactly the kind that show up in absence-indicator
##           or clock-like constructs). A negative reactant raised to
##           a non-integer or even power silently produces nonsense
##           propensities (NaN or wrong-signed) that can propagate
##           through the whole system without any error. Fix: clamp
##           the values used ONLY for the mass-action term (not the
##           true ODE state `y`, which deSolve still owns) to be
##           non-negative.
##  PATCH C: `events` is now a first-class parameter, using the SAME
##           simple convention as react_stochastic_frates_patched --
##           data.frame(time=, species=, amount=) -- instead of
##           requiring deSolve's own list(data=data.frame(var=,time=,
##           value=,method=)) shape and a manually-pre-named `ci`.
##           Internally this still just builds deSolve's native event
##           table and passes it through -- deSolve's own event engine
##           does the actual work, this only removes the friction of
##           needing to know its format and remember to name `ci`.
## ============================================================

react2 <- function(
    species, ci, reactions, ki, t,
    verbose = FALSE, engine = 'desolve',
    forced_concentrations = NULL,
    events = NULL,   ## >>> PATCH C: new parameter
                      ## data.frame(time=, species=, amount=) --
                      ## instantaneous additions (or, with a negative
                      ## amount, removals) at specific times,
                      ## independent of any reaction. Same shape as
                      ## react_stochastic_frates_patched()'s `events`.
    ...
) {
    if(!engine %in% c('desolve', 'diffeqr')) {
        stop(sprintf("Invalid engine: '%s'. Must be either 'desolve' or 'diffeqr'.", engine))
    }
    reactions <- check_crn(species, ci, reactions, ki, t)

    ## >>> PATCH C: ci must be named for deSolve to route events to the
    ## right state variable; do it here so the caller doesn't have to
    ## remember to (this is exactly the friction that made the old
    ## deSolve-passthrough route to events easy to misuse silently --
    ## an unnamed `ci` doesn't error, it just makes deSolve report
    ## "unknown state variable in event").
    if (is.null(names(ci))) ci <- setNames(as.numeric(ci), species)

    events_arg <- NULL
    if (!is.null(events)) {
        stopifnot(all(c("time","species","amount") %in% names(events)))
        if (!(engine == 'desolve')) {
            stop("events currently requires engine = 'desolve' (diffeqr event support is not implemented).")
        }
        ev_table <- data.frame(
            var    = events$species,
            time   = events$time,
            value  = events$amount,
            method = "add"
        )
        events_arg <- list(data = ev_table)
    }
    ## <<< END PATCH C

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
        result <- deSolve::ode(times = t, y = ci, func = fx, parms = NULL,
                                events = events_arg, ...)
        if(verbose) {
            print("= DESOLVE:")
            print(deSolve::diagnostics.deSolve(result))
        }
    } else if(engine == 'diffeqr') {
        if(!is.null(forced_concentrations)) {
            stop("The Julia call 'diffeqr' engine currently does not support forced_concentrations. Use 'desolve' for forced systems.")
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
