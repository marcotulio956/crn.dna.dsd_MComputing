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


#' Calculate the root-mean-square error of two data sets
#'
#' This function is used to calculate the root-mean-square error (RMSE)
#' of two data sets.
#'
#' @param data1  A numerical vector representing one of the data sets.
#' @param data2  A numerical vector representing the other data set.
#'
#' @return  The RMSE value.
#'
#' @export
rmse <- function(data1, data2) {
    return(sqrt(mean((data2 - data1)^2)))
}

#' Calculate the normalized root-mean-square error of two data sets
#'
#' Use this function to calculate the normalized root-mean-square error
#' (NRMSE) of two data sets. In this measures, a distinction between the
#' data sets is needed since the normalization is made using only one of
#' the data sets. For convention, one of the data sets is called simulated
#' data set, and the other one is called observed data set.
#'
#' @param sim_data  The simulated data set
#' @param obs_data  The observed data set (used in the normalization)
#'
#' @return  The NRMSE measure.
#'
#' @export
nrmse <- function(sim_data, obs_data) {
    rmse_num <- rmse(sim_data, obs_data)
    return(rmse_num / (max(obs_data) - min(obs_data)))
}

#' Compare the behavior of two reactions
#'
#' Use this function to compare the behavior of two reactions to see
#' the similarity between them for each species. The normalized
#' root-mean-square error (NRMSE) measure (\code{\link{nrmse}()}) is used
#' to make this comparison.
#'
#' For convention, one of the behaviors is called
#' simulated behavior, and the other one is called observed behavior. This
#' differentiation is important because the order that you pass the
#' behaviors impacts in the result.
#'
#' The normalization made by the NRMSE
#' uses the observed values (`bhv_obs` parameter) only, consequently,
#' `compare_behaviors_nrmse(data1, data2)` results in different measures
#' than `compare_behaviors_nrmse(data2, data1)`.
#'
#' @param bhv_sim             The simulated behavior.
#' @param bhv_obs             The observed behavior (used in the normalization).
#' @param ignore_time_column  Ignore the time column, the first column of
#'                            a behavior
#'
#' @return  A data frame with the same columns of the behaviors and one row.
#'          Each value is the NRMSE of that species.
#'
#' @export
compare_behaviors_nrmse <- function(bhv_sim, bhv_obs, ignore_time_column = T) {
    column_names <- names(bhv_sim)

    if(ignore_time_column) {
        column_names <- column_names[2:length(column_names)]
    }

    # Create an empty data frame with the same columns of the behaviors,
    # but the time column
    result <- data.frame(matrix(nrow = 1, ncol = length(column_names)))

    colnames(result) <- column_names

    # Calculate the NRMSE for each column
    for(i in column_names) {
        result[i] <- nrmse(bhv_sim[[i]], bhv_obs[[i]])
    }

    # Return the result data frame
    return(result)
}

#' This function returns the concentration derivative of each species
#'
#' This function can be used for study what is impacting each species and
#' how much. this is useful to analyse medium size (dozens of reactions) CRNs.
#' all parameters follows the parameters of \code{\link{react}()}, except
#' the optional `time_point` and `behavior`. If a `time_point` is passed,
#' a `behavior` must be passed as well. If both parameters are set,
#' this functions returns the concentration of each species
#' at a specific point in time within the derivative.
#'
#' @param behavior     The data returned by \code{\link{react}()}.
#' @param time_points  A vector of indexes (representing multiple points in
#'                     time) used for access lines of `behavior`.
#'
#' @return A data frame with the derivatives. To access the derivative
#'         of a species `'A'`, you just have to access `df['A']`.
#'
#' @export
analyze_behavior <- function(
    species,
    ci,
    reactions,
    ki,
    time_points = NULL,
    behavior = NULL
) {
    # Helper function to concat strings
    jn <- function(...) { paste(..., sep = '') }

    # Check if behavior exists (in case of a time_point has been passed)
    if(!is.null(time_points)) {
        assertthat::assert_that(!is.null(behavior))

        # Check if all time_points are within the behavior data
        assertthat::assert_that(
            max(time_points) <= dim(behavior)[[1]],
            msg = 'All the time points must be within the behavior data.'
        )
    }

    # Get stoichiometry information
    sto_info <- get_M(reactions, species)
    sto_prod <- t(sto_info$prod)
    sto_react <- t(sto_info$react)
    # Get the transpose of the M matrix
    Mt <- t(sto_info$M)

    # Get a list of data frames (one for each time point)
    # If no time point was passed, only one data frame should
    # be instantiated
    n_df <- 1
    if(!is.null(time_points)) {
        n_df <- length(time_points)
    }
    df_list <- lapply(rep(1, n_df), function(nothing) {
        df <- data.frame(matrix(nrow = 1, ncol = length(species)))
        names(df) <- species
        return(df)
    })

    for(t in (if(is.null(time_points)) 1:1 else 1:length(time_points))) {
        for(i in 1:length(species)) {
            # Set the left part of the derivative equation
            s <- jn('d[', species[i], ']/dt = ')
            for(j in 1:length(reactions)) {
                # Set the k with stoichiometry
                k <- ki[j] * Mt[i,j]

                # Go to the next reactions if this one has k = 0
                # (this reaction doesn't impact this species)
                if(k == 0) {
                    next
                }

                if(j != 1 && substr(s, nchar(s) - 1, nchar(s)) != '= ') {
                    s <- jn(s, ' + ')
                }
                s <- jn(s, '(', k)

                # Get the reactant names or values
                reactants <- get_reactants(reactions[j])
                for(reactant in reactants) {
                    reactant_idx <- match(reactant, species)

                    # If the reactant is not in the species list
                    if(is.na(reactant_idx)) {
                        break
                    }

                    # Set exponent
                    react_exp <- sto_react[reactant_idx, j]

                    # Set concentration wit exponent
                    if(is.null(time_points)) {
                        s <- jn(s, ' * [', reactant, ']')
                    } else {
                        s <- jn(s, ' * ',
                                behavior[time_points[[t]], reactant]^react_exp,
                                '[', reactant, ']')
                    }
                    if(react_exp > 1) {
                        s <- jn(s, '^', react_exp)
                    }
                }

                s <- jn(s, ')')
            }

            if(substr(s, nchar(s) - 1, nchar(s)) == '= ') {
                s <- jn(s, '0')
            }

            df_list[[t]][i] <- s
        }
    }

    # Return the data frame if there is only oen time point in the list
    if(length(df_list) == 1) {
        return(df_list[[1]])
    } else {
        return(df_list)
    }
}

#' Evaluate a derivative returned by
#' \code{\link{analyze_behavior}()}
#'
#' If \code{\link{analyze_behavior}()} was used with a `behavior` and
#' `time_point`, you can use this function to calculate the result of
#' the derivative.
#'
#' @param derivative  Derivative with concentration values returned by
#'                    \code{\link{analyze_behavior}()}.
#'
#' @return A numeric value representing the result of the derivative.
#'
#' @export
eval_derivative <- function(derivative) {
    # Get the part after the '='
    right_part <- stringr::str_split(derivative, '=')[[1]][2]

    # Remove the concentration names (with exponent, if they have)
    express <- stringr::str_replace_all(
        right_part,
        '\\[[^\\[\\]]*\\](\\^[0-9]+)?',
        ''
    )

    # Evaluate expression
    return(eval(parse(text = express)))
}

#' Evaluate subexpressions of a derivative returned by
#' \code{\link{analyze_behavior}()}
#'
#' This functions works like `\link{eval_derivative}()`, but instead of
#' evaluating the entire derivative, it will evaluate parts of it
#' (delimited by `()`).
#'
#' @param derivative  Derivative with concentration values returned by
#'                    \code{\link{analyze_behavior}()}.
#'
#' @return A numeric named vector value representing the result of each part
#'         of the the derivative. The names of the results are the evaluated
#'         subexpressions.
#'
#' @export
eval_derivative_part <- function(derivative) {
    # Get the part after the '='
    right_part <- stringr::str_split(derivative, '=')[[1]][2]

    # Calculating the result of each subexpression (within `()``)
    exps <- stringr::str_match_all(right_part, '\\(.+?\\)')[[1]]
    exp_results <- sapply(exps, function(exp) {
        # Remove the concentration names (with exponent, if they have)
        exp <- stringr::str_replace_all(
            exp,
            '\\[[^\\[\\]]*\\](\\^[0-9]+)?',
            ''
        )

        # Evaluate the subexpression
        eval(parse(text = exp))
    })

    # Return a vector with the results
    return(exp_results)
}

#' Detect spikes by threshold crossing with refractory time
#'
#' @param time       Numeric vector of sample times.
#' @param signal     Numeric vector of signal values (same length as time).
#' @param threshold  Spike threshold (same units as signal).
#' @param refractory Minimum time between detected spikes.
#'
#' @return A list with spike times and spike indices.
#' @export
detect_spikes_threshold <- function(time, signal, threshold, refractory = 0) {
    if(length(time) != length(signal)) {
        stop('time and signal must have same length')
    }
    if(length(time) < 2) {
        return(list(spike_times = numeric(0), spike_idx = integer(0)))
    }

    above <- signal >= threshold
    crossings <- which(diff(as.integer(above)) == 1) + 1
    if(!length(crossings)) {
        return(list(spike_times = numeric(0), spike_idx = integer(0)))
    }

    keep <- logical(length(crossings))
    last_t <- -Inf
    for(i in seq_along(crossings)) {
        t_i <- time[crossings[i]]
        if((t_i - last_t) >= refractory) {
            keep[i] <- TRUE
            last_t <- t_i
        }
    }

    idx <- crossings[keep]
    list(spike_times = time[idx], spike_idx = idx)
}

#' Summarize spike metrics for a single realization
#'
#' @param time       Numeric vector of sample times.
#' @param signal     Numeric vector of signal values.
#' @param threshold  Spike threshold.
#' @param refractory Refractory period in time units.
#' @param t_start    Analysis window start.
#' @param t_end      Analysis window end.
#'
#' @return One-row data frame with spike metrics.
#' @export
summarize_spike_metrics <- function(
    time,
    signal,
    threshold,
    refractory = 0,
    t_start = min(time),
    t_end = max(time)
) {
    if(length(time) != length(signal)) {
        stop('time and signal must have same length')
    }
    if(t_end <= t_start) {
        stop('t_end must be greater than t_start')
    }

    in_win <- which(time >= t_start & time <= t_end)
    if(!length(in_win)) {
        stop('analysis window has no samples')
    }

    t_win <- time[in_win]
    s_win <- signal[in_win]
    det <- detect_spikes_threshold(t_win, s_win, threshold = threshold, refractory = refractory)

    n_spikes <- length(det$spike_times)
    duration <- t_end - t_start
    firing_rate <- if(duration > 0) n_spikes / duration else NA_real_
    first_spike_latency <- if(n_spikes > 0) det$spike_times[1] - t_start else NA_real_
    isi <- if(n_spikes > 1) diff(det$spike_times) else numeric(0)

    data.frame(
        n_spikes = n_spikes,
        firing_rate = firing_rate,
        first_spike_latency = first_spike_latency,
        mean_isi = if(length(isi)) mean(isi) else NA_real_,
        sd_isi = if(length(isi) > 1) stats::sd(isi) else NA_real_,
        cv_isi = if(length(isi) > 1 && mean(isi) != 0) stats::sd(isi) / mean(isi) else NA_real_,
        threshold = threshold,
        refractory = refractory,
        t_start = t_start,
        t_end = t_end
    )
}

#' Summarize replicated spike metrics for grouped data
#'
#' @param data            Data frame with time-series rows.
#' @param signal_col      Column name containing signal values.
#' @param time_col        Column name containing time values.
#' @param group_cols      Grouping columns (e.g. model, regime, replicate).
#' @param threshold_fun   Function(values) -> threshold, evaluated per group.
#' @param refractory      Refractory period in time units.
#' @param t_start         Analysis start.
#' @param t_end           Optional analysis end.
#'
#' @return Data frame with one row per group and spike metrics.
#' @export
summarize_spike_metrics_grouped <- function(
    data,
    signal_col,
    time_col = 'time',
    group_cols,
    threshold_fun = function(x) mean(x) + 0.5 * stats::sd(x),
    refractory = 0,
    t_start = NULL,
    t_end = NULL
) {
    req <- c(signal_col, time_col, group_cols)
    missing_cols <- setdiff(req, colnames(data))
    if(length(missing_cols)) {
        stop(paste('missing columns:', paste(missing_cols, collapse = ', ')))
    }

    split_key <- interaction(data[group_cols], drop = TRUE, lex.order = TRUE)
    chunks <- split(data, split_key)

    rows <- lapply(chunks, function(df) {
        df <- df[order(df[[time_col]]), , drop = FALSE]
        sig <- as.numeric(df[[signal_col]])
        tt <- as.numeric(df[[time_col]])
        thr <- as.numeric(threshold_fun(sig))
        if(!is.finite(thr)) {
            thr <- mean(sig, na.rm = TRUE)
        }

        lo <- if(is.null(t_start)) min(tt) else t_start
        hi <- if(is.null(t_end)) max(tt) else t_end
        met <- summarize_spike_metrics(tt, sig, threshold = thr, refractory = refractory, t_start = lo, t_end = hi)

        cbind(df[1, group_cols, drop = FALSE], met)
    })

    do.call(rbind, rows)
}

#' Build confidence intervals for grouped metric summaries
#'
#' @param metrics_df   Data frame with one row per realization.
#' @param metric_col   Metric column to summarize.
#' @param group_cols   Grouping columns used to aggregate realizations.
#' @param conf_level   Confidence level (default 0.95).
#'
#' @return Data frame with mean, sd, n, and t-interval bounds.
#' @export
summarize_metric_ci <- function(metrics_df, metric_col, group_cols, conf_level = 0.95) {
    req <- c(metric_col, group_cols)
    missing_cols <- setdiff(req, colnames(metrics_df))
    if(length(missing_cols)) {
        stop(paste('missing columns:', paste(missing_cols, collapse = ', ')))
    }

    split_key <- interaction(metrics_df[group_cols], drop = TRUE, lex.order = TRUE)
    chunks <- split(metrics_df, split_key)

    out <- lapply(chunks, function(df) {
        x <- as.numeric(df[[metric_col]])
        x <- x[is.finite(x)]
        n <- length(x)
        mu <- if(n) mean(x) else NA_real_
        s <- if(n > 1) stats::sd(x) else NA_real_
        se <- if(n > 1) s / sqrt(n) else NA_real_
        alpha <- 1 - conf_level
        tcrit <- if(n > 1) stats::qt(1 - alpha / 2, df = n - 1) else NA_real_
        half <- if(n > 1) tcrit * se else NA_real_

        data.frame(
            df[1, group_cols, drop = FALSE],
            metric = metric_col,
            n = n,
            mean = mu,
            sd = s,
            se = se,
            conf_level = conf_level,
            ci_low = if(n > 1) mu - half else NA_real_,
            ci_high = if(n > 1) mu + half else NA_real_
        )
    })

    do.call(rbind, out)
}

calculate_spiking_metrics <- function(time, V, tmax, bin_size_ms = 50) { 
  # Dynamic threshold
  v_min <- min(V, na.rm = TRUE) 
  v_max <- max(V, na.rm = TRUE) 
  threshold <- v_min + 0.70 * (v_max - v_min) 
  
  # Spike detection 
  is_above <- V >= threshold 
  spike_train <- c(0, diff(is_above) == 1) 
  spike_idx <- which(spike_train == 1) 
  spike_times <- time[spike_idx] 
  n_spikes <- length(spike_times) 
  
  # 1. Delay & Peak of first action potential 
  delay_first <- ifelse(n_spikes > 0, spike_times[1], NA) 
  v_first_peak <- ifelse(n_spikes > 0, V[spike_idx[1]], NA) 
  
  # 2. Mean Firing Rate (Hz if tmax is ms) -> Converted to spikes/ms
  mean_fr <- n_spikes / tmax 
  
  # 3. Inter-Spike Intervals & CV_ISI 
  isis <- diff(spike_times) 
  cv_isi <- ifelse(length(isis) > 1, sd(isis) / mean(isis), NA) 
  mean_isi <- ifelse(length(isis) > 0, mean(isis), NA) # Added for ISI plot
  
  # 4. Fano Factor 
  breaks <- seq(0, tmax, by = bin_size_ms) 
  if(length(breaks) > 1 && n_spikes > 0) { 
    counts <- hist(spike_times, breaks = breaks, plot = FALSE)$counts 
    mean_count <- mean(counts) 
    var_count <- var(counts) 
    fano_factor <- ifelse(mean_count > 0, var_count / mean_count, NA) 
  } else { 
    fano_factor <- NA 
  } 
  
  # 5. Sub-threshold Variance 
  sub_v <- V[V < threshold] 
  sub_var <- ifelse(length(sub_v) > 0, var(sub_v), NA) 
  
  # 6. Signal-to-Noise Ratio (SNR) 
  total_var <- var(V, na.rm = TRUE) 
  snr <- ifelse(!is.na(sub_var) && sub_var > 0, total_var / sub_var, NA) 
  
  return(data.frame( 
    n_spikes = n_spikes,       # Added
    mean_fr = mean_fr, 
    mean_isi = mean_isi,       # Added
    delay_first = delay_first, 
    v_first_peak = v_first_peak, 
    cv_isi = cv_isi, 
    fano_factor = fano_factor, 
    sub_var = sub_var, 
    snr = snr 
  )) 
}