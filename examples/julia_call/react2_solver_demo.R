rm(list = ls())

source('R/parser.R')
source('R/util_functions.R')
source('R/crn_reactor.R')

library(ggplot2)
library(dplyr)

species <- c('A', 'B')
ci <- c(A = 1, B = 0)
reactions <- c('A -> B')
ki <- c(1.25)
timing <- seq(0, 8, by = 0.05)

result_desolve <- react2(
  species = species,
  ci = ci,
  reactions = reactions,
  ki = ki,
  t = timing,
  engine = 'desolve'
)

print(tail(result_desolve, 3))

if(requireNamespace('diffeqr', quietly = TRUE) &&
  'diffeq_setup' %in% getNamespaceExports('diffeqr')) {
  result_diffeqr <- react2(
    species = species,
    ci = ci,
    reactions = reactions,
    ki = ki,
    t = timing,
    engine = 'diffeqr'
  )

  comparison <- result_desolve %>%
    transmute(
      time = time,
      A_desolve = A,
      B_desolve = B,
      A_diffeqr = result_diffeqr$A,
      B_diffeqr = result_diffeqr$B
    )

  comparison$A_abs_diff <- abs(comparison$A_desolve - comparison$A_diffeqr)
  comparison$B_abs_diff <- abs(comparison$B_desolve - comparison$B_diffeqr)

  print(tail(comparison, 3))
  cat('max |A_desolve - A_diffeqr| = ', max(comparison$A_abs_diff), '\n', sep = '')
  cat('max |B_desolve - B_diffeqr| = ', max(comparison$B_abs_diff), '\n', sep = '')

  to_long <- function(df, engine_name) {
    rbind(
      data.frame(
        time = df$time,
        species = 'A',
        concentration = df$A,
        engine = engine_name
      ),
      data.frame(
        time = df$time,
        species = 'B',
        concentration = df$B,
        engine = engine_name
      )
    )
  }

  plot_data <- bind_rows(
    result_desolve %>% mutate(engine = 'desolve'),
    result_diffeqr %>% mutate(engine = 'diffeqr')
  )

  plot_data_long <- bind_rows(
    to_long(result_desolve, 'desolve'),
    to_long(result_diffeqr, 'diffeqr')
  )

  ggplot(plot_data_long, aes(x = time, y = concentration, color = engine)) +
    geom_line(linewidth = 0.8) +
    facet_wrap(~species, ncol = 1, scales = 'free_y') +
    labs(
      title = 'react2 comparison: deSolve vs diffeqr',
      x = 'Time',
      y = 'Concentration'
    ) +
    theme_minimal(base_size = 12)
} else {
  message('diffeqr is not installed or diffeq_setup() is unavailable, so only the deSolve reference run was produced.')
  ggplot(result_desolve, aes(x = time)) +
    geom_line(aes(y = A, color = 'A'), linewidth = 0.8) +
    geom_line(aes(y = B, color = 'B'), linewidth = 0.8) +
    labs(
      title = 'react2 demo using deSolve',
      x = 'Time',
      y = 'Concentration',
      color = 'Species'
    ) +
    theme_minimal(base_size = 12)
}
