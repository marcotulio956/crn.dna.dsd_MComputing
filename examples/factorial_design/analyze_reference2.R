# =============================================================================
#  plot_reference_regimes.R
#
#  Reads mlexactboth_timeseries.csv produced by generate_mlexactboth_dataset.m
#  and writes two PDF/PNG outputs:
#
#  OUTPUT 1 — ml_reference_regime_summary.{pdf,png}
#    One compound panel per excitability regime (stacked vertically), each
#    consisting of:
#      (a) Voltage panel
#            solid line  : mean V across replicates
#            ribbon      : ±1 SE around the mean
#            dotted lines: pointwise min / max
#            Title       : Includes overall Avg V and Spike Count metrics
#      (b) Spike-raster bar beneath the voltage panel
#            black tick  : every detected spike event (all replicates overlaid)
#
#  OUTPUT 2 — ml_reference_gate_summary.{pdf,png}
#    One compound panel per regime showing gate occupancy over time:
#      (a) Calcium gate  M / Mtot  — mean (solid) ± 1 SE (ribbon), min/max (dotted)
#      (b) Potassium gate  N / Ntot — same layout
#
#  Usage:
#    Rscript plot_reference_regimes.R [path/to/mlexactboth_timeseries.csv]
#
#  Dependencies: data.table, ggplot2, scales, patchwork
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(scales)
  library(patchwork)
})

# ── 0. Locate the data file ──────────────────────────────────────────────────

args <- commandArgs(trailingOnly = TRUE)

candidate_paths <- c(
  if (length(args) >= 1) args[1],
  "mlexactboth_timeseries_800ms.csv",
  file.path("factorial_design", "data", "mlexactboth_timeseries_800ms.csv")
)

data_file <- NULL
for (p in candidate_paths) {
  if (!is.null(p) && file.exists(p)) { data_file <- p; break }
}
if (is.null(data_file))
  stop(
    "Cannot find mlexactboth_timeseries.csv.\n",
    "Supply the path as the first argument:\n",
    "  Rscript plot_reference_regimes.R /path/to/mlexactboth_timeseries.csv\n"
  )

message("Reading: ", normalizePath(data_file))

# ── 1. Load & normalise ───────────────────────────────────────────────────────

dt <- fread(data_file)
setnames(dt, tolower(trimws(names(dt))))

required_cols <- c("regime", "replicate", "time", "v", "m", "n", "spike")
missing_cols  <- setdiff(required_cols, names(dt))
if (length(missing_cols) > 0)
  stop("Missing columns: ", paste(missing_cols, collapse = ", "),
       "\nFound: ", paste(names(dt), collapse = ", "))

# ── 2. Regime ordering & palette ─────────────────────────────────────────────

regime_order <- c(
  "low_drive_quiescent",
  "near_threshold_irregular",
  "tonic_spiking",
  "high_drive_fast_spiking",
  "channel_noise_dominant",
  "tonic_spiking_lV",
  "tonic_spiking_hV"
)

regime_labels <- c(
  low_drive_quiescent      = "Low Drive — Quiescent  (Iapp = 60)",
  near_threshold_irregular = "Near Threshold — Irregular  (Iapp = 80)",
  tonic_spiking            = "Tonic Spiking  (Iapp = 100)",
  high_drive_fast_spiking  = "High Drive — Fast Spiking  (Iapp = 130)",
  channel_noise_dominant   = "Channel-Noise Dominant  (Iapp = 100, N=M=20)",
  tonic_spiking_lV         = "Tonic Spiking  — Fewer Gates (N=M=5)",
  tonic_spiking_hV         = "Tonic Spiking  — Extra Gates (N=M=150)"
)

present_regimes <- intersect(regime_order, unique(dt$regime))
unlabelled      <- setdiff(unique(dt$regime), names(regime_labels))
if (length(unlabelled) > 0) {
  regime_labels   <- c(regime_labels, setNames(unlabelled, unlabelled))
  present_regimes <- c(present_regimes, unlabelled)
}

base_colors <- c("#2166AC", "#4DAC26", "#D6604D", "#762A83", "#E08214")
n_regimes   <- length(present_regimes)
palette     <- setNames(base_colors[seq_len(n_regimes)],
                        regime_labels[present_regimes])

# helper: map raw regime name → labelled factor level
relabel <- function(x)
  factor(x, levels = present_regimes, labels = regime_labels[present_regimes])

# ── 3. Summary statistics (voltage & gates) ──────────────────────────────────

n_rep_tbl <- dt[, .(n_rep = uniqueN(replicate)), by = regime]

make_summary <- function(col) {
  s <- dt[, .(
    mean_val = mean(get(col)),
    sd_val   = sd(get(col)),
    min_val  = min(get(col)),
    max_val  = max(get(col))
  ), by = .(regime, time)]
  s <- merge(s, n_rep_tbl, by = "regime")
  s[, se_val  := sd_val / sqrt(n_rep)]
  s[, se_lo   := mean_val - se_val]
  s[, se_hi   := mean_val + se_val]
  s[, regime  := relabel(regime)]
  s
}

v_sum <- make_summary("v")

# INCREMENT: Compute overall average voltage output per regime
v_overall_stats <- dt[, .(mean_v = mean(v)), by = regime]

# Da mtot und ntot bereits in der CSV enthalten sind, kopieren wir einfach dt
dt2 <- copy(dt)

# Berechne die offenen Fraktionen direkt ohne fehleranfälliges Mergen!
dt2[, m_frac := m / mtot]
dt2[, n_frac := n / ntot]

m_sum <- dt2[, .(
  mean_val = mean(m_frac),
  sd_val   = sd(m_frac),
  min_val  = min(m_frac),
  max_val  = max(m_frac)
), by = .(regime, time)]
m_sum <- merge(m_sum, n_rep_tbl, by = "regime")
m_sum[, se_val := sd_val / sqrt(n_rep)]
m_sum[, se_lo  := mean_val - se_val]
m_sum[, se_hi  := mean_val + se_val]
m_sum[, regime := relabel(regime)]

n_sum <- dt2[, .(
  mean_val = mean(n_frac),
  sd_val   = sd(n_frac),
  min_val  = min(n_frac),
  max_val  = max(n_frac)
), by = .(regime, time)]
n_sum <- merge(n_sum, n_rep_tbl, by = "regime")
n_sum[, se_val := sd_val / sqrt(n_rep)]
n_sum[, se_lo  := mean_val - se_val]
n_sum[, se_hi  := mean_val + se_val]
n_sum[, regime := relabel(regime)]

# ── 4. Spike data ─────────────────────────────────────────────────────────────

# spike times per replicate (all replicates, for the raster bar)
spike_events <- dt[spike == 1, .(regime, replicate, time)]
spike_events[, regime_lbl := relabel(regime)]

# mean ± sd spike count per regime (across replicates)
spike_counts <- dt[, .(n_spikes = sum(spike)), by = .(regime, replicate)]
spike_stats  <- spike_counts[, .(
  mean_spikes = mean(n_spikes),
  sd_spikes   = sd(n_spikes)
), by = regime]
spike_stats[is.na(sd_spikes), sd_spikes := 0.0]
spike_stats[, label := sprintf("%.1f ± %.1f spikes", mean_spikes, sd_spikes)]

# ── 5. Shared theme elements ──────────────────────────────────────────────────

base_theme <- function(base_size = 10) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid.major  = element_line(colour = "grey88", linewidth = 0.3),
      panel.grid.minor  = element_blank(),
      panel.border      = element_rect(colour = "grey60", linewidth = 0.4),
      strip.background  = element_rect(fill = "grey95", colour = "grey70"),
      strip.text        = element_text(size = 9, face = "bold",
                                       margin = margin(3, 4, 3, 4)),
      axis.title        = element_text(size = 9),
      axis.text         = element_text(size = 7.5),
      axis.ticks        = element_line(colour = "grey60", linewidth = 0.3),
      plot.margin       = margin(2, 8, 2, 8)
    )
}

x_scale <- scale_x_continuous(
  name   = "Time (ms)",
  labels = comma,
  expand = expansion(mult = 0.01)
)

# ── 6. Build per-regime compound panels (voltage + spike bar) ─────────────────

build_voltage_panel <- function(reg_lbl, colour, avg_v, spike_lbl) {
  vd   <- v_sum[regime == reg_lbl]
  
  # New Dynamic Title Layout displaying metrics beside the regime name
  combined_title <- sprintf("%s    |    Avg V: %.2f mV    |    %s", reg_lbl, avg_v, spike_lbl)
  
  ggplot(vd, aes(x = time)) +
    geom_line(aes(y = max_val), linetype = "dotted",
              linewidth = 0.4, colour = colour, alpha = 0.7) +
    geom_line(aes(y = min_val), linetype = "dotted",
              linewidth = 0.4, colour = colour, alpha = 0.7) +
    geom_ribbon(aes(ymin = se_lo, ymax = se_hi),
                fill = colour, colour = NA, alpha = 0.25) +
    geom_line(aes(y = mean_val), colour = colour, linewidth = 0.7) +
    x_scale +
    scale_y_continuous(name = "V (mV)") +
    ggtitle(combined_title) +
    base_theme() +
    theme(
      axis.title.x  = element_blank(),
      axis.text.x   = element_blank(),
      axis.ticks.x  = element_blank(),
      plot.title    = element_text(size = 9, face = "bold",
                                   colour = "black", # unified color for readability
                                   margin = margin(b = 4))
    )
}

build_spike_bar <- function(reg_lbl, colour, n_reps) {
  se   <- spike_events[regime_lbl == reg_lbl]
  tmax <- max(v_sum$time)
  
  p <- ggplot()
  
  # Add ticks only if spikes exist
  if (nrow(se) > 0) {
    p <- p + geom_tile(
      data    = se,
      aes(x = time, y = 0.5, width = 2, height = 1),
      fill    = colour,
      colour  = NA,
      alpha   = 0.55
    )
  }
  
  p + scale_x_continuous(
    limits = c(0, tmax),
    labels = comma,
    expand = expansion(mult = 0.01),
    name   = "Time (ms)"
  ) +
    scale_y_continuous(limits = c(0, 1), breaks = NULL,
                       name = "spikes") +
    base_theme() +
    theme(
      panel.grid     = element_blank(),
      panel.border   = element_rect(colour = "grey80", linewidth = 0.3),
      axis.text.y    = element_blank(),
      axis.ticks.y   = element_blank(),
      axis.title.y   = element_text(size = 7, colour = "grey50",
                                    angle = 90),
      axis.title.x   = element_text(size = 8),
      axis.text.x    = element_text(size = 7),
      plot.margin    = margin(0, 8, 4, 8)
    )
}

# Compose
compound_panels <- vector("list", n_regimes)

for (i in seq_len(n_regimes)) {
  rn  <- present_regimes[i]
  rl  <- regime_labels[rn]
  col <- base_colors[i]
  nr  <- n_rep_tbl[regime == rn, n_rep]
  
  # Extract values for the title header
  avg_v <- v_overall_stats[regime == rn, mean_v]
  ss    <- spike_stats[regime == rn]
  spike_lbl <- if (nrow(ss) > 0) ss$label else "0.0 ± 0.0 spikes"
  
  vp <- build_voltage_panel(rl, col, avg_v, spike_lbl)
  sp <- build_spike_bar(rl, col, nr)
  
  compound_panels[[i]] <- vp / sp + plot_layout(heights = c(4, 1))
}

# Stack all regimes
fig1 <- wrap_plots(compound_panels, ncol = 1) +
  plot_annotation(
    title    = "Morris–Lecar Reference Model — Voltage & Spike Summary",
    subtitle = sprintf(
      "%d replicates per regime  |  solid = mean  |  ribbon = ±1 SE  |  dotted = min/max  |  bar = spike events",
      n_rep_tbl$n_rep[1]
    ),
    caption  = paste0("Source: mlexactboth stochastic simulation  |  ",
                      format(Sys.time(), "%Y-%m-%d")),
    theme = theme(
      plot.title    = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(size = 8.5, colour = "grey35"),
      plot.caption  = element_text(size = 7,   colour = "grey50", hjust = 1)
    )
  )

# ── 7. Save Output 1 ─────────────────────────────────────────────────────────

out1_pdf <- "ml_reference_regime_summary_800ms.pdf"
out1_png <- "ml_reference_regime_summary_800ms.png"

fig1_h <- n_regimes * 5 + 0.6

ggsave(out1_pdf, fig1, width = 11, height = fig1_h, units = "in",
       device = cairo_pdf)
ggsave(out1_png, fig1, width = 11, height = fig1_h, units = "in", dpi = 150)

message("Saved: ", out1_pdf)
message("Saved: ", out1_png)

# ── 8. Build gate occupancy panels (Output 2) ─────────────────────────────────

build_gate_panel <- function(gsum, gate_label, colour) {
  ggplot(gsum, aes(x = time)) +
    geom_line(aes(y = max_val), linetype = "dotted",
              linewidth = 0.4, colour = colour, alpha = 0.7) +
    geom_line(aes(y = min_val), linetype = "dotted",
              linewidth = 0.4, colour = colour, alpha = 0.7) +
    geom_ribbon(aes(ymin = se_lo, ymax = se_hi),
                fill = colour, colour = NA, alpha = 0.22) +
    geom_line(aes(y = mean_val), colour = colour, linewidth = 0.7) +
    facet_wrap(~ regime, ncol = 1, scales = "free_x") +
    x_scale +
    scale_y_continuous(
      name   = gate_label,
      limits = c(0, 1),
      labels = percent_format(accuracy = 1)
    ) +
    base_theme() +
    theme(
      strip.text = element_text(size = 8, face = "bold",
                                colour = "grey20")
    )
}

gate_compounds <- vector("list", n_regimes)

for (i in seq_len(n_regimes)) {
  rn  <- present_regimes[i]
  rl  <- regime_labels[rn]
  col <- base_colors[i]
  
  md  <- m_sum[regime == rl]
  nd  <- n_sum[regime == rl]
  
  # Calcium panel
  gp_ca <- ggplot(md, aes(x = time)) +
    geom_line(aes(y = max_val), linetype = "dotted",
              linewidth = 0.35, colour = "#C0392B", alpha = 0.65) +
    geom_line(aes(y = min_val), linetype = "dotted",
              linewidth = 0.35, colour = "#C0392B", alpha = 0.65) +
    geom_ribbon(aes(ymin = se_lo, ymax = se_hi),
                fill = "#C0392B", colour = NA, alpha = 0.20) +
    geom_line(aes(y = mean_val), colour = "#C0392B", linewidth = 0.65) +
    x_scale +
    scale_y_continuous(
      name   = "Ca²⁺ open (M/Mtot)",
      limits = c(0, 1),
      labels = percent_format(accuracy = 1)
    ) +
    ggtitle(rl) +
    base_theme() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank(),
      plot.title   = element_text(size = 8.5, face = "bold",
                                  colour = col,
                                  margin = margin(b = 1))
    )
  
  # Potassium panel
  gp_k  <- ggplot(nd, aes(x = time)) +
    geom_line(aes(y = max_val), linetype = "dotted",
              linewidth = 0.35, colour = "#1A6B8A", alpha = 0.65) +
    geom_line(aes(y = min_val), linetype = "dotted",
              linewidth = 0.35, colour = "#1A6B8A", alpha = 0.65) +
    geom_ribbon(aes(ymin = se_lo, ymax = se_hi),
                fill = "#1A6B8A", colour = NA, alpha = 0.20) +
    geom_line(aes(y = mean_val), colour = "#1A6B8A", linewidth = 0.65) +
    x_scale +
    scale_y_continuous(
      name   = "K⁺ open (N/Ntot)",
      limits = c(0, 1),
      labels = percent_format(accuracy = 1)
    ) +
    base_theme()
  
  gate_compounds[[i]] <- gp_ca / gp_k + plot_layout(heights = c(1, 1))
}

fig2 <- wrap_plots(gate_compounds, ncol = 1) +
  plot_annotation(
    title    = "Morris–Lecar Reference Model — Gate Occupancy",
    subtitle = paste0(
      "Ca\u00b2\u207a gate (red) = M / Mtot   |   ",
      "K\u207a gate (blue) = N / Ntot   |   ",
      "solid = mean, ribbon = \u00b11 SE, dotted = min/max"
    ),
    caption  = paste0("Source: mlexactboth stochastic simulation  |  ",
                      format(Sys.time(), "%Y-%m-%d")),
    theme = theme(
      plot.title    = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(size = 8.5, colour = "grey35"),
      plot.caption  = element_text(size = 7,   colour = "grey50", hjust = 1)
    )
  )

# ── 9. Save Output 2 ─────────────────────────────────────────────────────────

out2_pdf <- "ml_reference_gate_summary_800ms.pdf"
out2_png <- "ml_reference_gate_summary_800ms.png"

fig2_h <- n_regimes * 5.6 + 0.6

ggsave(out2_pdf, fig2, width = 11, height = fig2_h, units = "in",
       device = cairo_pdf)
ggsave(out2_png, fig2, width = 11, height = fig2_h, units = "in", dpi = 150)

message("Saved: ", out2_pdf)
message("Saved: ", out2_png)

message("\nDone.")