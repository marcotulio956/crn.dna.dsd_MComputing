library(ggplot2)
library(dplyr)
library(grid)

# ==============================================================================
# Assumindo que results_df já existe com:
# iapp, mean_fr, res_fr
# ==============================================================================

theme_set(
  theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
)

# ==============================================================================
# 1. LINEAR MODEL
# ==============================================================================
fit <- lm(mean_fr ~ iapp, data = results_df)
fit_summary <- summary(fit)

intercept <- coef(fit)[1]
slope <- coef(fit)[2]
Fval <- fit_summary$fstatistic[1]
R2 <- fit_summary$r.squared

# ==============================================================================
# 2. SHAPIRO-WILK
# ==============================================================================
shapiro_res <- shapiro.test(results_df$res_fr)
Wval <- shapiro_res$statistic
Pval <- shapiro_res$p.value

# ==============================================================================
# 3. LABEL TEXT
# ==============================================================================
annotation_text <- paste0(
  "Linear Model:\n",
  "FR = ", sprintf("%.5f", intercept),
  " + ", sprintf("%.6f", slope), "·Iapp\n\n",
  "F = ", sprintf("%.2f", Fval), "\n",
  expression(R^2), " = ", sprintf("%.4f", R2), "\n"
)

# group means
means_df <- results_df %>%
  group_by(iapp) %>%
  summarise(mean_fr = mean(mean_fr), .groups = "drop")

# ==============================================================================
# 4. LINEAR FIT PLOT
# ==============================================================================
p_fit <- ggplot(results_df, aes(x = iapp, y = mean_fr)) +
  geom_jitter(
    color = "#1f77b4",
    alpha = 0.35,
    width = 1.2,
    size = 2
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#d62728",
    fill = "#ff9896",
    linewidth = 1.3
  ) +
  geom_point(
    data = means_df,
    aes(x = iapp, y = mean_fr),
    color = "#2ca02c",
    size = 4
  ) +
  annotate(
    "label",
    x = min(results_df$iapp) + 15,
    y = max(results_df$mean_fr) * 0.95,
    label = paste0(
      "FR = ", sprintf("%.5f", intercept),
      " + ", sprintf("%.6f", slope), "·Iapp\n",
      "F = ", sprintf("%.2f", Fval), "\n"
    ),
    hjust = 0,
    vjust = 1,
    size = 4.3,
    fill = "#f8f8f8"
  ) +
  labs(
    title = "Fit Linear",
    x = expression(I[app]),
    y = "Taxa Disparo"
  )

ggsave(
  "linear_fit_firing_rate.png",
  p_fit,
  width = 11,
  height = 7,
  dpi = 300
)

# ==============================================================================
# 5. QQ PLOT
# ==============================================================================
p_qq <- ggplot(results_df, aes(sample = res_fr)) +
  stat_qq(
    color = "#1f77b4",
    alpha = 0.8,
    size = 2
  ) +
  stat_qq_line(
    color = "#d62728",
    linewidth = 1.3
  ) +
  labs(
    title = "Q-Q Plot Residuos Taxa Disparo",
    subtitle = "Check de Normalidade",
    x = "Quantis Normais",
    y = "Quantis Amostra"
  )

ggsave(
  "qqplot_firing_rate.png",
  p_qq,
  width = 8,
  height = 6,
  dpi = 300
)

# show
p_fit
p_qq