###############################################
# global.R
# Shared libraries, themes, constants, helpers
###############################################

# Core Shiny libraries
library(shiny)
library(shinyWidgets)
library(bslib)

# Plotting / formatting
library(ggplot2)
library(scales)

# ------------------------------------------------
# Global constants
# ------------------------------------------------

# Number of Monte Carlo simulations
N_SIM <- 10000

# Global color used in histograms and curves
FAIR_PRIMARY_COLOR <- "#00563f"

# ------------------------------------------------
# Themes (dark / light switch)
# ------------------------------------------------

light <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  bg = "#FFFFFF",
  fg = "#2C3E50"
)

dark <- bs_theme(
  version = 5,
  bootswatch = "darkly",
  bg = "#2C3E50",
  fg = "#ECF0F1"
)

# ------------------------------------------------
# Utility: lognormal parameterisation from 90% CI
# ------------------------------------------------
# Given a minimum and maximum that represent the
# 5th and 95th percentiles of a lognormal distribution,
# compute the corresponding meanlog and sdlog so we
# can sample from rlnorm() consistently across the app.

lognorm_params <- function(min_val, max_val) {
  if (any(min_val <= 0) || any(max_val <= 0)) {
    stop("min_val and max_val must be strictly positive for lognormal.")
  }
  meanlog <- (log(max_val) + log(min_val)) / 2
  sdlog   <- (log(max_val) - log(min_val)) / 3.29
  list(meanlog = meanlog, sdlog = sdlog)
}

# ------------------------------------------------
# Utility: positive lognormal sampler
# ------------------------------------------------
# Wrapper around rlnorm() that:
#   - always returns strictly positive values
#   - can be reused for any quantity modelled as lognormal

r_lognorm_pos <- function(n, meanlog, sdlog) {
  x <- rlnorm(n, meanlog, sdlog)
  # Guard against any numerical zeros
  x[x <= 0] <- .Machine$double.eps
  x
}

# ------------------------------------------------
# Utility: simulate lognormal directly from min/max
# ------------------------------------------------
# This hides the meanlog/sdlog calculation and returns
# a vector of draws from the implied lognormal.

simulate_lognormal_from_range <- function(n, min_val, max_val) {
  params <- lognorm_params(min_val, max_val)
  r_lognorm_pos(n, params$meanlog, params$sdlog)
}

# ------------------------------------------------
# Utility: simulate secondary impact category
# ------------------------------------------------
# Used for all secondary loss categories (overall or per category).
# Inputs:
#   sl_min, sl_max: 90% confidence range of secondary loss likelihood
#   si_min, si_max: 90% confidence range of secondary loss impact
# Outputs:
#   list with:
#     likelihood: draw of secondary likelihood
#     impact:     draw of secondary impact
#     per_event:  likelihood * impact (expected secondary loss per event)

simulate_secondary_category <- function(n, sl_min, sl_max, si_min, si_max) {
  sl_draws <- simulate_lognormal_from_range(n, sl_min, sl_max)
  si_draws <- simulate_lognormal_from_range(n, si_min, si_max)
  
  list(
    likelihood = sl_draws,
    impact     = si_draws,
    per_event  = sl_draws * si_draws
  )
}

# ------------------------------------------------
# Utility: loss exceedance curve data
# ------------------------------------------------
# Given a vector of losses, compute the points for a
# loss exceedance curve (LEC):
#   x: sorted loss thresholds (positive, adjusted for log scale)
#   y: probability that loss exceeds x

lec_data <- function(losses) {
  losses <- as.numeric(losses)
  losses <- losses[is.finite(losses) & losses >= 0]
  
  if (length(losses) == 0) {
    return(list(x = numeric(0), y = numeric(0)))
  }
  
  # Ensure strictly positive for log scale
  positive_losses <- losses[losses > 0]
  if (length(positive_losses) == 0) {
    # If everything is zero, treat as a tiny positive value
    losses[] <- 1e-6
  } else {
    min_pos <- min(positive_losses)
    losses[losses <= 0] <- min_pos / 10
  }
  
  losses <- sort(losses)
  n      <- length(losses)
  prob_exceed <- 1 - (0:(n - 1)) / n
  
  list(x = losses, y = prob_exceed)
}

# ------------------------------------------------
# Utility: plot LEC (log x-axis) with percentile labels
# ------------------------------------------------
# This version:
#   - Enforces positive losses for log scale
#   - Computes P50, P90, P99 in dollars
#   - Draws vertical lines at those quantiles
#   - Places readable labels with the dollar values on the right

plot_lec <- function(losses, title = "Loss Exceedance Curve") {
  
  losses <- as.numeric(losses)
  losses <- losses[is.finite(losses) & losses > 0]
  
  if (length(losses) == 0) {
    return(ggplot() + ggtitle(title))
  }
  
  # Sort descending for exceedance probability
  sorted <- sort(losses, decreasing = TRUE)
  n <- length(sorted)
  prob <- seq(1 / n, 1, length.out = n)
  
  # Percentiles
  p50 <- quantile(losses, 0.50)
  p90 <- quantile(losses, 0.90)
  p99 <- quantile(losses, 0.99)
  
  df <- data.frame(loss = sorted, prob = prob)
  
  # Exceedance positions
  ecdf_fn <- ecdf(losses)
  y50 <- 1 - ecdf_fn(p50)
  y90 <- 1 - ecdf_fn(p90)
  y99 <- 1 - ecdf_fn(p99)
  
  # Dollar formatter
  short_dollar <- scales::label_dollar(scale_cut = scales::cut_short_scale())
  
  # Force labels to appear inside visible x-range
  x_min <- min(losses[losses > 0])
  x_max <- max(losses)
  x_buffer <- x_max * 2
  
  label_color <- if (isTRUE(shiny::isTruthy(getOption("shiny.theme.dark")))) "white" else "black"
  
  ggplot(df, aes(x = loss, y = prob)) +
    geom_line(color = "#00b386", linewidth = 1.1) +
    
    # Percentile vertical lines
    geom_vline(xintercept = p50, color = "gray70", linetype = "dashed") +
    geom_vline(xintercept = p90, color = "gray70", linetype = "dashed") +
    geom_vline(xintercept = p99, color = "gray70", linetype = "dashed") +
    
    # Labels always inside expanded domain
    annotate("text", x = p50, y = y50,
             label = paste0("P50: ", short_dollar(p50)),
             color = label_color, hjust = -0.2, vjust = -0.3, size = 4) +
    annotate("text", x = p90, y = y90,
             label = paste0("P90: ", short_dollar(p90)),
             color = label_color, hjust = -0.2, vjust = -0.3, size = 4) +
    annotate("text", x = p99, y = y99,
             label = paste0("P99: ", short_dollar(p99)),
             color = label_color, hjust = -0.2, vjust = -0.3, size = 4) +
    
    scale_x_log10(
      limits = c(x_min, x_buffer),
      labels = short_dollar
    ) +
    
    labs(
      title = title,
      x = "Loss (log scale)",
      y = "Exceedance Probability"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

# ------------------------------------------------
# Utility: generic histogram helpers
# ------------------------------------------------

plot_hist_basic <- function(x, bins, main, log_x = FALSE, label = TRUE) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  
  if (length(x) == 0) {
    plot.new()
    title(main)
    return(invisible(NULL))
  }
  
  par(xpd = TRUE)
  
  if (log_x) {
    # Avoid log(0) by dropping non-positive values
    x <- x[x > 0]
    hist(
      log(x),
      breaks = bins,
      col    = FAIR_PRIMARY_COLOR,
      main   = main,
      xlab   = "log(value)",
      labels = label
    )
  } else {
    hist(
      x,
      breaks = bins,
      col    = FAIR_PRIMARY_COLOR,
      main   = main,
      labels = label
    )
  }
}

# ------------------------------------------------
# Utility: simple mode function for discrete-ish data
# ------------------------------------------------

getmode <- function(v) {
  v <- as.numeric(v)
  v <- v[is.finite(v)]
  if (length(v) == 0) return(NA_real_)
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
