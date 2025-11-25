# server.R

# Core plotting and formatting libraries
# (These are also loaded in global.R, but loading again is harmless and explicit.)
library(ggplot2)
library(scales)

# ------------------------------------------------------------------------------
# Light logging helper (high-level only, not per-draw)
# ------------------------------------------------------------------------------

log_info <- function(message) {
  cat(sprintf("[INFO] %s\n", message))
}

# IMPORTANT:
# - We do NOT redefine plot_lec() here.
#   The canonical implementation is in global.R and will be used directly.
# - We also rely on getmode() from global.R.
# - We use N_SIM from global.R as the simulation count.

# ------------------------------------------------------------------------------
# Shiny server
# ------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  ##### Input Variables ########################################################
  # This section converts Shiny inputs to reactive variables used by the model.
  # All min/max values represent 90 percent confidence bounds.
  
  # Use the global simulation constant from global.R
  n <- N_SIM
  
  # Threat Event Frequency (Min, Max) 90% Confidence
  tef_min <- reactive(input$tef_min_num)
  tef_max <- reactive(input$tef_max_num)
  
  # Threat Capability (Min, Max) as percentages (convert to decimals)
  tcap_min_stage <- reactive(input$tcap_slider[1])
  tcap_min       <- reactive(tcap_min_stage() / 100)
  tcap_max_stage <- reactive(input$tcap_slider[2])
  tcap_max       <- reactive(tcap_max_stage() / 100)
  
  # Current Control Strength (Min, Max) as percentages (convert to decimals)
  cs_min_stage <- reactive(input$cs_slider[1])
  cs_min       <- reactive(cs_min_stage() / 100)
  cs_max_stage <- reactive(input$cs_slider[2])
  cs_max       <- reactive(cs_max_stage() / 100)
  
  # Future Control Strength (Min, Max) as percentages (convert to decimals)
  f_cs_min_stage <- reactive(input$f_cs_slider[1])
  f_cs_min       <- reactive(f_cs_min_stage() / 100)
  f_cs_max_stage <- reactive(input$f_cs_slider[2])
  f_cs_max       <- reactive(f_cs_max_stage() / 100)
  
  # Whether to apply future control strength
  future_option <- reactive(input$future_radio)
  
  # Vulnerability method option (TC/CS vs direct percentage)
  vuln_option <- reactive(input$vuln_radio)
  
  # Direct Vulnerability percentage
  vuln_percent_stage <- reactive(input$vuln_slider)
  vuln_percent       <- reactive(vuln_percent_stage() / 100)
  
  # Future Direct Vulnerability percentage
  f_vuln_percent_stage <- reactive(input$f_vuln_slider)
  f_vuln_percent       <- reactive(f_vuln_percent_stage() / 100)
  
  # Likelihood method option (TEF x Vuln vs direct likelihood)
  lkh_option <- reactive(input$lkh_radio)
  lkh_min    <- reactive(input$lkh_min_num)
  lkh_max    <- reactive(input$lkh_max_num)
  
  # Current Primary Impact (Min, Max) 90% Confidence
  pi_min <- reactive(input$pi_min_num)
  pi_max <- reactive(input$pi_max_num)
  
  # Future Primary Impact (Min, Max) 90% Confidence
  f_pi_min <- reactive(input$f_pi_min_num)
  f_pi_max <- reactive(input$f_pi_max_num)
  
  # Secondary Loss method option
  sl_option <- reactive(input$sl_radio)
  
  # Overall Current Secondary Impact Likelihood (as percentages -> decimals)
  oa_sl_min_stage <- reactive(input$oa_sl_slider[1])
  oa_sl_min       <- reactive(oa_sl_min_stage() / 100)
  oa_sl_max_stage <- reactive(input$oa_sl_slider[2])
  oa_sl_max       <- reactive(oa_sl_max_stage() / 100)
  
  # Overall Current Secondary Impact (Min, Max) 90% Confidence
  oa_si_min <- reactive(input$oa_si_min_num)
  oa_si_max <- reactive(input$oa_si_max_num)
  
  # Overall Future Secondary Impact Likelihood (as percentages -> decimals)
  f_oa_sl_min_stage <- reactive(input$f_oa_sl_slider[1])
  f_oa_sl_min       <- reactive(f_oa_sl_min_stage() / 100)
  f_oa_sl_max_stage <- reactive(input$f_oa_sl_slider[2])
  f_oa_sl_max       <- reactive(f_oa_sl_max_stage() / 100)
  
  # Overall Future Secondary Impact (Min, Max) 90% Confidence
  f_oa_si_min <- reactive(input$f_oa_si_min_num)
  f_oa_si_max <- reactive(input$f_oa_si_max_num)
  
  # The following blocks repeat this pattern for category-specific secondary loss:
  # Respond/Replace, Lost Productivity, Competitive Advantage, Reputational Damage,
  # and Legal/Regulatory, each with current and future likelihood and impact ranges.
  
  # Current Respond/Replace Secondary Impact Likelihood
  rr_sl_min_stage <- reactive(input$rr_sl_slider[1])
  rr_sl_min       <- reactive(rr_sl_min_stage() / 100)
  rr_sl_max_stage <- reactive(input$rr_sl_slider[2])
  rr_sl_max       <- reactive(rr_sl_max_stage() / 100)
  
  # Current Respond/Replace Secondary Impact (Min, Max)
  rr_si_min <- reactive(input$rr_si_min_num)
  rr_si_max <- reactive(input$rr_si_max_num)
  
  # Future Respond/Replace Secondary Impact Likelihood
  f_rr_sl_min_stage <- reactive(input$f_rr_sl_slider[1])
  f_rr_sl_min       <- reactive(f_rr_sl_min_stage() / 100)
  f_rr_sl_max_stage <- reactive(input$f_rr_sl_slider[2])
  f_rr_sl_max       <- reactive(f_rr_sl_max_stage() / 100)
  
  # Future Respond/Replace Secondary Impact (Min, Max)
  f_rr_si_min <- reactive(input$f_rr_si_min_num)
  f_rr_si_max <- reactive(input$f_rr_si_max_num)
  
  # Current Lost Productivity Secondary Impact Likelihood
  lp_sl_min_stage <- reactive(input$lp_sl_slider[1])
  lp_sl_min       <- reactive(lp_sl_min_stage() / 100)
  lp_sl_max_stage <- reactive(input$lp_sl_slider[2])
  lp_sl_max       <- reactive(lp_sl_max_stage() / 100)
  
  # Current Lost Productivity Secondary Impact (Min, Max)
  lp_si_min <- reactive(input$lp_si_min_num)
  lp_si_max <- reactive(input$lp_si_max_num)
  
  # Future Lost Productivity Secondary Impact Likelihood
  f_lp_sl_min_stage <- reactive(input$f_lp_sl_slider[1])
  f_lp_sl_min       <- reactive(f_lp_sl_min_stage() / 100)
  f_lp_sl_max_stage <- reactive(input$f_lp_sl_slider[2])
  f_lp_sl_max       <- reactive(f_lp_sl_max_stage() / 100)
  
  # Future Lost Productivity Secondary Impact (Min, Max)
  f_lp_si_min <- reactive(input$f_lp_si_min_num)
  f_lp_si_max <- reactive(input$f_lp_si_max_num)
  
  # Current Competitive Advantage Secondary Impact Likelihood
  ca_sl_min_stage <- reactive(input$ca_sl_slider[1])
  ca_sl_min       <- reactive(ca_sl_min_stage() / 100)
  ca_sl_max_stage <- reactive(input$ca_sl_slider[2])
  ca_sl_max       <- reactive(ca_sl_max_stage() / 100)
  
  # Current Competitive Advantage Secondary Impact (Min, Max)
  ca_si_min <- reactive(input$ca_si_min_num)
  ca_si_max <- reactive(input$ca_si_max_num)
  
  # Future Competitive Advantage Secondary Impact Likelihood
  f_ca_sl_min_stage <- reactive(input$f_ca_sl_slider[1])
  f_ca_sl_min       <- reactive(f_ca_sl_min_stage() / 100)
  f_ca_sl_max_stage <- reactive(input$f_ca_sl_slider[2])
  f_ca_sl_max       <- reactive(f_ca_sl_max_stage() / 100)
  
  # Future Competitive Advantage Secondary Impact (Min, Max)
  f_ca_si_min <- reactive(input$f_ca_si_min_num)
  f_ca_si_max <- reactive(input$f_ca_si_max_num)
  
  # Current Reputational Damage Secondary Impact Likelihood
  rd_sl_min_stage <- reactive(input$rd_sl_slider[1])
  rd_sl_min       <- reactive(rd_sl_min_stage() / 100)
  rd_sl_max_stage <- reactive(input$rd_sl_slider[2])
  rd_sl_max       <- reactive(rd_sl_max_stage() / 100)
  
  # Current Reputational Damage Secondary Impact (Min, Max)
  rd_si_min <- reactive(input$rd_si_min_num)
  rd_si_max <- reactive(input$rd_si_max_num)
  
  # Future Reputational Damage Secondary Impact Likelihood
  f_rd_sl_min_stage <- reactive(input$f_rd_sl_slider[1])
  f_rd_sl_min       <- reactive(f_rd_sl_min_stage() / 100)
  f_rd_sl_max_stage <- reactive(input$f_rd_sl_slider[2])
  f_rd_sl_max       <- reactive(f_rd_sl_max_stage() / 100)
  
  # Future Reputational Damage Secondary Impact (Min, Max)
  f_rd_si_min <- reactive(input$f_rd_si_min_num)
  f_rd_si_max <- reactive(input$f_rd_si_max_num)
  
  # Current Legal / Regulatory Secondary Impact Likelihood
  lr_sl_min_stage <- reactive(input$lr_sl_slider[1])
  lr_sl_min       <- reactive(lr_sl_min_stage() / 100)
  lr_sl_max_stage <- reactive(input$lr_sl_slider[2])
  lr_sl_max       <- reactive(lr_sl_max_stage() / 100)
  
  # Current Legal / Regulatory Secondary Impact (Min, Max)
  lr_si_min <- reactive(input$lr_si_min_num)
  lr_si_max <- reactive(input$lr_si_max_num)
  
  # Future Legal / Regulatory Secondary Impact Likelihood
  f_lr_sl_min_stage <- reactive(input$f_lr_sl_slider[1])
  f_lr_sl_min       <- reactive(f_lr_sl_min_stage() / 100)
  f_lr_sl_max_stage <- reactive(input$f_lr_sl_slider[2])
  f_lr_sl_max       <- reactive(f_lr_sl_max_stage() / 100)
  
  # Future Legal / Regulatory Secondary Impact (Min, Max)
  f_lr_si_min <- reactive(input$f_lr_si_min_num)
  f_lr_si_max <- reactive(input$f_lr_si_max_num)
  
  ##### Mean and Standard Deviation for Lognormal Distributions ###############
  # All of these use the standard trick for 90 percent CI of a lognormal:
  #   meanlog = (log(max) + log(min)) / 2
  #   sdlog   = (log(max) - log(min)) / 3.29
  
  # Threat Event Frequency
  tef_mean <- reactive({
    if (lkh_option() == 1) {
      (log(tef_max()) + log(tef_min())) / 2
    }
  })
  tef_sd <- reactive({
    if (lkh_option() == 1) {
      (log(tef_max()) - log(tef_min())) / 3.29
    }
  })
  
  # Threat Capability
  tcap_mean <- reactive({
    if (lkh_option() == 1 & vuln_option() == 1) {
      (log(tcap_max()) + log(tcap_min())) / 2
    }
  })
  tcap_sd <- reactive({
    if (lkh_option() == 1 & vuln_option() == 1) {
      (log(tcap_max()) - log(tcap_min())) / 3.29
    }
  })
  
  # Current Control Strength
  cs_mean <- reactive({
    if (lkh_option() == 1 & vuln_option() == 1) {
      (log(cs_max()) + log(cs_min())) / 2
    }
  })
  cs_sd <- reactive({
    if (lkh_option() == 1 & vuln_option() == 1) {
      (log(cs_max()) - log(cs_min())) / 3.29
    }
  })
  
  # Future Control Strength
  f_cs_mean <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & vuln_option() == 1) {
      (log(f_cs_max()) + log(f_cs_min())) / 2
    }
  })
  f_cs_sd <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & vuln_option() == 1) {
      (log(f_cs_max()) - log(f_cs_min())) / 3.29
    }
  })
  
  # Direct Likelihood
  lkh_mean <- reactive({
    if (lkh_option() == 2) {
      (log(lkh_max()) + log(lkh_min())) / 2
    }
  })
  lkh_sd <- reactive({
    if (lkh_option() == 2) {
      (log(lkh_max()) - log(lkh_min())) / 3.29
    }
  })
  
  # Current Primary Single Event Impact
  pi_mean <- reactive({
    (log(pi_max()) + log(pi_min())) / 2
  })
  pi_sd <- reactive({
    (log(pi_max()) - log(pi_min())) / 3.29
  })
  
  # Future Primary Single Event Impact
  f_pi_mean <- reactive({
    if (lkh_option() == 1 & future_option() == 2) {
      (log(f_pi_max()) + log(f_pi_min())) / 2
    }
  })
  f_pi_sd <- reactive({
    if (lkh_option() == 1 & future_option() == 2) {
      (log(f_pi_max()) - log(f_pi_min())) / 3.29
    }
  })
  
  # Overall Current Secondary Impact Likelihood
  oa_sl_mean <- reactive({
    (log(oa_sl_max()) + log(oa_sl_min())) / 2
  })
  oa_sl_sd <- reactive({
    (log(oa_sl_max()) - log(oa_sl_min())) / 3.29
  })
  
  # Overall Current Secondary Single Event Impact
  oa_si_mean <- reactive({
    (log(oa_si_max()) + log(oa_si_min())) / 2
  })
  oa_si_sd <- reactive({
    (log(oa_si_max()) - log(oa_si_min())) / 3.29
  })
  
  # Overall Future Secondary Impact Likelihood
  f_oa_sl_mean <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 1) {
      (log(f_oa_sl_max()) + log(f_oa_sl_min())) / 2
    }
  })
  f_oa_sl_sd <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 1) {
      (log(f_oa_sl_max()) - log(f_oa_sl_min())) / 3.29
    }
  })
  
  # Overall Future Secondary Single Event Impact
  f_oa_si_mean <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 1) {
      (log(f_oa_si_max()) + log(f_oa_si_min())) / 2
    }
  })
  f_oa_si_sd <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 1) {
      (log(f_oa_si_max()) - log(f_oa_si_min())) / 3.29
    }
  })
  
  # Category-specific secondary loss parameters
  # Respond/Replace
  rr_sl_mean <- reactive({
    if (sl_option() == 2) {
      (log(rr_sl_max()) + log(rr_sl_min())) / 2
    }
  })
  rr_sl_sd <- reactive({
    if (sl_option() == 2) {
      (log(rr_sl_max()) - log(rr_sl_min())) / 3.29
    }
  })
  rr_si_mean <- reactive({
    if (sl_option() == 2) {
      (log(rr_si_max()) + log(rr_si_min())) / 2
    }
  })
  rr_si_sd <- reactive({
    if (sl_option() == 2) {
      (log(rr_si_max()) - log(rr_si_min())) / 3.29
    }
  })
  f_rr_sl_mean <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 2) {
      (log(f_rr_sl_max()) + log(f_rr_sl_min())) / 2
    }
  })
  f_rr_sl_sd <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 2) {
      (log(f_rr_sl_max()) - log(f_rr_sl_min())) / 3.29
    }
  })
  f_rr_si_mean <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 2) {
      (log(f_rr_si_max()) + log(f_rr_si_min())) / 2
    }
  })
  f_rr_si_sd <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 2) {
      (log(f_rr_si_max()) - log(f_rr_si_min())) / 3.29
    }
  })
  
  # Lost Productivity
  lp_sl_mean <- reactive({
    if (sl_option() == 2) {
      (log(lp_sl_max()) + log(lp_sl_min())) / 2
    }
  })
  lp_sl_sd <- reactive({
    if (sl_option() == 2) {
      (log(lp_sl_max()) - log(lp_sl_min())) / 3.29
    }
  })
  lp_si_mean <- reactive({
    if (sl_option() == 2) {
      (log(lp_si_max()) + log(lp_si_min())) / 2
    }
  })
  lp_si_sd <- reactive({
    if (sl_option() == 2) {
      (log(lp_si_max()) - log(lp_si_min())) / 3.29
    }
  })
  f_lp_sl_mean <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 2) {
      (log(f_lp_sl_max()) + log(f_lp_sl_min())) / 2
    }
  })
  f_lp_sl_sd <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 2) {
      (log(f_lp_sl_max()) - log(f_lp_sl_min())) / 3.29
    }
  })
  f_lp_si_mean <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 2) {
      (log(f_lp_si_max()) + log(f_lp_si_min())) / 2
    }
  })
  f_lp_si_sd <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 2) {
      (log(f_lp_si_max()) - log(f_lp_si_min())) / 3.29
    }
  })
  
  # Competitive Advantage
  ca_sl_mean <- reactive({
    if (sl_option() == 2) {
      (log(ca_sl_max()) + log(ca_sl_min())) / 2
    }
  })
  ca_sl_sd <- reactive({
    if (sl_option() == 2) {
      (log(ca_sl_max()) - log(ca_sl_min())) / 3.29
    }
  })
  ca_si_mean <- reactive({
    if (sl_option() == 2) {
      (log(ca_si_max()) + log(ca_si_min())) / 2
    }
  })
  ca_si_sd <- reactive({
    if (sl_option() == 2) {
      (log(ca_si_max()) - log(ca_si_min())) / 3.29
    }
  })
  f_ca_sl_mean <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 2) {
      (log(f_ca_sl_max()) + log(f_ca_sl_min())) / 2
    }
  })
  f_ca_sl_sd <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 2) {
      (log(f_ca_sl_max()) - log(f_ca_sl_min())) / 3.29
    }
  })
  f_ca_si_mean <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 2) {
      (log(f_ca_si_max()) + log(f_ca_si_min())) / 2
    }
  })
  f_ca_si_sd <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 2) {
      (log(f_ca_si_max()) - log(f_ca_si_min())) / 3.29
    }
  })
  
  # Reputational Damage
  rd_sl_mean <- reactive({
    if (sl_option() == 2) {
      (log(rd_sl_max()) + log(rd_sl_min())) / 2
    }
  })
  rd_sl_sd <- reactive({
    if (sl_option() == 2) {
      (log(rd_sl_max()) - log(rd_sl_min())) / 3.29
    }
  })
  rd_si_mean <- reactive({
    if (sl_option() == 2) {
      (log(rd_si_max()) + log(rd_si_min())) / 2
    }
  })
  rd_si_sd <- reactive({
    if (sl_option() == 2) {
      (log(rd_si_max()) - log(rd_si_min())) / 3.29
    }
  })
  f_rd_sl_mean <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 2) {
      (log(f_rd_sl_max()) + log(f_rd_sl_min())) / 2
    }
  })
  f_rd_sl_sd <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 2) {
      (log(f_rd_sl_max()) - log(f_rd_sl_min())) / 3.29
    }
  })
  f_rd_si_mean <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 2) {
      (log(f_rd_si_max()) + log(f_rd_si_min())) / 2
    }
  })
  f_rd_si_sd <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 2) {
      (log(f_rd_si_max()) - log(f_rd_si_min())) / 3.29
    }
  })
  
  # Legal / Regulatory
  lr_sl_mean <- reactive({
    if (sl_option() == 2) {
      (log(lr_sl_max()) + log(lr_sl_min())) / 2
    }
  })
  lr_sl_sd <- reactive({
    if (sl_option() == 2) {
      (log(lr_sl_max()) - log(lr_sl_min())) / 3.29
    }
  })
  lr_si_mean <- reactive({
    if (sl_option() == 2) {
      (log(lr_si_max()) + log(lr_si_min())) / 2
    }
  })
  lr_si_sd <- reactive({
    if (sl_option() == 2) {
      (log(lr_si_max()) - log(lr_si_min())) / 3.29
    }
  })
  f_lr_sl_mean <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 2) {
      (log(f_lr_sl_max()) + log(f_lr_sl_min())) / 2
    }
  })
  f_lr_sl_sd <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 2) {
      (log(f_lr_sl_max()) - log(f_lr_sl_min())) / 3.29
    }
  })
  f_lr_si_mean <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 2) {
      (log(f_lr_si_max()) + log(f_lr_si_min())) / 2
    }
  })
  f_lr_si_sd <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 2) {
      (log(f_lr_si_max()) - log(f_lr_si_min())) / 3.29
    }
  })
  
  ##### FAIR Analysis Calculations ############################################
  
  # Random seed for reproducible runs within a session
  rand <- floor(runif(1, min = 1000, max = 999999))
  set.seed(rand)
  
  # Threat Event Frequency (lognormal instead of Poisson-lognormal)
  tef <- reactive({
    if (lkh_option() == 1) {
      rlnorm(n, tef_mean(), tef_sd())
    }
  })
  
  # Threat Capability, Control Strength, and Future Control Strength
  tcap <- reactive({
    if (lkh_option() == 1 & vuln_option() == 1) {
      rlnorm(n, tcap_mean(), tcap_sd())
    }
  })
  cs <- reactive({
    if (lkh_option() == 1 & vuln_option() == 1) {
      rlnorm(n, cs_mean(), cs_sd())
    }
  })
  f_cs <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & vuln_option() == 1) {
      rlnorm(n, f_cs_mean(), f_cs_sd())
    }
  })
  
  # Direct Likelihood (lognormal)
  lkh_temp2 <- reactive({
    if (lkh_option() == 2) {
      rlnorm(n, lkh_mean(), lkh_sd())
    }
  })
  
  # Current Residual Vulnerability using TC and CS, or direct percentage
  vuln_temp4 <- reactive({
    if (lkh_option() == 1 & vuln_option() == 1) {
      tcap() - cs()
    }
  })
  vuln_temp3 <- reactive({
    if (lkh_option() == 1 & vuln_option() == 1) {
      pmax(vuln_temp4(), 0)
    }
  })
  vuln_temp2 <- reactive({
    if (lkh_option() == 1 & vuln_option() == 1) {
      pmin(vuln_temp3(), 1)
    }
  })
  vuln_temp1 <- reactive({
    if (lkh_option() == 1 & vuln_option() == 1) {
      (sum(vuln_temp2() > 0)) / n
    }
  })
  vuln <- reactive({
    if (lkh_option() == 1 & vuln_option() == 1) {
      vuln_temp1()
    } else {
      vuln_percent()
    }
  })
  
  # Future Residual Vulnerability
  f_vuln_temp4 <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & vuln_option() == 1) {
      tcap() - f_cs()
    }
  })
  f_vuln_temp3 <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & vuln_option() == 1) {
      pmax(f_vuln_temp4(), 0)
    }
  })
  f_vuln_temp2 <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & vuln_option() == 1) {
      pmin(f_vuln_temp3(), 1)
    }
  })
  f_vuln_temp1 <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & vuln_option() == 1) {
      (sum(f_vuln_temp2() > 0)) / n
    }
  })
  f_vuln <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & vuln_option() == 1) {
      f_vuln_temp1()
    } else {
      f_vuln_percent()
    }
  })
  
  # Inherent Vulnerability (assumed constant)
  in_vuln <- 0.95
  
  # Current Residual Primary Likelihood
  lkh_temp1 <- reactive({
    if (lkh_option() == 1) {
      tef() * as.vector(vuln())
    }
  })
  lkh <- reactive({
    if (lkh_option() == 1) {
      round(lkh_temp1(), digits = 2)
    } else {
      round(lkh_temp2(), digits = 2)
    }
  })
  
  # Future Residual Primary Likelihood
  f_lkh <- reactive({
    round(tef() * as.vector(f_vuln()), digits = 2)
  })
  
  # Inherent Threat Event Frequency (assumed to increase 10 percent without controls)
  in_tef <- reactive({
    tef() * 1.1
  })
  in_lkh <- reactive({
    round(in_tef() * in_vuln, digits = 2)
  })
  
  # Primary Impact per event (current, inherent, future)
  pi <- reactive({
    rlnorm(n, pi_mean(), pi_sd())
  })
  in_pi <- reactive({
    pi() * 1.2  # inherent primary impact assumes +20 percent without controls
  })
  f_pi <- reactive({
    rlnorm(n, f_pi_mean(), f_pi_sd())
  })
  
  # Current Secondary Residual Risk – Overall method
  oa_sl <- reactive({
    if (sl_option() == 1) {
      rlnorm(n, oa_sl_mean(), oa_sl_sd())
    }
  })
  oa_si <- reactive({
    if (sl_option() == 1) {
      rlnorm(n, oa_si_mean(), oa_si_sd())
    }
  })
  
  # Current Secondary Residual Risk – Category method (all lognormal)
  rr_sl <- reactive({
    if (sl_option() == 2) {
      rlnorm(n, rr_sl_mean(), rr_sl_sd())
    }
  })
  rr_si <- reactive({
    if (sl_option() == 2) {
      rlnorm(n, rr_si_mean(), rr_si_sd())
    }
  })
  
  lp_sl <- reactive({
    if (sl_option() == 2) {
      rlnorm(n, lp_sl_mean(), lp_sl_sd())
    }
  })
  lp_si <- reactive({
    if (sl_option() == 2) {
      rlnorm(n, lp_si_mean(), lp_si_sd())
    }
  })
  
  ca_sl <- reactive({
    if (sl_option() == 2) {
      rlnorm(n, ca_sl_mean(), ca_sl_sd())
    }
  })
  ca_si <- reactive({
    if (sl_option() == 2) {
      rlnorm(n, ca_si_mean(), ca_si_sd())
    }
  })
  
  rd_sl <- reactive({
    if (sl_option() == 2) {
      rlnorm(n, rd_sl_mean(), rd_sl_sd())
    }
  })
  rd_si <- reactive({
    if (sl_option() == 2) {
      rlnorm(n, rd_si_mean(), rd_si_sd())
    }
  })
  
  lr_sl <- reactive({
    if (sl_option() == 2) {
      rlnorm(n, lr_sl_mean(), lr_sl_sd())
    }
  })
  lr_si <- reactive({
    if (sl_option() == 2) {
      rlnorm(n, lr_si_mean(), lr_si_sd())
    }
  })
  
  # Current Residual Secondary Impact (single-event)
  si <- reactive({
    if (sl_option() == 1) {
      oa_si()
    } else {
      rr_si() + lp_si() + ca_si() + rd_si() + lr_si()
    }
  })
  
  # Future Secondary Residual Risk – Overall method
  f_oa_sl <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 1) {
      rlnorm(n, f_oa_sl_mean(), f_oa_sl_sd())
    }
  })
  f_oa_si <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 1) {
      rlnorm(n, f_oa_si_mean(), f_oa_si_sd())
    }
  })
  
  # Future Secondary Residual Risk – Category method
  f_rr_sl <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 2) {
      rlnorm(n, f_rr_sl_mean(), f_rr_sl_sd())
    }
  })
  f_rr_si <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 2) {
      rlnorm(n, f_rr_si_mean(), f_rr_si_sd())
    }
  })
  
  f_lp_sl <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 2) {
      rlnorm(n, f_lp_sl_mean(), f_lp_sl_sd())
    }
  })
  f_lp_si <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 2) {
      rlnorm(n, f_lp_si_mean(), f_lp_si_sd())
    }
  })
  
  f_ca_sl <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 2) {
      rlnorm(n, f_ca_sl_mean(), f_ca_sl_sd())
    }
  })
  f_ca_si <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 2) {
      rlnorm(n, f_ca_si_mean(), f_ca_si_sd())
    }
  })
  
  f_rd_sl <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 2) {
      rlnorm(n, f_rd_sl_mean(), f_rd_sl_sd())
    }
  })
  f_rd_si <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 2) {
      rlnorm(n, f_rd_si_mean(), f_rd_si_sd())
    }
  })
  
  f_lr_sl <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 2) {
      rlnorm(n, f_lr_sl_mean(), f_lr_sl_sd())
    }
  })
  f_lr_si <- reactive({
    if (lkh_option() == 1 & future_option() == 2 & sl_option() == 2) {
      rlnorm(n, f_lr_si_mean(), f_lr_si_sd())
    }
  })
  
  # Future Residual Secondary Impact (single-event)
  f_si <- reactive({
    if (sl_option() == 1) {
      f_oa_si()
    } else {
      f_rr_si() + f_lp_si() + f_ca_si() + f_rd_si() + f_lr_si()
    }
  })
  
  # Inherent Secondary Impact (scaled up)
  in_si <- reactive({
    si() * 1.2
  })
  
  # Inherent Impact
  in_ipt <- reactive({
    round(in_pi() + in_si())
  })
  
  # Secondary Impact per event – current and future
  sie <- reactive({
    if (sl_option() == 1) {
      oa_si() * oa_sl()
    } else {
      (rr_sl() * rr_si()) +
        (lp_sl() * lp_si()) +
        (ca_sl() * ca_si()) +
        (rd_sl() * rd_si()) +
        (lr_sl() * lr_si())
    }
  })
  f_sie <- reactive({
    if (sl_option() == 1) {
      f_oa_si() * f_oa_sl()
    } else {
      (f_rr_sl() * f_rr_si()) +
        (f_lp_sl() * f_lp_si()) +
        (f_ca_sl() * f_ca_si()) +
        (f_rd_sl() * f_rd_si()) +
        (f_lr_sl() * f_lr_si())
    }
  })
  
  # Current and Future Residual Impact per event
  ipt <- reactive({
    round(pi() + sie())
  })
  f_ipt <- reactive({
    round(f_pi() + f_sie())
  })
  
  # Annual Loss Expectancy (1 year)
  rale <- reactive({
    round(lkh() * ipt())
  })
  f_rale <- reactive({
    round(f_lkh() * f_ipt())
  })
  in_ale <- reactive({
    round(in_lkh() * in_ipt())
  })
  
  # 10-year ALE
  rale_ten <- reactive({
    rale() * 10
  })
  f_rale_ten <- reactive({
    f_rale() * 10
  })
  in_ale_ten <- reactive({
    in_ale() * 10
  })
  
  ##### Percentile Outputs for Current, Future, and Inherent ALE ##############
  # Note: getmode() is provided by global.R and used here.
  # Note: quantile() does not accept maxDecimals; that argument is removed
  # and rounding is handled by scales::dollar or left as is.
  
  # Current Residual ALE
  output$rale_10 <- renderPrint({
    print("Tenth Percentile")
    dollar(quantile(rale(), probs = 0.10))
  })
  output$rale_mode <- renderPrint({
    print("Most Likely")
    dollar(getmode(rale()))
  })
  output$rale_90 <- renderPrint({
    print("Ninetieth Percentile")
    dollar(quantile(rale(), probs = 0.90))
  })
  output$rale_99 <- renderPrint({
    print("Ninety-Ninth Percentile")
    dollar(quantile(rale(), probs = 0.99))
  })
  
  # Current 10-year ALE percentiles
  output$rale_ten_10 <- renderPrint({
    print("Tenth Percentile")
    dollar(quantile(rale_ten(), probs = 0.10))
  })
  output$rale_ten_90 <- renderPrint({
    print("Ninetieth Percentile")
    dollar(quantile(rale_ten(), probs = 0.90))
  })
  output$rale_ten_99 <- renderPrint({
    print("Ninety-Ninth Percentile")
    dollar(quantile(rale_ten(), probs = 0.99))
  })
  
  # Future Residual ALE percentiles
  output$f_rale_10 <- renderPrint({
    print("Tenth Percentile")
    dollar(quantile(f_rale(), probs = 0.10))
  })
  output$f_rale_mode <- renderPrint({
    print("Most Likely")
    dollar(getmode(f_rale()))
  })
  output$f_rale_90 <- renderPrint({
    print("Ninetieth Percentile")
    dollar(quantile(f_rale(), probs = 0.90))
  })
  output$f_rale_99 <- renderPrint({
    print("Ninety-Ninth Percentile")
    dollar(quantile(f_rale(), probs = 0.99))
  })
  
  # Future 10-year ALE percentiles
  output$f_rale_ten_10 <- renderPrint({
    print("Tenth Percentile")
    dollar(quantile(f_rale_ten(), probs = 0.10))
  })
  output$f_rale_ten_90 <- renderPrint({
    print("Ninetieth Percentile")
    dollar(quantile(f_rale_ten(), probs = 0.90))
  })
  output$f_rale_ten_99 <- renderPrint({
    print("Ninety-Ninth Percentile")
    dollar(quantile(f_rale_ten(), probs = 0.99))
  })
  
  # Inherent ALE percentiles
  output$in_ale_10 <- renderPrint({
    print("Tenth Percentile")
    dollar(quantile(in_ale(), probs = 0.10))
  })
  output$in_ale_mode <- renderPrint({
    print("Most Likely")
    dollar(getmode(in_ale()))
  })
  output$in_ale_90 <- renderPrint({
    print("Ninetieth Percentile")
    dollar(quantile(in_ale(), probs = 0.90))
  })
  output$in_ale_99 <- renderPrint({
    print("Ninety-Ninth Percentile")
    dollar(quantile(in_ale(), probs = 0.99))
  })
  
  # Inherent 10-year ALE percentiles
  output$in_ale_ten_10 <- renderPrint({
    print("Tenth Percentile")
    dollar(quantile(in_ale_ten(), probs = 0.10))
  })
  output$in_ale_ten_90 <- renderPrint({
    print("Ninetieth Percentile")
    dollar(quantile(in_ale_ten(), probs = 0.90))
  })
  output$in_ale_ten_99 <- renderPrint({
    print("Ninety-Ninth Percentile")
    dollar(quantile(in_ale_ten(), probs = 0.99))
  })
  
  ###############################################
  # INHERENT ALE HISTOGRAM AND LEC (STACKED)
  ###############################################
  
  # Histogram bins reactive (UI uses input$in_ale_bins)
  in_ale_bins <- reactive(input$in_ale_bins)
  
  # Inherent ALE histogram (log-scale)
  output$in_ale_hist <- renderPlot({
    req(in_ale())
    vals <- in_ale()
    
    par(xpd = TRUE)
    hist(
      log(vals),
      breaks = in_ale_bins(),
      col = FAIR_PRIMARY_COLOR,
      main = "Inherent Annual Loss Expectancy (Histogram, log scale)",
      xlab = "log(ALE)",
      labels = TRUE
    )
  })
  
  # Inherent ALE LEC
  output$in_ale_lec <- renderPlot({
    req(in_ale())
    log_info("Rendering inherent ALE LEC.")
    print(plot_lec(in_ale(), "Inherent Loss Exceedance Curve"))
  })
  
  ##### Vulnerability Outputs ##################################################
  
  output$r_vuln <- renderPrint({
    print(percent(vuln()))
  })
  output$f_r_vuln <- renderPrint({
    print(percent(f_vuln()))
  })
  
  ##### Inherent Risk Summaries and Plots ######################################
  
  output$in_lkh_sum <- renderPrint({
    summary(in_lkh())
  })
  
  ilb <- reactive(input$in_lkh_bins)
  
  output$in_lkh_hist <- renderPlot({
    req(in_lkh())
    par(xpd = TRUE)
    hist(in_lkh(), col = FAIR_PRIMARY_COLOR, breaks = ilb(), labels = TRUE)
  })
  
  output$in_ipt_sum <- renderPrint({
    dollar(c(summary(in_ipt())))
  })
  
  iib <- reactive(input$in_ipt_bins)
  
  output$in_ipt_hist <- renderPlot({
    req(in_ipt())
    par(xpd = TRUE)
    hist(in_ipt(), col = FAIR_PRIMARY_COLOR, breaks = iib(), labels = TRUE)
  })
  
  output$in_ale_sum <- renderPrint({
    log_info("Rendering inherent ALE summary.")
    dollar(c(summary(in_ale())))
  })
  
  output$in_ale_ten_sum <- renderPrint({
    dollar(c(summary(in_ale_ten())))
  })
  
  # Inherent ALE plots:
  #   - in_ale_hist_1: log-histogram of inherent ALE
  #   - in_ale_hist_2: LEC using plot_lec()
  iab <- reactive(input$in_ale_bins)
  
  # Log-histogram of inherent ALE
  output$in_ale_hist_1 <- renderPlot({
    req(in_ale())
    par(xpd = TRUE)
    hist(log(in_ale()), col = FAIR_PRIMARY_COLOR, breaks = iab(), labels = TRUE)
  })
  
  # Inherent Loss Exceedance Curve
  output$in_ale_hist_2 <- renderPlot({
    req(in_ale())
    log_info("Rendering inherent ALE LEC.")
    print(plot_lec(in_ale(), "Inherent Loss Exceedance Curve"))
  })
  
  ##### Current Residual Risk Summaries and Plots ##############################
  
  output$lkh_sum <- renderPrint({
    summary(lkh())
  })
  
  lb <- reactive(input$lkh_bins)
  
  output$lkh_hist <- renderPlot({
    req(lkh())
    par(xpd = TRUE)
    hist(lkh(), col = FAIR_PRIMARY_COLOR, breaks = lb(), labels = TRUE)
  })
  
  output$ipt_sum <- renderPrint({
    dollar(c(summary(ipt())))
  })
  
  ib <- reactive(input$ipt_bins)
  
  output$ipt_hist <- renderPlot({
    req(ipt())
    par(xpd = TRUE)
    hist(ipt(), col = FAIR_PRIMARY_COLOR, breaks = ib(), labels = TRUE)
  })
  
  output$rale_sum <- renderPrint({
    log_info("Rendering current residual ALE summary.")
    dollar(c(summary(rale())))
  })
  
  output$rale_ten_sum <- renderPrint({
    dollar(c(summary(rale_ten())))
  })
  
  rab <- reactive(input$rale_bins)
  
  # First plot: log-histogram of current residual ALE
  output$rale_hist_1 <- renderPlot({
    req(rale())
    par(xpd = TRUE)
    hist(log(rale()), col = FAIR_PRIMARY_COLOR, breaks = rab(), labels = TRUE)
  })
  
  # Second plot: Loss Exceedance Curve (LEC) for current residual ALE
  output$rale_hist_2 <- renderPlot({
    req(rale())
    log_info("Rendering current residual ALE LEC.")
    print(plot_lec(rale(), "Current Residual Loss Exceedance Curve"))
  })
  
  ##### Future Residual Risk Summaries and Plots ###############################
  
  output$f_lkh_sum <- renderPrint({
    summary(f_lkh())
  })
  
  f_lb <- reactive(input$f_lkh_bins)
  
  output$f_lkh_hist <- renderPlot({
    req(f_lkh())
    par(xpd = TRUE)
    hist(f_lkh(), col = FAIR_PRIMARY_COLOR, breaks = f_lb(), labels = TRUE)
  })
  
  output$f_ipt_sum <- renderPrint({
    dollar(c(summary(f_ipt())))
  })
  
  f_ib <- reactive(input$f_ipt_bins)
  
  output$f_ipt_hist <- renderPlot({
    req(f_ipt())
    par(xpd = TRUE)
    hist(f_ipt(), col = FAIR_PRIMARY_COLOR, breaks = f_ib(), labels = TRUE)
  })
  
  output$f_rale_sum <- renderPrint({
    log_info("Rendering future residual ALE summary.")
    dollar(c(summary(f_rale())))
  })
  
  output$f_rale_ten_sum <- renderPrint({
    dollar(c(summary(f_rale_ten())))
  })
  
  f_rab <- reactive(input$f_rale_bins)
  
  # First plot: log-histogram of future residual ALE
  output$f_rale_hist_1 <- renderPlot({
    req(f_rale())
    par(xpd = TRUE)
    hist(log(f_rale()), col = FAIR_PRIMARY_COLOR, breaks = f_rab(), labels = TRUE)
  })
  
  # Second plot: Loss Exceedance Curve (LEC) for future residual ALE
  output$f_rale_hist_2 <- renderPlot({
    req(f_rale())
    log_info("Rendering future residual ALE LEC.")
    print(plot_lec(f_rale(), "Future Residual Loss Exceedance Curve"))
  })
  
  ##### Theme toggle (Light / Dark) ###########################################
  # Uses 'light' and 'dark' themes defined in global.R
  # with version = 5 to match ui.R Bootstrap 5 config.
  
  observe({
    session$setCurrentTheme(
      if (isTRUE(input$dark_mode)) light else dark
    )
  })
}
