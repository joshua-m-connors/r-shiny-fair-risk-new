library(shiny)
library(shinyWidgets)
library(bslib)

ui <- fluidPage(
  
  # Use the dark theme from global.R by default
  theme = dark,
  
  # Light / Dark mode toggle
  div(
    class = "custom-control custom-switch", 
    tags$input(
      id = "dark_mode",
      type = "checkbox",
      class = "custom-control-input",
      onclick = HTML("Shiny.setInputValue('dark_mode', document.getElementById('dark_mode').value);")
    ),
    tags$label(
      "Light/Dark Mode",
      `for` = "dark_mode",
      class = "custom-control-label"
    )
  ),
  
  titlePanel("Factor Analysis for Information Risk (FAIR) Analysis"),
  
  sidebarLayout(
    
    # -------------------------------------------------------------------------
    # LEFT SIDEBAR – INPUTS
    # -------------------------------------------------------------------------
    sidebarPanel(
      width = 4,
      
      # ------------------------------------------------
      # General metadata
      # ------------------------------------------------
      fluidRow(column(12, textInput("text", h3("Risk Analysis Title"), width = "100%", value = "Title..."))),
      fluidRow(column(12, textInput("text", h3("Risk Analyst Name"), width = "100%", value = "Name..."))),
      fluidRow(column(12, textAreaInput("text", h3("Risk Analysis Participants"), width = "100%", value = "Names/Titles..."))),
      fluidRow(column(12, dateInput("date", h3("Date of Risk Analysis"), value = Sys.Date()))),
      
      checkboxGroupInput(
        "threatcheckGroup",
        h4("Threat Type(s) In Scope"),
        choices = list(
          "External Deliberate" = 1,
          "External Not Deliberate" = 2,
          "Internal Deliberate" = 3,
          "Internal Not Deliberate" = 4,
          "Natural" = 5
        )
      ),
      
      checkboxGroupInput(
        "vulncheckGroup",
        h4("Vulnerability Type(s) In Scope"),
        choices = list(
          "People" = 1,
          "Process" = 2,
          "Technology" = 3,
          "Partners" = 4
        )
      ),
      
      checkboxGroupInput(
        "losscheckGroup",
        h4("Impact/Loss Type(s) In Scope"),
        choices = list(
          "Confidentiality" = 1,
          "Integrity" = 2,
          "Availability" = 3,
          "Privacy" = 4
        )
      ),
      
      fluidRow(column(12, textAreaInput("text", h3("Risk Analysis Description"), width = "100%", value = "Description..."))),
      
      # Future Risk toggle
      conditionalPanel(
        condition = "input.lkh_radio == 1",
        fluidRow(
          radioButtons(
            "future_radio",
            h4("- Assess Future Risk"),
            choices = list("No" = 1, "Yes" = 2),
            selected = 1,
            width = "100%"
          )
        )
      ),
      
      h5("Note: Estimates below should be made with 90 percent confidence"),
      h2("Likelihood/Frequency"),
      
      # Likelihood method
      fluidRow(
        radioButtons(
          "lkh_radio",
          h4("- Likelihood Methods"),
          choices = list(
            "Threat Event Frequency and Vulnerability" = 1,
            "Direct Likelihood - No Inherent or Future Risk Calculations Performed" = 2
          ),
          selected = 1,
          width = "100%"
        )
      ),
      
      # ------------------------------------------------
      # Threat Event Frequency + Vulnerability
      # ------------------------------------------------
      conditionalPanel(
        condition = "input.lkh_radio == 1",
        
        h3("Threat Event Frequency"),
        h5("- Annual frequency that Threats are encountered."),
        
        fluidRow(column(12, textAreaInput("text", h4("Threat Description"), width = "100%", value = "Threat..."))),
        fluidRow(column(12, formatNumericInput("tef_min_num", "Threat Event Frequency - Minimum", 1,  format="dotDecimalCharCommaSeparator", width="100%", align="right"))),
        fluidRow(column(12, formatNumericInput("tef_max_num", "Threat Event Frequency - Maximum", 10, format="dotDecimalCharCommaSeparator", width="100%", align="right"))),
        
        h3("Vulnerability"),
        fluidRow(column(12, textAreaInput("text", h4("Vulnerability Description"), width = "100%", value = "Vulnerability..."))),
        
        radioButtons(
          "vuln_radio",
          h4("- Vulnerability Methods"),
          choices = list(
            "Threat Capability and Control Strength" = 1,
            "Direct Vulnerability Percentage" = 2
          ),
          selected = 1
        ),
        
        # -------------------- TCAP + CS path --------------------
        conditionalPanel(
          condition = "input.vuln_radio == 1",
          
          h5("- Threat Capability and Current/Future Control Strength percentile ranges."),
          
          chooseSliderSkin("Flat", color = "#00563f"),
          fluidRow(
            column(
              12,
              sliderInput("tcap_slider", h4("- Threat Capability (%)"), min = 1, max = 99, value = c(10, 50)),
              sliderInput("cs_slider",   h4("- Current Control Strength (%)"), min = 1, max = 99, value = c(25, 75)),
              
              conditionalPanel(
                condition = "input.future_radio == 2",
                sliderInput("f_cs_slider", h4("- Future Control Strength (%)"), min = 1, max = 99, value = c(35, 85))
              )
            )
          ),
          
          conditionalPanel(
            condition = "input.future_radio == 2",
            fluidRow(column(12, textAreaInput("text", h4("Future Controls Description"), width = "100%", value = "Future Controls...")))
          ),
          
          h4("- Current Vulnerability Percentage"),
          verbatimTextOutput("r_vuln"),
          
          conditionalPanel(
            condition = "input.future_radio == 2",
            h4("- Future Vulnerability Percentage"),
            verbatimTextOutput("f_r_vuln")
          )
        ),
        
        # -------------------- Direct Vulnerability path --------------------
        conditionalPanel(
          condition = "input.vuln_radio == 2",
          h5("- Direct vulnerability percentage."),
          sliderInput("vuln_slider", h4("- Current Vulnerability (%)"), min = 1, max = 99, value = 10),
          
          conditionalPanel(
            condition = "input.future_radio == 2",
            sliderInput("f_vuln_slider", h4("- Future Vulnerability (%)"), min = 1, max = 99, value = 10)
          )
        )
      ),
      
      # ------------------------------------------------
      # Direct Likelihood method
      # ------------------------------------------------
      conditionalPanel(
        condition = "input.lkh_radio == 2",
        h5("- Likelihood of realizing the Impact."),
        fluidRow(column(12, textAreaInput("text", h4("Likelihood Description"), width="100%", value="Likelihood..."))),
        fluidRow(column(12, formatNumericInput("lkh_min_num", "Likelihood - Minimum", 1,  format="dotDecimalCharCommaSeparator", width="100%", align="right"))),
        fluidRow(column(12, formatNumericInput("lkh_max_num", "Likelihood - Maximum", 10, format="dotDecimalCharCommaSeparator", width="100%", align="right")))
      ),
      
      # ------------------------------------------------
      # Impact Inputs
      # ------------------------------------------------
      h2("Impact/Magnitude"),
      
      h3("Primary Impact"),
      h5("- Impacts incurred with every successful event."),
      fluidRow(column(12, textAreaInput("text", h4("Primary Impact Description"), width="100%", value="Impact..."))),
      fluidRow(column(12, currencyInput("pi_min_num", "Current Primary Impact - Minimum ($)", 1000, format="dollar", width="100%", align="right"))),
      fluidRow(column(12, currencyInput("pi_max_num", "Current Primary Impact - Maximum ($)", 10000, format="dollar", width="100%", align="right"))),
      
      conditionalPanel(
        condition = "input.future_radio == 2",
        fluidRow(column(12, currencyInput("f_pi_min_num", "Future Primary Impact - Minimum ($)", 1000, format="dollar", width="100%", align="right"))),
        fluidRow(column(12, currencyInput("f_pi_max_num", "Future Primary Impact - Maximum ($)", 10000, format="dollar", width="100%", align="right")))
      ),
      
      # ------------------------------------------------
      # Secondary Impact Inputs
      # ------------------------------------------------
      h3("Secondary Impact"),
      h5("- Secondary impacts incurred with less than 100 percent likelihood."),
      
      radioButtons(
        "sl_radio",
        h4("- Secondary Loss Methods"),
        choices = list(
          "Overall Secondary Loss" = 1,
          "Secondary Loss by Category" = 2
        ),
        selected = 1
      ),
      
      fluidRow(column(12, textAreaInput("text", h4("Secondary Impact Description"), width="100%", value="Impact..."))),
      
      # ------------------------------------------------
      # Overall Secondary Loss
      # ------------------------------------------------
      conditionalPanel(
        condition = "input.sl_radio == 1",
        
        sliderInput("oa_sl_slider", h4("- Current Secondary Loss Likelihood (%)"), min = 1, max = 100, value = c(1, 5)),
        currencyInput("oa_si_min_num", "Current Secondary Loss Impact - Minimum ($)", 5000, format="dollar", width="100%", align="right"),
        currencyInput("oa_si_max_num", "Current Secondary Loss Impact - Maximum ($)", 50000, format="dollar", width="100%", align="right"),
        
        conditionalPanel(
          condition = "input.future_radio == 2",
          sliderInput("f_oa_sl_slider", h4("- Future Secondary Loss Likelihood (%)"), min = 1, max = 100, value = c(1, 5)),
          currencyInput("f_oa_si_min_num", "Future Secondary Loss Impact - Minimum ($)", 5000, format="dollar", width="100%", align="right"),
          currencyInput("f_oa_si_max_num", "Future Secondary Loss Impact - Maximum ($)", 50000, format="dollar", width="100%", align="right")
        )
      ),
      # ------------------------------------------------
      # Secondary Loss by Category
      # ------------------------------------------------
      conditionalPanel(
        condition = "input.sl_radio == 2",
        
        h4("- Category-based Secondary Loss"),
        h5("- Set likelihood and impact ranges per secondary loss category."),
        
        h4("Respond/Replace"),
        sliderInput("rr_sl_slider", h4("- Current Respond/Replace Likelihood (%)"), min = 1, max = 100, value = c(1, 10)),
        currencyInput("rr_si_min_num", "Current Respond/Replace Impact - Minimum ($)", 5000, format="dollar", width="100%", align="right"),
        currencyInput("rr_si_max_num", "Current Respond/Replace Impact - Maximum ($)", 50000, format="dollar", width="100%", align="right"),
        
        conditionalPanel(
          condition = "input.future_radio == 2",
          sliderInput("f_rr_sl_slider", h4("- Future Respond/Replace Likelihood (%)"), min = 1, max = 100, value = c(1, 10)),
          currencyInput("f_rr_si_min_num", "Future Respond/Replace Impact - Minimum ($)", 5000, format="dollar", width="100%", align="right"),
          currencyInput("f_rr_si_max_num", "Future Respond/Replace Impact - Maximum ($)", 50000, format="dollar", width="100%", align="right")
        ),
        
        h4("Lost Productivity"),
        sliderInput("lp_sl_slider", h4("- Current Lost Productivity Likelihood (%)"), min = 1, max = 100, value = c(1, 10)),
        currencyInput("lp_si_min_num", "Current Lost Productivity Impact - Minimum ($)", 5000, format="dollar", width="100%", align="right"),
        currencyInput("lp_si_max_num", "Current Lost Productivity Impact - Maximum ($)", 50000, format="dollar", width="100%", align="right"),
        
        conditionalPanel(
          condition = "input.future_radio == 2",
          sliderInput("f_lp_sl_slider", h4("- Future Lost Productivity Likelihood (%)"), min = 1, max = 100, value = c(1, 10)),
          currencyInput("f_lp_si_min_num", "Future Lost Productivity Impact - Minimum ($)", 5000, format="dollar", width="100%", align="right"),
          currencyInput("f_lp_si_max_num", "Future Lost Productivity Impact - Maximum ($)", 50000, format="dollar", width="100%", align="right")
        ),
        
        h4("Competitive Advantage"),
        sliderInput("ca_sl_slider", h4("- Current Competitive Advantage Likelihood (%)"), min = 1, max = 100, value = c(1, 10)),
        currencyInput("ca_si_min_num", "Current Competitive Advantage Impact - Minimum ($)", 5000, format="dollar", width="100%", align="right"),
        currencyInput("ca_si_max_num", "Current Competitive Advantage Impact - Maximum ($)", 50000, format="dollar", width="100%", align="right"),
        
        conditionalPanel(
          condition = "input.future_radio == 2",
          sliderInput("f_ca_sl_slider", h4("- Future Competitive Advantage Likelihood (%)"), min = 1, max = 100, value = c(1, 10)),
          currencyInput("f_ca_si_min_num", "Future Competitive Advantage Impact - Minimum ($)", 5000, format="dollar", width="100%", align="right"),
          currencyInput("f_ca_si_max_num", "Future Competitive Advantage Impact - Maximum ($)", 50000, format="dollar", width="100%", align="right")
        ),
        
        h4("Reputational Damage"),
        sliderInput("rd_sl_slider", h4("- Current Reputational Damage Likelihood (%)"), min = 1, max = 100, value = c(1, 10)),
        currencyInput("rd_si_min_num", "Current Reputational Damage Impact - Minimum ($)", 5000, format="dollar", width="100%", align="right"),
        currencyInput("rd_si_max_num", "Current Reputational Damage Impact - Maximum ($)", 50000, format="dollar", width="100%", align="right"),
        
        conditionalPanel(
          condition = "input.future_radio == 2",
          sliderInput("f_rd_sl_slider", h4("- Future Reputational Damage Likelihood (%)"), min = 1, max = 100, value = c(1, 10)),
          currencyInput("f_rd_si_min_num", "Future Reputational Damage Impact - Minimum ($)", 5000, format="dollar", width="100%", align="right"),
          currencyInput("f_rd_si_max_num", "Future Reputational Damage Impact - Maximum ($)", 50000, format="dollar", width="100%", align="right")
        ),
        
        h4("Legal/Regulatory"),
        sliderInput("lr_sl_slider", h4("- Current Legal/Regulatory Likelihood (%)"), min = 1, max = 100, value = c(1, 10)),
        currencyInput("lr_si_min_num", "Current Legal/Regulatory Impact - Minimum ($)", 5000, format="dollar", width="100%", align="right"),
        currencyInput("lr_si_max_num", "Current Legal/Regulatory Impact - Maximum ($)", 50000, format="dollar", width="100%", align="right"),
        
        conditionalPanel(
          condition = "input.future_radio == 2",
          sliderInput("f_lr_sl_slider", h4("- Future Legal/Regulatory Likelihood (%)"), min = 1, max = 100, value = c(1, 10)),
          currencyInput("f_lr_si_min_num", "Future Legal/Regulatory Impact - Minimum ($)", 5000, format="dollar", width="100%", align="right"),
          currencyInput("f_lr_si_max_num", "Future Legal/Regulatory Impact - Maximum ($)", 50000, format="dollar", width="100%", align="right")
        )
      )
    ),
    
    # =========================================================================
    # RIGHT PANEL – TABS
    # =========================================================================
    mainPanel(
      width = 8,
      
      tabsetPanel(
        selected = "Current Risk",
        
        # ---------------------------------------------------------------------
        # INHERENT RISK TAB – stacked histogram + LEC
        # ---------------------------------------------------------------------
        tabPanel(
          "Inherent Risk",
          conditionalPanel(
            condition = "input.lkh_radio == 1",
            
            h1("Inherent Risk"),
            h2("Inherent Likelihood Summary"),
            verbatimTextOutput("in_lkh_sum"),
            
            sliderInput("in_lkh_bins", h3("# of bins"), width="50%", min=1, max=50, value=25),
            plotOutput("in_lkh_hist"),
            
            h2("Inherent Impact Summary"),
            verbatimTextOutput("in_ipt_sum"),
            
            sliderInput("in_ipt_bins", h3("# of bins"), width="50%", min=1, max=50, value=25),
            plotOutput("in_ipt_hist"),
            
            h2("Inherent Risk Summary"),
            h4("Inherent Annual Loss Expectancy (1 year)"),
            verbatimTextOutput("in_ale_sum"),
            verbatimTextOutput("in_ale_10"),
            verbatimTextOutput("in_ale_mode"),
            verbatimTextOutput("in_ale_90"),
            verbatimTextOutput("in_ale_99"),
            
            h4("Inherent Annual Loss Expectancy (10 years)"),
            verbatimTextOutput("in_ale_ten_sum"),
            verbatimTextOutput("in_ale_ten_10"),
            verbatimTextOutput("in_ale_ten_90"),
            verbatimTextOutput("in_ale_ten_99"),
            
            sliderInput("in_ale_bins", h4("# of bins"), width="50%", min=1, max=50, value=25),
            
            # Stacked layout
            plotOutput("in_ale_hist"),
            plotOutput("in_ale_lec")
          )
        ),
        
        # ---------------------------------------------------------------------
        # CURRENT RISK TAB – now also stacked
        # ---------------------------------------------------------------------
        tabPanel(
          "Current Risk",
          
          h1("Current Residual Risk"),
          
          h2("Current Residual Likelihood Summary"),
          verbatimTextOutput("lkh_sum"),
          
          sliderInput("lkh_bins", h4("# of bins"), width="50%", min=1, max=50, value=25),
          plotOutput("lkh_hist"),
          
          h2("Current Residual Impact Summary"),
          verbatimTextOutput("ipt_sum"),
          
          sliderInput("ipt_bins", h4("# of bins"), width="50%", min=1, max=50, value=25),
          plotOutput("ipt_hist"),
          
          h2("Current Residual Risk Summary"),
          
          h4("Current Residual Annual Loss Expectancy (1 year)"),
          verbatimTextOutput("rale_sum"),
          verbatimTextOutput("rale_10"),
          verbatimTextOutput("rale_mode"),
          verbatimTextOutput("rale_90"),
          verbatimTextOutput("rale_99"),
          
          h4("Current Residual Annual Loss Expectancy (10 years)"),
          verbatimTextOutput("rale_ten_sum"),
          verbatimTextOutput("rale_ten_10"),
          verbatimTextOutput("rale_ten_90"),
          verbatimTextOutput("rale_ten_99"),
          
          sliderInput("rale_bins", h4("# of bins"), width="50%", min=1, max=50, value=25),
          
          # Stacked layout
          plotOutput("rale_hist_1"),
          plotOutput("rale_hist_2")
        ),
        
        # ---------------------------------------------------------------------
        # FUTURE RISK TAB – now also stacked
        # ---------------------------------------------------------------------
        tabPanel(
          "Future Risk",
          
          conditionalPanel(
            condition = "input.lkh_radio == 1 && input.future_radio == 2",
            
            h1("Future Residual Risk"),
            
            h2("Future Residual Likelihood Summary"),
            verbatimTextOutput("f_lkh_sum"),
            
            sliderInput("f_lkh_bins", h4("# of bins"), width="50%", min=1, max=50, value=25),
            plotOutput("f_lkh_hist"),
            
            h2("Future Residual Impact Summary"),
            verbatimTextOutput("f_ipt_sum"),
            
            sliderInput("f_ipt_bins", h4("# of bins"), width="50%", min=1, max=50, value=25),
            plotOutput("f_ipt_hist"),
            
            h2("Future Residual Risk Summary"),
            
            h4("Future Residual Annual Loss Expectancy (1 year)"),
            verbatimTextOutput("f_rale_sum"),
            verbatimTextOutput("f_rale_10"),
            verbatimTextOutput("f_rale_mode"),
            verbatimTextOutput("f_rale_90"),
            verbatimTextOutput("f_rale_99"),
            
            h4("Future Residual Annual Loss Expectancy (10 years)"),
            verbatimTextOutput("f_rale_ten_sum"),
            verbatimTextOutput("f_rale_ten_10"),
            verbatimTextOutput("f_rale_ten_90"),
            verbatimTextOutput("f_rale_ten_99"),
            
            sliderInput("f_rale_bins", h4("# of bins"), width="50%", min=1, max=50, value=25),
            
            # Stacked layout
            plotOutput("f_rale_hist_1"),
            plotOutput("f_rale_hist_2")
          )
        )
      )
    )
  )
)
