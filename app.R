# Step 1: Simulation Engine
library(dplyr)

set.seed(123)

# 1. Simulate 1,000 "Years" of claims
n_simulations <- 1000
avg_claims_per_year <- 50

# 2. Generate frequency (how many claims) and severity (how big is each claim)
sim_data <- data.frame(
  sim_id = rep(1:n_simulations, each = avg_claims_per_year),
  claim_amount = rgamma(n_simulations * avg_claims_per_year, shape = 2, scale = 5000)
)

# 3. Define the Reinsurance Function
apply_reinsurance <- function(claims, retention, limit) {
  # Amount the company keeps
  net_claims <- pmin(claims, retention)
  
  # Amount recovered from reinsurer
  recoveries <- pmin(pmax(claims - retention, 0), limit)
  
  return(list(net = net_claims, recovered = recoveries))
}

# 4. Test the logic with a $10k retention and $50k limit
test_results <- apply_reinsurance(sim_data$claim_amount, 10000, 50000)
sim_data$net_claim <- test_results$net
sim_data$recovery <- test_results$recovered

# View the impact
summary(sim_data$claim_amount) # Gross
summary(sim_data$net_claim)    # Net after Reinsurance


library(shiny)
library(bslib)
library(ggplot2)

# Define UI
ui <- page_sidebar(
  title = "Reinsurance Strategy Optimizer",
  
  # Sidebar for user inputs
  sidebar = sidebar(
    title = "Contract Parameters",
    sliderInput("retention", "Company Retention ($):", 
                min = 1000, max = 20000, value = 5000, step = 500),
    sliderInput("limit", "Reinsurance Limit ($):", 
                min = 10000, max = 100000, value = 50000, step = 5000),
    hr(),
    helpText("Adjust the sliders to see how risk transfer impacts the net loss distribution.")
  ),
  
  # Main layout with cards for plots and stats
  layout_column_wrap(
    width = 1,
    card(
      card_header("Gross vs. Net Loss Distribution"),
      plotOutput("lossPlot")
    ),
    layout_column_wrap(
      width = 1/2,
      value_box(
        title = "Expected Recovery",
        value = textOutput("recoveryVal"),
        showcase = bsicons::bs_icon("piggy-bank")
      ),
      value_box(
        title = "Value at Risk (95%)",
        value = textOutput("varVal"),
        showcase = bsicons::bs_icon("graph-up-arrow")
      )
    )
  )
)

# Empty server for now
server <- function(input, output) {}

shinyApp(ui, server)


server <- function(input, output) {
  
  # 1. Reactive simulation logic
  processed_data <- reactive({
    # Creating a base distribution
    set.seed(123)
    base_claims <- rgamma(5000, shape = 2, scale = 5000)
    
    # Apply Excess of Loss logic based on SLIDERS
    net_claims <- pmin(base_claims, input$retention)
    recoveries <- pmin(pmax(base_claims - input$retention, 0), input$limit)
    
    data.frame(Gross = base_claims, Net = net_claims, Recovered = recoveries)
  })
  
  # 2. Render the Plot
  output$lossPlot <- renderPlot({
    df <- processed_data()
    ggplot(df) +
      geom_density(aes(x = Gross, fill = "Gross Loss"), alpha = 0.3) +
      geom_density(aes(x = Net, fill = "Net Loss"), alpha = 0.6) +
      scale_fill_manual(values = c("Gross Loss" = "grey", "Net Loss" = "#2c3e50")) +
      theme_minimal() +
      labs(x = "Claim Amount ($)", y = "Density", fill = "Type")
  })
  
  # 3. Calculate Value Boxes
  output$recoveryVal <- renderText({
    paste0("$", round(sum(processed_data()$Recovered) / 1000, 1), "K")
  })
  
  output$varVal <- renderText({
    # 95th Percentile of Net Losses (Value at Risk)
    paste0("$", round(quantile(processed_data()$Net, 0.95), 0))
  })
}


library(shiny)
library(bslib)
library(bsicons)
library(ggplot2)
library(dplyr)

# --- UI SECTION ---
ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "Reinsurance Strategy Optimizer",
  
  sidebar = sidebar(
    title = "Contract Parameters",
    # User inputs for simulation
    sliderInput("retention", "Company Retention ($):", 
                min = 1000, max = 20000, value = 5000, step = 500),
    sliderInput("limit", "Reinsurance Limit ($):", 
                min = 10000, max = 100000, value = 50000, step = 5000),
    
    hr(),
    # Export capability for audit and reporting
    helpText("Export simulation results for internal review:"),
    downloadButton("downloadData", "Download Results (CSV)", class = "btn-primary")
  ),
  
  # --- DASHBOARD LAYOUT ---
  layout_column_wrap(
    width = 1,
    card(
      card_header("Gross vs. Net Loss Distribution"),
      plotOutput("lossPlot"),
      card_footer("Visualizing risk transfer impact on the tail of the distribution.")
    ),
    layout_column_wrap(
      width = 1/2,
      value_box(
        title = "Total Expected Recovery",
        value = textOutput("recoveryVal"),
        showcase = bs_icon("piggy-bank"),
        theme = "success"
      ),
      value_box(
        title = "Net Value at Risk (95%)",
        value = textOutput("varVal"),
        showcase = bs_icon("graph-up-arrow"),
        theme = "danger"
      )
    )
  )
)

# --- SERVER SECTION ---
server <- function(input, output) {
  
  # 1. Simulation Engine: Reactive data based on inputs
  processed_data <- reactive({
    set.seed(123) # For reproducibility
    # Simulating 5000 claims using a Gamma distribution (common in severity modeling)
    base_claims <- rgamma(5000, shape = 2, scale = 5000)
    
    # Applying Excess of Loss (XoL) Logic
    net_claims <- pmin(base_claims, input$retention)
    recoveries <- pmin(pmax(base_claims - input$retention, 0), input$limit)
    
    data.frame(
      Claim_ID = 1:5000,
      Gross_Loss = base_claims, 
      Net_Loss = net_claims, 
      Reinsurance_Recovery = recoveries
    )
  })
  
  # 2. Render the Probability Density Plot
  output$lossPlot <- renderPlot({
    df <- processed_data()
    ggplot(df) +
      geom_density(aes(x = Gross_Loss, fill = "Gross Loss (Before RI)"), alpha = 0.3) +
      geom_density(aes(x = Net_Loss, fill = "Net Loss (After RI)"), alpha = 0.6) +
      scale_fill_manual(values = c("Gross Loss (Before RI)" = "#95a5a6", 
                                   "Net Loss (After RI)" = "#2c3e50")) +
      theme_minimal() +
      labs(x = "Claim Amount ($)", y = "Density", fill = "Risk Profile") +
      theme(legend.position = "bottom")
  })
  
  # 3. Calculate Summary Metrics for Value Boxes
  output$recoveryVal <- renderText({
    total_rec <- sum(processed_data()$Reinsurance_Recovery)
    paste0("$", formatC(total_rec, format = "f", big.mark = ",", digits = 0))
  })
  
  output$varVal <- renderText({
    # 95th Percentile calculation for Net Losses (Value at Risk)
    var_95 <- quantile(processed_data()$Net_Loss, 0.95)
    paste0("$", formatC(var_95, format = "f", big.mark = ",", digits = 0))
  })
  
  # 4. Download Handler for CSV Export
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("reinsurance-sim-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(processed_data(), file, row.names = FALSE)
    }
  )
}

# Run the App
shinyApp(ui, server)

