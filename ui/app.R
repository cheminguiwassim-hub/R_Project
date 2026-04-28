library(shiny)

source("../functions/clt_functions.R")
source("../functions/lln_functions.R")

ui <- fluidPage(
  titlePanel("📊 Exploring Limit Theorems — LLN & CLT"),
  
  sidebarLayout(
    sidebarPanel(
      h4("🎛️ Controls"),
      br(),
      
      selectInput(
        inputId  = "theorem",
        label    = "Select Theorem",
        choices  = c("Central Limit Theorem (CLT)" = "CLT",
                     "Law of Large Numbers (LLN)"  = "LLN")
      ),
      
      br(),
      
      selectInput(
        inputId  = "dist",
        label    = "Distribution type",
        choices  = c("Exponential" = "exp",
                     "Uniform"     = "unif",
                     "Bernoulli"   = "bern")
      ),
      
      br(),
      
      sliderInput(
        inputId = "n",
        label   = "Sample size (n)",
        min     = 10, max = 1000, value = 100, step = 10
      ),
      
      br(),
      
      sliderInput(
        inputId = "sims",
        label   = "Number of simulations",
        min     = 100, max = 5000, value = 1000, step = 100
      ),
      
      br(),
      actionButton("run", "▶ Run Simulation", class = "btn-primary btn-block")
    ),
    
    mainPanel(
      h4("📈 Visualization"),
      br(),
      
      tabsetPanel(
        
        tabPanel("CLT",
                 br(),
                 h4("Central Limit Theorem — Distribution of Sample Means"),
                 plotOutput("clt_plot", height = "400px"),
                 br(),
                 verbatimTextOutput("clt_summary")
        ),
        
        tabPanel("LLN",
                 br(),
                 h4("Law of Large Numbers — Convergence of Sample Mean"),
                 plotOutput("lln_plot", height = "400px"),
                 br(),
                 verbatimTextOutput("lln_summary")
        )
        
      )
    )
  )
)
server <- function(input, output, session) {
  
  
  get_data <- reactive({
    input$run
    isolate({
      if (input$dist == "exp")  return(rexp)
      if (input$dist == "unif") return(runif)
      if (input$dist == "bern") return(function(n) rbinom(n, 1, 0.5))
    })
  })
  
  
  output$clt_plot <- renderPlot({
    input$run
    isolate({
      gen  <- get_data()
      n    <- input$n
      sims <- input$sims
      
      means <- replicate(sims, mean(gen(n)))
      
      hist(means,
           breaks  = 40,
           freq    = FALSE,
           col     = "#4C72B0",
           border  = "white",
           main    = paste("CLT — Sample means (n =", n, ", sims =", sims, ")"),
           xlab    = "Sample mean",
           ylab    = "Density")
      
      curve(dnorm(x, mean(means), sd(means)),
            add = TRUE, col = "red", lwd = 2)
      
      legend("topright",
             legend = "Normal curve",
             col    = "red", lwd = 2)
    })
  })
  
  
  output$lln_plot <- renderPlot({
    input$run
    isolate({
      gen <- get_data()
      n   <- input$n
      
      cumulative_means <- cumsum(gen(n)) / seq_len(n)
      theoretical_mean <- if (input$dist == "exp")  1
      else if (input$dist == "unif") 0.5
      else 0.5
      
      plot(cumulative_means,
           type = "l",
           col  = "#4C72B0",
           lwd  = 2,
           main = paste("LLN — Convergence of sample mean (n =", n, ")"),
           xlab = "Number of observations",
           ylab = "Cumulative mean")
      
      abline(h   = theoretical_mean,
             col = "red",
             lwd = 2,
             lty = 2)
      
      legend("topright",
             legend = c("Sample mean", "Theoretical mean"),
             col    = c("#4C72B0", "red"),
             lwd    = 2,
             lty    = c(1, 2))
    })
  })
  
 
  output$clt_summary <- renderPrint({
    input$run
    isolate({
      gen   <- get_data()
      means <- replicate(input$sims, mean(gen(input$n)))
      cat("=== CLT Statistics ===\n")
      cat("Mean of sample means :", round(mean(means), 4), "\n")
      cat("Std dev of sample means:", round(sd(means), 4), "\n")
      cat("Expected std dev (σ/√n):", round(sd(gen(10000)) / sqrt(input$n), 4), "\n")
    })
  })
  
  output$lln_summary <- renderPrint({
    input$run
    isolate({
      gen <- get_data()
      theoretical_mean <- if (input$dist == "exp")  1
      else if (input$dist == "unif") 0.5
      else 0.5
      sample_mean <- mean(gen(input$n))
      cat("=== LLN Statistics ===\n")
      cat("Theoretical mean :", theoretical_mean, "\n")
      cat("Sample mean      :", round(sample_mean, 4), "\n")
      cat("Difference       :", round(abs(sample_mean - theoretical_mean), 4), "\n")
    })
  })
}

shinyApp(ui = ui, server = server)