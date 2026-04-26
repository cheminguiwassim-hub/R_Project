library(shiny)

source("../functions/clt_functions.R")
source("../functions/lln_functions.R")

ui <- fluidPage(
  titlePanel("LLN & CLT Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("seed", "Random seed:", value = 123, min = 1),
      selectInput("type", "Choose:",
                  choices = c("CLT", "LLN")),
      
      selectInput("dist", "Distribution:",
                  choices = c("exp", "unif", "bern")),
      helpText("Exponential = skewed, Uniform = flat, Bernoulli = discrete"),
      selectInput("mode", "Mode:",
            choices = c("Single", "Compare n")),
      conditionalPanel(
        condition = "input.type == 'CLT'",
        checkboxInput("qq", "Show Q-Q Plot (CLT only)", FALSE),
      ),
      sliderInput("n", "Sample size (n):", 5, 100, 30),
      sliderInput("sim", "Number of simulations:", 1000, 20000, 5000),
      verbatimTextOutput("explanation"),
      verbatimTextOutput("stats")
    ),
    
    mainPanel(
      h4("Visualization"),
      plotOutput("plot",height = "600px")
    )
  )
)

server <- function(input, output) {
  
  output$plot <- renderPlot({
    set.seed(input$seed)
    # =====================
    # CLT PART
    # =====================
    
    if (input$type == "CLT") {
      # Compare mode
      if (input$mode == "Compare n") {
        par(mfrow=c(2,2))
  
        for (n in c(5,10,30,100)) {
          means <- simulate_clt(input$dist, n, input$sim)
          hist(means, main=paste("n =", n), probability=TRUE,col="lightblue", border="white")
          curve(dnorm(x, mean=mean(means), sd=sd(means)), add=TRUE, col="red")
        }
        par(mfrow=c(1,1))
      } else {
    
        means <- simulate_clt(input$dist, input$n, input$sim)
        if (input$qq) {
          qqnorm(means,main=paste("Q-Q Plot -", input$dist, "n =", input$n))
          qqline(means, col="red")
        } else {
          theoretical_mean <- switch(input$dist,
            "exp" = 1,
            "unif" = 0.5,
            "bern" = 0.5
          )
          plot_clt(means, theoretical_mean, 
          paste("CLT:", input$dist, "| n =", input$n))        
        }
      }
    }
    # =====================
    # LLN PART
    # ===================== 
    else {
      if (input$mode == "Compare n") {
        par(mfrow=c(2,2))

        sizes <- c(100, 500, 1000, 5000)

        for (n in sizes) {
  
          values <- generate_lln(input$dist, n)
          cum_mean <- cumsum(values) / (1:n)
  
          plot(cum_mean, type="l",
            main=paste("n =", n))
          theoretical <- switch(input$dist,
            "exp" = 1,
            "unif" = 0.5,
            "bern" = 0.5
          )
          abline(h = theoretical, col = "red", lwd = 2)
        }
        par(mfrow=c(1,1))
      } else {


        values <- if (input$dist=="exp") rexp(input$sim)
                else if (input$dist=="unif") runif(input$sim)
                else rbinom(input$sim,1,0.5)
        theoretical <- switch(input$dist,
          "exp" = 1,
          "unif" = 0.5,
          "bern" = 0.5
        )
        plot_lln(values, theoretical,
               title=paste("LLN -", input$dist))
        }
      }
  })

  # =====================
  # Explanation panel
  # =====================
  output$explanation <- renderText({
  
  if (input$type == "CLT") {
    paste(
      "Central Limit Theorem (CLT):\n",
      "- The distribution of sample means approaches a normal distribution.\n",
      "- This holds even if the original distribution is skewed.\n",
      "- Increasing n improves the approximation.\n",
      "- Q-Q plot helps visually assess normality.",
      "CLT:\n- Mean ≈ theoretical mean\n- Variance ≈ σ²/n\n- Larger n → tighter distribution\n- Q-Q plot checks normality"
    )
  } else {
    paste(
      "Law of Large Numbers (LLN):\n",
      "- The sample mean converges to the true expected value.\n",
      "- Larger sample sizes reduce variability.\n",
      "- Confidence bands illustrate decreasing uncertainty.",
      "LLN:\n- Sample mean → expected value\n- Variability decreases with n\n- Confidence band shrinks over time"
    )
  }
})
  output$stats <- renderPrint({
  if (input$type == "CLT") {
    means <- simulate_clt(input$dist, input$n, input$sim)

    theoretical_mean <- switch(input$dist,
      "exp" = 1,
      "unif" = 0.5,
      "bern" = 0.5
    )

    theoretical_var <- switch(input$dist,
      "exp" = 1,
      "unif" = 1/12,
      "bern" = 0.25
    )

    cat("Empirical Mean:", mean(means), "\n")
    cat("Theoretical Mean:", theoretical_mean, "\n\n")
    cat("Empirical Variance:", var(means), "\n")
    cat("Theoretical Variance (CLT):", theoretical_var / input$n, "\n")
  } else {
    values <- generate_lln(input$dist, input$sim)
    theoretical_mean <- switch(input$dist,
      "exp" = 1,
      "unif" = 0.5,
      "bern" = 0.5
    )
    cat("Final Empirical Mean:",
    tail(cumsum(values)/(1:length(values)),1), "\n")
    cat("Theoretical Mean:", theoretical_mean, "\n")
  }
})
}

shinyApp(ui, server)