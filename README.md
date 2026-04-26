# 📊 IDS3 Project — Law of Large Numbers & Central Limit Theorem

## 🎯 Project Description

This project simulates and visualizes two fundamental statistical theorems:

- Law of Large Numbers (LLN)
- Central Limit Theorem (CLT)

Using Monte Carlo simulations in R, we demonstrate convergence behavior and distributional properties.

---

## 📁 Project Structure
IDS3_Project/
│
├── main.R                      # Main simulation script
├── functions/
│   ├── clt_functions.R
│   └── lln_functions.R
│
├── ui/
│   └── app.R                   # Shiny interactive application
│
├── plots/                      # Generated figures
│
├── report/
│   ├── report.Rmd              # Final academic report
|   └── interpretation.txt      # Summary of results
│
└── README.md

---

## 🚀 How to Run the Project
## Requirements
- R (>= 4.0)
- Packages: shiny, rmarkdown, knitr, tinytex
### 1. Install packages
```r
install.packages(c("shiny", "rmarkdown", "knitr"))
```
### 2. Run simulations (generate plots)

```r
source("main.R")
```
### 3. Run interactive app (Shiny)
```r
shiny::runApp("c:/Users/chemi/code/R/IDS3_Project/ui")
```
### 4. Generate final report
```r
install.packages("rmarkdown")
install.packages("knitr")
install.packages("tinytex")
tinytex::install_tinytex()
rmarkdown::render("c:/Users/chemi/code/R/IDS3_Project/report/report.Rmd")
```
# 📊 Features
CLT Module
    Exponential, Uniform, Bernoulli distributions
    Sample size variation
    Histogram + Normal curve
    Q-Q plots
LLN Module
    Convergence of sample mean
    Comparison with theoretical expectation
Shiny App
Interactive selection of:
    Distribution
    Sample size
    Simulation number
    LLN vs CLT
🧠 Key Insight
LLN → stability of averages
CLT → emergence of normality
normality
👨‍💻 Authors
CLT module:
LLN module:
UI & integration:
## 📌 Notes
- `main.R` generates static plots for reporting
- `app.R` provides interactive exploration
- `report.Rmd` produces the final PDF report

## 💡 Recommendation
Use the Shiny app for intuition and the report for formal explanation.