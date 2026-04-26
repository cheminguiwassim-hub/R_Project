plot_lln <- function(values, mean_theoretical, title="LLN Simulation") {
  
  n <- length(values)
  cum_mean <- cumsum(values) / (1:n)
  # standard error band
  se <- sd(values) / sqrt(1:n)
  plot(cum_mean, type = "l", lwd = 2, col = "blue", main = title,
       xlab = "n", ylab = "Cumulative Mean")
  
  abline(h = mean_theoretical, col = "red", lwd = 2)
  # confidence band
  lines(mean_theoretical + 2*se, col="gray", lty=2)
  lines(mean_theoretical - 2*se, col="gray", lty=2)
  mtext(paste("Final mean:", round(tail(cum_mean,1),4)), side=3)
  legend("bottomright",
         legend = c("Empirical mean", "Theoretical mean", "Confidence band"),
         col = c("blue", "red", "gray"),
         lwd = c(2, 2, 1), lty = c(1, 1, 2))

}
generate_lln <- function(dist, n) {
  
  switch(dist,
    "exp"  = rexp(n),
    "unif" = runif(n),
    "bern" = rbinom(n, 1, 0.5)
  )
}