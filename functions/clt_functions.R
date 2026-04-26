simulate_clt <- function(dist = "exp", n = 30, sim = 10000) {
  
  means <- numeric(sim)
  
  for (i in 1:sim) {
    
    if (dist == "exp") {
      sample <- rexp(n, rate = 1)
    } else if (dist == "unif") {
      sample <- runif(n, 0, 1)
    } else if (dist == "bern") {
      sample <- rbinom(n, 1, 0.5)
    }
    
    means[i] <- mean(sample)
  }
  
  return(means)
}

plot_clt <- function(means, theoretical_mean, title="CLT Simulation") {
  
  hist(means,
       probability = TRUE,
       main = title,
       xlab = "Sample Means",
       col = "lightblue",
       border = "white")
  
  curve(dnorm(x, mean = mean(means), sd = sd(means)),
        col = "red", lwd = 2, add = TRUE)
  abline(v = mean(means), col = "blue", lwd = 2)  
  abline(v = theoretical_mean, col = "darkgreen", lwd = 2, lty = 2)
  mtext(paste("Var ≈", round(var(means),4)), side=3)
  legend("topright",
         legend = c("Simulation", "Normal approximation", "Mean"),
         col = c("lightblue", "red", "blue"),
         lwd = 2)
}