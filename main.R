
set.seed(123)

source("functions/clt_functions.R")
source("functions/lln_functions.R")

dir.create("plots", showWarnings = FALSE)

sample_sizes <- c(5, 10, 30, 100)
sizes <- c(100, 500, 1000, 5000)

# =========================
# CLT SIMULATIONS
# =========================

dists <- c("exp", "unif", "bern")

for (dist in dists) {
  for (n in sample_sizes) {
    means <- simulate_clt(dist, n, 10000)
    png(paste0("plots/clt_", dist, "_n", n, ".png"))
    theoretical_mean <- switch(dist,
      "exp" = 1,
      "unif" = 0.5,
      "bern" = 0.5
    )
    plot_clt(means, theoretical_mean,
         paste("CLT -", dist, "(n =", n, ")"))    
    dev.off()
  }
}

# Q-Q Plot (best CLT check)
means <- simulate_clt("exp", 30, 10000)

png("plots/clt_exp_qq.png")
qqnorm(means)
qqline(means, col = "red")
dev.off()

# =========================
# LLN SIMULATIONS
# =========================



for (n in sizes) {
  values <- generate_lln("unif", n)
  png(paste0("plots/lln_uniform_n", n, ".png"))
  plot_lln(values, 0.5, paste("LLN - Uniform (n =", n, ")"))
  dev.off()
  
  values <- generate_lln("exp", n)
  png(paste0("plots/lln_exp_n", n, ".png"))
  plot_lln(values, 1, paste("LLN - Exponential (n =", n, ")"))
  dev.off()
  
  values <- generate_lln("bern", n)
  png(paste0("plots/lln_bern_n", n, ".png"))
  plot_lln(values, 0.5, paste("LLN - Bernoulli (n =", n, ")"))
  dev.off()
}


values <- generate_lln("unif", 1000)

png("plots/lln_uniform.png")
plot_lln(values, 0.5, "LLN - Uniform")
dev.off()

values <- generate_lln("exp", 1000)

png("plots/lln_exp.png")
plot_lln(values, 1, "LLN - Exponential")
dev.off()

values <- generate_lln("bern", 1000)

png("plots/lln_bern.png")
plot_lln(values, 0.5, "LLN - Bernoulli")
dev.off()

# =========================
# CLT COMPARISON PLOT
# =========================
png("plots/clt_comparison.png",
    width = 1200, height = 800)
par(mfrow = c(3, 4))
for (n in sample_sizes) {
  hist(simulate_clt("exp", n, 5000), main = paste("Exp n=", n))
  hist(simulate_clt("unif", n, 5000), main = paste("Unif n=", n))
  hist(simulate_clt("bern", n, 5000), main = paste("Bern n=", n))
}
par(mfrow = c(1, 1))
dev.off()

cat("✔ All simulations completed\n")