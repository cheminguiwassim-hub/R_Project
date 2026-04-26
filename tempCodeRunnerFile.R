set.seed(123)
source("c:/Users/chemi/code/R/IDS3_Project/functions/clt_functions.R")
source("c:/Users/chemi/code/R/IDS3_Project/functions/lln_functions.R")

dir.create("c:/Users/chemi/code/R/IDS3_Project/plots", showWarnings = FALSE)

# ===== CLT (Exponential) =====
sample_sizes <- c(5, 10, 30, 100)

for (n in sample_sizes) {
  means <- simulate_clt("exp", n, 10000)
  
  png(paste0("c:/Users/chemi/code/R/IDS3_Project/plots/clt_exp_n", n, ".png"))
  plot_clt(means, paste("CLT - Exponential (n =", n, ")"))
  dev.off()
}
means <- simulate_clt("exp", 30, 10000)

png("c:/Users/chemi/code/R/IDS3_Project/plots/clt_exp_qq.png")
qqnorm(means)
qqline(means, col = "red")
dev.off()
# ===== CLT (Uniform) =====
for (n in sample_sizes) {
  means <- simulate_clt("unif", n, 10000)
  
  png(paste0("c:/Users/chemi/code/R/IDS3_Project/plots/clt_unif_n", n, ".png"))
  plot_clt(means, paste("CLT - Uniform (n =", n, ")"))
  dev.off()
}
# ===== CLT (Bernoulli) =====
for (n in sample_sizes) {
  means <- simulate_clt("bern", n, 10000)
  
  png(paste0("c:/Users/chemi/code/R/IDS3_Project/plots/clt_bern_n", n, ".png"))
  plot_clt(means, paste("CLT - Bernoulli (n =", n, ")"))
  dev.off()
}
# ===== LLN =====
# ===== LLN =====
# Uniform
values <- runif(1000)

png("c:/Users/chemi/code/R/IDS3_Project/plots/lln_uniform.png")
plot_lln(values, mean_theoretical = 0.5,
         title = "LLN - Uniform Distribution")
dev.off()

# Exponential
values <- rexp(1000)

png("c:/Users/chemi/code/R/IDS3_Project/plots/lln_exp.png")
plot_lln(values, mean_theoretical = 1,
         title = "LLN - Exponential Distribution")
dev.off()

# Bernoulli (ADD THIS)
values <- rbinom(1000,1,0.5)

png("c:/Users/chemi/code/R/IDS3_Project/plots/lln_bern.png")
plot_lln(values, mean_theoretical = 0.5,
         title = "LLN - Bernoulli Distribution")
dev.off()
# ===== CLT Comparison (Exponential) =====
png("c:/Users/chemi/code/R/IDS3_Project/plots/clt_exp_comparison.png")

par(mfrow=c(2,2))

for (n in c(5, 10, 30, 100)) {
  means <- simulate_clt("exp", n, 10000)
  hist(means, probability=TRUE,
       main=paste("n =", n))
  curve(dnorm(x, mean=mean(means), sd=sd(means)),
        add=TRUE, col="red")
}
dev.off()

# ===== Q-Q Plot =====
png("c:/Users/chemi/code/R/IDS3_Project/plots/clt_exp_qq.png")
qqnorm(means)
qqline(means, col="red")
dev.off()

png("c:/Users/chemi/code/R/IDS3_Project/plots/clt_full_comparison.png",
    width = 1200, height = 800)

par(mfrow=c(3,4))

for (n in c(5,10,30,100)) {
  
  hist(simulate_clt("exp", n, 5000), probability=TRUE,
       main=paste("Exp n=",n))
  
  hist(simulate_clt("unif", n, 5000), probability=TRUE,
       main=paste("Unif n=",n))
  
  hist(simulate_clt("bern", n, 5000), probability=TRUE,
       main=paste("Bern n=",n))
}

dev.off()
print("All simulations completed successfully.")