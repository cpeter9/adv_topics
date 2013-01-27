# Author: Christopher Peters
# Email: cpeter9@gmail.com
# Homework 1: simulation

# 1. simulation for two-sample t-test power calculation

# Fix alpha = 0.05, sample sizes: m = 50 and n = 50, 
# simulate samples from populations with two population means fixed at 140 and 150

# Sample sizes
m <- 50
n <- 50

# Population means
m_mean <- 150 
n_mean <- 140

# Population standard deviations
m_sd <- 15  
n_sd <- 2

# Number of bootstraps
B <- 10000
rejects <- rep(0, B)

for(i in 1:B){
  set.seed(i)
  
  x <- rnorm(m, mean = m_mean, sd = m_sd)
  y <- rnorm(n, mean = n_mean, n_sd)
  
  rejects[i] <- t.test(x, y, alternative = "two.sided", conf.level = 0.95)$p.value
}

# Count proportion rejected
power <- table(rejects < 0.05)[2] / B
print(power) # Power equals 0.995







