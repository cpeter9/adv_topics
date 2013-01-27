# Author: Christopher Peters
# Email: cpeter9@gmail.com
# Homework 1: simulation

# 1. simulation for two-sample t-test power calculation

# Fix alpha = 0.05, sample sizes: m = 50 and n = 50, 
# simulate samples from populations with two population means fixed at 140 and 150

# a. Normal populations with very different spreads

# Number of bootstraps
B <- 10000
ct <- rep(0, B)

for(i in 1:B){
  set.seed(i)
  
  x <- rnorm(50, mean = 150, sd = 15)
  y <- rnorm(50, mean = 140, sd = 2)
  
 ct[i] <- t.test(x, y, alternative = "two.sided", conf.level = 0.95)$p.value
}

# Count proportion rejected
true.power <- table(ct < 0.05)[2] / B
true.power # Power equals 0.995


# b. T populations, 10 degrees of freedom, and equal spreads
B <- 10000
ct <- rep(0, B)

for(i in 1:B){
  set.seed(i)
  
  x <- rt(50, df = 10) + 150
  y <- rt(50, df = 10) + 140
  
 ct[i] <- t.test(x, y, alternative = "two.sided", conf.level = 0.95)$p.value
}

# Count proportion rejected
true.power <- table(ct < 0.05)[2] / B
true.power # Power equals 100%

# b. Exponential populations
B <- 10000
ct <- rep(0, B)

for(i in 1:B){
  set.seed(i)
  
  x <- rexp(50, rate = 1/15) + 135
  y <- rexp(50, rate = 1/2) + 138
  
  ct[i] <- t.test(x, y, alternative = "two.sided", conf.level = 0.95)$p.value
}

# Count proportion rejected
true.power <- table(ct < 0.05)[2] / B
true.power # Power equals 100%

# b. One normal and one exponential
B <- 10000
ct <- rep(0, B)

for(i in 1:B){
  set.seed(i)
  
  x <- rexp(50, rate = 1/15) + 135
  y <- rnorm(50, mean = 140, sd = 2)
  
  ct[i] <- t.test(x, y, alternative = "two.sided", conf.level = 0.95)$p.value
}

# Count proportion rejected
true.power <- table(ct < 0.05)[2] / B
true.power # Power equals 100%








