# Author: Christopher Peters
# Email: cpeter9@gmail.com
# Homework 2: More simulation

# install.packages("MASS")
library(MASS)

# install.packages("munsell")
library(munsell)

# install.packages("dichromat")
library(dichromat)

# install.packages("labeling")
library(labeling)

# install.packages("ggplot2")
library(ggplot2)

# install.packages("plyr")
library(plyr)

setwd("C:/R_stuff/adv_topics/homework_2/")

data <- read.csv("hw1_data1.csv", stringsAsFactors = FALSE)

# Code date as an object of class POSIX
data$Date <- as.Date(data$Date)

# Reverse sort of data
data <- data[order(data$Date), ]

# Diff and log close price
data$returns <- 0

for(i in 2:length(data$Date)){
  data$returns[i] <- log(data$Close[i] / data$Close[i - 1])
}

# Fit normal distribution to returns
fit <- fitdistr(data$returns, "normal")

mean <- fit$estimate[1]
sd <- fit$estimate[2]

# Find 0.01 quantile of distribution
quantile_1_pct <- qnorm(0.01, mean, sd)

# Simulate data from model and return mean and sd
sim_output <- function(){
  sim <- list(mean = mean(rnorm(length(data$returns), mean, sd)), 
              sd = sd(rnorm(length(data$returns), mean, sd)))
  return(sim)
} 

find_0.01_quantile <- function(input_list){
  qnorm(0.01, input_list$mean, input_list$sd)
}

find_0.01_quantile(list(mean = 5, sd = 2)) # outputs 0.347

B <- 10000
bootstrapped_0.01_quantile <- rep(0, B)
for(i in 1:B){
  bootstrapped_0.01_quantile[i] <- find_0.01_quantile(sim_output())
}

# 95 percent confidence interval
conf_interval <- quantile(bootstrapped_0.01_quantile, c(0.025, (1 - 0.025)))

# Compare to 0.01 quantile of data
quantile(data$returns, 0.01) # No, this does not fall within the confidence interval! Maybe we should have used a t-distribution?

library(ggplot2)
ggplot(data, aes(x = returns)) +
  geom_histogram(aes(y = ..density..), alpha = 0.2) +
  geom_density(aes(y = ..density..), colour = "blue") +
  stat_function(fun = dnorm,
                args = c(mean,
                         sd),
                colour = "red") +
  ggtitle("Histogram of returns plotted against theoretical normal distribution(red)\n Mean: -6.4e-5, SD: 0.0139 and\n non-parametric density estimate (blue)")


boostrap_q_0.01 <- function(B){
  q_est <- rep(0, B)

  for(i in 1:B){
    set.seed(i)
    input_sample <- sample(data$returns, 100, replace = TRUE)
    q_est[i] <- quantile(input_sample, 0.01)
  }

  output <- as.data.frame(list(q_est = q_est))
  return(output)
}

B <- 1000
estimates <- boostrap_q_0.01(B)

boot.estimates <- list(lower = quantile(estimates$q_est, 0.025),
                       point = mean(estimates$q_est),
                       upper = quantile(estimates$q_est, 0.975)) # lower: -0.06, mean: -0.035, upper: -0.021

conf_interval # Bootstrapped confidence interval is much much wider than that bootstrapped from the parametric distribution

# Fit AR(1) model to log returns
ar_fit <- arima(data$returns, order = c(1, 0, 0), include.mean = FALSE)

ar_fit # B_est = -0.0822, s.e. = 0.0198, s.e. of error = 0.0139

sqrt(ar_fit$var.coef)

# Find bootstrapped standard error estimate of Beta by bootstrapping residuals
residuals <- ar_fit$residuals

B <- 1000
boot.beta <- rep(0, B)
for(i in 1:B){
  set.seed(i)
  tmp <- sample(1:length(ar_fit$residuals), size = length(ar_fit$residuals), replace = T)
  tmp2 <- data.frame(error = data$returns + residuals[tmp])
  boot.beta[i] <- arima(tmp2$error, order = c(1, 0, 0), include.mean = FALSE)$coef
  
}

# Standard error by bootstrapping
sd(boot.beta) # standard error by bootstrap isd 0.016 vs. 0.0198 by single model fit.


# Problem 7
data <- read.table("Brambles.txt", stringsAsFactors = FALSE, header = TRUE)

ggplot(data, aes(x = X, y = Y)) + 
  geom_point()

groups <- matrix(rep(0, 25), nrow = 5)
for(i in 0:4){
  for(j in 0:4){
    groups[i + 1, j + 1] <- ifelse(!is.na(table(data$X > j & data$X <= (j + 1) & data$Y > i & data$Y <= (i + 1))[2]),
                                    table(data$X > j & data$X <= (j + 1) & data$Y > i & data$Y <= (i + 1))[2], 0)
  }
}

groups <- as.vector(groups)

mead <- function(x){
  t.bar <- mean(x)
  tmp1 <- 25 * (t.bar^2)
  tss <- sum(x^2) - tmp1
  bss <- (1/5)*(sum(x[1:5])^2 + sum(x[6:10])^2 + sum(x[11:15])^2 + sum(x[16:20])^2 + sum(x[21:25])^2) - tmp1
  q <- bss / tss
  return(q)
}

q.obs <- mead(groups)

iter <- 5000
q.rt <- rep(0, iter)
for(i in 1:iter){
  set.seed(i)
  cnt.rand <- sample(groups, size = 25, replace = FALSE)
  q.rt[i] <- mead(cnt.rand)
}

1 - length(which(q.rt >= q.obs)) / iter




