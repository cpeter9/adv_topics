# Author: Christopher Peters
# Email: cpeter9@gmail.com
# Homework 2: More simulation

# install.packages("MASS")
library(MASS)

# install.packages("ggplot2")
library(ggplot2)

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
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dt,
                args = c(df = sample.size,
                         ncp = mean(badge.means$badge.means)),
                colour = "red") +
  stat_function(fun = dnorm,
                args = c(mean = mean(badge.means$badge.means),
                         sd = sd(badge.means$badge.means)),
                colour = "blue") +
  ggtitle("Histogram of mean badges earned within two weeks of membership\nplotted against theoretical t-distribution (red) and normal distribution (blue)")








