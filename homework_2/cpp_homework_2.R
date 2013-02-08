# Author: Christopher Peters
# Email: cpeter9@gmail.com
# Homework 2: More simulation

# install.packages("MASS")
library(MASS)

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
sim_output <- list(sim_mean = mean(rnorm(length(data$returns), mean, sd)), 
                   sim_sd = sd(rnorm(length(data$returns), mean, sd)))


