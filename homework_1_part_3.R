# Author: Christopher Peters
# Email: cpeter9@gmail.com
# Homework 1: simulation

# 3. Receiver operating characteristic curve

data <- as.data.frame(list(T = c(0.062, 0.177, 0.202, 0.206, 0.266, 
                                 0.372, 0.380, 0.384, 0.498, 0.573,
                                 0.629, 0.661, 0.687, 0.718, 0.770,
                                 0.777, 0.898, 0.908, 0.945, 0.992),
                           D = c(0, 0, 0, 0, 0,
                                 0, 0, 0, 1, 1,
                                 1, 1, 0, 1, 1,
                                 0, 1, 1, 0, 1)))


# True positive rate is 1 at c = 0.4
table(data$T[data$D == 1] > 0.4)

# True positive rate is 0.77 at c = 0.6
table(data$T[data$D == 1] > 0.6)

# True positive rate is 0.33 at c = 0.8
table(data$T[data$D == 1] > 0.8)

# False positive rate is 0.27 at c = 0.4
table(data$T[data$D == 0] > 0.4)

# False positive rate is 0.27 at c = 0.6
table(data$T[data$D == 0] > 0.6)

# False positive rate is 0.09 at c = 0.8
table(data$T[data$D == 0] > 0.8)


for(c in seq(0, 1, by = 0.01)){
  
  if(c == 0){
    tpr <- mean(data$T[data$D == 1] > c)
    fpr <- mean(data$T[data$D == 0] > c)
      output <- as.data.frame(list(c = c, tpr = tpr, fpr = fpr))} else
      {
        tpr <- mean(data$T[data$D == 1] > c)
        fpr <- mean(data$T[data$D == 0] > c)
        
        output <- rbind(output, as.data.frame(list(c = c, tpr = tpr, fpr = fpr)))}
  
}

# install.packages("reshape2")
library(reshape2)

# install.packages("ggplot2")
library(ggplot2)

# output <- melt(output, "c")

ggplot(output, aes(y = tpr, x = fpr)) +
  geom_point() +
  geom_path() +
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  ggtitle("Receiver Operating Characteristic") +
  ylab("True positive rate") +
  xlab("False positive rate")

# AUC isd equal to the probability that a true positive is scored greater than a true negative
auc <- mean(sample(data$T[data$D == 1], 10000, replace = TRUE) > sample(data$T[data$D == 0], 10000, replace = TRUE))

# AUC = 0.8155

# Bootstrap of AUC
hist(replicate(1000, mean(sample(data$T[data$D == 1], 10000, replace = TRUE) > sample(data$T[data$D == 0], 10000, replace = TRUE))))



