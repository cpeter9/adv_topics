# Author: Christopher Peters

# install.packages("MASS")
library(MASS)

# install.packages("mlbench")
library(mlbench)

# install.packages("caret")
library(caret)

# install.packages("elasticnet")
library(elasticnet)

# install.packages("leaps")
library(leaps)

# install.packages("penalized")
library(penalized)

data(BostonHousing)
df <- BostonHousing
df$chas <- as.numeric(df$chas)
df$chas <- ifelse(df$chas == 1, 0, 1) # recode 1 and 2 to 0 and 1, though not really necessary

pairs(medv ~ crim, data = df)

# 
for(i in c(1, 3, 5:(length(df) - 1))){
  if(abs(cor(df[ , length(df)], df[ , i])) > abs(cor(df[ , length(df)], log(df[ , i])))){
    df[ , i] <- df[ , i]} else {
      df[ , i] <- log(df[ , i])
      names(df)[i] <- paste("log", names(df)[i], sep = "_")
    }
}
  
# Randomly split data into training and testing sizes 400 and 106, respectively
training_size <- 400

training_index <- sample(1:length(df$medv), training_size)
testing_index <- which(!(1:length(df$medv) %in% training_index))

training <- df[training_index, ]
testing <- df[testing_index, ]

for(i in 1:20){
run <- i
linear_model <- lm(medv ~ ., data = training)

mspe <- (1/training_size)*sum((predict(linear_model, testing) - testing$medv)^2)

output <- as.data.frame(list(model = "linear_model", mspe = mspe))

# install.packages("reshape2")
library(reshape2)
output_temp <- as.data.frame(unlist(linear_model$coefficients))
output_temp$coef_names <- row.names(output_temp)

output <- cbind(output, dcast(output_temp, run ~ coef_names, value.var = output[ , 1]))

fitControl <- trainControl(## 5-fold CV
   method = "repeatedcv",
   number = 5,
  ## repeated once
   repeats = 1,
  ## Save all the resampling results
   returnResamp = "all")


lambdaGrid <- expand.grid(.lambda1 = 0,
                          .lambda2 = seq(0, 10, 0.5))

ridgefit <- train(training[ , -length(training)], training[, length(training)],
                 method = "penalized",
                 trControl = fitControl,
                  tuneGrid = lambdaGrid)

output_temp <- as.data.frame(unlist(coefficients(ridgefit$finalModel, "penalized")))
output_temp$coef_names <- row.names(output_temp)
names(output_temp)[1] <- "co_vars"


mspe <- (1/training_size)*sum((predict(ridgefit$finalModel, medv ~ ., data = testing) - testing$medv)^2)

output_temp <- as.data.frame(list(model = "ridge", mspe = mspe,
                                  dcast(output_temp, 
                                        run ~ coef_names, value.var = "co_vars")))

output_temp$intercept <- 0
names(output_temp)[grep("intercept", names(output_temp))] <- "(Intercept)"

output <- rbind(output, output_temp)

fractionGrid <- expand.grid(.fraction = seq(0, 1, 0.01))

lassofit <- train(training[ , -length(training)], training[, length(training)],
                  method = "lars",
                  trControl = fitControl,
                  type = "lasso",
                  tuneGrid = fractionGrid)

output_temp <- as.data.frame(unlist(lassofit$finalModel$beta[which.min(lassofit$finalModel$RSS), ]))
output_temp$coef_names <- row.names(output_temp)
names(output_temp)[1] <- "co_vars"

output_temp <- as.data.frame(list(model = "lasso", mspe = lassofit$finalModel$RSS[which.min(lassofit$finalModel$RSS)] / 400,
                   dcast(output_temp, 
                         run ~ coef_names, value.var = "co_vars")))

output_temp$intercept <- 0
names(output_temp)[grep("intercept", names(output_temp))] <- "(Intercept)"
                  
output_run <- rbind(output, output_temp)

if(run == 1){final_output <- output_run} else {final_output <- rbind(final_output, output_run)}
}
