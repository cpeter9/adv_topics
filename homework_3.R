# Author: Christopher Peters

# install.packages("mlbench")
library(mlbench)

data(BostonHousing)
df <- BostonHousing

pairs(medv ~ crim, data = df)

# 
for(i in c(1, 3, 5:(length(df) - 1))){
  if(abs(cor(df[ , length(df)], df[ , i])) > abs(cor(df[ , length(df)], log(df[ , i])))){
    df[ , i] <- df[ , i]} else {
      df[ , i] <- log(df[ , i])
      names(df)[i] <- paste("log", names(df)[i], sep = "_")
    }
}
  




