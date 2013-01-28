# Author: Christopher Peters
# Email: cpeter9@gmail.com
# Homework 1: simulation

# 2. Linear regression

library(MASS)

# a.
pairs(trees) # Girth and volume appear to be relatively linear, hieght and volume have some linearity.

# b.
tree.model <- lm(Volume ~ Height + Girth, data = trees)

plot(tree.model)

# c.
log.tree.model <- lm(log(Volume) ~ log(Height) + log(Girth), data = trees)

plot(log.tree.model)



