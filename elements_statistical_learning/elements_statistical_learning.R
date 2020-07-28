# Elements Statistical Learning

# Chapter 3: Linear Regression

library(MASS)
library(ISLR)

# Simple Linear Regression
names(Boston)
?Boston
plot(medv~lstat,Boston)
fit1 = lm(medv~lstat,data=Boston)
fit1
summary(fit1)
abline(fit1,col="red")
names(fit1)
