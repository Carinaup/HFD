######################################## Wrapper Methods ####################################
library(tidyverse)
library(caret)
library(glmnet)
library(MASS)
library('Rlab')

## function to generate data
generator <- function (nrow, ncol, distribution, parameter) {
  if (distribution == 'normal'){
    x = rnorm (nrow * ncol, mean = parameter[1], sd = parameter[2])
  }
  else if (distribution == 'bernoulli'){
    x = rbern (nrow * ncol, p = parameter)
  }
  X = matrix(x,nrow=nrow)
  return (X)
}

## Activation Function
activation <- function (X) {
  X[which(X<0)] = 0
  X[which(X>0)] = 1
}

## Generate Data
x = generator(1000,100,'normal',c(0,1))
a = generator(1,100,'bernoulli',0.1)
b = generator(1,100,'normal',c(0,1))
beta = a * b
y = x %*% t(beta)
y[which(y<0)] = 0
y[which(y>0)] = 1

data = data.frame(x,y)

## Wrapper Methods (AIC is smaller means the models is better)
model_lm = lm(y ~ ., data = data)
summary(model_lm)
stepAIC(model_lm, direction = "both")
stepAIC(model_lm, direction = "backward")
stepAIC(model_lm, direction = "forward")

model_glm = glm(y ~ ., data = data)
stepAIC(model_glm, direction = "both")
stepAIC(model_glm, direction = "backward")
stepAIC(model_glm, direction = "forward")

model = glm(y ~ . , family = "binomial", data = data)
stepAIC(model, direction = "both")
stepAIC(model, direction = "backward")
stepAIC(model, direction = "forward")






