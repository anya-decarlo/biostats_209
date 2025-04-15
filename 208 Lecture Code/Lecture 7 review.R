install.packages("jtools")
install.packages("readstata13")
install.packages("psych")
install.packages("epiDisplay")
install.packages("ggplot2")
install.packages("marginaleffects")
install.packages("boot")
install.packages("fastDummies")
install.packages("car")
install.packages("lspline")
install.packages("rms")
install.packages("describedata")
install.packages("estimatr")
install.packages("gridExtra")
install.packages("dplyr")
install.packages("glmnet")

library("jtools")
library("readstata13")
library("psych")
library("epiDisplay")
library("ggplot2")
library("marginaleffects")
library("boot")
library("fastDummies")
library("car")
library("lspline")
library("rms")
library("describedata")
library("estimatr")
library("gridExtra")
library("dplyr")
library("glmnet")

data <- read.dta13("/Users/aaron.scheffler/Downloads/BIOS 208 2025/labs/lab7/lab7.dta")
data <- na.omit(data)

# example model selection: stepwise regression vs lasso

set.seed(2182020)
# Split data into learning and vaidation subsets

data$group <- factor(sample(c(1:2), nrow(data), prob = c(.75, .25), replace = TRUE))
form <- as.formula(paste("bmd ~", paste(names(data)[-c(1, 23)], collapse = "+")))

x_train <- model.matrix(form, data = data[which(data$group == 1), ])
x_test <- model.matrix(form, data = data[which(data$group == 2), ])

y_train <- data$bmd[which(data$group == 1)]
y_test <- data$bmd[which(data$group == 2)]

### 1 ### lasso model selected by 10-fold cross-validation (group 1)
lasso_model <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 10)
plot(lasso_model)

# If you need to extract the coefficients for the non-zero predictors
lasso_coef <- coef(lasso_model, s = "lambda.min")

# Print the coefficients
print(lasso_coef)

pred_train_las <- predict(lasso_model,x_train, s = "lambda.min", data = x_train)
pred_test_las <- predict(lasso_model,x_test, s = "lambda.min", data = x_test)

# Correlation and R2
cor(pred_train_las, y_train)^2
cor(pred_test_las, y_test)^2

# MSE
mean((pred_train_las - y_train)^2)
mean((pred_test_las - y_test)^2)


### 2 ### linear model with all variables. 
reg_model <- lm(form, data = data[which(data$group == 1), ])
jtools::summ(reg_model, confint = TRUE, digits = 3)

pred_test_lr <- predict(reg_model, newdata = data[which(data$group == 2),])

data$age2 <- data$age^2
form2 <- update.formula(form,  ~ . + age2)
reg_model2 <- lm(form2, data = data[which(data$group == 1), ])
vif(reg_model) # greater than 5 may be of concern

### 2 ### backwards selection for linear model

backward_model <- step(reg_model, direction = "backward", trace = 0, k = 2)
jtools::summ(backward_model, confint = TRUE, digits = 3)
pred_test_sw <- predict(backward_model, data = newdata[which(data$group == 2),])

# compare R^2 from lasso, linear regression, and backward models

cor(y_test, pred_test_lr)
plot(y_test, pred_test_lr)
abline(0, 1, col = "red")

cor(y_test, pred_test_las)
plot(y_test, pred_test_las)
abline(0, 1, col = "red")

cor(y_test, pred_test_bw)
plot(y_test, pred_test_bw)
abline(0, 1, col = "red")