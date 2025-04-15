# Load the necessary libraries
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

library(jtools)
library(readstata13)
library(psych)
library(epiDisplay)
library(ggplot2)
library(marginaleffects)
library(boot)
library(fastDummies)
library(car)
library(lspline)
library(rms)
library(describedata)
library(estimatr)
library(gridExtra)
library(dplyr)
library(glmnet)

data <- read.dta13("~/biostats_208/data/lab7.dta")

# Identify outcome, primary exposure, and A, B list confounders
outcome <- "bmd"
primary <- "eeu"
alist <- c("age", "lweight", "estrogen", "calsupp", "diuretic", "etid", "momhip")
blist <- c("poorhlth", "usearms", "tandstnd", "has10", "gs10", "gaitspd", "caffeine", "drnkspwk", "smoker")


# cic model selection
final_model <- cic(data, "bmd", "eeu", alist, blist, criterion = 5, complete = FALSE)
jtools::summ(final_model, confint = TRUE, digits = 3)


hypotheses(final_model, "eeu * 1.5 = 0")

# Given that average BMD is about 0.73 g/cm2, would that effect on BMD seem clinically significant? 
hypotheses(final_model, "100*(eeu * 1.5)/0.73 = 0")

# Given that the average BMD is about 0.73 g/cm2, would the effect on BMD seem clinically significant 
# at the 95% confidence level comparing the null as the average effect of BMD in in the group with no 
# exposure 

# Given that the average BMD about 0.73 g/cm2, would that effect on BMD seem clinically significant? 
# Using proportion, NOT percent 
hypotheses(final_model, "(eeu *1.5/0.73) = 0")

# How does the effect of 150 Kcal/day compare in magnitude to the effect of a 10% increase in weight or current use of estrogen? 
hypotheses(final_model, "lweight * log(1.1) = 0")

# Why test against 0 and not against average change in bone density
hypotheses(final_model, "estrogencurrent = 0")

# criterion 10
final_model <- cic(data, "bmd", "eeu", alist, blist, criterion = 10, complete = FALSE)

# complete data for all variables
final_model <- cic(data, "bmd", "eeu", alist, blist, criterion = 5, complete = TRUE)


# Stepwise regression
# complete case data only when comparing AIC values
data <- data[complete.cases(data),] 
# Define formulas
form_all <- as.formula(paste(outcome, "~", primary, "+", paste(alist, collapse = "+"), "+", paste(blist, collapse = "+")))
form_a <- as.formula(paste(outcome, "~", primary, "+", paste(alist, collapse = "+")))

# Define base model
all_model <- lm(form_all, data = data)
jtools::summ(all_model, confint = TRUE, digits = 3)

# Backwards selection based on AIC with k = 2
step_model <- step(all_model, scope = list(lower = form_a, upper = form_all), k = 2, direction = "backward") 

# Backwards selection based on AIC with k = 0.5
step_model <- step(all_model, scope = list(lower = form_a, upper = form_all), k = 0.5, direction = "backward") 

# Backwards selection based on AIC with k = 10
step_model <- step(all_model, scope = list(lower = form_a, upper = form_all), k = 10, direction = "backward") 

# Lasso
# Set seed for reproducibility
set.seed(2202020)

# Assuming `data` is your data frame and `bmd` is the outcome variable
# Split the data into training and validation sets
data <- data[complete.cases(data),]
split <- sample.int(n = nrow(data), size = floor(0.75 * nrow(data)), replace = FALSE)
train <- data[split, ]
test <- data[-split, ]

# Create a group indicator
data$group <- ifelse(1:nrow(data) %in% split, 1, 2)

# Display group and overall mean outcome values
aggregate(bmd ~ group, data, mean)

# Prepare the matrix of predictors for the glmnet model
# Note: glmnet requires a matrix as input for x
x_train <- model.matrix(form_all, data = train)

x_test <- model.matrix(form_all, data = test)

# Prepare the outcome variable
y_train <- train$bmd
y_test <- test$bmd

# Run LASSO regression with cross validation 
lasso_model <- cv.glmnet(x_train, y_train, alpha = 1)
plot(lasso_model)

# If you need to extract the coefficients for the non-zero predictors
lasso_coef <- coef(lasso_model, s = "lambda.min")

# Print the coefficients
print(lasso_coef)


pred_train <- predict(lasso_model, x_train, s = "lambda.min")
pred_test <- predict(lasso_model, x_test, s = "lambda.min")

# MSE
mean((pred_train - y_train)^2)
mean((pred_test - y_test)^2)


