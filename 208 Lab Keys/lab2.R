# Load the necessary libraries
install.packages("readstata13")
install.packages("psych")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("multcomp")
install.packages("marginaleffects")

library("readstata13")
library("psych")
library("ggplot2")
library("dplyr")
library("multcomp")
library("marginaleffects")

data <- read.dta13("~/Downloads/BIOS 208 2025/labs/lab2/lab2.dta")

# Normality check
ggplot(data, aes(sample = glucose)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of Glucose", x = "Theoretical Quantiles", y = "Sample Quantiles")

ggplot(data, aes(x = glucose)) +
  geom_density(aes(y = ..density..), fill = "skyblue", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(data$glucose), sd = sd(data$glucose)), 
                color = "red", size = 1.5) +
  labs(title = "Density Plot of Glucose with Normal Distribution Overlay",
       x = "Glucose", y = "Density")

# Boxplot
ggplot(data, aes(x = exercise, y = glucose)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(title = "Boxplot of Glucose by Exercise",
       x = "Exercise", y = "Glucose")

# Summary statistics
summary_stats <- data %>%
  group_by(exercise) %>%
  summarise(
    count = n(),
    sum_bmi = sum(glucose, na.rm = TRUE),
    mean_glucose = mean(glucose, na.rm = TRUE),
    median_glucose = median(glucose, na.rm = TRUE),
    sd_glucose = sd(glucose, na.rm = TRUE),
    min_glucose = min(glucose, na.rm = TRUE),
    max_glucose = max(glucose, na.rm = TRUE),
    percentile_25 = quantile(glucose, 0.25, na.rm = TRUE),
    percentile_50 = quantile(glucose, 0.50, na.rm = TRUE),
    percentile_75 = quantile(glucose, 0.75, na.rm = TRUE)
  )

# View detailed summary statistics with percentiles
summary_stats

# Density plots stratified by exercise group
ggplot(data) +
  geom_density(aes(x = glucose, color = factor(exercise))) +
  labs(title = "Density Plot of Glucose by Exercise",
       x = "Glucose", y = "Density") 


# t test
t_test_result <- t.test(glucose ~ exercise, data = data, var.equal = TRUE)
# View the t-test results
print(t_test_result)

# Regression models with linear contrasts for exercise
model <- lm(glucose ~ exercise, data = data)

# Summary of the regression model
jtools::summ(model, confint = TRUE, digits = 3)

# Testing _cons + exercise
hypotheses(model, "`(Intercept)` + exerciseyes = 0")

# Testing parameter exercise
coef_length <- length(model$coef)
hypotheses(model, joint = c(1:coef_length)[grep("exercise", names(model$coefficients))], 
           hypotheses = rep(0, length(grep("exercise", names(model$coefficients)))))

# Regression models for bmi
model <- lm(glucose ~ bmi, data = data)
jtools::summ(model, confint = TRUE, digits = 3)

# Predicting fitted values and residuals
data$fitted <- fitted(model)
data$resid <- resid(model)

# Generating residuals squared
data$residsq <- data$resid^2

# Calculating residual sum of squares (RSS)
rss <- sum(data$residsq)
print(rss)

# Scatter plot with fitted regression line
ggplot(data, aes(x = bmi, y = glucose)) +
  geom_point() +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Linear regression line
  labs(title = "Scatter Plot of Glucose vs BMI with Linear Regression Line",
       x = "BMI", y = "Glucose")

# Scatter plot with lowess smoother
ggplot(data, aes(x = bmi, y = glucose)) +
  geom_point() +  # Scatter plot
  geom_smooth(method = "loess", se = FALSE, color = "blue") +  # LOWESS line
  labs(title = "Scatter Plot of Glucose vs BMI with LOWESS Line",
       x = "BMI", y = "Glucose")

# Scatter plot with lowess smoother (bw = 0.5)
ggplot(data, aes(x = bmi, y = glucose)) +
  geom_point() +  # Scatter plot
  geom_smooth(method = "loess", se = FALSE, color = "green", span = 0.5) +  # LOWESS line with bandwidth
  labs(title = "Scatter Plot of Glucose vs BMI with LOWESS Line (Bandwidth = 0.5)",
       x = "BMI", y = "Glucose")

# Scatter plot with both
ggplot(data, aes(x = bmi, y = glucose)) +
  geom_point() +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Linear regression line in red
  geom_smooth(method = "loess", se = FALSE, color = "green", span = 0.5) +  # LOWESS line in green with bandwidth
  labs(title = "Scatter Plot of Glucose vs BMI with Regression Lines",
       x = "BMI", y = "Glucose")

# Scale BMI
data$bmi5 <- data$bmi / 5  # Creating a new variable 'bmi5' by dividing 'bmi' by 5

# Regression analysis with 'glucose' against 'bmi5'
model <- lm(glucose ~ bmi5, data = data)

# Summary of the regression model
jtools::summ(model, confint = TRUE, digits = 3)

# Center BMI

mean_bmi <- mean(data$bmi, na.rm = TRUE)  # Calculating the mean of 'bmi'

# Creating a new variable 'cbmi' by subtracting the mean from 'bmi'
data$cbmi <- data$bmi - mean_bmi

# Regression analysis with 'glucose' against 'cbmi'
model <- lm(glucose ~ cbmi, data = data)

# Summary of the regression model
jtools::summ(model, confint = TRUE, digits = 3)

# Standardize BMI

# Function to center and standardize a variable
std <- function(x) {
    xc <- x - mean(x, na.rm = TRUE)
    xstd <- xc/sd(x, na.rm = TRUE)
}

# Standardize 'bmi'
data$std_bmi <- std(data$bmi)
mean(data$std_bmi)
sd(data$std_bmi)

# Regression analysis with 'glucose' against standardized 'bmi'
model <- lm(glucose ~ std_bmi, data = data)

# Summary of the regression model
jtools::summ(model, confint = TRUE, digits = 3)

# Using factors for categorical predictors

lm(glucose ~ as.factor(exercise), data = data)


##### Optional: Introduction to simulation in R

# Set number of observations
n <- 1000

# Generate data from a gamma distribution
r <- rgamma(n, shape = 1, scale = 0.1)

# Center the data to have approximately zero mean
r <- r - 0.1

# Plot kernel density estimation with normal reference
plot(density(r, bw = "SJ"), main = "Kernel Density Estimation", xlab = "r")
curve(dnorm(x, mean = mean(r), sd = sd(r)), col = "blue", lty = 2, add = TRUE, yaxt = "n")
legend("topright", legend = c("Density", "Normal"), col = c("black", "blue"), lty = c(1, 2))

# Function to simulate regression
regprg <- function(n) {
  # Generate data
  r <- rgamma(n, shape = 1, scale = 0.1) - 0.1
  x <- rbinom(n, 1, 0.5)
  y <- 1 + x * 2 + r
  
  # Fit linear model
  model <- lm(y ~ x)
  
  # Return coefficients
  return(coef(model)[2])
}

# Set up simulations for different sample sizes
set.seed(123)  # for reproducibility
sample_sizes <- c(10, 25, 50, 100)
sim_results <- lapply(sample_sizes, function(n) replicate(1000, regprg(n)))

# Plotting results
par(mfrow = c(4, 2))
for (i in seq_along(sample_sizes)) {
  hist(sim_results[[i]], main = paste("Sample size:", sample_sizes[i]), xlab = "Coefficient estimate (x)")
  qqnorm(sim_results[[i]], main = paste("Q-Q Plot - Sample size:", sample_sizes[i]))
  qqline(sim_results[[i]])
}


