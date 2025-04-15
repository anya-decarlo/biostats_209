# Load the necessary libraries
install.packages("readstata13")
install.packages("psych")
install.packages("ggplot2")
install.packages("jtools")
install.packages("boot")
install.packages("epiDisplay")

library(readstata13)
library(psych)
library(ggplot2)
library(jtools)
library(boot)
library(epiDisplay)
data <- read.dta13("~/Downloads/BIOS 208 2024/labs/lab1/lab1.dta")

# Data summary
str(data)
summary(data)

# Summary statistics
summary(data$logvl0)
summary(data$logvlch)
describe(data$logvl0)
describe(data$logvlch)

tab1(data$tx)
tab1(data$codon10)
tab1(data$codon71)
tab1(data$codon90)

table(as.numeric(data$tx), data$tx)

# Histograms
ggplot(data, aes(logvl0)) + geom_histogram(bins  = 10) + labs(title = "Histogram of logvl0")
ggplot(data, aes(logvlch)) + geom_histogram(bins  = 10) + labs(title = "Histogram of logvlch")
ggplot(data, aes(logvlch)) + geom_histogram(bins  = 20) + labs(title = "Histogram of logvlch")

# Boxplots
ggplot(data, aes(y=logvlch)) + geom_boxplot() + labs(title = "Boxplot of logvlch")

# Q-Q Normal Plots
ggplot(data, aes(sample = logvl0)) + stat_qq() + stat_qq_line(col = "red") + labs(title = "Q-Q plot of logvl0") + xlab("Inverse Normal") + ylab("Observed Data")
ggplot(data, aes(sample = logvlch)) + stat_qq() + stat_qq_line(col = "red") + labs(title = "Q-Q plot of logvlch") + xlab("Inverse Normal") + ylab("Observed Data")


data$rannor <- rnorm(nrow(data))
ggplot(data, aes(sample = rannor)) + stat_qq() + stat_qq_line(col = "red") + labs(title = "Q-Q plot of logvlch")


x <- rnorm(25)
ggplot(data.frame(x), aes(sample = x)) + stat_qq() + stat_qq_line(col = "red") + labs(title = "Q-Q plot of logvlch")


# Normal Density Plots
ggplot(data, aes(logvl0)) + geom_density() + stat_function(fun = dnorm, args = list(mean = mean(data$logvl0, na.rm = TRUE), sd = sd(data$logvl0, na.rm = TRUE)), colour = "red") + labs(title = "Normal Density plot of logvl0")
ggplot(data, aes(logvlch)) + geom_density() + stat_function(fun = dnorm, args = list(mean = mean(data$logvlch, na.rm = TRUE), sd = sd(data$logvlch, na.rm = TRUE)), colour = "red") + labs(title = "Normal Density plot of logvlch")
ggplot(data, aes(logvl0)) + geom_density(bw = 0.1) + stat_function(fun = dnorm, args = list(mean = mean(data$logvl0, na.rm = TRUE), sd = sd(data$logvl0, na.rm = TRUE)), colour = "red") + labs(title = "Normal Density plot of logvl0")

ggplot(data, aes(logvlch, color =factor(codon90))) + geom_density(alpha=0.4)

# Simple Linear Regression
model <- lm(logvlch ~ factor(codon90), data = data)
jtools::summ(model, confint = TRUE, digits = 3)


residuals <- data.frame(resid(model))
ggplot(data = residuals, aes(x = resid.model.)) + geom_density() + 
  stat_function(fun = dnorm, args = list(mean = mean(residuals$resid.model., na.rm = TRUE), 
                                         sd = sd(residuals$resid.model., na.rm = TRUE)), colour = "red")

# Bootstrap CI for the association coefficient
set.seed(123)
boot.out <- boot(data, function(data, indices) {
  data <- data[indices, ] # allows bootstrapping
  fit <- lm(logvlch ~ factor(codon90), data = data)
  return(coef(fit))
}, R = 1000)
boot.ci(boot.out, index = 1) # intercept  
boot.ci(boot.out, index = 2) # codon90 coefficient

# Wilcoxon rank-sum test
wilcox.test(logvlch ~ factor(codon90), data = data)




