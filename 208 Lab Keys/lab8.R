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
install.packages("rstatix")
install.packages("epiR")

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
library("rstatix")
library("epiR")

data <- read.dta13("~/Downloads/BIOS 208 2024/labs/lab8/lab8.dta")
setwd("~/Downloads/BIOS 208 2024/labs/lab8")

# Basic descriptives
str(data)
summary(data)

# Assoc. between CHD and arcsu
# Using epiR
tab <- table(data$arcus, data$chd69) # risk factor first, outcome second
epi.2by2(tab[c(2:1),c(2:1)]) # need to modify table ordering slightly

# using logistic
model <- glm(chd69 ~ arcus, data = data, family = binomial)
jtools::summ(model, confint = TRUE, digits = 3, exp = TRUE)

# descriptive summaries of age by outcome status
ggplot(data, aes(x = factor(chd69), y = age)) +
  geom_boxplot() +
  labs(x = "chd69", y = "Age") +
  ggtitle("Box plot of Age by chd69") +
  theme_minimal()

ggplot(data, aes(x = age, color = factor(chd69))) +
  geom_density() +
  ggtitle("Density lot of Age by chd69") +
  theme_minimal()

# categorical treatment of age
data$agec <- factor(cut(data$age, 
                 breaks = c(36,40, 45, 50, 55, 60), 
                 labels = c("36_40", "41_45", "46_50", "51_55", "56_60"), 
                 right = TRUE))



# CHD by categorical age - frequency tables
tab <- table(data$chd69, data$agec)

# Calculate column percentages
col_percentages <- prop.table(tab, margin = 2) * 100
print(round(addmargins(col_percentages), 2)) # proportions
round((col_percentages[1,1]/col_percentages[2,1])/(col_percentages[1,]/col_percentages[2,]), 2) # odds ratios

# Test of homogeneity and test for trend
prop_test(tab)
prop_trend_test(tab)

# CHD by categorical age - logistic model
model <- glm(chd69 ~ agec, data = data, family = binomial)
jtools::summ(model, confint = TRUE, digits = 3, exp = TRUE)
hypotheses(model, joint = "agec", joint_test = "chisq")

# Test of linearity 
data$agec2 <- data$agec
contrasts(data$agec2) <- contr.poly(nlevels(data$agec)) 
model <- glm(chd69 ~ agec2, data = data, family = binomial)
jtools::summ(model, confint = TRUE, digits = 3)

# Contrast of the age group 4 and 3 
model <- glm(chd69 ~ agec, data = data, family = binomial)
hypotheses(model, "agec51_55 - agec46_50 = 0") # (log odds scale)
hypotheses(model, "exp(agec51_55 - agec46_50) = 0") # odds scale, exponentiate!


# relevel to change reference level
data$agec2 = relevel(data$agec, ref = "46_50")
model <- glm(chd69 ~ agec2, data = data, family = binomial)
jtools::summ(model, confint = TRUE, digits = 3, exp = TRUE)

# lowess fit & linear logistic model for age as continuous

# lowess smoothing
logitloess <- function(x, y, s) {
  
  logit <- function(pr) {
    log(pr/(1-pr))
  }
  
  if (missing(s)) {
    locspan <- 0.7
  } else {
    locspan <- s
  }
  
  loessfit <- predict(loess(y~x,span=locspan))
  pi <- pmax(pmin(loessfit,0.9999),0.0001)
  logitfitted <- logit(pi)
  
  plot(sort(x), logitfitted[order(x)], ylab="logit", type = "l")
  
}

logitloess(data$age, data$chd69, s = 0.5)

# fit model with age as linear predictor
model <- glm(chd69 ~ age, data = data, family = binomial)
jtools::summ(model, confint = TRUE, digits = 3, exp = TRUE)
jtools::summ(model, confint = TRUE, digits = 3)

data$lp <- predict(model)

# generate spline basis and fit model
model_cs <- glm(chd69 ~ rcs(age), data = data, family = binomial)
jtools::summ(model_cs, confint = TRUE, digits = 3, exp = TRUE)
data$lpcs <- predict(model_cs)
coef_length <- length(model_cs$coef)
hypotheses(model_cs, joint = c(1:coef_length)[grep("rcs", names(model_cs$coefficients))][-1], 
           hypotheses = rep(0, coef_length)[-1], joint_test = "chisq")


logitloess(data$age, data$chd69, s = 0.5) # loess
lines(data$age, data$lp, col = "red") # linear line
lines(sort(data$age), data$lpcs[order(data$age)], col = "blue", lwd = 2) # spline

model_factor <- glm(chd69 ~ as.factor(age), data = data, family = binomial)
plot_dat <- predict(model_factor, newdata = data, se.fit = TRUE)
data$lp_fac <- exp(plot_dat$fit)
data$lp_fac_lci <- exp(plot_dat$fit - plot_dat$se.fit * 1.96)
data$lp_fac_uci <- exp(plot_dat$fit + plot_dat$se.fit * 1.96)

ggplot(data = data, aes(x = factor(age), y = lp_fac)) + geom_point() + geom_errorbar(aes(ymin = lp_fac_lci, ymax = lp_fac_uci)) # predicted odds

# generate log-odds estimates and pointwise 95% CI for unique ages based on the previously fitted spline model
plot_dat_cs <- predict(model_cs, newdata = data, se.fit = TRUE)
data$lp_cs <- plot_dat_cs$fit
data$lp_cs_lci <- plot_dat_cs$fit - plot_dat_cs$se.fit * 1.96
data$lp_cs_uci <- plot_dat_cs$fit + plot_dat_cs$se.fit * 1.96

ggplot(data = data, aes(x = age, y = lp_cs)) + geom_line() + ylab("log odds of CHD") +
  geom_ribbon(aes(ymin = lp_cs_lci, ymax = lp_cs_uci), alpha = 0.2) # predicted log odds

# convert to probability scale
data$lp_cs <- exp(data$lp_cs)/(1+exp(data$lp_cs))
data$lp_cs_lci <- exp(data$lp_cs_lci)/(1+exp(data$lp_cs_lci))
data$lp_cs_uci <- exp(data$lp_cs_uci)/(1+exp(data$lp_cs_uci))

ggplot(data = data, aes(x = age, y = lp_cs)) + geom_line() + ylab("Probability of CHD") + 
  geom_ribbon(aes(ymin = lp_cs_lci, ymax = lp_cs_uci), alpha = 0.2) # predicted log odds

