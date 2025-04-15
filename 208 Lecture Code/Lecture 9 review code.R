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
install.packages("mediation")

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
library("mediation")

hers <- read.dta13("/Users/aaron.scheffler/Downloads/BIOS 208 2025/lectures/lecture9/data/hersbase.dta", nonint.factors = TRUE)

# center age & BMI
describe(hers$bmi)
hers$bmic <- hers$bmi - mean(hers$bmi)
describe(hers$age)
hers$agec <- hers$age - mean(hers$age)

# main effect model
fit <- glm(hypt ~ ifg + bmic + agec, data = hers, family = "binomial")
jtools::summ(fit, confint = TRUE, digits = 3, exp = TRUE)

# interaction model
fit <- glm(hypt ~ ifg * bmic + agec, data = hers, family = "binomial")
jtools::summ(fit, confint = TRUE, digits = 3, exp = TRUE)
jtools::summ(fit, confint = TRUE, digits = 3)

# OR for IFG among those with average BMI
hypotheses(fit, "exp(ifg) = 0")

# OR for IFG among those with BMI = mean(BMI)+1
hypotheses(fit, "exp(ifg + `ifg:bmic`) = 0")

# OR for IFG among those with BMI=35
hypotheses(fit, "exp(ifg + (35-27.87753) * `ifg:bmic`) = 0")

# OR for a 5 unit BMI increase in among those with/without IFG
hypotheses(fit, "exp(5 * bmic + 5 * `ifg:bmic`) = 0")
hypotheses(fit, "exp(5 * bmic) = 0")

# estimated log odds as a function of BMI for average age individuals without/with IFG
hers$lods <- fit$coefficients["(Intercept)"] + hers$ifg * fit$coefficients["ifg"] +
             hers$bmic * fit$coefficients["bmic"] + hers$ifg * hers$bmic * fit$coefficients["ifg:bmic"]

ggplot(hers, aes(x = bmic, y = lods, color = factor(ifg))) + geom_line()

# evaluation of additivive interaction (not converging)
fit <- risks::riskdiff(hypt ~ ifg + bmic + agec, data = hers, approach = "glm_startp")
jtools::summ(fit, confint = TRUE, digits = 3)

# Fit the mediator model (linear regression for bmic)
model.M <- lm(bmic ~ exer3 + edu + csmker + agec, data = hers)

# Fit the outcome model (logistic regression for hypt)
model.Y <- glm(hypt ~ exer3 + edu + csmker + agec + bmic, 
               family = "binomial", 
               data = hers)

# Run the mediation analysis
results <- mediate(model.M, model.Y, 
                   treat = "exer3", 
                   mediator = "bmic", 
                   boot = TRUE,    # use bootstrapping for inference
                   sims = 1000)    # number of simulations
summary(results)
