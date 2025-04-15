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
install.packages("epitools")

library("jtools")
library("readstata13")
library("psych")
library("epiDisplay")
library("epitools")
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
library("risks")


data <- read.dta13("~/Downloads/BIOS 208 2024/labs/lab9/lab9.dta")
setwd("~/Downloads/BIOS 208 2024/labs/lab9")

### smoking indicator
# Recode 'smoke' such that 0 remains 0, and any value greater than 0 is recoded to 1
data$smoke <- ifelse(data$ncigs > 0, 1, 0)
# Factor the 'smoke' variable and define labels for the factor levels
data$smoke <- factor(data$smoke, levels = c(0, 1), labels = c("nonsmoker", "smoker"))

### Adjusted association between CHD and type A behavior

# Unadjusted odds ratio frequency table analysis
oddsratio(data$chd69, data$dibpat) 
oddsratio(data$chd69[which(data$smoke == "nonsmoker")], data$dibpat[which(data$smoke == "nonsmoker")]) 
oddsratio(data$chd69[which(data$smoke == "smoker")], data$dibpat[which(data$smoke == "smoker")]) 

# Using Mantel-Haenszel test frequency table analysis
mantelhaen.test(data$chd69, data$dibpat, data$smoke) # MH combined stratified by smoking

# Using logistic regression
model <- glm(chd69 ~ dibpat + smoke, data = data, family = "binomial")
jtools::summ(model, confint = TRUE, digits = 3, exp = TRUE)


# binary version of age
data$dage <- cut(data$age,
               breaks = c(38, 49, 59), # Define breaks such that 39-49 maps to 0 and 50-59 maps to 1
               labels = c("39_49", "50_59"),
               right = TRUE, # This makes the intervals closed on the right: [39,49), [50,59)
               include.lowest = TRUE)

# Association between CHD, age & arcus
oddsratio(data$chd69, data$arcus) 
oddsratio(data$chd69[which(data$dage == "39_49")], data$arcus[which(data$dage == "39_49")])  
oddsratio(data$chd69[which(data$dage == "50_59")], data$arcus[which(data$dage == "50_59")])  
mantelhaen.test(data$chd69, data$arcus, data$dage)


oddsratio(data$chd69, data$dage) 
oddsratio(data$chd69[which(data$arcus == 1)], data$dage[which(data$arcus == 1)]) 
oddsratio(data$chd69[which(data$arcus == 0)], data$dage[which(data$arcus == 0)]) 
mantelhaen.test(data$chd69, data$dage, data$arcus)

# Logistic model for age-arcus interaction
data$arcus_dage50_59 <- as.numeric(data$arcus) * (as.numeric(data$dage) - 1) # manually form interaction
model <- glm(chd69 ~ arcus + dage + arcus_dage50_59, data = data, family = "binomial")
jtools::summ(model, confint = TRUE, digits = 3, exp = TRUE)

# calculation of log OR for arcus effect among the old
print(model$coefficients["(Intercept)"] + 1 * model$coefficients["arcus"] + 1 * model$coefficients["dage50_59"] + 1 * model$coefficients["arcus_dage50_59"]) 
print(model$coefficients["(Intercept)"] + 0 * model$coefficients["arcus"] + 1 * model$coefficients["dage50_59"] + 0 * model$coefficients["arcus_dage50_59"]) 

# log OR
print(model$coefficients["arcus"] + 1 * model$coefficients["arcus_dage50_59"]) 
# OR
print(exp(model$coefficients["arcus"] + 1 * model$coefficients["arcus_dage50_59"]))

# with hypotheses: odds ratio
hypotheses(model, "exp(arcus + arcus_dage50_59) = 0")
hypotheses(model, "exp(dage50_59 + arcus_dage50_59) = 0")
hypotheses(model, "exp(arcus) = 0")
hypotheses(model, "exp(dage50_59) = 0")
hypotheses(model, "exp(arcus + dage50_59 + arcus_dage50_59) = 0")

# risk difference regression model
model <- riskdiff(chd69 ~ arcus + dage + arcus_dage50_59, data = data, approach = "glm")
jtools::summ(model, confint = TRUE, digits = 3)

# Interaction moel with age continuous (and linear)
data$arcus_age <- as.numeric(data$arcus) * data$age # manually form interaction
model <- glm(chd69 ~ arcus + age + arcus_age, data = data, family = "binomial", na.action = na.exclude)
jtools::summ(model, confint = TRUE, digits = 3)

# OR for the effect of arcus in 55 year-olds
hypotheses(model, "exp(`(Intercept)` + 55 * age  + 1 * arcus + 1 * 55 * arcus_age) = 0")
hypotheses(model, "exp(`(Intercept)` + 55 * age) = 0")
hypotheses(model, "exp(arcus + 1 * 55 * arcus_age) = 0")

# OR for the effect of arcus in 40 year-olds
hypotheses(model, "exp(`(Intercept)` + 40 * age + 1 * arcus + 1 * 40 * arcus_age) = 0")
hypotheses(model, "exp(`(Intercept)` + 40 * age) = 0")
hypotheses(model, "exp(arcus + 1 * 40 * arcus_age) = 0")

# fit a line
data$lp <- predict(model)

ggplot(data = data[complete.cases(data),], aes(x = age, y = lp, color = factor(arcus))) + geom_line() + 
  # Set the title for the y-axis
  labs(y = "log odds(CHD)", color = "Arcus Status")

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
  
  return(logitfitted)
  
}

data$logitloess <- NA
arc <- which(data$arcus == 0)
data$logitloess[arc] <- logitloess(data$age[arc], data$chd69[arc], s = 0.8)
data$logitloess[-arc] <- logitloess(data$age[-arc], data$chd69[-arc], s = 0.8)

ggplot(data = data[complete.cases(data),], aes(x = age, y = lp, color = factor(arcus))) +
geom_line() + geom_line(aes(x = age, y = logitloess)) +
  # Set the title for the y-axis
  labs(y = "log odds(CHD)", color = "Arcus Status")

# Risk difference model
model <- riskdiff(chd69 ~ arcus + age + arcus_age, data = data, approach = "glm")
jtools::summ(model, confint = TRUE, digits = 3)

# Likelihood ratio test example
model <- glm(chd69 ~ chol + sbp + age + smoke + bmi + factor(behpat) , data = data, family = "binomial")
data$behpat2 <- as.factor(5 - data$behpat)
table(data$behpat, data$behpat2)
model <- glm(chd69 ~ chol + sbp + age + smoke + bmi + behpat2 , data = data, family = "binomial")
model0 <- glm(chd69 ~ chol + sbp + age + smoke + bmi , data = data, family = "binomial")
anova(model0, model, test = "Chisq")

model_dichot <- glm(chd69 ~ chol + sbp + age + smoke + bmi + dibpat, data = data, family = "binomial")
anova(model_dichot, model, test = "Chisq")


###### RERI for RR and OR (see lab comments)

### RERI_RR 

# relative risk regression model
model <- riskratio(chd69 ~ arcus + dage + arcus_dage50_59, data = data, approach = "glm")
jtools::summ(model, confint = TRUE, digits = 3, exp = TRUE)
hypotheses(model,"exp(arcus + dage50_59 + arcus_dage50_59) - exp(arcus) - exp(dage50_59) + 1 = 0")


### RERI_OR (uses logistic model (OR) rather than log-binomial (RR) model)

#### RERI dichotomous age
model <- glm(chd69 ~ arcus + dage + arcus_dage50_59, data = data, family = "binomial")
hypotheses(model,"exp(arcus + dage50_59 + arcus_dage50_59) - exp(arcus) - exp(dage50_59) + 1 = 0")

#### RERI continuous age
model <- glm(chd69 ~ arcus + age + arcus_age, data = data, family = "binomial", na.action = na.exclude)
hypotheses(model,"exp(arcus + 10 * age  + 60 * arcus_age) - exp(arcus + 50 * arcus_age) - exp(10 *age) + 1 = 0")