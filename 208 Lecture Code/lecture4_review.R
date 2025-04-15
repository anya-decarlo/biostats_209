# Lecture 4 - discussion example
#
install.packages("jtools")
install.packages("readstata13")
install.packages("psych")
install.packages("epiDisplay")
install.packages("ggplot2")
install.packages("marginaleffects")
install.packages("boot")
install.packages("fastDummies")

library("jtools")
library("readstata13")
library("psych")
library("epiDisplay")
library("ggplot2")
library("marginaleffects")
library("boot")
library("fastDummies")
library("readstata13")

data <- read.dta13("~/Desktop/hersdata.dta")

# (1) view DAG
# age, BMI, smoking & drinking possible confounders of exercise effect
#
# unadjusted marginal means for exercise
mod.ua <- lm(LDL ~ exercise, data = data)
jtools::summ(mod.ua, confint = TRUE, digits = 3)
# adjusted marginal effect estimate for exercise
# Q: note how effect changed - negative or positive confounding?
mod.a <- lm(LDL ~ exercise + age + BMI + smoking + drinkany, data = data)
jtools::summ(mod.a, confint = TRUE, digits = 3)
# Q: what assumptions need to hold for the results inference to be correct?
avg_slopes(mod.a, variables = "exercise")
# note equivalence of marginal effect for multiple predictor linear regression models

# (2) view DAG
# BMI as a mediator of exercise
# Q: How many models should I anticipate running?
# Model 1: assess BMI for possible mediation role
mod.med <- lm(BMI ~ exercise + age + smoking + drinkany, data = data)
jtools::summ(mod.med, confint = TRUE, digits = 3)
# Model 2: overall effect of exercise on LDL cholesterol, not adjusting for mediator BMI, yet 
mod.oe <- lm(LDL ~ exercise + age + smoking + drinkany, data = data)
jtools::summ(mod.oe, confint = TRUE, digits = 3)
ovef = mod.oe$coefficients["exerciseyes"]
# Model 3: direct effect on exposure on outcome, blocking or adjusting for mediator 
mod.de <- lm(LDL ~ exercise + BMI + age + smoking + drinkany, data = data)
jtools::summ(mod.de, confint = TRUE, digits = 3)
deff = mod.de$coefficients["exerciseyes"]
# indirect effect
print(ovef - deff)
# % explained by mediation
print(100*((ovef-deff)/ovef))

# discuss joint test of conf->med path and med->outcome path

# Bootstrap - how to calculate CI for statistics that come from multiple models
# 95% BC percentile bootstrap CI


# Define the mediation function
mediate <- function(data, outcome, exposure, mediator, covars) {
  formula_total <- as.formula(paste(outcome, "~", exposure, covars))
  formula_direct <- as.formula(paste(outcome, "~", exposure, mediator, covars))
  
  model_total <- lm(formula_total, data = data)
  model_direct <- lm(formula_direct, data = data)
  
  b_overall <- coef(model_total)[2]
  b_direct <- coef(model_direct)[2]
  
  pe <- (b_overall - b_direct) / b_overall * 100
  return(pe)
}

# Function to be used in bootstrapping
boot_fun <- function(data, indices) {
  d <- data[indices, ]
  mediate(d, "LDL", "exercise + ", "BMI +", "age + smoking + drinkany")
}

# Your dataset name (replace 'your_data' with your actual dataset)

# Bootstrapping
results <- boot(data, boot_fun, R = 1000) # need to run large number of iterations to achieve bca

# Bias-corrected bootstrap confidence intervals
results
boot.ci(results, type = "bca")

# Q: note the range, bounds of CI.

# mediate can achieve results comparable to above - covered in lab

# dagitty model code:
#
# confounding
dag {
bb="0,0,1,1"
BMI [pos="0.614,0.402"]
LDL [outcome,pos="0.878,0.269"]
age [pos="0.641,0.131"]
alcohol [pos="0.322,0.405"]
exercise [exposure,pos="0.108,0.276"]
smoking [pos="0.362,0.128"]
BMI -> LDL
BMI -> exercise
age -> BMI
age -> LDL
age -> alcohol
age -> exercise
age -> smoking
alcohol -> BMI
alcohol -> LDL
alcohol -> exercise
exercise -> LDL
smoking -> BMI
smoking -> LDL
smoking -> exercise
}


# mediation
dag {
bb="0,0,1,1"
BMI [pos="0.614,0.402"]
LDL [outcome,pos="0.878,0.269"]
age [pos="0.641,0.131"]
alcohol [pos="0.322,0.405"]
exercise [exposure,pos="0.108,0.276"]
smoking [pos="0.362,0.128"]
BMI -> LDL
age -> BMI
age -> LDL
age -> alcohol
age -> exercise
age -> smoking
alcohol -> BMI
alcohol -> LDL
alcohol -> exercise
exercise -> BMI
exercise -> LDL
smoking -> BMI
smoking -> LDL
smoking -> exercise
}
