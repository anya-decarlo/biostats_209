# Load the necessary libraries
install.packages("jtools")
install.packages("readstata13")
install.packages("psych")
install.packages("epiDisplay")
install.packages("ggplot2")
install.packages("marginaleffects")
install.packages("boot")
install.packages("fastDummies")
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
library("dplyr")
library("mediation")

data <- read.dta13("~/Desktop/Biostat_208/lab4.dta")

# indicator for being relatively inactive
data$lessactive <- ifelse(data$physact %in% c("much less active", "somewhat less active"), 1, 0)
data$lessactive <- factor(data$lessactive, labels = c("no", "yes"))

# three-level alcohol use variable
data$drinkamt <- ifelse(data$drnkspwk >= 0.2 & data$drnkspwk < 5, 1, ifelse(data$drnkspwk >= 5, 2, 0))
data$drinkamt <- factor(data$drinkamt, labels = c("none", "<5 drinks/week", ">= 5 drinks/week"))

# natural log of triglyceride level
data$lntg <- log(data$tgl)

# Select for complete data
data <- data[complete.cases(data[, c("lncreat", "bmi", "age", "raceth", "educyrs", "drinkamt", "lessactive", "lntg")]),]

# unadjusted model for crude association of BMI with log-creatinine levels
model <- glm(lncreat ~ bmi, data = data, family = "gaussian")
jtools::summ(model, confint = TRUE, digits = 3, exp = TRUE)

#Relative increment in creatinine associated with 5-kg/m^2 increment in BMI
hypotheses(model,"exp(bmi * 5) = 0")

#Percent increment in creatinine associated with 5-kg/m^2 increment in BMI
hypotheses(model,"100 * (exp(bmi * 5) - 1) = 0")

# model adjusting for confounders
model <- glm(lncreat ~ bmi + age + raceth + educyrs + drinkamt + lessactive, data = data, family = "gaussian")
jtools::summ(model, confint = TRUE, digits = 3, exp = TRUE)

#Relative increment in creatinine associated with 5-kg/m^2 increment in BMI
hypotheses(model,"exp(bmi * 5) = 0")

#Percent increment in creatinine associated with 5-kg/m^2 increment in BMI
hypotheses(model,"100 * (exp(bmi * 5) - 1) = 0")

# Mediation

# Model 1
model <- lm(lntg ~ bmi + age + raceth + educyrs + drinkamt + lessactive, data = data)
jtools::summ(model, confint = TRUE, digits = 3)

# store coefficient needed to calculate indirect effect 
link1 <- model$coefficients["bmi"]
# Percent increase in TG for a 5-kg/m2 increase in BMI
hypotheses(model,"100 * (exp(bmi * 5) - 1) = 0")

# Model 2
model <- lm(lncreat ~ bmi + age + raceth + educyrs + drinkamt + lessactive, data = data)
jtools::summ(model, confint = TRUE, digits = 3)
# store coefficient needed to calculate PE 
b_overall = model$coefficients["bmi"]
# Overall BMI effect: percent increase in creatinine for a 5-kg/m^2 increase in BMI
hypotheses(model,"100 * (exp(bmi * 5) - 1) = 0")

# Model 3
model <- lm(lncreat ~ bmi + age + raceth + educyrs + drinkamt + lessactive + lntg, data = data)
jtools::summ(model, confint = TRUE, digits = 3)
# store coefficient needed to calculate PE 
link2 = model$coefficients["lntg"]
b_direct = model$coefficients["bmi"]

# Overall BMI effect: percent increase in creatinine for a 5-kg/m^2 increase in BMI
hypotheses(model,"100 * (exp(bmi * 5) - 1) = 0")

# Relative increase in creatinine for a 25% increase in TG
hypotheses(model,"exp((lntg*log(1.25))) = 0")

# Percent increase in creatinine for a 25% increase in TG
hypotheses(model,"100 * (exp((lntg*log(1.25))) -1) = 0")


# Indirect effect of BMI on creatinine via TG
# on log-creatinine scale, for comparison to medeff results below
link1 * link2
# percent increase in creatinine for a 5-kg/m^2 increase in BMI
print(100*(exp(5*link1*link2)-1))
# Now compute the indirect via subtracting the direct effect from the total effect
# on log-creatinine scale
b_overall-b_direct
# percent increase in creatinine for a 5-kg/m^2 increase in BMI
100*(exp(5*(b_overall-b_direct))-1)

# Percentage of adjusted BMI effect on log-creatinine explained by TG
pe = (b_overall - b_direct)/b_overall*100
round(pe, 1)

# Bootstrap
library(boot)
set.seed(123)
boot.out <- boot(data, function(data, indices){
  df <- data[indices, ] # allows bootstrapping
  mod1 <- lm(lncreat ~ bmi + age + raceth + educyrs + drinkamt + lessactive, df) 
  mod2 <- lm(lncreat ~ bmi + age + raceth + educyrs + drinkamt + lessactive + lntg, df)
  pe <- (mod1$coefficients["bmi"] - mod2$coefficients["bmi"])/mod1$coefficients["bmi"]  * 100
  return(pe)
}, R = 5000)
boot.ci(boot.out, type = "bca")

# Mediation analysis using mediation package
out.fit <- lm(lncreat ~ bmi + age + raceth + educyrs + drinkamt + lessactive + lntg, data = data) 
med.fit <- lm(lntg ~ bmi + age + raceth + educyrs + drinkamt + lessactive, data = data)
med.out <- mediate(med.fit, out.fit, treat = "bmi", mediator = "lntg", robustSE = TRUE, sims = 1000)
summary(med.out)
