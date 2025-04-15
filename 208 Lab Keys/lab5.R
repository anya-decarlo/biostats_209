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

library(jtools)
library(readstata13)
library(psych)
library(epiDisplay)
library(ggplot2)
library(marginaleffects)
library(boot)
library(fastDummies)
library(dplyr)

data <- read.dta13("~/Desktop/Biostat_208/lab5.dta")

# generate values
data$ldlch = data$ldl1 - data$ldl0

# calculate group means
group_stats <- data %>% group_by(ht, statins) %>%
  summarise(mean_values = mean(ldlch, na.rm = TRUE),
            sd_values = sd(ldlch, na.rm = TRUE))
print(group_stats)

# Regress with interaction
model <- lm(ldlch ~ ht * statins, data = data)
jtools::summ(model, confint = TRUE, digits = 3)

# effect of HT in non-users
hypotheses(model, "`hthormone therapy` = 0")

# effect of HT in statin users
hypotheses(model, "`hthormone therapy` + `hthormone therapy:statinsyes` = 0")

# effect of statins in placebo
hypotheses(model, "`statinsyes` = 0")

# effect of statins in HT
hypotheses(model, "`statinsyes` + `hthormone therapy:statinsyes` = 0")

# Recode 'glucose' into 'dmgrp' based on specified criteria
data$dmgrp <- cut(data$glucose,
                  breaks = c(-Inf, 99, 100, 125, Inf),
                  labels = c(1, 2, 2, 3),
                  include.lowest = TRUE)

# Replace values in 'dmgrp' if 'diabetes' equals 1
data$dmgrp[data$diabetes == "yes"] <- 3

# Convert 'dmgrp' to a factor with labels
data$dmgrp <- as.factor(data$dmgrp)
levels(data$dmgrp) <- c("normal", "IFG", "diabetes")

# center BMI
data$cbmi = data$bmi - 28.6
model <- lm(sbp ~ dmgrp * cbmi, data = data)
jtools::summ(model, confint = TRUE, digits = 3)

# overall 2-df F test for interaction
coef_length <- length(model$coef)
hypotheses(model, joint = c(1:coef_length)[grep(":cbmi", names(model$coefficients))], 
           hypotheses = rep(0, length(grep(":cbmi", names(model$coefficients)))))

# effects of 5-unit increase in BMI in the normal, IFG, and DM groups
hypotheses(model, "5 * cbmi = 0")
hypotheses(model, "5 * (`cbmi` + `dmgrpIFG:cbmi`)= 0")
hypotheses(model, "5 * (`cbmi` + `dmgrpdiabetes:cbmi`)= 0")

# between group contrasts by BMI level
hypotheses(model, "(`dmgrpdiabetes` - `dmgrpIFG`) + 1.4 * (`dmgrpdiabetes:cbmi` - `dmgrpIFG:cbmi`) = 0")

# interaction of BMI and glucose in model for SBP
data$cglucose <- data$glucose-100
model <- lm(sbp ~ age + raceth + cbmi * cglucose, data = data)
jtools::summ(model, confint = TRUE, digits = 3)

# effect of 5-unit increase in BMI at selected glucose values      
for(g in c(80, 90, 100, 125)){
  print(paste("Effect of a 5-unit increase in BMI on SBP when glucose =", g))
  print(hypotheses(model, "5 * (cbmi + (eval(g - 100) * `cbmi:cglucose`)) = 0"))
}

# effect of 10-unit increase in glucose at selected BMI values

for(g in c(18.5, 25, 28.6, 30, 35)){
  print(paste("Effect of a 10-unit increase in glucose on SBP when BMI =", g))
  print(hypotheses(model, "10 * (cglucose + (eval(g - 28.6) * `cbmi:cglucose`)) = 0"))
}
