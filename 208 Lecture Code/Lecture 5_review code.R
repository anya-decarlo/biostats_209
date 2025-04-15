install.packages("jtools")
install.packages("readstata13")
install.packages("psych")
install.packages("epiDisplay")
install.packages("ggplot2")
install.packages("marginaleffects")
install.packages("boot")
install.packages("fastDummies")

library(jtools)
library(readstata13)
library(psych)
library(epiDisplay)
library(ggplot2)
library(marginaleffects)
library(boot)
library(fastDummies)

hers <- read.dta13("~/Desktop/Biostat_208/hersdata.dta")


describe(hers$age)

# continuous * categorical interaction
fit <- lm(SBP ~ physact * age, data = hers)
jtools::summ(fit, confint = TRUE, digits = 3)
coef_length <- length(fit$coef)
hypotheses(fit, joint = c(1:coef_length)[grep(":age", names(fit$coefficients))], 
           hypotheses = rep(0, length(grep(":age", names(fit$coefficients)))))
hypotheses(fit, "10 * age + 10 * `physactmuch more active:age` = 0")

# categorical * categorical interaction
fit <- lm(SBP ~ physact * smoking, data = hers)
jtools::summ(fit, confint = TRUE, digits = 3)
coef_length <- length(fit$coef)
hypotheses(fit, joint = c(1:coef_length)[grep(":smokingyes", names(fit$coefficients))], 
           hypotheses = rep(0, length(grep(":smokingyes", names(fit$coefficients)))))

hypotheses(fit, "smokingyes + `physactsomewhat less active:smokingyes` = 0")
hypotheses(fit, "smokingyes + `physactabout as active:smokingyes` = 0")
hypotheses(fit, "smokingyes + `physactsomewhat more active:smokingyes` = 0")
hypotheses(fit, "smokingyes + `physactmuch more active:smokingyes` = 0")

slopes(fit, variables = "smoking", newdata = datagrid(physact = levels(hers$physact)))
avg_slopes(fit, variables = "smoking", by = "physact")


hypotheses(fit, "`physactsomewhat less active` + `physactsomewhat less active:smokingyes` = 0")
hypotheses(fit, "`physactabout as active` + `physactabout as active:smokingyes` = 0")
hypotheses(fit, "`physactsomewhat more active` + `physactsomewhat more active:smokingyes` = 0")
hypotheses(fit, "`physactmuch more active` + `physactmuch more active:smokingyes` = 0")

slopes(fit, variables = "physact", newdata = datagrid(smoking = levels(hers$smoking)))
avg_slopes(fit, variables = "physact", by = "smoking")




