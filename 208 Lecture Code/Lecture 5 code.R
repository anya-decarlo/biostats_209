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

mb = mean(hers$BMI, na.rm = TRUE)
hers$BMIc <- hers$BMI - mb
hers$active <- ifelse(as.numeric(hers$physact) %in% 1:2, 0,
                                      ifelse(as.numeric(hers$physact) %in% 3:5, 1, NA))


fit <- lm(SBP ~ active * BMIc, data = hers)
jtools::summ(fit, confint = TRUE, digits = 3)
fit <- lm(SBP ~ active + BMIc + active : BMIc, data = hers)
jtools::summ(fit, confint = TRUE, digits = 3)
hers$actxbmi <- hers$active * hers$BMI
fit <- lm(SBP ~ active + BMIc + actxbmi, data = hers)
jtools::summ(fit, confint = TRUE, digits = 3)
fit <- lm(SBP ~ active * BMIc, data = hers)
fit <- lm(SBP ~ active * BMIc, data = hers, na.action = na.exclude)
hers$p <- fitted(fit)
ggplot(data = hers, aes(x = BMIc, y = p, color = factor(active))) + geom_point(aes(x = BMIc, y = SBP, color = factor(active)), alpha = 1/5) + geom_line(linewidth= 1.5) + 
  theme_minimal() + ylab("SBP")


hypotheses(fit, "active + 1.4 * `active:BMIc` = 0")
hypotheses(fit, "BMIc +  `active:BMIc`= 0")
hypotheses(fit, "active +  6.4*`active:BMIc`= 0")
slopes(fit, variables = "active", newdata = datagrid(BMIc = c(6.4)))
hypotheses(fit, "5 * (BMIc + `active:BMIc`) = 0")
hypotheses(fit, "5 * BMIc = 0")

fit <- lm(SBP ~ active * BMI, data = hers)
jtools::summ(fit, confint = TRUE, digits = 3)
fit <- lm(SBP ~ physact * BMIc, data = hers)
jtools::summ(fit, confint = TRUE, digits = 3)
coef_length <- length(fit$coef)
# F test for interaction between active and BMIc 
hypotheses(fit, joint = c(1:coef_length)[grep(":BMIc", names(fit$coefficients))], 
           hypotheses = rep(0, length(grep(":BMIc", names(fit$coefficients)))))

tab1(hers$HT)
hers$cldl0 <- hers$LDL - mean(hers$LDL, na.rm = TRUE)
hers$ldlch <- hers$LDL1 - hers$LDL
fit <- lm(ldlch ~ HT *  cldl0, data = hers)
jtools::summ(fit, confint = TRUE, digits = 3)
fit <- lm(ldlch ~ HT *  cldl0, data = hers, na.action = na.exclude)
hers$p <- fitted(fit)
ggplot(data = hers, aes(x = cldl0, y = p, color = factor(HT))) + geom_line(linewidth= 1.5) + 
  geom_point(aes(x = cldl0, y = ldlch, color = factor(HT)), size = 0.25) + theme_minimal()
predictions(fit, variables = "HT", newdata = datagrid(cldl0 = c(0, 25, 50)))
slopes(fit, variables = "HT", newdata = datagrid(cldl0 = c(0, 25, 50)))
hers$ldlpctch <- hers$ldlch/hers$LDL * 100
fit <- lm(ldlpctch ~ HT * cldl0, data = hers, na.action = na.exclude)
jtools::summ(fit, confint = TRUE, digits = 3)
hers$p <- fitted(fit)
ggplot(data = hers, aes(x = cldl0, y = p, color = factor(HT))) + geom_line(linewidth= 1.5) + 
  geom_point(aes(x = cldl0, y = ldlch, color = factor(HT)), size = 0.25) + theme_minimal()



