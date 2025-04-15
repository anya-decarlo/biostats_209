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

hers <- read.dta13("/Users/aaron.scheffler/Downloads/BIOS 208 2025/lectures/lecture2/hersdata.dta")
fit <- lm(SBP ~ BMI, data = hers)
jtools::summ(fit, confint = TRUE, digits = 3)
fit <- lm(SBP ~ BMI + age, data = hers)
jtools::summ(fit, confint = TRUE, digits = 3)
fit <- lm(LDL ~ BMI, data = hers)
jtools::summ(fit, confint = TRUE, digits = 3)
fit <- lm(LDL ~ BMI + age10 + nonwhite + smoking + drinkany, data = hers)
jtools::summ(fit, confint = TRUE, digits = 3)
avg_slopes(fit, variables = "BMI")
# model 1
fit1 <- lm(glucose ~ BMI + age + exercise + drinkany, data = hers)
jtools::summ(fit1, confint = TRUE, digits = 3)
link1 <- fit1$coefficients["BMI"]
# model 2
fit2 <- lm(SBP ~ BMI + age + exercise + drinkany, data = hers)
jtools::summ(fit2, confint = TRUE, digits = 3)
b_overall <- fit2$coefficients["BMI"]
# model 3
fit3 <- lm(SBP ~ BMI + age + exercise + drinkany + glucose, data = hers)
jtools::summ(fit3, confint = TRUE, digits = 3)
link2 <- fit3$coefficients["glucose"]
b_direct <- fit3$coefficients["BMI"]

# two ways of calculating indirect effect
print(b_overall - b_direct)
print(link1 * link2)
# PE
print(round((b_overall - b_direct) / b_overall * 100))

# Model 1
fit1 <- lm(glucose ~ BMI + age + exercise + drinkany, data = hers)
jtools::summ(fit1, confint = TRUE, digits = 3)

# Model 3
fit3 <- lm(SBP ~ BMI + age + exercise + drinkany + glucose, data = hers)
jtools::summ(fit3, confint = TRUE, digits = 3)

set.seed(123)
boot.out <- boot(hers, function(data, indices){
  df <- data[indices, ] # allows bootstrapping
  mod1 <- lm(SBP ~ BMI + age + exercise + drinkany, df) 
  mod2 <- lm(SBP ~ BMI + age + exercise + drinkany + glucose, df)
  pe <- (mod1$coefficients["BMI"] - mod2$coefficients["BMI"])/mod1$coefficients["BMI"]  * 100
  return(pe)
}, R = 500)
boot.out
boot.ci(boot.out, type = "bca")



