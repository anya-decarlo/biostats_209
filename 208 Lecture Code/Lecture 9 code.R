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

wcgs <- read.dta13("/Users/aaron.scheffler/Downloads/BIOS 208 2025/lectures/lecture8/wcgs.dta")


fit <- glm(chd69 ~ arcus, data = wcgs, family = "binomial")
jtools::summ(fit, confint = TRUE, digits = 3, exp = TRUE)
jtools::summ(fit, confint = TRUE, digits = 3)


fit <- glm(chd69 ~ age, data = wcgs, family = "binomial")
jtools::summ(fit, confint = TRUE, digits = 3, exp = TRUE)
jtools::summ(fit, confint = TRUE, digits = 3)

fit <- glm(chd69 ~ chol + sbp, data = wcgs[which(wcgs$chol < 600), ], family = "binomial")
jtools::summ(fit, confint = TRUE, digits = 3, exp = TRUE)
jtools::summ(fit, confint = TRUE, digits = 3)

hers <- read.dta13("/Users/aaron.scheffler/Downloads/BIOS 208 2025/lectures/lecture9/data/hersbase.dta")

# unadjusted logistic model
fit <- glm(hypt ~ exer3, data = hers, family = "binomial")
jtools::summ(fit, confint = TRUE, digits = 3, exp = TRUE)

# Stratified data: one row per (C, X, Y) combination
dat <- rbind(
  data.frame(C=1, X=1, Y=1)[rep(1, 50), ],
  data.frame(C=1, X=1, Y=0)[rep(1, 50), ],
  data.frame(C=1, X=0, Y=1)[rep(1, 20), ],
  data.frame(C=1, X=0, Y=0)[rep(1, 80), ],
  
  data.frame(C=0, X=1, Y=1)[rep(1, 80), ],
  data.frame(C=0, X=1, Y=0)[rep(1, 20), ],
  data.frame(C=0, X=0, Y=1)[rep(1, 50), ],
  data.frame(C=0, X=0, Y=0)[rep(1, 50), ]
)

mantelhaen.test(dat$Y, dat$X, dat$C)

# odds ratio
fit <- glm(Y ~ X + C, data = dat, family = "binomial")
jtools::summ(fit, confint = TRUE, digits = 3, exp = TRUE)
avg_comparisons(fit, variables = "X", comparison = "lnor", 
newdata = datagrid(C = c(0, 1), grid_type = "counterfactual"), transform = exp)

# risk difference
fit <- risks::riskdiff(Y ~ X + C, data = dat, approach = "glm")
jtools::summ(fit, confint = TRUE, digits = 3)

avg_comparisons(fit, variables = "X", comparison = "difference", 
                newdata = datagrid(C = c(0, 1), grid_type = "counterfactual"))
# risk ratio
fit <- risks::riskratio(Y ~ X + C, data = dat, approach = "glm_startd")
jtools::summ(fit, confint = TRUE, digits = 3, exp = TRUE)
avg_comparisons(fit, variables = "X", comparison = "ratio", 
                newdata = datagrid(C = c(0, 1), grid_type = "counterfactual"))

# adjusted logistic model
fit <- glm(hypt ~ exer3 + edu + csmker + age + rcs(bmi), data = hers, family = "binomial")
jtools::summ(fit, confint = TRUE, digits = 3, exp = TRUE)

predictions(fit, by = "exer3", newdata = datagrid(exer3 = c(0, 1), grid_type = "counterfactual"))

# marginal OR
avg_comparisons(fit, variables = "exer3", comparison = "lnor", newdata = datagrid(exer3 = c(0, 1), grid_type = "counterfactual"), transform = exp)
# marginal RR
avg_comparisons(fit, variables = "exer3", comparison = "ratio", newdata = datagrid(exer3 = c(0, 1), grid_type = "counterfactual"))
# marginal RD 
avg_comparisons(fit, variables = "exer3", comparison = "difference", newdata = datagrid(exer3 = c(0, 1), grid_type = "counterfactual"))

fit <- lm(bmi ~ exer3 + edu + csmker + age, data = hers)
jtools::summ(fit, confint = TRUE, digits = 3)
fit <- glm(hypt ~ exer3 + edu + csmker + age, data = hers, family = "binomial")
jtools::summ(fit, confint = TRUE, digits = 3, exp = TRUE)
fit <- glm(hypt ~ exer3 + edu + csmker + age + bmi, data = hers, family = "binomial")
jtools::summ(fit, confint = TRUE, digits = 3, exp = TRUE)

# Obtain marginal effects at a fixed level of BMI (in this case, the mean BMI)
avg_comparisons(fit, variables = "exer3", comparison = "lnor", newdata = datagrid(exer3 = c(0, 1), bmi = mean(hers$bmi), grid_type = "counterfactual"), transform = exp)
avg_comparisons(fit, variables = "exer3", comparison = "ratio", newdata = datagrid(exer3 = c(0, 1), bmi = mean(hers$bmi), grid_type = "counterfactual"))
avg_comparisons(fit, variables = "exer3", comparison = "difference",newdata = datagrid(exer3 = c(0, 1), bmi = mean(hers$bmi), grid_type = "counterfactual"))

mediator_model <- lm(bmi ~ exer3 + edu + csmker + age, data = hers)
outcome_model <- glm(hypt ~ exer3 + edu + csmker + age + bmi, family = "binomial", data = hers)
mediation_result <- mediate(mediator_model, outcome_model, 
                            treat = "exer3", mediator = "bmi", 
                            boot = TRUE, sims = 1000)
summary(mediation_result)


# create binary indicator of BMI greater than or equal to 35
hers$bmihi <- ifelse(hers$bmi <= 34.9999, 0, 1)
fit <- glm(hypt ~ ifg * bmihi, data = hers, family = "binomial")
jtools::summ(fit, confint = TRUE, digits = 3, exp = TRUE)
jtools::summ(fit, confint = TRUE, digits = 3)

# odds scale
hypotheses(fit, "exp(ifg) = 0")
hypotheses(fit, "exp(bmihi) = 0")
hypotheses(fit, "exp(ifg + `ifg:bmihi`) = 0")
hypotheses(fit, "exp(bmihi + `ifg:bmihi`) = 0")
hypotheses(fit, "exp(ifg + bmihi + `ifg:bmihi`) = 0")

# log odds scale
hypotheses(fit, "ifg = 0")
hypotheses(fit, "bmihi = 0")
hypotheses(fit, "ifg + `ifg:bmihi` = 0")
hypotheses(fit, "bmihi + `ifg:bmihi` = 0")
hypotheses(fit, "ifg + bmihi + `ifg:bmihi` = 0")

# risk ratio regression
hypotheses(fit, "exp(ifg + bmihi + `ifg:bmihi`) - exp(ifg) - exp(bmihi) + 1 = 0")
fit <- risks::riskratio(hypt ~ ifg*bmihi, data = hers, approach = "glm")
jtools::summ(fit, confint = TRUE, digits = 3, exp = TRUE)
hypotheses(fit, "exp(ifg + bmihi + `ifg:bmihi`) - exp(ifg) - exp(bmihi) + 1 = 0")

# risk difference 
fit <- risks::riskdiff(hypt ~ ifg*bmihi, data = hers, approach = "glm")
jtools::summ(fit, confint = TRUE, digits = 3)

#
fit <- glm(hypt ~ exer3*bmihi + edu + csmker + age, data = hers, family = "binomial")
#predictions(fit, by = "exer3", newdata = datagrid(exer3 = c(0, 1), bmihi = 0, grid_type = "counterfactual"))
avg_comparisons(fit, variables = "exer3", comparison = "difference",newdata = datagrid(exer3 = c(0, 1), bmihi = 0, grid_type = "counterfactual"))
predictions(fit, by = "exer3", newdata = datagrid(exer3 = c(0, 1), bmihi = 1, grid_type = "counterfactual"))
avg_comparisons(fit, variables = "exer3", comparison = "difference",newdata = datagrid(exer3 = c(0, 1), bmihi = 1, grid_type = "counterfactual"))

#
# First, fit full model including behpat: 
fit <- glm(chd69 ~ age + chol + sbp + bmi + smoke + behpat, data = wcgs, family = "binomial")
jtools::summ(fit, confint = TRUE, digits = 3, exp = TRUE)

# Next, the desired “nested” model excluding predictor(s) of interest is fit, and compared 
# to the previous model using the anova command:
fit0 <- glm(chd69 ~ age + chol + sbp + bmi + smoke, data = wcgs, family = "binomial")
jtools::summ(fit0, confint = TRUE, digits = 3, exp = TRUE)
# perform likelihood ratio test comparing fitted model to full model (A):
anova(fit0, fit, test = "Chisq")