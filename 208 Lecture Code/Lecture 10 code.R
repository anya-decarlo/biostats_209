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
install.packages("ResourceSelection")
install.packages("pROC")
install.packages("plotROC")
install.packages("cutpointr")
install.packages("logistf")


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
library("ResourceSelection")
library("pROC")
library("plotROC")
library("cutpointr")
library("logistf")

wcgs <- read.dta13("/Users/aaron.scheffler/Downloads/BIOS 208 2025/lectures/lecture8/wcgs.dta")

wcgs$age_10 <- wcgs$age/10
wcgs$chol_50 <- wcgs$chol/50
wcgs$sbp_50 <- wcgs$sbp/50
wcgs$bmi_10 <-  wcgs$bmi/10
wcgs$bmichol <-  wcgs$bmi * wcgs$chol
wcgs$bmisbp <-  wcgs$bmi * wcgs$sbp

fit <- glm(chd69 ~ rcs(age) + chol + sbp + bmi + smoke + dibpat, data = wcgs, family = "binomial")
jtools::summ(fit, confint = TRUE, digits = 3, exp = TRUE)
coef_length <- length(fit$coef)
hypotheses(fit, joint = c(1:coef_length)[grep("rcs", names(fit$coefficients))][-1], hypotheses = rep(0, coef_length)[-1])

fit <- glm(chd69 ~ age_10 + chol_50 + sbp_50 + bmi_10 + smoke + dibpat + bmichol + bmisbp, data = wcgs, family = "binomial", na.action = na.exclude)
jtools::summ(fit, confint = TRUE, digits = 3, exp = TRUE)
wcgs$p <- predict(fit, type = "response")
wcgs$r <- residuals(fit)
wcgs$h <- hatvalues(fit)
wcgs$db <- cooks.distance(fit)
print(wcgs[c(1:10), c("id", "chd69","age", "arcus", "dibpat", "chol", "r")])

ggplot(wcgs, aes(x = p, y = r)) + geom_point() + ylab("Pearson residuals") + xlab("predicted probability")
ggplot(wcgs, aes(x = p, y = db)) + geom_point() + ylab("Cook's distance") + xlab("predicted probability")
ggplot(wcgs, aes(x = r, y = h)) + geom_point() + ylab("leverage") + xlab("Pearson residuals")

print(wcgs[wcgs$id %in% c(10078,12237, 12453, 12752,12873), c("chd69","age", "chol", "sbp", "bmi", "smoke", "dibpat")])

fit <- glm(chd69 ~ age_10 + chol_50 + sbp_50 + bmi_10 + smoke + dibpat + bmichol + bmisbp, data = wcgs[-which(wcgs$id %in% c(12237, 12453)),], family = "binomial", na.action = na.exclude)
jtools::summ(fit, confint = TRUE, digits = 3, exp = TRUE)

fit <- glm( dibpat ~ age_10 + chol_50 + sbp_50 + bmi_10 + smoke + dibpat + bmichol + bmisbp, data = wcgs[-which(wcgs$id %in% c(12237, 12453)),], family = "binomial", na.action = na.exclude)
wcgs$prop <-NA
wcgs$prop[-which(wcgs$id %in% c(12237, 12453))] <- predict(fit, type = "response")
ggplot(wcgs, aes(x = prop, color = dibpat)) + geom_density()

# Pregibons Goodness-of-link test (similar to Stata's link test)
fit <- glm(chd69 ~ age_10 + chol_50 + sbp_50 + bmi_10 + smoke + dibpat + bmichol + bmisbp, 
      data = wcgs[-which(wcgs$id %in% c(12237, 12453)),], family = "binomial", na.action = na.exclude)
wcgs$hat <- NA
wcgs$hat[-which(wcgs$id %in% c(12237, 12453))] <- predict(fit)
wcgs$hatsq <- wcgs$hat^2
link <- glm(chd69 ~ hat + hatsq, data = wcgs, family = "binomial", na.action = na.exclude)
jtools::summ(link, confint = TRUE, digits = 3)

# Pregibons Goodness-of-link test (similar to Stata's link test) - excluding interactions
fit <- glm(chd69 ~ age_10 + chol_50 + sbp_50 + bmi_10 + smoke + dibpat, 
           data = wcgs[-which(wcgs$id %in% c(12237, 12453)),], family = "binomial", na.action = na.exclude)
wcgs$hat <- NA
wcgs$hat[-which(wcgs$id %in% c(12237, 12453))] <- predict(fit)
wcgs$hatsq <- wcgs$hat^2
link <- glm(chd69 ~ hat + hatsq, data = wcgs, family = "binomial", na.action = na.exclude)
jtools::summ(link, confint = TRUE, digits = 3)

# Hosmer-Lemeshow
fit <- glm(chd69 ~ age_10 + chol_50 + sbp_50 + bmi_10 + smoke + dibpat + bmichol + bmisbp, 
           data = wcgs[-which(wcgs$id %in% c(12237, 12453)),], family = "binomial")
jtools::summ(fit, confint = TRUE, digits = 3)
hltest(fit, df = 8)

fit <- glm(chd69 ~ age_10 + chol_50 + sbp_50 + bmi_10 + smoke + dibpat + bmichol + bmisbp, 
           data = wcgs[-which(wcgs$id %in% c(12237, 12453)),], family = "binomial", na.action = na.exclude)
wcgs$p <- NA
wcgs$p[-which(wcgs$id %in% c(12237, 12453))] <- predict(fit, type = "response")
bins <- quantile(wcgs$p, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
wcgs$percentile_bin <- cut(wcgs$p, breaks = bins, include.lowest = TRUE, 
                         labels = c("0-10", "10-20", "20-30", 
                                    "30-40", "40-50","50-60","60-70","70-80","80-90", "90-100"))
summary_table <- wcgs %>%
  group_by(percentile_bin) %>%
  summarise(mean_value = mean(p), min_value  = min(p),max_value  = max(p)
  )
print(summary_table)
print(314 * .010283)
print(314 * (1-.010283))

pROC::roc(predictor = wcgs$p, response = wcgs$chd69)
ggplot(wcgs, aes(d = chd69, m = p)) + geom_roc(n.cuts = 50, labels = FALSE)

# Train/test
set.seed(1234)
wcgs$group <- sample(c(1, 2), nrow(wcgs),replace = TRUE, prob = c(0.75, 0.25))
fit <- glm(chd69 ~ age_10 + chol_50 + sbp_50 + bmi_10 + smoke + dibpat + bmichol + bmisbp, 
           data = wcgs[which(wcgs$group == 1),], family = "binomial")
jtools::summ(fit, confint = TRUE, digits = 3)
hltest(fit, df = 8)

fit <- glm(chd69 ~ age_10 + chol_50 + sbp_50 + bmi_10 + smoke + dibpat + bmichol + bmisbp, 
           data = wcgs[which(wcgs$group == 1),], family = "binomial", na.action = na.exclude)
wcgs$p <- NA
wcgs$p[which(wcgs$group == 1)] <- predict(fit, type = "response")
myROC <-pROC::roc(predictor = wcgs$p, response = wcgs$chd69)
print(myROC)
ggplot(wcgs, aes(d = chd69, m = p)) + geom_roc(n.cuts = 50, labels = FALSE)

myROC <- with(myROC, data.frame(specificities, sensitivities, thresholds))
ggplot(myROC, aes(x = thresholds, y = sensitivities)) + geom_line(color = "blue") + 
geom_line(aes(x = thresholds, y = specificities), color = "red") + ylab("specificities/sensitivities")
cp <- cutpointr(wcgs[which(wcgs$group == 1),], p, chd69, na.rm = TRUE, method = oc_manual, cutpoint = .085)
summary(cp)

### Cross validation
# Step 1: restrict to learning sample, divide into 10 mutually exclusive subsets
wcgs$groupcv <- NA
wcgs$groupcv[which(wcgs$group == 1)] <- cut(seq(1,nrow(wcgs[which(wcgs$group == 1), ])), breaks=10, labels=FALSE)
wcgs$pcv <- NA
# Step 2 and 3: estimate model omitting each subset and store cross validated predictions
for(i in 1:10){
  
  fit <- glm(chd69 ~ age_10 + chol_50 + sbp_50 + bmi_10 + smoke + dibpat + bmichol + bmisbp, 
             data = wcgs[-which(wcgs$groupcv == i | wcgs$group == 2),], family = "binomial", na.action = na.exclude)
  wcgs$pcv[which(wcgs$groupcv == i)] <- predict(fit, newdata = wcgs[which(wcgs$groupcv == i),], type = "response")
}
# Step 4: calculate cross-validated (i.e. optimism-corrected) area under ROC curve 
pROC::roc(predictor = wcgs$pcv, response = wcgs$chd69)
ggplot(wcgs, aes(d = chd69, m = pcv)) + geom_roc(n.cuts = 50, labels = FALSE)


fit <- glm(chd69 ~ age_10 + chol_50 + sbp_50 + bmi_10 + smoke + dibpat + bmichol + bmisbp, 
           data = wcgs[-which(wcgs$group == 2),], family = "binomial", na.action = na.exclude)
wcgs$p_fitted_train[which(wcgs$group == 2)] <- predict(fit, wcgs[which(wcgs$group == 2), ], type = "response")
pROC::roc(predictor = wcgs$p_fitted_train, response = wcgs$chd69)
ggplot(wcgs, aes(d = chd69, m = p_fitted_train)) + geom_roc(n.cuts = 50, labels = FALSE)
cp <- cutpointr(wcgs[which(wcgs$group == 2),], p_fitted_train, chd69, na.rm = TRUE, method = oc_manual, cutpoint = .085)
summary(cp)


# binary regression methods
# glm
fit <- glm(chd69 ~ age + bmi, data = wcgs[which(wcgs$bmi > 12), ], family = "binomial")
jtools::summ(fit, confint = TRUE, digits = 3, exp = TRUE)

# relative risk 
fit <- risks::riskratio(chd69 ~ age + bmi, data = wcgs[which(wcgs$bmi > 12), ])
summary(fit)

# relative risk (robust variance)
wcgs$chd69_int <- as.numeric(wcgs$chd69) - 1 # need to convert chd69 to 0/1 coding
fit <- risks::riskratio(chd69_int ~ age + bmi, data = wcgs[which(wcgs$bmi > 12), ], approach = "robpoisson")
jtools::summ(fit, confint = TRUE, digits = 3, exp = TRUE)

# risk difference 
fit <- risks::riskdiff(chd69 ~ age + bmi, data = wcgs[which(wcgs$bmi > 12), ], approach = "glm")
jtools::summ(fit, confint = TRUE, digits = 3)

wcgs <- wcgs %>% mutate(bmicat = ntile(bmi, 4), )
wcgs$bmicat <- as.factor(wcgs$bmicat)
fit <- risks::riskdiff(chd69 ~ age + bmicat, data = wcgs, approach = "glm")
jtools::summ(fit, confint = TRUE, digits = 3)


# Exact logistic regression (not satisfactory R function for exact logistic regression available)
cdc <- read.dta13("/Users/aaron.scheffler/Downloads/BIOS 208 2025/lectures/lecture10/cdcn.dta")
fit <- glm(hivp ~ sexd60 + aids, family = "binomial", data = cdc)
jtools::summ(fit, confint = TRUE, digits = 3, exp = TRUE)

# Firth logistic regression
icu <- read.dta13("/Users/aaron.scheffler/Downloads/BIOS 208 2025/lectures/lecture10/icu.dta")
# Recode age
icu$agecat <- cut(icu$age,
                   breaks = c(0, 47, 63, 72, Inf),
                   labels = c(1, 2, 3, 4),
                   include.lowest = TRUE)

# Recode sys
icu$syscat <- cut(icu$sys,
                   breaks = c(0, 110, 130, 150, Inf),
                   labels = c(1, 2, 3, 4),
                   include.lowest = TRUE)

fit <- glm(sta ~ factor(crn) + factor(typ) + factor(can) + factor(loc) + agecat + syscat, family = "binomial", data = icu)
jtools::summ(fit, confint = TRUE, digits = 3, exp = TRUE)
fit <- logistf(sta ~ factor(crn) + factor(typ) + factor(can) + factor(loc) + agecat + syscat, data = icu, firth = TRUE)
summary(fit)