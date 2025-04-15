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
install.packages("rstatix")
install.packages("epiR")

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
library("rstatix")
library("epiR")

wcgs <- read.dta13("/Users/aaron.scheffler/Downloads/BIOS 208 2025/lectures/lecture8/wcgs.dta")

# Lowess smoother for binary outcomes

# lowess smoothing
logitloess <- function(y, x, s) {
  logit <- function(pr) {
    log(pr/(1-pr))
  }
  if (missing(s)) {
    locspan <- 0.7
  } else {
    locspan <- s
  }
  loessfit <- predict(loess(y ~ x))
  pi <- pmax(pmin(loessfit,0.9999),0.0001)
  logitfitted <- logit(pi)
  plot(sort(x), logitfitted[order(x)], ylab="logit", type = "l", lwd = 2)
  return(logitfitted)
}


### unadjusted association between behavior pattern and CHD (categorical exposure)
# discuss test for homogeneity and test for trend

tab <- table(wcgs$chd69, wcgs$behpat)

# Calculate column percentages
col_percentages <- prop.table(tab, margin = 2) * 100
(col_percentages[2,]/col_percentages[1,]) # odds of CHD within each strata of Behpat

# Test of homogeneity
prop_test(tab)

# Test for trend
prop_trend_test(tab)

fit <- glm(chd69 ~ behpat, data = wcgs, family = "binomial")
jtools::summ(fit, confint = TRUE, digits = 3)
jtools::summ(fit, confint = TRUE, digits = 3, exp = TRUE)

# global test (homogeneity of odds)
coef_length <- length(fit$coef)
hypotheses(fit, joint = c(1:coef_length)[grep("behpat", names(fit$coefficients))], 
           hypotheses = rep(0, coef_length), joint_test = "chisq")


### CHD-cholesterol relationship (continuous exposure)
# lowess smoothing
wcgs.c <- na.omit(wcgs[,c("chd69", "chol")])
logitloess(as.numeric(wcgs.c$chd69) - 1, wcgs.c$chol, s = 0.8)

# ordinal categorical 
tab <- table(wcgs$chd69, wcgs$agec)
col_percentages <- prop.table(tab, margin = 2) * 100
(col_percentages[2,]/col_percentages[1,]) # odds of CHD within each strata of agec

# Test of homogeneity
prop_test(tab)

# Test for trend
prop_trend_test(tab)

# compare homogeneity test with LRT test from logistic output
fit <- glm(chd69 ~ agec, data = wcgs, family = "binomial")
jtools::summ(fit, confint = TRUE, digits = 3, exp = TRUE)

# linear
loess_fit <- logitloess(as.numeric(wcgs.c$chd69) - 1, wcgs.c$chol, s = 0.25)
fit <- glm(chd69 ~ chol, data = wcgs.c[which(wcgs.c$chol < 600), ], family = "binomial")
jtools::summ(fit, confint = TRUE, digits = 3)

# save predicted log odds of CHD
wcgs.c$xb[which(wcgs.c$chol < 600)] <- predict(fit, type = "link")
wcgs.c$loess_fit <- loess_fit
ggplot(data = wcgs.c, aes(x = chol, y = xb)) + 
  geom_line(col = "blue") + geom_line(aes(x = chol, y = loess_fit)) + theme_minimal() + xlim(c(100, 400)) + ylim(c(-10, 0))

# test for nonlinearity
fit <- glm(chd69 ~ rcs(chol), data = wcgs.c, family = "binomial")
coef_length <- length(fit$coef)
hypotheses(fit, joint = c(1:coef_length)[grep("rcs", names(fit$coefficients))[-1]], hypotheses = rep(0, coef_length - 1))
wcgs.c$xb2 <- predict(fit, type = "link")
ggplot(data = wcgs.c, aes(x = chol, y = xb)) + 
  geom_line(col = "blue") + geom_line(aes(x = chol, y = loess_fit)) + geom_line(aes(x = chol, y = xb2), col = "red") + 
  theme_minimal() + xlim(c(100, 400)) + ylim(c(-10, 0))

### relationship between smoking (Y/N) and CHD
# unadjusted association
tab <- table(wcgs$smoke, wcgs$chd69)
epi.2by2(tab[c(2:1),c(2:1)])
fit <- glm(chd69 ~ smoke, data = wcgs, family = "binomial")
jtools::summ(fit, confint = TRUE, digits = 3, exp = TRUE)

# adjusted for behavior pattern
mantelhaen.test(wcgs$chd69, wcgs$smoke, wcgs$dibpat) # R does not check homogeneity assumption
fit <- glm(chd69 ~ smoke + dibpat, data = wcgs, family = "binomial")
jtools::summ(fit, confint = TRUE, digits = 3, exp = TRUE)

# adjusted for behavior pattern, age
fit <- glm(chd69 ~ smoke + dibpat + age, data = wcgs, family = "binomial")
jtools::summ(fit, confint = TRUE, digits = 3, exp = TRUE)


