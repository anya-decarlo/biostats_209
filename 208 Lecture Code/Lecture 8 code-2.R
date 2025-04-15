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

cdc <- read.dta13("/Users/aaron.scheffler/Downloads/BIOS 208 2025/lectures/lecture8/cdc.dta")
tab1(cdc$hivp, cdc$aids)


wcgs <- read.dta13("/Users/aaron.scheffler/Downloads/BIOS 208 2025/lectures/lecture8/wcgs.dta")
tab <- table(wcgs$arcus, wcgs$chd69)
epi.2by2(tab[c(2:1),c(2:1)])


tab <- table(cdc$aids, cdc$hivp)
epi.2by2(tab[c(2:1),c(2:1)])

wcgs$agce <- cut(wcgs$age, breaks = c(34, 40, 45, 50, 55, 60), 
             labels = c("35-40", "41-45", "46-50", "51-55", "56-60"), right = TRUE)
tab <-table(wcgs$chd69, wcgs$agec)
# Calculate column percentages
col_percentages <- prop.table(tab, margin = 2) * 100
print(addmargins(col_percentages)) # proportions
(col_percentages[1,1]/col_percentages[2,1])/(col_percentages[1,]/col_percentages[2,]) # odds ratios
# Test of homogeneity
prop_test(tab)
# Test for trend
prop_trend_test(tab)

fit <- glm(chd69 ~ arcus, data = wcgs, family = "binomial")
jtools::summ(fit, confint = TRUE, digits = 3, exp = TRUE)
logik(fit)
jtools::summ(fit, confint = TRUE, digits = 3)
print(exp(.2287286))
print(exp(.7549002))

fit <- glm(chd69 ~ 1, data = wcgs[which(complete.cases(wcgs$arc))], family = "binomial")
jtools::summ(fit, confint = TRUE, digits = 3)
logLik(fit)

fit <- glm(chd69 ~ chol, data = wcgs, family = "binomial")
jtools::summ(fit, confint = TRUE, digits = 3)

wcgs$lchol <- log(wcgs$chol)
fit <- glm(chd69 ~ lchol, data = wcgs, family = "binomial")
jtools::summ(fit, confint = TRUE, digits = 3)
print(exp(fit$coefficients["lchol"]))
hypotheses(fit, "lchol * log(1.2) = 0")
hypotheses(fit, "exp(lchol * log(1.2)) = 0")

fit <- glm(chd69 ~ behpat, data = wcgs, family = "binomial")
jtools::summ(fit, confint = TRUE, digits = 3, exp = TRUE)
coef_length <- length(fit$coef)
hypotheses(fit, joint = c(1:coef_length)[grep("behpat", names(fit$coefficients))], hypotheses = rep(0, coef_length), joint_test = "chisq")

fit <- glm(chd69 ~ dibpat, data = wcgs, family = "binomial")
jtools::summ(fit, confint = TRUE, digits = 3, exp = TRUE)

fit <- glm(chd69 ~ agec, data = wcgs, family = "binomial")
jtools::summ(fit, confint = TRUE, digits = 3, exp = TRUE)

fit <- glm(chd69 ~ dibpat + smoke, data = wcgs, family = "binomial")
jtools::summ(fit, confint = TRUE, digits = 3, exp = TRUE)

fit <- glm(chd69 ~ chol + sbp, data = wcgs[which(wcgs$chol < 600), ], family = "binomial")
jtools::summ(fit, confint = TRUE, digits = 3, exp = TRUE)
jtools::summ(fit, confint = TRUE, digits = 3)

wcgs$chol_25 <- (wcgs$chol - mean(wcgs$chol, na.rm = TRUE))/25
wcgs$sbp_25 <- (wcgs$sbp - mean(wcgs$sbp, na.rm = TRUE))/25
fit <- glm(chd69 ~ chol_25 + sbp_25, data = wcgs[which(wcgs$chol < 600), ], family = "binomial")
jtools::summ(fit, confint = TRUE, digits = 3, exp = TRUE)

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
}
wcgs.c <- na.omit(wcgs[,c("chd69", "chol")])
logitloess(as.numeric(wcgs.c$chd69) - 1, wcgs.c$chol, s = 0.25)

fit <- glm(chd69 ~ rcs(chol) * smoke, data = wcgs[which(wcgs$chol < 600), ], family = "binomial", na.action = na.exclude)
wcgs$xb[which(wcgs$chol < 600)] <- predict(fit, type = "link")
ggplot(data = wcgs, aes(x = chol, y = xb, color = factor(smoke))) + geom_line(linewidth = 2) + theme_minimal() + xlim(c(100, 400))

