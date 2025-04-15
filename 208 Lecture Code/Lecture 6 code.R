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

rvpPlot <- function(data, predictor) {
  g <- ggplot(data = data, aes_string(x = predictor, y = "resid")) +
    geom_point(alpha = 0.15) +
    geom_smooth(method = "loess", se = FALSE, color = "blue", size = 1.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1.5) + theme_minimal()
  return(g)}

bmd <- read.dta13("/Users/aaron.scheffler/Downloads/BIOS 208 2025/labs/lab6/lab6.dta")
fit <- lm(bmd ~ age + bmi, data = bmd, na.action = na.exclude)
bmd$resid <- fit$residuals
rvpPlot(bmd, "bmi")
crPlots(fit, terms = "bmi", main = "Conditional Partial Residual Plot with LOWESS")

bmd$bmicat <- factor(cut(bmd$bmi, 
              breaks = c(-Inf, 25, 35, Inf), 
              labels = c(1, 2, 3), 
              right = FALSE))
fit <- lm(bmd ~ bmicat, data = bmd)
jtools::summ(fit, confint = TRUE, digits = 3)
bmd$xb <- fit$fitted.values
ggplot(data = bmd, aes(x = bmi, y = bmd)) + geom_point(size = 0.25) + 
  geom_smooth(method = "loess", se = FALSE, color = "blue", linewidth = 1.5) +
  geom_line(aes(x = bmi, y = xb), col = "red", linewidth = 1.5) + theme_minimal()

bmd$bmicat <- factor(cut(bmd$bmi, 
                         breaks = c(-Inf,18.5, 25, 30, 35, Inf), 
                         labels = c(1, 2, 3, 4, 5), 
                         right = FALSE))
fit <- lm(bmd ~ bmicat, data = bmd)
jtools::summ(fit, confint = TRUE, digits = 3)
bmd$xb <- fit$fitted.values
ggplot(data = bmd, aes(x = bmi, y = bmd)) + geom_point(size = 0.25) + 
  geom_smooth(method = "loess", se = FALSE, color = "blue", linewidth = 1.5) +
  geom_line(aes(x = bmi, y = xb), col = "red", linewidth = 1.5) + theme_minimal()




# linear basis
l_basis <- data.frame(lspline(bmd$bmi, knots = c(18.5, 25, 30, 35), marginal = FALSE))
names(l_basis) <-  paste("bmi", 1:5, sep = "")
bmd <- cbind(bmd, l_basis)
form <- as.formula(paste("bmd ~ ", paste(names(l_basis), collapse= "+")))
fit <- lm(form, data = bmd)
jtools::summ(fit, confint = TRUE, digits = 3)
coef_length <- length(fit$coef)
hypotheses(fit, joint = c(1:coef_length)[grep("bmi", names(fit$coefficients))[-1]], hypotheses = rep(0, coef_length - 1))

bmd <- read.dta13("/Users/aaron.scheffler/Downloads/BIOS 208 2025/labs/lab6/lab6.dta")
l_basis <- data.frame(lspline(bmd$bmi, knots = c(18.5, 25, 30, 35), marginal = TRUE))
names(l_basis) <-  paste("bmi", 1:5, sep = "")
bmd <- cbind(bmd, l_basis)
form <- as.formula(paste("bmd ~ ", paste(names(l_basis), collapse= "+")))
fit <- lm(form, data = bmd)
jtools::summ(fit, confint = TRUE, digits = 3)
coef_length <- length(fit$coef)
hypotheses(fit, joint = c(1:coef_length)[grep("bmi", names(fit$coefficients))[-1]], hypotheses = rep(0, coef_length - 1))


# restricted cubic spline

fit <- lm(bmd ~ rcs(bmi), data = bmd)
jtools::summ(fit, confint = TRUE, digits = 3)
coef_length <- length(fit$coef)
hypotheses(fit, joint = c(1:coef_length)[grep("rcs", names(fit$coefficients))], hypotheses = rep(0, coef_length))
hypotheses(fit, joint = c(1:coef_length)[grep("rcs", names(fit$coefficients))[-1]], hypotheses = rep(0, coef_length - 1))

# hers data
hers <- read.dta13("/Users/aaron.scheffler/Downloads/BIOS 208 2025/lectures/lecture2/hersdata.dta")
fit <- lm(LDL ~ BMI + age + nonwhite + smoking + drinkany, data = hers, na.action = na.exclude)
jtools::summ(fit, confint = TRUE, digits = 3)
# save residuals in new variable
hers$residuals <- residuals(fit)

residual_plot <- function(data){ # requires residuals to be named as such in dataframe
g1 <- ggplot(data, aes(x = residuals)) +
  geom_histogram(color = "black") + labs(title = "Histogram Plot of Residuals", x = "Residuals", y = "Density") + theme_minimal()
g2 <- ggplot(data, aes(y = residuals)) +
  geom_boxplot(color = "black") + labs(title = "Boxplot of Residuals", x = "Residuals", y = "Density") + theme_minimal()
g3 <- ggplot(data, aes(x = residuals)) +
 geom_density(color = "red") + stat_function(fun = dnorm, args = list(mean = mean(data$residuals, na.rm = TRUE), sd = sd($residuals, na.rm = TRUE))) +
  labs(title = "Kernel Density Plot of Residuals", x = "Residuals", y = "Density") + theme_minimal()
g4 <- ggplot(data, aes(sample = residuals)) + stat_qq() + stat_qq_line() + labs(title = "QQ Plot of Residuals") + theme_minimal()
return(grid.arrange(g1, g2, g3, g4, nrow = 2))}

residual_plot(hers)

gladder(hers$LDL)


fit <- lm(log(LDL) ~ BMI + age + nonwhite + smoking + drinkany, data = hers, na.action = na.exclude)
hers$residuals <- residuals(fit)
residual_plot(hers)

# Boot
fit <- lm(LDL ~ BMI + age + nonwhite + smoking + drinkany, data = hers[complete.cases(hers), ]) # Boot requires fully observed data
betahat.boot <- Boot(fit, R = 1000)
summary(betahat.boot)  # default summary
confint(betahat.boot, type = c("norm")) 

fit <- lm(LDL ~ BMI + age + nonwhite + smoking + drinkany, data = hers[complete.cases(hers), ]) # Boot requires fully observed data
betahat.boot <- Boot(fit, R = 1000)
summary(betahat.boot)  # default summary
confint(betahat.boot, type = c("bca")) 

# Transformations
hers10 <- dplyr::slice_sample(hers, prop = .1) 
fit <- lm(glucose ~ diabetes + physact + age + raceth + smoking + drinkany, data = hers10, na.action = na.exclude)
hers10$residuals <- residuals(fit)
plot(fit, which = 1)


describeBy(hers10$residuals, group = hers10$physact)
describeBy(hers10$residuals, group = hers10$diabetes)

hers10$sqrt_glucose <- sqrt(hers10$glucose)
fit <- lm(sqrt_glucose ~ diabetes + physact + age + raceth + smoking + drinkany, data = hers10, na.action = na.exclude)
plot(fit, which = 1)

# Robust SE
fit <- lm_robust(glucose ~ diabetes + physact + age + raceth + smoking + drinkany, data = hers, se_type = "HC1")
summary(fit)
fit <- lm_robust(glucose ~ diabetes + physact + age + raceth + smoking + drinkany, data = hers, se_type = "HC3")
summary(fit)

# Dfbeta
fit <- lm(LDL ~ BMI + age + nonwhite + smoking + drinkany, data = hers)
dfb <- dfbetas(fit)
boxplot(dfb)
sorted_indices <- order(dfb, decreasing = TRUE)
top_10_indices <- sorted_indices[1:min(10, length(dfb))]
top_10_coords <- arrayInd(top_10_coords[,1], .dim = dim(dfb))
print(cbind(dfb[top_10_indices), hers[top_10_coords, c("BMI", "age", "nonwhite", "smoking", "drinkany")]))

# Propensity score
# logistic model for statin use
fit <- glm(statins ~ rcs(age) + raceth + smoking*physact + diabetes, data = hers, family = "binomial", na.action = na.exclude)
# calculate logit propensity score
hers$xb <- fitted(fit)
# density plots of logit scores in statin users and non-users
ggplot(data = hers, aes(x = xb, color = statins)) + geom_density(linewidth = 2)