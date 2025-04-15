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

wcgs <- read.dta13("/Users/aaron.scheffler/Downloads/BIOS 208 2025/lectures/lecture9/wcgs.dta")

### ASK QUESTIONS AT EACH POINT


############################################################################
# objective: assess smoking effect on SBP
fit <- lm(sbp ~ age + bmi + smoke + dibpat, data = wcgs)
jtools::summ(fit, confint = TRUE, digits = 3)

############################################################################
# assess nonlinearity in age and BMI

# residual vs predictor
wcgs$resid <- fit$residuals

rvpPlot(wcgs, "age")
rvpPlot(wcgs, "bmi")

# component + residual
crPlots(fit, terms = "age", main = "Conditional Partial Residual Plot with LOWESS")
crPlots(fit, terms = "bmi", main = "Conditional Partial Residual Plot with LOWESS")

# test nonlinearity in age and BMI via restricted cubic splines
fit <- lm(sbp ~ rcs(age) + rcs(bmi) + smoke + dibpat, data = wcgs)
jtools::summ(fit, confint = TRUE, digits = 3)

coef_length <- length(fit$coef)
hypotheses(fit, joint = c(1:coef_length)[grep("rcs\\(age\\)", names(fit$coefficients))][-1], hypotheses = rep(0, coef_length))
hypotheses(fit, joint = c(1:coef_length)[grep("rcs\\(bmi\\)", names(fit$coefficients))[-1]], hypotheses = rep(0, coef_length - 1))

# candidate model
fit <- lm(sbp ~ age + rcs(bmi) + smoke + dibpat, data = wcgs)
jtools::summ(fit, confint = TRUE, digits = 3)

# marginal smoking effect
avg_slopes(fit, variable = "smoke")

############################################################################
#assess normality of residuals
wcgs$residuals <- fit$residuals

residual_plot <- function(data){ # requires residuals to be named as such in dataframe
  g1 <- ggplot(data, aes(x = residuals)) +
    geom_histogram(color = "black") + labs(title = "Histogram Plot of Residuals", x = "Residuals", y = "Density") + theme_minimal()
  g2 <- ggplot(data, aes(y = residuals)) +
    geom_boxplot(color = "black") + labs(title = "Boxplot of Residuals", x = "Residuals", y = "Density") + theme_minimal()
  g3 <- ggplot(data, aes(x = residuals)) +
    geom_density(color = "red") + stat_function(fun = dnorm, args = list(mean = mean(data$residuals, na.rm = TRUE), sd = sd(data$residuals, na.rm = TRUE))) +
    labs(title = "Kernel Density Plot of Residuals", x = "Residuals", y = "Density") + theme_minimal()
  g4 <- ggplot(data, aes(sample = residuals)) + stat_qq() + stat_qq_line() + labs(title = "QQ Plot of Residuals") + theme_minimal()
  return(grid.arrange(g1, g2, g3, g4, nrow = 2))}

residual_plot(wcgs)

gladder(wcgs$sbp)

wcgs$invsbp <- 1/wcgs$sbp^2

fit <- lm(invsbp ~ age + rcs(bmi) + smoke + dibpat, data = wcgs)
jtools::summ(fit, confint = TRUE, digits = 3)

hypotheses(fit, joint = c(1:coef_length)[grep("rcs\\(bmi\\)", names(fit$coefficients))[-1]], hypotheses = rep(0, coef_length - 1))

# residual vs predictor plots
wcgs$residuals <- fit$residuals
residual_plot(wcgs)

# marginal smoking effect - original & inverse square scales
avg_predictions(fit, variable = "smoke", 
                newdata = datagrid(smoke = levels(wcgs$smoke), grid_type = "counterfactual"))

fit <- lm(sbp ~ age + rcs(bmi) + smoke + dibpat, data = wcgs)
avg_slopes(fit, variable = "smoke",  
                newdata = datagrid(smoke = levels(wcgs$smoke), grid_type = "counterfactual"))


#############################################################################
# assess equal variance assumption

wcgs$residuals <- fit$residuals
wcgs$fit <- predict(fit)
ggplot(wcgs, aes( x = fit, y = residuals)) + geom_point(alpha = .15) + geom_hline(yintercept = 0)

describeBy(wcgs$residuals, group = wcgs$smoke)
describeBy(wcgs$residuals, group = wcgs$dibpat)

fit <- lm_robust(sbp ~ age + rcs(bmi) + smoke + dibpat, data = wcgs, se_type = "HC3")
avg_slopes(fit, variable = "smoke",  
           newdata = datagrid(smoke = levels(wcgs$smoke), grid_type = "counterfactual"))



############################################################################
# influential observations
fit <- lm(sbp ~ age + rcs(bmi) + smoke + dibpat, data = wcgs)
dfb <- dfbetas(fit)
boxplot(dfb)

sorted_indices <- order(dfb, decreasing = TRUE)
top_10_indices <- sorted_indices[1:min(10, length(dfb))]
top_10_coords <- arrayInd(top_10_indices, .dim = dim(dfb))
print(cbind(dfb[top_10_indices], wcgs[top_10_coords[,1], c("id", "bmi", "age", "smoke", "dibpat")]))

# omitting outlier on BMI (with/without robust variances)
fit <- lm(sbp ~ age + rcs(bmi) + smoke + dibpat, data = wcgs[-which(wcgs$id == 12453), ])
avg_slopes(fit, variable = "smoke",  
           newdata = datagrid(smoke = levels(wcgs$smoke), grid_type = "counterfactual"))


fit <- lm_robust(sbp ~ age + rcs(bmi) + smoke + dibpat, data = wcgs[-which(wcgs$id == 12453), ], se_type = "HC3")
avg_slopes(fit, variable = "smoke",  
           newdata = datagrid(smoke = levels(wcgs$smoke), grid_type = "counterfactual"))




