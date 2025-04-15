# Load the necessary libraries
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
install.packages("doBy")
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
library("doBy")

data <- read.dta13("~/Downloads/BIOS 208 2024/labs/lab6/lab6.dta")
setwd("~/Downloads/BIOS 208 2024/labs/lab6")

# univariate linearity
p <- ggplot(data, aes(x = age, y = bmd)) +
  geom_point() +  # Add points for the data
  geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Add LOWESS curve
  labs(title = "LOWESS Fit", x = "Age", y = "BMD")
print(p)
# Export the plot to a PDF file
ggsave("linearity.pdf", plot = p, device = "pdf", width = 8, height = 6, units = "in", dpi = 300)


#multivariate linearity
model <- lm(bmd ~ age + weight, data = data)
jtools::summ(model, confint = TRUE, digits = 3)
crPlots(model, terms = "weight", main = "Conditional Partial Residual Plot with LOWESS")


model <- lm(bmd ~ age + lweight, data = data)
cpr_plot <- crPlots(model, terms = "lweight", main = "Conditional Partial Residual Plot with LOWESS")
jtools::summ(model, confint = TRUE, digits = 3)
hypotheses(model, "lweight * log(1.1) = 0")

# normality of residuals
model <- lm(bmd ~ age + weight + weight2, data = data)
jtools::summ(model, confint = TRUE, digits = 3)
residuals <- resid(model)
plot_data <- data.frame(weight = data$weight, resid = residuals)

# Create the residual versus predictor plot with LOWESS curve using ggplot2
rvpPlot <- function(data, predictor, resid) {
  g <- ggplot(data = data, aes_string(x = predictor, y = "resid")) +
    geom_point(alpha = 0.25) +
    geom_smooth(method = "loess", se = FALSE, color = "red", size = 1.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1.5) + theme_minimal()
  return(g)}


rvpPlot(plot_data, "weight")

qqnorm(residuals)
qqline(residuals)

ggplot(data.frame(residuals), aes(x = residuals)) +
  geom_density(fill = "skyblue", color = "black") + 
  stat_function(fun = dnorm, args = list(mean = mean(residuals), sd = sd(residuals)))
  labs(title = "Kernel Density Plot of Residuals", x = "Residuals", y = "Density")

# eeu

model <- lm(eeu ~ age + poorhlth + gaitspd, data = data)
jtools::summ(model, confint = TRUE, digits = 3)
residuals <- resid(model)

qqnorm(residuals)
qqline(residuals)

ggplot(data.frame(residuals), aes(x = residuals)) +
  geom_density(fill = "skyblue", color = "black") + 
  stat_function(fun = dnorm, args = list(mean = mean(residuals), sd = sd(residuals)))
labs(title = "Kernel Density Plot of Residuals", x = "Residuals", y = "Density")

data$eeu <- data$eeu + .01
gladder(data$eeu)

# log transform eeu 
data$lneeu <- log(data$eeu)
model <- lm(lneeu ~ age + poorhlth + gaitspd, data = data, na.action = na.exclude)
jtools::summ(model, confint = TRUE, digits = 3)
residuals <- resid(model)

qqnorm(residuals)
qqline(residuals)

ggplot(data.frame(residuals), aes(x = residuals)) +
  geom_density(fill = "skyblue", color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(residuals, na.rm = TRUE), sd = sd(residuals, na.rm = TRUE))) +
  labs(title = "Kernel Density Plot of Residuals", x = "Residuals", y = "Density")

# Constant variance

# Residuals vs. Fitted plot
plot(model, which = 1) 

# Residuals vs. Predictor plot for 'age' and 'gaitspd'
residuals <- resid(model)
data$resid<- residuals

# Create the residual versus predictor plot with LOWESS curve using ggplot2
rvpPlot(data, "age")
rvpPlot(data, "gaitspd")

# Create a table for 'poorhlth' showing count and standard deviation of residuals(model)
describeBy(data$resid, data$poorhlth)

# Influential points

model <- lm(bmd ~ age + lweight, data = data)
jtools::summ(model, confint = TRUE, digits = 3)

dfb <- data.frame(dfbetas(model))
dfb_data <- data.frame(
  Variable = rep(c("age", "log(weight)"), each = nrow(dfb)),
  DFBETA = c(dfb[, 2], dfb[, 3])
)

ggplot(dfb_data, aes(x = Variable, y = DFBETA)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Standardized DFBETAs", x = "Variable", y = "DFBETA Values")

data[which(abs(dfb$age)>0.2 ),c("bmd", "age", "lweight")]

data[which(abs(dfb$lweight)>0.2 ),c("bmd", "age", "lweight")]

model <- lm(bmd ~ age + lweight, data = data[-c(which(abs(dfb$age)>0.2 ), which(abs(dfb$lweight)>0.2 )),])
jtools::summ(model, confint = TRUE, digits = 3)

# Covariate overlap

# Recode 'estrogen' and create 'ht_current' variable
data$ht_current <- ifelse(data$estrogen == "current", 1, 0)
# Label values
data$ht_current <- factor(data$ht_current, levels = c(0, 1), labels = c("HT: No", "HT: Yes"))
# Tabulate 'estrogen' and 'ht_current'
table(data$estrogen, data$ht_current)

# Summary statistics for numerical variables by 'ht_current'
data.complete <- data[complete.cases(data),]
for (x in c("age", "bmi", "gaitspd", "has10")) {
  print(summaryBy(data = data.complete, formula = as.formula(paste(x, "~ ht_current")),
                  FUN = list(mean = mean, p5 = function(x) quantile(x, probs = 0.05),
                             min = min, p95 = function(x) quantile(x, probs = 0.95),
                             max = max), fun.names = c("mean", "p5", "min", "p95", "max")))
}

# Crosstabulation for categorical variables by 'ht_current'
for (x in c("usearms", "poorhlth", "calsupp", "etid", "nsfx2")) {
  print(x)
  print(table(data$ht_current, data[[x]]))
}


# Perform logistic regression
logistic_model <- glm(ht_current ~ rcs(data$age) + calsupp, data = data, family = "binomial")
jtools::summ(logistic_model, confint = TRUE, digits = 3, exp = TRUE)
# Predict logit propensity scores
data$logit_pscore <- predict(logistic_model) # log odds
# Plot density of logit propensity scores for estrogen users and non-users
ggplot(data = data, aes(x = logit_pscore, color = factor(ht_current))) + geom_density(bw = 0.25)

# Optional: linear and restricted cubic splines

# Assuming your data frame is named 'data'

# Recode 'bmi' into categorical variable 'bmicat'
data$bmicat <- as.factor(cut(data$bmi, breaks = c(-Inf, 18.5001, 25.001, 30.001, 35.001, Inf), labels = c(1, 2, 3, 4, 5)))

### Fit a linear regression with categorical 'bmicat' 
model_cat <- lm(bmd ~ bmicat + age + estrogen, data = data)
jtools::summ(model_cat, confint = TRUE, digits = 3)

# Test for linear trend and departure from linear trend
data$bmicat.cont <- data$bmicat
contrasts(data$bmicat.cont) <- contr.poly(nlevels(data$bmicat))
model_cat <- lm(bmd ~ bmicat.cont + age + estrogen, data = data)
jtools::summ(model_cat, confint = TRUE, digits = 3)

# Linear spline fit for 'bmi'

### Fit a linear regression with spline terms
l_basis <- data.frame(lspline(data$bmi, knots = c(18.5, 25, 30, 35), marginal = TRUE)) # create linear spline basis
names(l_basis) <-  paste("bmi", 1:5, sep = "") # name the basis functions
data <- cbind(data, l_basis) # bind with existing dataframe
form <- as.formula(paste("bmd ~ ", paste(c(names(l_basis), "age", "estrogen"), collapse= "+"))) # construct formula
model_ls <- lm(form, data = data) 
jtools::summ(model_ls, confint = TRUE, digits = 3)

# Test for non-linearity
coef_length <- length(model_ls$coef)
spline_coef <- c(1:coef_length)[grep("bmi", names(model_ls$coefficients))][-1]
hypotheses(model_ls, joint = spline_coef, hypotheses = rep(0, length(spline_coef)))

### Restricted cubic splines for 'bmi'
model_cs <- lm(bmd ~ rcs(bmi) + age + estrogen, data = data) # restricted cubic splines
jtools::summ(model_cs, confint = TRUE, digits = 3)

# Test for overall effect of BMI
coef_length <- length(model_cs)
hypotheses(model_cs, joint = c(1:coef_length)[grep("rcs", names(model_cs$coefficients))], hypotheses = rep(0, coef_length))

# Test for non-linearity of BMI
hypotheses(model_cs, joint = c(1:coef_length)[grep("rcs", names(model_cs$coefficients))[-1]], hypotheses = rep(0, coef_length - 1))

# Create plot contrasting three fits
pred.data <- data
pred.data$age <- mean(data$age)
pred.data$estrogen <- "never"
pred.data$pred_cat <- predict(model_cat, newdata = pred.data)
pred.data$pred_ls <- predict(model_ls, newdata = pred.data)
pred.data$pred_cs <- predict(model_cs, newdata = pred.data)

ggplot(pred.data, aes(x = bmi, y = bmd)) +
  geom_line(aes(y = pred_cat), color = "blue", show.legend = TRUE) +
  geom_line(aes(y = pred_ls), color = "red", show.legend = TRUE) +
  geom_line(aes(y = pred_cs), color = "green", show.legend = TRUE) +
  geom_smooth(method = "loess", se = FALSE, color = "purple", show.legend = TRUE) +
  geom_point(alpha = .25) +
  labs(title = "Model Fit Comparison across BMI",
       x = "BMI", y = "BMD") + theme_minimal() + 
  scale_color_manual(values = c("blue", "red", "green", "purple"),
                     labels = c("Categorical", "Linear Splines", 
                                "Restricted Cubic Splines", "lowess"),
                     name = "Models")   # Styling guides globally
