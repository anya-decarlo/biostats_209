# Load the necessary libraries

install.packages("jtools")
install.packages("readstata13")
install.packages("psych")
install.packages("epiDisplay")
install.packages("ggplot2")
install.packages("marginaleffects")
install.packages("boot")

library("jtools")
library("readstata13")
library("psych")
library("epiDisplay")
library("ggplot2")
library("marginaleffects")
library("boot")
library("fastDummies")

data <- read.dta13("~/Downloads/BIOS 208 2024/labs/lab3/lab3.dta")

# Find mean BMI by physical activity

group_stats <- data %>% group_by(physact) %>%
  summarise(mean_values = mean(bmi, na.rm = TRUE),
            sd_values = sd(bmi, na.rm = TRUE), count = n())
print(group_stats)

# Regress physact on BMI
model <- lm(bmi ~ physact, data = data)
jtools::summ(model, confint = TRUE, digits = 3)
  
# Compute mean BMI at each level of physact
hypothesesmodel, "`(Intercept)` = 0")(
hypotheses(model, "`(Intercept)` + `physactsomewhat less active` = 0")
hypotheses(model, "`(Intercept)` + `physactabout as active` = 0")
hypotheses(model, "`(Intercept)` + `physactsomewhat more active` = 0")
hypotheses(model, "`(Intercept)` + `physactmuch more active` = 0")

# Margins command
predictions(model, by = "physact",
            newdata = datagrid(physact = levels(data$physact), grid_type = "counterfactual"))

# Test for heterogeneity (equivalent to testparm)
hypotheses(model, joint = "physact")

# Test for linear trend and departure from linear trend
data$physact.cont <- data$physact
contrasts(data$physact.cont) <- contr.poly(nlevels(data$physact))
model <- lm(bmi ~ physact.cont, data = data)
jtools::summ(model, confint = TRUE, digits = 3)

# repeat with adjustment for age, smoking, and alcohol use
model <- lm(bmi ~ physact + age10 + smoking + drnkspwk, data = data)

# compute marginal mean BMI at each level of physact, averaging over other predictors
predictions(model, by = "physact",
            newdata = datagrid(physact = levels(data$physact), grid_type = "counterfactual"))

# Confirm marginal means
y1 <- model$coef["(Intercept)"] + model$coef["physactsomewhat less active"] * 0 + model$coef["physactabout as active"] * 0  +
      model$coef["physactsomewhat more active"] * 0 + model$coef["physactmuch more active"] * 0 +
      model$coef["age10"] * data$age10 + model$coef["smokingyes"] * (as.numeric(data$smoking) - 1) +
      model$coef["drnkspwk"] * data$drnkspwk

y2 <- model$coef["(Intercept)"] + model$coef["physactsomewhat less active"] * 1 + model$coef["physactabout as active"] * 0  +
  model$coef["physactsomewhat more active"] * 0 + model$coef["physactmuch more active"] * 0 +
  model$coef["age10"] * data$age10 + model$coef["smokingyes"] * (as.numeric(data$smoking) - 1) +
  model$coef["drnkspwk"] * data$drnkspwk

y3 <- model$coef["(Intercept)"] + model$coef["physactsomewhat less active"] * 0 + model$coef["physactabout as active"] * 1  +
  model$coef["physactsomewhat more active"] * 0 + model$coef["physactmuch more active"] * 0 +
  model$coef["age10"] * data$age10 + model$coef["smokingyes"] * (as.numeric(data$smoking) - 1) +
  model$coef["drnkspwk"] * data$drnkspwk

y4 <- model$coef["(Intercept)"] + model$coef["physactsomewhat less active"] * 0 + model$coef["physactabout as active"] * 0  +
  model$coef["physactsomewhat more active"] * 1 + model$coef["physactmuch more active"] * 0 +
  model$coef["age10"] * data$age10 + model$coef["smokingyes"] * (as.numeric(data$smoking) - 1) +
  model$coef["drnkspwk"] * data$drnkspwk

y5 <- model$coef["(Intercept)"] + model$coef["physactsomewhat less active"] * 0 + model$coef["physactabout as active"] * 0  +
  model$coef["physactsomewhat more active"] * 0 + model$coef["physactmuch more active"] * 1 +
  model$coef["age10"] * data$age10 + model$coef["smokingyes"] * (as.numeric(data$smoking) - 1) +
  model$coef["drnkspwk"] * data$drnkspwk

summ(y1)
summ(y2)
summ(y3)
summ(y4)
summ(y5)

# Find marginal differences (all pairwise)
avg_slopes(model, variables = "physact", by = "physact",
           newdata = datagrid(physact = levels(data$physact), grid_type = "counterfactual"))

# Find marginal differences (focus on contrasting first level vs rest)
avg_slopes(model, variables = "physact", by = "physact",
           newdata = datagrid(physact = levels(data$physact)[1], grid_type = "counterfactual"))


# Compute marginal mean BMI at each level of physact, with other predictors fixed at mean values (or modes for binary/categorical predictors)
predictions(model, by = "physact", newdata = "mean")

# Verifiy results from last command first generate mean values for predictors in estimation sample
z1 <- model$coef["(Intercept)"] + model$coef["physactsomewhat less active"] * 0 + model$coef["physactabout as active"] * 0  +
  model$coef["physactsomewhat more active"] * 0 + model$coef["physactmuch more active"] * 0 +
  model$coef["age10"] * mean(data$age10) + model$coef["smokingyes"] * 0 +
  model$coef["drnkspwk"] * mean(data$drnkspwk)

z2 <- model$coef["(Intercept)"] + model$coef["physactsomewhat less active"] * 1 + model$coef["physactabout as active"] * 0  +
  model$coef["physactsomewhat more active"] * 0 + model$coef["physactmuch more active"] * 0 +
  model$coef["age10"] * mean(data$age10) + model$coef["smokingyes"] * 0 +
  model$coef["drnkspwk"] * mean(data$drnkspwk)

z3 <- model$coef["(Intercept)"] + model$coef["physactsomewhat less active"] * 0 + model$coef["physactabout as active"] * 1  +
  model$coef["physactsomewhat more active"] * 0 + model$coef["physactmuch more active"] * 0 +
  model$coef["age10"] * mean(data$age10) + model$coef["smokingyes"] * 0 +
  model$coef["drnkspwk"] * mean(data$drnkspwk)

z4 <- model$coef["(Intercept)"] + model$coef["physactsomewhat less active"] * 0 + model$coef["physactabout as active"] * 0  +
  model$coef["physactsomewhat more active"] * 1 + model$coef["physactmuch more active"] * 0 +
  model$coef["age10"] * mean(data$age10) + model$coef["smokingyes"] * 0 +
  model$coef["drnkspwk"] * mean(data$drnkspwk)

z5 <- model$coef["(Intercept)"] + model$coef["physactsomewhat less active"] * 0 + model$coef["physactabout as active"] * 0  +
  model$coef["physactsomewhat more active"] * 0 + model$coef["physactmuch more active"] * 1 +
  model$coef["age10"] * mean(data$age10) + model$coef["smokingyes"] * 0 +
  model$coef["drnkspwk"] * mean(data$drnkspwk)

summ(z1)
summ(z2)
summ(z3)
summ(z4)
summ(z5)

# Test for heterogeneity (equivalent to testparm)
hypotheses(model, joint = "physact")

# Test for linear trend and departure from linear trend
data$physact.cont <- data$physact
contrasts(data$physact.cont) <- contr.poly(nlevels(data$physact))
model <- lm(bmi ~ physact.cont + age10 + smoking + drnkspwk, data = data)
jtools::summ(model, confint = TRUE, digits = 3)

# Models using log of creatinine level as outcome
plot(density(data$creat, na.rm = TRUE))

fit <- glm(lncreat ~ bmi + age, data = data, family = "gaussian")
jtools::summ(fit, confint = TRUE, digits = 3, exp = FALSE)
jtools::summ(fit, confint = TRUE, digits = 3, exp = TRUE)
hypotheses(fit,"100 * (exp(bmi) - 1) = 0")
fit <- glm(sbp ~ lncreat + age + diabetes, data = data, family = "gaussian")
jtools::summ(fit, confint = TRUE, digits = 3)
hypotheses(fit, "lncreat * log(1.25) = 0")


