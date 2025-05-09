###############################################
# Repeated Measures – Lab 2 (Answers Script)  #
# Author: Anya DeCarlo                       #
# Date:   2025-05-08                         #
###############################################


#### 1. Load packages ----
required_pkgs <- c(
  "tidyverse",   # data wrangling / visualisation
  "haven",       # read Stata .dta files
  "nlme",        # mixed-effects models (lme)
  "lme4",        # mixed-effects models (lmer)
  "geepack",     # Generalized Estimating Equations (GEE)
  "gtsummary",   # nice tables
  "ggplot2",     # plotting (loaded with tidyverse)
  "survival",     # survival objects if needed later
  "readstata13"
)

invisible(lapply(required_pkgs, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg)
  library(pkg, character.only = TRUE)
}))

#### 2. Import data ----
# NOTE: adjust the path below if your working directory differs
fec <- read.dta13("~/biostats_209/Data/fecfat.dta", convert.factors = TRUE)

# Peek at the structure
head(fec)

## Make sure pilltype is is categorical variable
fec$pilltype <- as.factor(fec$pilltype)
## Two approaches for mixed-effects model


## The fecal fat data can also be analyzed using a two-way ANOVA, with factors of pilltype and patient.  This can be performed using regression (as we learned in Biostat 208).  Use the regression command to do this:

# Using nlme package
library(nlme)
mixed_fit1 <- lme(fecfat ~ pilltype, random = ~1|patid, data=fec)
summary(mixed_fit1)

# Or using lme4 package
library(lme4)
mixed_fit2 <- lmer(fecfat ~ pilltype + (1|patid), data=fec)
summary(mixed_fit2)

## Run two way ANOVA
anova(two_way_fit)

# Two-way ANOVA using regression approach
two_way_fit <- lm(fecfat ~ pilltype + factor(patid), data=fec)
summary(two_way_fit)

# Get the ANOVA table
anova(two_way_fit)


#### OAI Data Analysis ----
# Load OAI data
library(data.table)
oai_thru_18m <- read.dta13("~/biostats_209/Data/oai_thru_18_months.dta", convert.factors = TRUE)
# Convert to data.table for easier summarization
setDT(oai_thru_18m)

# Check dataset structure
str(oai_thru_18m)
names(oai_thru_18m)
head(oai_thru_18m)


# Create summary statistics by sex and visit
tab_oai <- oai_thru_18m[, .(mean_womac = mean(womac_pain, na.rm = TRUE), 
                            N = sum(!is.na(womac_pain))), 
                         by = .(sex, visit)]
print(tab_oai)


male_mean <- tab_oai[sex==1]
female_mean <- tab_oai[sex==2]


ggplot() + 
geom_line(data = male_mean, aes(x = visit, y = mean_womac), color = "blue") +
geom_line(data = female_mean, aes(x = visit, y = mean_womac), color = "red") +
xlab("visit") +
ylab("mean") +
ggtitle("Mean by visit and sex")

womac_mix_fit <- lmer(womac_pain ~ factor(sex)+factor(visit)+factor(sex)*factor(visit)+(1|id) ,data=oai_thru_18m)
summary(womac_mix_fit)
anova(womac_mix_fit)

# Interpret sex by visit interaction at 12 months
# Create data for predictions at 12 months
newdata_12mo <- expand.grid(
  sex = c(1, 2),  # 1=male, 2=female
  visit = 12      # 12 months
)

# Get predicted values using womac_mix_fit
preds_12mo <- predict(womac_mix_fit, newdata=newdata_12mo, re.form=~0)

# Calculate the sex difference at 12 months
sex_diff_12mo <- preds_12mo[2] - preds_12mo[1]  # Female - Male difference

# Display results
print(newdata_12mo)
print(paste("Predicted values at 12 months:", 
            "Male =", round(preds_12mo[1], 2),
            "Female =", round(preds_12mo[2], 2)))
print(paste("Sex difference at 12 months:", round(sex_diff_12mo, 2)))

# Residual analysis for WOMAC mixed model ----
# Extract residuals from the womac_mix_fit model
womac_resid <- residuals(womac_mix_fit)
womac_fitted <- fitted(womac_mix_fit)

# Create diagnostic plots in a 2x2 layout
par(mfrow=c(2,2))

# Plot 1: Residuals vs Fitted
plot(womac_fitted, womac_resid, main="Residuals vs Fitted Values",
     xlab="Fitted values", ylab="Residuals")
abline(h=0, lty=2)
lines(lowess(womac_fitted, womac_resid), col="red")

# Plot 2: Normal Q-Q plot of residuals
qqnorm(womac_resid, main="Normal Q-Q Plot of Residuals")
qqline(womac_resid)

# Plot 3: Scale-Location plot (sqrt|residuals| vs fitted)
sqrt_abs_resid <- sqrt(abs(womac_resid))
plot(womac_fitted, sqrt_abs_resid, main="Scale-Location Plot",
     xlab="Fitted values", ylab="√|Residuals|")
lines(lowess(womac_fitted, sqrt_abs_resid), col="red")

# Plot 4: Residuals by sex
boxplot(womac_resid ~ oai_thru_18m$sex, main="Residuals by Sex",
        xlab="Sex", ylab="Residuals")

# Return to single plot layout
par(mfrow=c(1,1))

# Additional diagnostics: residuals by visit
boxplot(womac_resid ~ oai_thru_18m$visit, main="Residuals by Visit",
        xlab="Visit", ylab="Residuals")

# Check the random effects normality
ranef_vals <- ranef(womac_mix_fit)$id
qqnorm(ranef_vals[[1]], main="Random Effects Q-Q Plot")
qqline(ranef_vals[[1]])

# Evaluate model fit: observed vs fitted
plot(oai_thru_18m$womac_pain, womac_fitted, 
     xlab="Observed Pain Scores", ylab="Fitted Values",
     main="Observed vs Fitted Pain Scores")
abline(0, 1, lty=2, col="red")


# Mixed model to test for sex differences in pain trajectory
library(nlme)
pain_model1 <- lme(maxpain ~ visit, random = ~1|id, data=oai_thru_18m)
pain_model2 <- lme(maxpain ~ sex * visit, random = ~1|id, data=oai_thru_18m)

# Likelihood ratio test
anova_result <- anova(pain_model1, pain_model2)
print(anova_result)

# Calculate p-value from likelihood ratio test
# Take the F-value (which is 6.2574 in this example) and multiply by df
# Then use chi-square distribution with df degrees of freedom
F_value <- 6.2574  # Replace with actual F from your output
df <- 3            # Replace with actual df from your "npar" column
p_val <- 1-pchisq(df*F_value, df)
print(paste("P-value for sex effect:", p_val))

## Load the backpain data 
backpain <- read.dta13("~/biostats_209/Data/backpain.dta", convert.factors = TRUE)

str(backpain)

logistic_fit <- 
glm(undrstnd ~ age+factor(educ), data=backpain, family="binomial")
summary(logistic_fit)

gee_fit <- geeglm(undrstnd ~ age + factor(educ), 
                 data = backpain, 
                 id = doctor, 
                 family = binomial(link = "logit"), 
                 corstr = "exchangeable")
summary(gee_fit)



gee_fit_1 <- geeglm(undrstnd ~ pracstyl + age + thoraic + factor(educ), 
                 data = backpain, 
                 id = doctor, 
                 family = binomial(link = "logit"), 
                 corstr = "exchangeable")
summary(gee_fit_1)

## load the georgias babies dataset 
gb <- read.dta13("~/biostats_209/Data/GAbabies.dta", convert.factors = TRUE)
str(gb)

gee_fit_1 <- geeglm(bweight ~ birthord + initage, 
                 data = gb, 
                 id = momid, 
                 family = gaussian(), 
                 corstr = "exchangeable")
summary(gee_fit_1)

gab_regr <- glm(bweight ~ birthord + initage, data=gb)
 
gee_fit_2 <- geeglm(bweight ~ birthord + initage, 
                 data = gb, 
                 id = momid, 
                 family = gaussian(), 
                 corstr = "independence")
summary(gee_fit_2)

# Compare SEs for birthord across all fits
# (Assume you have three models: gab_regr, gee_fit_1, gee_fit_2)

# Helper function to extract SEs for birthord from a model summary
get_birthord_se <- function(model, type = c("glm", "gee")) {
  type <- match.arg(type)
  if(type == "glm") {
    se <- summary(model)$coefficients[grep("birthord", rownames(summary(model)$coefficients)), 2]
  } else {
    se <- summary(model)$coefficients[grep("birthord", rownames(summary(model)$coefficients)), "Std.err"]
  }
  return(se)
}

se_table <- data.frame(
  Model = c("GLM", "GEE-exchangeable", "GEE-independence"),
  t(sapply(
    list(gab_regr, gee_fit_1, gee_fit_2),
    function(mod) get_birthord_se(mod, ifelse(inherits(mod, "geeglm"), "gee", "glm"))
  ))
)
colnames(se_table)[-1] <- names(get_birthord_se(gab_regr, "glm"))
print(se_table)









