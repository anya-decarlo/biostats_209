# Load the necessary libraries
install.packages("devtools")
library(devtools)
install_github("OnofriAndreaPG/aomisc")
install.packages("readstata13")
install.packages("psych")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("margins")
install.packages("emmeans")
install.packages("ggplotify")
install.packages("describedata")
install.packages("splines")
install.packages("lspline")
install.packages("rms")
install.packages("doBy")
install.packages("glmnet")
install.packages("epitools")
install.packages("risks")
install.packages("ResourceSelection")
install.packages("pROC")
install.packages("caret")
install.packages("survival")
install.packages("epiDisplay")
install.packages("vcd")
library(readstata13)
library(psych)
library(ggplot2)
library(multcomp)
library(margins)
library(emmeans)
library(dplyr)
library(ggplotify)
library(describedata)
library(splines)
library(lspline)
library(stats)
library(rms)
library(glmnet)
library(doBy)
library(epitools)
library(risks)
library(aomisc)
library(ResourceSelection)
library(pROC)
library(caret)
library(survival)
library(epiDisplay)
library(vcd)
data <- read.dta13("~/Downloads/BIOS 208 2024/labs/lab11/esoph.dta")
setwd("~/Downloads/BIOS 208 2024/labs/lab11")

data$tobgp <- as.factor(data$tobgp)
data$alcgp <- as.factor(data$alcgp)
data$agegp <- as.factor(data$agegp)

describeBy(data)
table(data$agegp)
table(data$alcgp)
table(data$tobgp)

# Question 1
oddsratio(data$alcgp, data$case, method = "wald")
chisq.test(data$case, data$alcgp)

oddsratio(data$tobgp, data$case, method = "wald")
chisq.test(data$case, data$tobgp)

oddsratio(data$agegp, data$case, method = "wald")
chisq.test(data$case, data$agegp)

# Question 2

# Unable to reproduce code in R - see lab document for Stata output and requestions.

# Question 3

model_alc <- glm(case ~ alcgp + agegp, data = data, family = "binomial")
model_tob <- glm(case ~ tobgp + agegp, data = data, family = "binomial")

model_all <- glm(case ~ tobgp + alcgp + agegp, data = data, family = "binomial")

anova(model_alc, model_all, test = "LRT")
anova(model_tob, model_all, test = "LRT")

# Question 4

data$alcb <- as.factor(ifelse(data$alcgp %in% c(1, 2), 0, 1))
model_allb <- glm(case ~ tobgp + alcb + agegp, data = data, family = "binomial")
anova(model_allb, model_all, test = "LRT")

# Question 5
data <- read.dta13("~/Downloads/BIOS 208 2024/labs/lab11/endom.dta")
describeBy(data)
data$gall <- as.factor(data$gall)
data$est <- as.factor(data$est)
data$ob <- as.factor(data$ob)
data$set <- as.factor(data$set)

View(data[which(data$set %in% paste(1:5)),])

mantelhaen.test(data$case, data$est, data$set)

model <- clogit(case ~ est + strata(set), data = data)

table(data$ob, data$case)

model <- clogit(case ~ ob + strata(set), data = data)
data$obes <- ifelse(data$ob == "1", 1, 0)

model <- clogit(case ~ est + hyp + non + gall + obes + strata(set), data = data)