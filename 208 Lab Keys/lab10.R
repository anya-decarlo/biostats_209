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
install.packages("glmtoolbox")


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
library("glmtoolbox")

data <- read.dta13("~/Downloads/BIOS 208 2024/labs/lab10/lab10.dta", nonint.factors = TRUE)
setwd("~/Downloads/BIOS 208 2024/labs/lab10")


summary(data$chol)
data <- data[-which(data$chol == 645 | is.na(data$chol)),]
data <- data[-which(data$bmi < 12), ]

# Question 1
model <- glm(chd69 ~ chol + sbp + dibpat + smoke + age + bmi, family = "binomial", data = data) # full model
jtools::summ(model, confint = TRUE, digits = 3)


# Question 2
data$phat <- predict(model, type = "response")
data$dxb <- cooks.distance(model) # use Cooks distance instead (Pregibons influence statistics not availaible as in STATA)
plot(data$phat, data$dxb)
text(data$phat + .04, data$dxb, labels = data$id, cex= 0.7)
data[which(data$id == 10078), c("bmi", "weight", "height", "chol")]
data[order(data$dxb, decreasing = TRUE),][1:10,]

model <- glm(chd69 ~ chol + sbp + dibpat + smoke + age + bmi, family = "binomial", data = data[-which(data$id == 10078),])
jtools::summ(model, confint = TRUE, digits = 3, exp = TRUE)

# Question 3
model <- glm(chd69 ~ chol + sbp + dibpat + smoke + age + bmi, family = "binomial", data = data)  
data$phat <- predict(model, type = "response")
hltest(model, df = 8)

data$bmic <- data$bmi - mean(data$bmi)
data$cholc <- data$chol - mean(data$chol)
data$sbpc <- data$sbp - mean(data$sbp)
model <- glm(chd69 ~ dibpat + age + smoke + bmic * cholc + sbpc * bmic, data = data, family = "binomial") 
data$phat <- predict(model, type = "response")
hltest(model, df = 8)
jtools::summ(model, confint = TRUE, digits = 3, exp = TRUE, vif = TRUE) # note different values from R

# Question 4
set.seed(1234)
train <- sample(1:dim(data)[1], round(0.9 * dim(data)[1]))
data$train <- NA
data$train[train] <- "train"
data$train[-train] <- "test"
table(data$chd, data$train)
model <- glm(chd69 ~ dibpat + age + smoke + bmic * cholc + sbpc * bmic, data = data[train,], family = "binomial") 
data$phat <- predict(model, newdata = data, type = "response")

#AUROC
ggplot(data[train,], aes(d = chd69, m = phat)) + geom_roc(n.cuts = 50, labels = FALSE)
myROC_train <-pROC::roc(predictor = data$p[train], response = data$chd69[train])
print(myROC_train)

ggplot(data[-train, ], aes(d = chd69, m = phat)) + geom_roc(n.cuts = 50, labels = FALSE)
myROC_test <-pROC::roc(predictor = data$p[-train], response = data$chd69[-train])
print(myROC_test)

# Sensitivity, specifcity
myROC_plot <- with(myROC_train, data.frame(specificities, sensitivities, thresholds))
ggplot(myROC_plot, aes(x = thresholds, y = sensitivities)) + geom_line(color = "blue") + 
  geom_line(aes(x = thresholds, y = specificities), color = "red") + ylab("specificities/sensitivities")

print(myROC_plot[which(abs(myROC_plot$sensitivities - myROC_plot$specificities) <.001),])

summary(cutpointr(data[train,], phat, chd69, na.rm = TRUE, method = oc_manual, cutpoint = 0.085))
summary(cutpointr(data[-train,], phat, chd69, na.rm = TRUE, method = oc_manual, cutpoint = 0.085))

cp <- cutpointr(data[train,], phat, chd69, na.rm = TRUE, method = oc_youden_kernel)
summary(cp)



# Question 5
data$chd69_int <- as.numeric(data$chd69) - 1 # convert chd69 to integer binary indicator variable
model <- riskratio(chd69_int ~ dibpat + age + smoke + bmic * cholc + sbpc * bmic, data = data)
summary(model, confint = TRUE, digits = 3, exp = TRUE)

model <- riskratio(chd69_int ~ dibpat + age + smoke + bmic * cholc + sbpc * bmic, data = data, approach = "robpoisson")
jtools::summ(model, confint = TRUE, digits = 3, exp = TRUE)
