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

bmd <- read.dta13("/Users/aaron.scheffler/Downloads/BIOS 208 2025/labs/lab6/lab6.dta")
bmd <- na.omit(bmd)

set.seed(2182020)
#Create training set and test set
bmd$group <- factor(sample(c(1:2), nrow(bmd), prob = c(.75, .25), replace = TRUE))
describeBy(bmd$bmd, bmd$group)


full_model <- lm(bmd ~ eeu + age + lweight + estrogen + calsupp + diuretic + 
                   etid + momhip + usearms + tandstnd + has10 + gs10 + gaitspd + 
                   poorhlth + caffeine + drnkspwk + smoker, data = bmd[which(bmd$group == 1), ])
backward_model <- step(full_model, direction = "backward", trace = 0, k = 2)
summary(backward_model)

# Predict outcomes for validation (1/4) set
bmd$pr <- predict(backward_model, newdata = bmd)
# Estimate rho
cor(bmd$bmd[which(bmd$group == 2)], bmd$pr[which(bmd$group == 2)])
# Estimate rho^2
cor(bmd$bmd[which(bmd$group == 2)], bmd$pr[which(bmd$group == 2)])^2

summary(backward_model)$r.squared

ggplot(data = bmd[which(bmd$group ==2), ], aes(x = bmd, y = pr)) + geom_point() + 
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") + geom_abline() + theme_minimal()

set.seed(2182020)
k <- 10
bmd$group <- factor(sample(c(1:k), nrow(bmd), prob = rep(.1, k), replace = TRUE))
bmd$pr2 <- NA
r2 <- rep(NA, k)
for(i in 1:k){
  mod <- lm(bmd ~ lweight + estrogen + calsupp + has10 + gaitspd + caffeine + smoker,
            data = bmd[which(bmd$group != i), ])
  bmd$pr2[which(bmd$group == i)] <- predict(mod, newdata = bmd[which(bmd$group == i), ])
  r2[i] <-  cor(bmd$bmd[which(bmd$group == i)], bmd$pr2[which(bmd$group == i)])^2
}
print(r2)
mean(r2)
full_model <- lm(bmd ~ lweight + estrogen + calsupp + has10 + gaitspd + caffeine + smoker, data = bmd)
summary(full_model)$r.squared

bmd$estuse <- 1
bmd$estuse[which(as.numeric(bmd$estrogen) == 1)] <- 0
fit <- lm(bmd ~ estuse + age + weight + weight2, data = bmd)
jtools::summ(fit, confint = TRUE, digits = 3)

rcorr(as.matrix(bmd[,c("bmd", "weight", "weight2")]))
vif(fit)

bmd$cweight2 <- (bmd$weight - mean(bmd$weight))^2
fit <- lm(bmd ~ estuse + age + weight + cweight2, data = bmd)
jtools::summ(fit, confint = TRUE, digits = 3)
rcorr(as.matrix(bmd[,c("bmd", "weight", "cweight2")]))
vif(fit)
