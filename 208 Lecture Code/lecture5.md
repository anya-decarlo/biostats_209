
## Interpreting coefficient p-values 
The p-value associated with a coefficient tests the null hypothesis that the coefficient is zero ( H_0: \beta = 0 ). In other words, it answers the question:

	“If the true effect of this variable were actually zero, what is the probability that we would observe an effect as extreme (or more extreme) as the one we estimated?”

A small p-value suggests that the observed effect is unlikely under the assumption that  H_0  is true, leading us to reject the null hypothesis and conclude that the variable likely has a meaningful effect on  Y .

## ---------------------------------------------------------------------------------------------------------------------


## What Does the F-Test Tell Us?

The F-test at the bottom (F-statistic: 15.62, p-value: < 0.0001) tests the null hypothesis:


H_0: \beta_1 = \beta_2 = \beta_3 = 0


which means none of the predictors explain variation in  Y .
	•	Since the p-value is very small (< 0.0001), we reject  H_0  and conclude that at least one of the predictors  (X_1, X_2, X_3)  has a statistically significant effect on  Y .
	•	This tells us that the model as a whole is significant and explains a meaningful portion of the variance in  Y .


## ---------------------------------------------------------------------------------------------------------------------