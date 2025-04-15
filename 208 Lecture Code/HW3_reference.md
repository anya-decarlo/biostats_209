# Homework 3 Reference Guide

## Residual vs. Predicted Plot Interpretation

A residual vs. predicted plot is a crucial diagnostic tool for checking linear regression assumptions:

### Key Things to Look For:

1. **Linearity Assessment**
   - Ideal: Random scatter around zero line
   - Problem: Any systematic patterns or curves
   - Interpretation: Non-random patterns suggest non-linear relationships

2. **Homoscedasticity (Constant Variance)**
   - Ideal: Even spread across all predicted values
   - Problem: Funnel shapes or changing spread
   - Interpretation: Fanning patterns suggest non-constant variance

3. **Outlier Detection**
   - Look for: Points far from zero line
   - Importance: May indicate influential cases affecting model estimates

### Application to TGL Analysis

When comparing linear and log-transformed TGL models:
- Use to evaluate if log transformation improved linearity
- Check if variance stabilized after transformation
- Identify if different transformations might be needed

### Comparing Model Linearity Using Correlation Tests

When comparing linear and log-transformed models, correlation tests between the predictor (e.g., WHR) and fitted values can help assess which model achieves better linearity:

```R
# For original model
data_frame <- data.frame(x = df$whr[model_rows], y = fitted(model))
pearsons_r <- cor.test(x = data_frame$x, y = data_frame$y, method = "pearson")

# For log-transformed model
dataFrame <- data.frame(x = df$whr[model_rows], y = fitted(model_nl))
pearsons_r_nl <- cor.test(x = dataFrame$x, y = dataFrame$y, method = "pearson")
```

**Interpretation:**
- Higher correlation coefficient (closer to 1 or -1) indicates better linearity
- Compare coefficients between models to see which transformation (if any) improved the linear relationship
- Remember that correlation only measures linear association, so also check residual plots

### Comparing Prediction Methods in R

When calculating adjusted means in R, there are two main approaches that may give slightly different results:

1. `predictions()` with `datagrid(grid_type = "counterfactual")`:
   - Uses the full dataset's covariate distribution
   - Sets other variables to their observed values for each prediction
   - Similar to Stata's `margins` with `at()` option

2. `avg_predictions()`:
   - Averages across the covariate distributions within each subgroup
   - This means if subgroups differ in their covariate distributions, results will differ from `predictions()`
   - Recommended when following Stata's documentation that specifically mentions `avg_predictions`
   - More appropriate when subgroups might have different covariate patterns

The choice between methods depends on whether you want to:
- Use the full sample's covariate distribution (`predictions()`)
- Or account for different covariate patterns in each subgroup (`avg_predictions()`)

Both methods show similar patterns and lead to the same substantive conclusions, but numerical values may differ due to these different averaging approaches.

### DFBETA Analysis

DFBETA measures how much each observation (person in your dataset) influences the coefficient estimates in your model. Here's how it works:

1. For each person in your dataset:
   - It removes that person
   - Refits the model without them
   - Measures how much the coefficients changed
   - The bigger the change, the more influential that person is

2. Standardized DFBETAs (dfbetas):
   - Values > 2/√n (where n is sample size) are considered potentially influential
   - Shows which observations are disproportionately affecting your WHR and BMI category coefficients

3. In practice:
   - Looking at how each person affects the WHR category coefficients
   - A large DFBETA would suggest that person is strongly influencing your WHR-triglyceride relationship
   - Helps identify outliers or influential points that might be affecting your conclusions about WHR categories

### DFBETA Reference Line

The standard cutoff for identifying influential points in DFBETA analysis is ±2/√n, where n is the sample size. This is calculated as:

```R
n <- nrow(df)  # number of observations
ref_line <- 2/sqrt(n)  # cutoff for influential points
```

This cutoff is a widely-used rule of thumb in statistical literature:
- Values above +2/√n or below -2/√n are considered potentially influential
- The cutoff gets smaller as sample size increases
- This makes sense because with larger samples, individual points should have less influence

### Creating the Plot in R
```R
# Basic residual vs. predicted plot
plot(fitted(model), resid(model),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residual vs. Fitted Plot")
abline(h = 0, col = "red", lty = 2)

# Or using ggplot2 for more customization
ggplot(data = data.frame(fitted = fitted(model), resid = resid(model)), 
       aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(x = "Fitted Values", 
       y = "Residuals",
       title = "Residual vs. Fitted Plot") +
  theme_minimal()
```

### log transforming outcome vs predictor
 Positive continuous variables are frequently log-transformed in regression models
- outcomes: normalize and equalize variance
- predictors: address non-linearity or interaction issues, reduce influence of
outliers