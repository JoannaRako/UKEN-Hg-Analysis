#---------------------------
setwd("c:/Users/joasi/UKEN_Hg_analysis")
data <- read.csv("Data/Materials/example_cal_curve.csv")
data <- na.omit(data)
head(data)

true_rows <- data[data$T.F == 'True', ]
false_rows <- data[data$T.F == 'False', ]

lin_model <- lm(PEAK ~ STD..ng., 
                data = true_rows)

sum <- summary(lin_model)
res <- summary(lin_model)$residuals[]
coef <- summary(lin_model)$coefficients[]

statistic_names <- c("Min", "1Q", "Median", "3Q", "Max", 
                     "Estimate_Intercept", "Estimate_STD..ng.", 
                     "Std.Error_Intercept", "Std.Error_STD..ng.", 
                     "t.value_Intercept", "t.value_STD..ng.", 
                     "Pr(>|t|)_Intercept", "Pr(>|t|)_STD..ng.", 
                     "Residual_standard_error", "Degrees_of_freedom", 
                     "Multiple_R-squared", "Adjusted_R-squared", 
                     "F-statistic", "P-value")

statistic_values <- c(Min = min(res),
  Q1 = quantile(res, probs = 0.25),
  Median = median(res),
  Q3 = quantile(res, probs = 0.75),
  Max = max(res),
  Estimate_Intercept = coef[1,1],
  Estimate_STD..ng. = coef[2,1], 
  Std.Error_Intercept = coef[1,2],
  Std.Error_STD..ng. = coef[2,2], 
  t.value_Intercept = coef[1,3],
  t.value_STD..ng. = coef[2,3], 
  Pr..t...Intercept = coef[1,4],
  Pr..t...STD..ng. = coef[2,4], 
  Residual_standard_error = summary(lin_model)$sigma, #residual standard error of regression model
  Degrees_of_freedom = sum$fstatistic[3], 
  Multiple_R.squared = sum$r.squared,
  Adjusted_R.squared = sum$adj.r.squared, 
  F_statistic = sum$fstatistic[1],
  P_value = pf(sum$fstatistic[1],sum$fstatistic[2],sum$fstatistic[3],lower.tail=FALSE)
)



regression_table <- data.frame(Column1 = statistic_names, Column2 = statistic_values)
regression_table