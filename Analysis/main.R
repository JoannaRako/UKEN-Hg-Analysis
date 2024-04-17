# This is main script
### 0 
# PACKAGE just for convert XLS for CSV
library(readxl)

# Conversion of .xlsx to .csv
# ---------------------------
xlsx_path <- "Data/Materials/example_cal_curve.xls"
xlsx <- read_excel(xlsx_path)
csv <- "Data/Materials/example_cal_curve.csv"
write.csv(xlsx, csv, row.names = FALSE)


############## START #################

# Import, read and remove NA
setwd("c:/Users/joasi/UKEN_Hg_analysis")
data <- read.csv("Data/Materials/example_cal_curve.csv")
data <- na.omit(data)
head(data)

true_rows <- data[data$T.F == 'True', ]
false_rows <- data[data$T.F == 'False', ]
head(true_rows)


# 3rd degree model
# raw=TRUE -> causes the polynomial to be created from the original data rather than the normalized data
model <- lm(
    PEAK ~ poly( STD..ng., 3, raw=TRUE ), 
    data = true_rows )

summary(model)
summary_info <- summary( lin_model )

# Extract R-squared value
rsquared <- summary_info$r.squared

# Generate plot and calibration line (based on model)
plot(true_rows$STD..ng., true_rows$PEAK, 
     main = "Krzywa kalibracyjna",
     xlab = "STD..ng.", 
     ylab = "PEAK", 
     pch = 16,
     col = "chartreuse3",
     )



# Add calibration line
curve(predict(model, data.frame(STD..ng.=x)), 
      add = TRUE, 
      col = "black",
      lty = "dashed",
      lwd = 0
      )

# Add false_rows point to plot - red
points(false_rows$STD..ng., false_rows$PEAK, 
       pch = 16, 
       col = "red"
       )









# 6. Identification of outliers based on residuals
res <- residuals(model)
IQR_res <- IQR(res)
upper_bound <- quantile(res, 0.75) + 1.5 * IQR_res
lower_bound <- quantile(res, 0.25) - 1.5 * IQR_res
outliers <- which(res > upper_bound | res < lower_bound)

# 7. Remove outliers and refit the model
clean_data_no_outliers <- clean_data[-outliers, ]
model_no_outliers <- lm(PEAK ~ poly(STD..ng., 3, raw=TRUE), data = clean_data_no_outliers)
summary(model_no_outliers)

# 8. Generate a plot of data without outliers and a model curve
plot(clean_data_no_outliers$STD..ng., clean_data_no_outliers$PEAK, main="Krzywa kalibracyjna - Wielomian 3 stopnia (bez odstających)",
     xlab="STD..ng.", ylab="PEAK", pch=19, col="blue")
curve(predict(model_no_outliers, newdata=data.frame(STD..ng.=x)), add=TRUE, col="red", lwd=2)


########COLORFUL VERSION OF THE SAME#########

# Fitting the model to the original data
model_original <- lm(PEAK ~ poly(STD..ng., 3, raw=TRUE), data = clean_data)
summary_model_original <- summary(model_original)

# Fitting the model to data without outliers
model_no_outliers <- lm(PEAK ~ poly(STD..ng., 3, raw=TRUE), data = clean_data_no_outliers)
summary_model_no_outliers <- summary(model_no_outliers)

# View the regression equations and R-squared values ​​for both models
cat("Model oryginalny:\n")
cat("R-squared:", summary_model_original$r.squared, "\n")
cat("Adjusted R-squared:", summary_model_original$adj.r.squared, "\n\n")

cat("Model bez wartości odstających:\n")
cat("R-squared:", summary_model_no_outliers$r.squared, "\n")
cat("Adjusted R-squared:", summary_model_no_outliers$adj.r.squared, "\n")

# Plots
plot(clean_data$STD..ng., clean_data$PEAK, main="Oryginalny model",
     xlab="STD..ng.", ylab="PEAK", pch=19, col="blue")
curve(predict(model_original, newdata=data.frame(STD..ng.=x)), add=TRUE, col="red", lwd=2)
legend("topleft", legend=c("Dane oryginalne", "Krzywa modelu"), col=c("blue", "red"), pch=19, lty=1)

plot(clean_data_no_outliers$STD..ng., clean_data_no_outliers$PEAK, main="Model bez wartości odstających",
     xlab="STD..ng.", ylab="PEAK", pch=19, col="green")
curve(predict(model_no_outliers, newdata=data.frame(STD..ng.=x)), add=TRUE, col="orange", lwd=2)
legend("topleft", legend=c("Dane bez odstających", "Krzywa modelu"), col=c("green", "orange"), pch=19, lty=1)

