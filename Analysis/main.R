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




### 1 
# START - read file
#---------------------------
data <- read.csv("Data/Materials/example_cal_curve.csv")
head(data)
clean_data <- na.omit(data)


### 2 ###
# Creation of Calibration Curve
# -----------------------------
# OX and OY
x <- clean_data$STD..ng.
y <- clean_data$PEAK #(zalezna?)

# Generate Plot
plot(x, y, xlab = "Standard (ng)", ylab = "Peak", main = "Krzywa kalibracyjna")

# 0. Linear
lin_model <- lm(PEAK ~ STD..ng., data = clean_data)
summary(lin_model)
plot(clean_data$STD..ng., clean_data$PEAK)
abline(lin_model, col = "blue")



# 1. Import, read and remove NA
data <- read.csv("Data/Materials/example_cal_curve_up3.csv")
clean_data <- na.omit(data)

# 2. 3rd degree model
model <- lm(PEAK ~ poly(STD..ng., 3, raw=TRUE), data = clean_data)
summary(model)

# 3. Generate plot and calibration curve
plot(clean_data$STD..ng., clean_data$PEAK, main="Krzywa kalibracyjna - z dokl do 3",
     xlab="STD..ng.", ylab="PEAK", pch=18)
curve(predict(model, newdata=data.frame(STD..ng.=x)), add=TRUE, col="red", lwd=2)

# 4. Identification of outliers based on residuals
res <- residuals(model)
IQR_res <- IQR(res)
upper_bound <- quantile(res, 0.75) + 1.5 * IQR_res
lower_bound <- quantile(res, 0.25) - 1.5 * IQR_res
outliers <- which(res > upper_bound | res < lower_bound)

# 5. Remove outliers and refit the model
clean_data_no_outliers <- clean_data[-outliers, ]
model_no_outliers <- lm(PEAK ~ poly(STD..ng., 3, raw=TRUE), data = clean_data_no_outliers)
summary(model_no_outliers)

# 6. Generate a plot of data without outliers and a model curve
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

