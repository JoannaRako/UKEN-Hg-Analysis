# This is main script
### 0 ###
# PACKAGE just for convert XLS for CSV
# ---------------------------
library(readxl)

# Conversion of .xlsx to .csv
xlsx_path <- "Data/Materials/example_cal_curve.xls"
xlsx <- read_excel(xlsx_path)
csv <- "Data/Materials/example_cal_curve.csv"
write.csv(xlsx, csv, row.names = FALSE)




### 1 ###
# START 
# -------------------------

date <- format(Sys.Date(), "%d%m%Y")

# Import, read and remove NA
data <- read.csv("Data/Materials/example_cal_curve.csv")
data <- na.omit(data)
head(data)

true_rows <- data[data$T.F == 'True', ]
false_rows <- data[data$T.F == 'False', ]
head(true_rows)



### 2 ###
# POLY 3rd DEGREE - MODEL CALCULATION
# -----------------------------------

# raw=TRUE -> causes the polynomial to be created 
# from the original data rather than the normalized data

## Filter
poly_model <- lm(
    PEAK ~ poly(STD..ng., 3, raw=TRUE), 
    data = true_rows )

## Each
poly_original <- lm(
    PEAK ~ poly(STD..ng., 3, raw=TRUE), 
    data = data)

summary(poly_model)
summary(poly_original)

# Extract R-squared value
rsquared <- summary(poly_model)$r.squared



### 3 ###
# Creation of Calibration Curve
# -----------------------------

pdf(
  paste("Raw/Plots/", 
        "Rplot_poly3rd_", 
        date, ".pdf",
        sep = ''),
  paper = "special"
)

# Generate plot and calibration line (based on model)
dfPlot <-  plot(true_rows$STD..ng., true_rows$PEAK, 
           main = "Krzywa Model Wielomian 3st",
           xlab = "STD..ng.", 
           ylab = "PEAK", 
           pch = 16,
           col = "chartreuse3",
           cex = 1,        # point_size
           cex.lab = 1.1   # lab_size
)

# Add false_rows point to plot - red
points(false_rows$STD..ng., false_rows$PEAK, 
       pch = 16, 
       col = "red"
)

# Add calibration line
curve(predict(poly_model, data.frame(STD..ng.=x)), 
      add = TRUE, 
      col = "black",
      lty = "dashed",
      lwd = 0
      )


# Top-center add R ^ 2
mtext(bquote(
  R^2 == .(rsquared)),
  side = 3
)

#  Add legend
legend("topleft",
       legend=c("true", "false","deleted"),
       col=c("chartreuse3", "red","orange"),
       pch=16,
       bty="n"  # without border
)


dfPlot
dev.off()



### 4 ###
# Writing statistics for original and filtered data
# -------------------------------------------------


## Summarize
directory <- "C:/Users/joasi/UKEN_Hg_Analysis/Raw/Tables/"

# ORIGINAL VALUES
original_output <- capture.output(summary(poly_original))

fileName = paste(directory, 'stats_org_poly_', date,'.csv', sep='')
write.table(original_output[-1], 
            fileName, 
            quote = FALSE, 
            row.names = FALSE, 
            col.names = FALSE)

# CHOSEN VALUES
chosen_output <- capture.output(summary(poly_model))

fileName = paste(directory, 'stats_ch_poly_', date,'.csv', sep='')
write.table(chosen_output[-1], 
            fileName, 
            quote = FALSE,
            row.names = FALSE, 
            col.names = FALSE)






######################## is it possible to do next  manually? Yes, SHINY.R 
######################## https://shiny.posit.co #






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

