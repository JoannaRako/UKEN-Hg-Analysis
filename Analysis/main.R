# This is main script

# PACKAGE just for convert XLS for CSV
library(readxl)



### 1 ###
# Conversion of .xlsx to .csv
# ---------------------------
xlsx_path <- "Data/Materials/example_cal_curve.xls"
xlsx <- read_excel(xlsx_path)
csv <- "Data/Materials/example_cal_curve.csv"
write.csv(xlsx, csv, row.names = FALSE)

# Open raw csv table
data <- read.csv("Data/Materials/example_cal_curve.csv")
head(data)


### 2 ###
# Creation of Calibration Curve
# -----------------------------

# OX and OY
x <- data$STD..ng.
y <- data$PEAK

# Generate Plot
plot(x, y, xlab = "Standard (ng)", ylab = "Peak", main = "Krzywa kalibracyjna")

# Dodanie linii regresji liniowej



