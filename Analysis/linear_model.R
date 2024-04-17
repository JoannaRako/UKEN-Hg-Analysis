### 1 
# START - read file
#---------------------------
setwd("c:/Users/joasi/UKEN_Hg_analysis")
data <- read.csv("Data/Materials/example_cal_curve.csv")
data <- na.omit(data)
head(data)

true_rows <- data[data$T.F == 'True', ]
false_rows <- data[data$T.F == 'False', ]


### 2 ###
# Creation of Calibration Curve
# -----------------------------

# OX and OY
x <- true_rows$STD..ng.
y <- true_rows$PEAK 

#  Linear
lin_model <- lm(PEAK ~ STD..ng., 
             data = true_rows)

summary(lin_model)

# Extract R-squared value OR ADJUST???
rsquared <- summary(lin_model)$r.squared

#  Generate Plot
plot(x, 
     y,
     xlab = substitute(paste(bold("STD..ng."))), 
     ylab = substitute(paste(bold("PEAK"))), 
     main = "Krzywa kalibracyjna",
     pch=16,
     col = "chartreuse3",
     cex = 1,      # point_size
     cex.lab = 1.1   # lab_size
     )

# Top-center add R ^ 2
mtext(
      bquote(
            R^2 == .(rsquared)
            ), 
      side = 3)

#  Add calibration line
abline(lin_model, 
       col="black",
       lty=6,
       lwd=0
       )

#  Add false_rows point to plot - red
points(false_rows$STD..ng., false_rows$PEAK, 
       pch=16, 
       col="red",
       cex = 0.8   
       )

#  Add legend
legend("topleft",
      legend=c("true", "false","deleted"),
      col=c("chartreuse3", "red","orange"),
      pch=16,
      bty="n"  # without border
      )



### 3 ###
# Summarize the statistic
# clearing summary 
output <- capture.output(summary(lin_model))

output_cleaned <- noquote(output[!output == ""])
output_cleaned <- gsub('\t', " ", output_cleaned)

cleaned_summary <-output_cleaned
print(cleaned_summary)
