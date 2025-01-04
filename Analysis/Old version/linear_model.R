date <- format(Sys.Date(), "%d%m%Y")

### 1 ###
# START - read file
#---------------------------

# Import, read and remove NA
data <- read.csv("Data/Materials/example_cal_curve.csv")
data <- na.omit(data)
head(data)

true_rows <- data[data$T.F == 'True', ]
false_rows <- data[data$T.F == 'False', ]


### 2 ###
# LINEAR - MODEL CALCULATION
# --------------------------

# OX and OY
x <- true_rows$STD..ng.
y <- true_rows$PEAK 


## Filter
lin_model <- 
      lm(PEAK ~ STD..ng., 
      data = true_rows
)

## Each
lin_original <- 
      lm(PEAK ~ STD..ng.,
      data = data
)

a <- summary(lin_model)
summary(lin_original)

#  Extract R-squared value OR ADJUST???
rsquared <- summary(lin_model)$r.squared



### 3 ###
# Creation of Calibration Curve
# -----------------------------

pdf(
    paste("Raw/Plots/", 
          "Rplot_linear_", 
          date, ".pdf",
          sep = ''),
    paper = "special"
)

#  Generate Plot
dfPlot <- plot( x, 
               y,
               main = "Krzywa kalibracyjna",
               xlab = substitute(paste(bold("STD [ng]"))), 
               ylab = substitute(paste(bold("PEAK"))), 
               pch=16,
               col = "chartreuse3",
               cex = 1,        # point_size
               cex.lab = 1.1   # lab_size
               )

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

#  Add false_rows point to plot - red
points(false_rows$STD..ng., false_rows$PEAK, 
       pch=16, 
       col="red",
       cex = 0.8   
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
original_output <- capture.output(summary(lin_original))

fileName = paste(directory, 'stats_org_lm_', date,'.csv', sep='')
write.table(original_output[-1], 
            fileName, 
            quote = FALSE, 
            row.names = FALSE, 
            col.names = FALSE)

# CHOSEN VALUES
chosen_output <- capture.output(summary(lin_model))

fileName = paste(directory, 'stats_ch_lm_', date,'.csv', sep='')
write.table(chosen_output[-1], 
          fileName, 
          quote = FALSE, 
          row.names = FALSE, 
          col.names = FALSE)




