library(shiny)

# Outside the server function: Data preparation
date <- format(Sys.Date(), "%d%m%Y")

# Import, read and remove NA
data <- read.csv("Data/Materials/example_cal_curve.csv")
data <- na.omit(data)
head(data)

true_rows <- data[data$T.F == 'True', ]
false_rows <- data[data$T.F == 'False', ]
head(true_rows)

# POLY 3rd DEGREE - MODEL CALCULATION
poly_model <- lm(
  PEAK ~ poly(STD..ng., 3, raw = TRUE),
  data = true_rows
)

poly_original <- lm(
  PEAK ~ poly(STD..ng., 3, raw = TRUE),
  data = data
)

summary(poly_model)
summary(poly_original)

# Extract R-squared value
rsquared <- summary(poly_model)$r.squared

# Define UI
ui <- fluidPage(
  titlePanel("Calibration Curve Analysis"),
  sidebarLayout(
    sidebarPanel(
      downloadButton("downloadPlot", "Download Plot"),
      downloadButton("downloadStats", "Download Statistics"),
      actionButton("reset", "Reset")
    ),
    mainPanel(
      plotOutput("calibrationPlot", click = "plot_click"),
      verbatimTextOutput("statsOutput")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive value to store selected outliers
  selected_outliers <- reactiveVal(integer(0))
  
  
  
############### This code block effectively allows users to interactively include or  ########
###############  exclude points from the plot by clicking on them ############################
  
  # observeEvent <- monitors for click events on the plot. It executes the 
  # enclosed code block every time the user clicks on the plot
  observeEvent(input$plot_click, {
    click <- input$plot_click
    
    # new_point <- Determines which data point in true_rows is closest to the 
    # coordinates of the click. It calculates the distance between the click 
    # location and each point in the dataset, selecting the point with the minimum
    # distance. This effectively identifies the nearest point to the user's click.
    new_point <- which.min((true_rows$STD..ng. - click$x)^2 + (true_rows$PEAK - click$y)^2)
    outliers <- selected_outliers()
    
    # Checks if the newly clicked point (new_point) is already in the list of outliers:
    # If it is: The point is removed from the outliers list, which means the point will be included in the plot again.
    # If it is not: The point is added to the outliers list, which means the point will be excluded from the plot.
    
    if (new_point %in% outliers) {
      outliers <- outliers[outliers != new_point]
    } else {
      outliers <- c(outliers, new_point)
    }
    selected_outliers(outliers)
  })
  
  # Reset button to clear selected outliers
  observeEvent(input$reset, {
    selected_outliers(integer(0))
  })
  
  # Render Plot
  output$calibrationPlot <- renderPlot({
    outliers <- selected_outliers()
    true_rows$included <- TRUE
    true_rows$included[outliers] <- FALSE
    
    included_rows <- true_rows[true_rows$included, ]
    excluded_rows <- true_rows[!true_rows$included, ]
    
    poly_model <- lm(
      PEAK ~ poly(STD..ng., 3, raw = TRUE),
      data = included_rows
    )
    rsquared <- summary(poly_model)$r.squared
    
    plot(included_rows$STD..ng., included_rows$PEAK, 
         main = "Calibration Curve - Polynomial 3rd Degree",
         xlab = "STD..ng.", 
         ylab = "PEAK", 
         pch = 16,
         col = "orange",
         cex = 1,        # point_size
         cex.lab = 1.1   # lab_size
    )
    
    points(false_rows$STD..ng., false_rows$PEAK, 
           pch = 16, 
           col = "red"
    )
    
    points(excluded_rows$STD..ng., excluded_rows$PEAK, 
           pch = 16, 
           col = "chartreuse3"
    )
    
    curve(predict(poly_model, data.frame(STD..ng.=x)), 
          add = TRUE, 
          col = "black",
          lty = "dashed",
          lwd = 1
    )
    
    mtext(bquote(
      R^2 == .(rsquared)),
      side = 3
    )
    
    legend("topleft",
           legend=c("Included", "Excluded", "False"),
           col=c("orange", "chartreuse3", "red"),
           pch=16,
           bty="n"  # without border
    )
  })
  
  # Render Statistics
  output$statsOutput <- renderPrint({
    outliers <- selected_outliers()
    true_rows$included <- TRUE
    true_rows$included[outliers] <- FALSE
    
    included_rows <- true_rows[true_rows$included, ]
    
    poly_model <- lm(
      PEAK ~ poly(STD..ng., 3, raw = TRUE),
      data = included_rows
    )
    
    cat("Original Model Summary:\n")
    print(summary(poly_original))
    cat("\nFiltered Model Summary:\n")
    print(summary(poly_model))
  })
  
  # Download Plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("calibration_curve_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)
      # Generate the plot directly within the downloadHandler
      outliers <- selected_outliers()
      true_rows$included <- TRUE
      true_rows$included[outliers] <- FALSE
      
      included_rows <- true_rows[true_rows$included, ]
      excluded_rows <- true_rows[!true_rows$included, ]
      
      poly_model <- lm(
        PEAK ~ poly(STD..ng., 3, raw = TRUE),
        data = included_rows
      )
      rsquared <- summary(poly_model)$r.squared
      
      plot(included_rows$STD..ng., included_rows$PEAK, 
           main = "Calibration Curve - Polynomial 3rd Degree",
           xlab = "STD..ng.", 
           ylab = "PEAK", 
           pch = 16,
           col = "orange",
           cex = 1,        # point_size
           cex.lab = 1.1   # lab_size
      )
      
      points(false_rows$STD..ng., false_rows$PEAK, 
             pch = 16, 
             col = "red"
      )
      
      points(excluded_rows$STD..ng., excluded_rows$PEAK, 
             pch = 16, 
             col = "chartreuse3"
      )
      
      curve(predict(poly_model, data.frame(STD..ng.=x)), 
            add = TRUE, 
            col = "black",
            lty = "dashed",
            lwd = 1
      )
      
      mtext(bquote(
        R^2 == .(rsquared)),
        side = 3
      )
      
      legend("topleft",
             legend=c("Included", "Excluded", "False"),
             col=c("orange", "chartreuse3", "red"),
             pch=16,
             bty="n"  # without border
      )
      
      dev.off()
    }
  )
  
  # Download Statistics
  output$downloadStats <- downloadHandler(
    filename = function() {
      paste("stats_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      # Capture and write statistics directly within the downloadHandler
      outliers <- selected_outliers()
      true_rows$included <- TRUE
      true_rows$included[outliers] <- FALSE
      
      included_rows <- true_rows[true_rows$included, ]
      
      poly_model <- lm(
        PEAK ~ poly(STD..ng., 3, raw = TRUE),
        data = included_rows
      )
      
      capture.output({
        cat("Original Model Summary:\n")
        print(summary(poly_original))
        cat("\nFiltered Model Summary:\n")
        print(summary(poly_model))
      }, file = file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
