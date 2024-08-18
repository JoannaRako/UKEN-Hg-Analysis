library(shiny)

# Outside the server function: Data preparation
date <- format(Sys.Date(), "%d%m%Y")

# Import, read and remove NA
data <- read.csv("Data/Materials/example_cal_curve.csv")
data <- na.omit(data)
head(data)

# POLY 3rd DEGREE - MODEL CALCULATION
poly_original <- lm(
  PEAK ~ poly(STD..ng., 3, raw = TRUE),
  data = data
)

# Save the summary of the original model as a string
original_model_summary <- capture.output(summary(poly_original))

# Extract R-squared value from the original model
rsquared_original <- summary(poly_original)$r.squared

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
  
  # Reactive value to store selected outliers and included false points
  selected_outliers <- reactiveVal(integer(0))
  included_false_points <- reactiveVal(integer(0))
  
  # Reactive expressions for filtering data
  true_rows <- reactive({
    data[data$T.F == 'True', ]
  })
  
  false_rows <- reactive({
    data[data$T.F == 'False', ]
  })
  
  # Observe click events on the plot
  observeEvent(input$plot_click, {
    click <- input$plot_click
    
    # Determine which data point is closest to the click location
    true_rows_df <- true_rows()
    false_rows_df <- false_rows()
    
    new_true_point <- which.min((true_rows_df$STD..ng. - click$x)^2 + (true_rows_df$PEAK - click$y)^2)
    new_false_point <- which.min((false_rows_df$STD..ng. - click$x)^2 + (false_rows_df$PEAK - click$y)^2)
    
    outliers <- selected_outliers()
    included_false <- included_false_points()
    
    if ((true_rows_df$STD..ng.[new_true_point] - click$x)^2 + (true_rows_df$PEAK[new_true_point] - click$y)^2 <
        (false_rows_df$STD..ng.[new_false_point] - click$x)^2 + (false_rows_df$PEAK[new_false_point] - click$y)^2) {
      
      if (new_true_point %in% outliers) {
        outliers <- outliers[outliers != new_true_point]
      } else {
        outliers <- c(outliers, new_true_point)
      }
      selected_outliers(outliers)
      
    } else {
      if (new_false_point %in% included_false) {
        included_false <- included_false[included_false != new_false_point]
      } else {
        included_false <- c(included_false, new_false_point)
      }
      included_false_points(included_false)
    }
  })
  
  # Reset button to clear selected outliers and included false points
  observeEvent(input$reset, {
    selected_outliers(integer(0))
    included_false_points(integer(0))
  })
  
  # Render Plot
  output$calibrationPlot <- renderPlot({
    outliers <- selected_outliers()
    included_false <- included_false_points()
    
    true_rows_df <- true_rows()
    false_rows_df <- false_rows()
    
    true_rows_df$True <- TRUE
    true_rows_df$True[outliers] <- FALSE
    
    false_rows_df$Included <- FALSE
    false_rows_df$Included[included_false] <- TRUE
    
    true_points <- true_rows_df[true_rows_df$True, ]
    excluded_points <- true_rows_df[!true_rows_df$True, ]
    included_false_points_df <- false_rows_df[false_rows_df$Included, ]
    
    combined_data <- rbind(true_points[, c("STD..ng.", "PEAK")], included_false_points_df[, c("STD..ng.", "PEAK")])
    
    poly_model <- lm(
      PEAK ~ poly(STD..ng., 3, raw = TRUE),
      data = combined_data
    )
    rsquared <- summary(poly_model)$r.squared
    
    plot(true_points$STD..ng., true_points$PEAK, 
         main = "Calibration Curve - Polynomial 3rd Degree",
         xlab = "STD..ng.", 
         ylab = "PEAK", 
         pch = 16,
         col = "green",
         cex = 1,        
         cex.lab = 1.1   
    )
    
    points(false_rows_df$STD..ng., false_rows_df$PEAK, 
           pch = 16, 
           col = "red"
    )
    
    points(excluded_points$STD..ng., excluded_points$PEAK, 
           pch = 16, 
           col = "orange"
    )
    
    points(included_false_points_df$STD..ng., included_false_points_df$PEAK, 
           pch = 16, 
           col = "blue"
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
           legend=c("True", "Excluded", "False", "Included False"),
           col=c("green", "orange", "red", "blue"),
           pch=16,
           bty="n"  
    )
  })
  
  # Render Statistics
  output$statsOutput <- renderPrint({
    outliers <- selected_outliers()
    included_false <- included_false_points()
    
    true_rows_df <- true_rows()
    false_rows_df <- false_rows()
    
    true_rows_df$True <- TRUE
    true_rows_df$True[outliers] <- FALSE
    
    false_rows_df$Included <- FALSE
    false_rows_df$Included[included_false] <- TRUE
    
    true_points <- true_rows_df[true_rows_df$True, ]
    included_false_points_df <- false_rows_df[false_rows_df$Included, ]
    
    combined_data <- rbind(true_points[, c("STD..ng.", "PEAK")], included_false_points_df[, c("STD..ng.", "PEAK")])
    
    poly_model <- lm(
      PEAK ~ poly(STD..ng., 3, raw = TRUE),
      data = combined_data
    )
    
    cat("Original Model Summary:\n")
    cat(original_model_summary, sep = "\n")
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
      included_false <- included_false_points()
      
      true_rows_df <- true_rows()
      false_rows_df <- false_rows()
      
      true_rows_df$True <- TRUE
      true_rows_df$True[outliers] <- FALSE
      
      false_rows_df$Included <- FALSE
      false_rows_df$Included[included_false] <- TRUE
      
      true_points <- true_rows_df[true_rows_df$True, ]
      excluded_points <- true_rows_df[!true_rows_df$True, ]
      included_false_points_df <- false_rows_df[false_rows_df$Included, ]
      
      combined_data <- rbind(true_points[, c("STD..ng.", "PEAK")], included_false_points_df[, c("STD..ng.", "PEAK")])
      
      poly_model <- lm(
        PEAK ~ poly(STD..ng., 3, raw = TRUE),
        data = combined_data
      )
      rsquared <- summary(poly_model)$r.squared
      
      plot(true_points$STD..ng., true_points$PEAK, 
           main = "Calibration Curve - Polynomial 3rd Degree",
           xlab = "STD..ng.", 
           ylab = "PEAK", 
           pch = 16,
           col = "green",
           cex = 1,        
           cex.lab = 1.1   
      )
      
      points(false_rows_df$STD..ng., false_rows_df$PEAK, 
             pch = 16, 
             col = "red"
      )
      
      points(excluded_points$STD..ng., excluded_points$PEAK, 
             pch = 16, 
             col = "orange"
      )
      
      points(included_false_points_df$STD..ng., included_false_points_df$PEAK, 
             pch = 16, 
             col = "blue"
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
             legend=c("True", "Excluded", "False", "Included False"),
             col=c("green", "orange", "red", "blue"),
             pch=16,
             bty="n"  
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
      included_false <- included_false_points()
      
      true_rows_df <- true_rows()
      false_rows_df <- false_rows()
      
      true_rows_df$True <- TRUE
      true_rows_df$True[outliers] <- FALSE
      
      false_rows_df$Included <- FALSE
      false_rows_df$Included[included_false] <- TRUE
      
      true_points <- true_rows_df[true_rows_df$True, ]
      included_false_points_df <- false_rows_df[false_rows_df$Included, ]
      
      combined_data <- rbind(true_points[, c("STD..ng.", "PEAK")], included_false_points_df[, c("STD..ng.", "PEAK")])
      
      poly_model <- lm(
        PEAK ~ poly(STD..ng., 3, raw = TRUE),
        data = combined_data
      )
      
      capture.output({
        cat("Original Model Summary:\n")
        cat(original_model_summary, sep = "\n")
        cat("\nFiltered Model Summary:\n")
        print(summary(poly_model))
      }, file = file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
