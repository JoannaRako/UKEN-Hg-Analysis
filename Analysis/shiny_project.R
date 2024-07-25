

#Outside the server function:
  
#  Loading libraries.
# Defining the ui and server functions.
# Calling shinyApp() to run the application.
# Inside the server function:
#  
#  Data preparation.
# Model calculation.
# Plot creation and rendering.

library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Calibration Curve Analysis"),
  sidebarLayout(
    sidebarPanel(
      downloadButton("downloadPlot", "Download Plot"),
      downloadButton("downloadStats", "Download Statistics")
    ),
    mainPanel(
      plotOutput("calibrationPlot"),
      verbatimTextOutput("statsOutput")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expression to process data
  data <- reactive({
    df <- read.csv("Data/Materials/example_cal_curve.csv")
    df <- na.omit(df)
    true_rows <- df[df$T.F == 'True', ]
    false_rows <- df[df$T.F == 'False', ]
    list(df = df, true_rows = true_rows, false_rows = false_rows)
  })
  
  # Reactive expression for model fitting
  poly_models <- reactive({
    true_rows <- data()$true_rows
    poly_model <- lm(
      PEAK ~ poly(STD..ng., 3, raw=TRUE), 
      data = true_rows
    )
    poly_original <- lm(
      PEAK ~ poly(STD..ng., 3, raw=TRUE), 
      data = data()$df
    )
    list(poly_model = poly_model, poly_original = poly_original)
  })
  
  # Render Plot
  output$calibrationPlot <- renderPlot({
    true_rows <- data()$true_rows
    false_rows <- data()$false_rows
    poly_model <- poly_models()$poly_model
    rsquared <- summary(poly_model)$r.squared
    
    plot(true_rows$STD..ng., true_rows$PEAK, 
         main = "Calibration Curve - Polynomial 3rd Degree",
         xlab = "STD..ng.", 
         ylab = "PEAK", 
         pch = 16,
         col = "chartreuse3",
         cex = 1,        # point_size
         cex.lab = 1.1   # lab_size
    )
    
    points(false_rows$STD..ng., false_rows$PEAK, 
           pch = 16, 
           col = "red"
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
           legend=c("True", "False"),
           col=c("chartreuse3", "red"),
           pch=16,
           bty="n"  # without border
    )
  })
  
  # Render Statistics
  output$statsOutput <- renderPrint({
    poly_original <- poly_models()$poly_original
    poly_model <- poly_models()$poly_model
    
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
      true_rows <- data()$true_rows
      false_rows <- data()$false_rows
      poly_model <- poly_models()$poly_model
      rsquared <- summary(poly_model)$r.squared
      
      plot(true_rows$STD..ng., true_rows$PEAK, 
           main = "Calibration Curve - Polynomial 3rd Degree",
           xlab = "STD..ng.", 
           ylab = "PEAK", 
           pch = 16,
           col = "chartreuse3",
           cex = 1,        # point_size
           cex.lab = 1.1   # lab_size
      )
      
      points(false_rows$STD..ng., false_rows$PEAK, 
             pch = 16, 
             col = "red"
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
             legend=c("True", "False"),
             col=c("chartreuse3", "red"),
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
      capture.output({
        cat("Original Model Summary:\n")
        print(summary(poly_models()$poly_original))
        cat("\nFiltered Model Summary:\n")
        print(summary(poly_models()$poly_model))
      }, file = file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
