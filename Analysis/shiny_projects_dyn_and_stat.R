library(shiny)

# Outside the server function: Data preparation
date <- format(Sys.Date(), "%d%m%Y")

# Import, read and remove NA
data <- read.csv("Data/Materials/example_cal_curve.csv")
data <- na.omit(data)

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
  
  # Layout without sidebarPanel
  fluidRow(
    column(12, # Adjust the width as needed
           downloadButton("downloadPlot", "Download Filtered Plot"),
           downloadButton("downloadStats", "Download Filtered Statistics"),
           downloadButton("downloadOriginalPlot", "Download Original Plot"),
           downloadButton("downloadOriginalStats", "Download Original Statistics"),
           actionButton("reset", "Reset")
    )
  ),
  
  fluidRow(
    column(12,
           # Adjust the plotOutput size
           plotOutput("calibrationPlot", 
                      click = "plot_click",
                      width = "100%",   # Stretch to full width of the container
                      height = "100vh"  # Set the height of the plot
           )
    )
  ),
  
  fluidRow(
    column(12,
           verbatimTextOutput("clickCoords")
    ),
  ),
  
  fluidRow(
    column(6,
           verbatimTextOutput("originalStatsOutput")
    ),
    column(6,
           verbatimTextOutput("filteredStatsOutput")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  ##################
  ### Just for click
  ##################
  
  observe({
    click <- input$plot_click
    if (!is.null(click)) {
      Sys.sleep(0.1)  # Small delay
      output$clickCoords <- renderText({
        paste("X:", click$x, "Y:", click$y)
      })
    }
  })
  
  ##########
  ### Values
  ##########
  
  excluded_true_points <- reactiveVal(integer(0))
  included_false_points <- reactiveVal(integer(0))
  
  true_rows <- reactive({
    data[data$T.F == 'True', ]
  })
  
  false_rows <- reactive({
    data[data$T.F == 'False', ]
  })
  
  observeEvent(input$plot_click, {
    click <- input$plot_click
    true_rows_df <- true_rows()
    false_rows_df <- false_rows()
    
    new_true_point <- which.min((true_rows_df$STD..ng. - click$x)^2 + (true_rows_df$PEAK - click$y)^2)
    new_false_point <- which.min((false_rows_df$STD..ng. - click$x)^2 + (false_rows_df$PEAK - click$y)^2)
    
    excluded_true <- excluded_true_points()
    included_false <- included_false_points()
    
    if ((true_rows_df$STD..ng.[new_true_point] - click$x)^2 + (true_rows_df$PEAK[new_true_point] - click$y)^2 <
        (false_rows_df$STD..ng.[new_false_point] - click$x)^2 + (false_rows_df$PEAK[new_false_point] - click$y)^2) {
      
      if (new_true_point %in% excluded_true) {
        excluded_true <- excluded_true[excluded_true != new_true_point]
      } else {
        excluded_true <- c(excluded_true, new_true_point)
      }
      excluded_true_points(excluded_true) 
      
    } else {
      if (new_false_point %in% included_false) {
        included_false <- included_false[included_false != new_false_point]
      } else {
        included_false <- c(included_false, new_false_point)
      }
      included_false_points(included_false)
    }
  })
  
  
  ################
  # Functionality
  ################
  
  
  observeEvent(input$reset, {
    excluded_true_points(integer(0))
    included_false_points(integer(0))
  })
  
  # Function to generate plot and model summary
  generate_plot_and_model <- function() {
    excluded_true <- excluded_true_points()
    included_false <- included_false_points()
    
    true_rows_df <- true_rows()
    false_rows_df <- false_rows()
    
    true_rows_df$True <- TRUE
    true_rows_df$True[excluded_true] <- FALSE
    
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
    
    list(
      plot = function() {
        plot(true_points$STD..ng., true_points$PEAK, 
             main = "Calibration Curve - Polynomial 3rd Degree",
             xlab = "STD..ng.", 
             ylab = "PEAK", 
             pch = 16,
             col = "green",
             cex = 0.9,        
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
               legend=c("True", "Excluded True", "False", "Included False"),
               col=c("green", "orange", "red", "blue"),
               pch=16,
               bty="n"  
        )
      },
      summary = function() {
        cat("Filtered Model Summary:\n")
        capture.output(summary(poly_model))
      }
    )
  }
  
  # Render Plot
  output$calibrationPlot <- renderPlot({
    generate_plot_and_model()$plot()
  })
  
  # Render Filtered Statistics
  output$filteredStatsOutput <- renderPrint({
    cat(generate_plot_and_model()$summary(), sep = "\n")
  })
  
  # Render Original Statistics
  output$originalStatsOutput <- renderPrint({
    cat("Original Model Summary:\n")
    cat(original_model_summary, sep = "\n")
  })
  
  
  
  ############# FILTERED STAT DOWNLOAD ############
  
  # Download Filtered Plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("filtered_calibration_curve_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)
      generate_plot_and_model()$plot()
      dev.off()
    }
  )
  
  # Download Filtered Statistics
  output$downloadStats <- downloadHandler(
    filename = function() {
      paste("Filtered_stats_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      capture.output({
        cat(generate_plot_and_model()$summary(), sep = "\n")
      }, file = file)
    }
  )
  
  
  
  ############# ORIGINAL STAT DOWNLOAD ############
  
  # Download Original Plot
  output$downloadOriginalPlot <- downloadHandler(
    filename = function() {
      paste("original_calibration_curve_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)
      
      plot(data$STD..ng.[data$T.F == "True"], data$PEAK[data$T.F == "True"], 
           main = "Original Calibration Curve - Polynomial 3rd Degree",
           xlab = "STD..ng.", 
           ylab = "PEAK", 
           pch = 16,
           col = "green",
           cex = 0.9,        
           cex.lab = 1.1,
           ylim = range(data$PEAK),
           xlim = range(data$STD..ng.)
      )
      
      points(data$STD..ng.[data$T.F == "False"], data$PEAK[data$T.F == "False"], 
             pch = 16, 
             col = "red"
      )
      
      curve(predict(poly_original, data.frame(STD..ng.=x)), 
            add = TRUE, 
            col = "black",
            lty = "dashed",
            lwd = 1
      )
      
      mtext(bquote(
        R^2 == .(rsquared_original)),
        side = 3
      )
      
      legend("topleft",
             legend = c("True", "False"),
             col = c("green", "red"),
             pch = 16,
             bty = "n"
      )
      
      dev.off()
    }
  )
  
  # Download Original Statistics
  output$downloadOriginalStats <- downloadHandler(
    filename = function() {
      paste("Original_stats_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      title <- "Original Model Summary:\n"
      cleaned_summary <- capture.output(summary(poly_original))
      content <- paste(title, paste(cleaned_summary, collapse = "\n"), sep = "")
      
      writeLines(content, con = file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
