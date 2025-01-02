library(shiny)

# Global Data Preparation
date <- format(Sys.Date(), "%d%m%Y")
data <- na.omit(read.csv("Data/Materials/example_cal_curve.csv")) ## Read/Remove NA

########################   Model Calculations   ########################
poly_original <- lm(                      ## POLY 3rd DEGREE
  PEAK ~ poly(STD..ng., 3, raw = TRUE),
  data = data
)   
linear_original <- lm(                    ## LINEAR
  PEAK ~ STD..ng.,
  data = data
)
# Save the summaries of the original models as strings
original_model_summary_poly <- capture.output(summary(poly_original))
original_model_summary_linear <- capture.output(summary(linear_original))
# Extract R-squared values from the original models
rsquared_original_poly <- summary(poly_original)$r.squared
rsquared_original_linear <- summary(linear_original)$r.squared

########################        Define UI       ########################
ui <- fluidPage(
  # Layout sidebarPanel for model selection
  titlePanel("Calibration Curve Analysis"),
  fluidRow(
    column(4,
           selectInput("modelType", "Select Model Type",
                       choices = c("Polynomial 3rd Degree" = "poly", "Linear" = "linear"),
                       selected = "poly")
    ),
    column(2, numericInput("lod_x", "LoD (x):", value = 0.05, step = 0.01)),
    column(2, numericInput("loq_x", "LoQ (x):", value = 0.1, step = 0.01)),
    column(2, numericInput("lol_x", "LoL (x):", value = 1.5, step = 0.1))
  ),
  fluidRow(
    column(12,
           downloadButton("downloadPlot", "Download Filtered Plot"),
           downloadButton("downloadStats", "Download Filtered Statistics"),
           downloadButton("downloadOriginalPlot", "Download Original Plot"),
           downloadButton("downloadOriginalStats", "Download Original Statistics"),
           actionButton("reset", "Reset"))
  ),
  fluidRow(column(12,
                  div(style = "position: relative;",
                      uiOutput("plot_ui"),
                      absolutePanel(
                        top = 10, right = 10,
                        actionButton("zoom_toggle", label = "+")
                      )
                  )
  )),
  verbatimTextOutput("clickCoords"),
  fluidRow(column(6, 
                  verbatimTextOutput("originalStatsOutput")),
           column(6, 
                  verbatimTextOutput("filteredStatsOutput")))
)

# Define global variables to store after app closes
final_filtered_model <- NULL
final_filtered_data <- NULL

########################   Define Server Logic  ########################
server <- function(input, output, session) {
  # variables before click
  excluded_true_points <- reactiveVal(integer(0))
  included_false_points <- reactiveVal(integer(0))
  true_rows <- reactive({ data[data$T.F == 'True', ] })
  false_rows <- reactive({ data[data$T.F == 'False', ] })
  
  # Reactive value to track zoom mode
  zoom_mode <- reactiveVal(FALSE)
  
  # Reactive ranges for zooming
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  # Toggle zoom mode when "+" button is clicked
  observeEvent(input$zoom_toggle, {
    zoom_mode(!zoom_mode())
    # Update button label
    if (zoom_mode()) {
      updateActionButton(session, "zoom_toggle", label = "–")
    } else {
      updateActionButton(session, "zoom_toggle", label = "+")
    }
  })
  
  # Reactive displaying click coordinates
  observe({
    click <- input$plot_click            
    if (!is.null(click)) {
      Sys.sleep(0.1)
      output$clickCoords <- renderText({ paste("X:", click$x, "Y:", click$y) })
    }
  })
  
  ## The logic of selecting the closest point
  observeEvent(input$plot_click, {
    # Only process clicks when not in zoom mode
    if (!zoom_mode()) {
      click <- input$plot_click
      true_rows_df <- true_rows()
      false_rows_df <- false_rows()
      
      # Variables after click
      new_true_point <- which.min((true_rows_df$STD..ng. - click$x)^2 + (true_rows_df$PEAK - click$y)^2)
      new_false_point <- which.min((false_rows_df$STD..ng. - click$x)^2 + (false_rows_df$PEAK - click$y)^2)
      excluded_true <- excluded_true_points()
      included_false <- included_false_points()
      
      # Euclidean distance
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
    }
  })
  
  ## Reset points
  observeEvent(input$reset, {
    excluded_true_points(integer(0))
    included_false_points(integer(0))
    ranges$x <- NULL
    ranges$y <- NULL
  })
  
  ## Zooming logic
  observeEvent(input$plot_dblclick, {
    if (zoom_mode()) {
      brush <- input$plot_brush
      if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)
      } else {
        ranges$x <- NULL
        ranges$y <- NULL
      }
    }
  })
  
  ########################  MAIN PLOT FUNCTION  ########################
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
    
    # Not excluded true and included false
    combined_data <- rbind(true_points[, c("STD..ng.", "PEAK")], included_false_points_df[, c("STD..ng.", "PEAK")])
    
    model <- if (input$modelType == "poly") {
      lm(PEAK ~ poly(STD..ng., 3, raw = TRUE), data = combined_data)
    } else {
      lm(PEAK ~ STD..ng., data = combined_data)
    }
    rsquared <- summary(model)$r.squared
    
    # Store the filtered model and data
    final_filtered_model <<- model
    final_filtered_data <<- combined_data
    
    # Use user-input LOD, LOQ, LOL values
    lod_x <- input$lod_x
    loq_x <- input$loq_x
    lol_x <- input$lol_x
    
    lod_y <- predict(model, data.frame(STD..ng. = lod_x))
    loq_y <- predict(model, data.frame(STD..ng. = loq_x))
    lol_y <- predict(model, data.frame(STD..ng. = lol_x))
    
    list( plot = function() {
      plot(true_points$STD..ng., true_points$PEAK, 
           main = ifelse(input$modelType == "poly", "Calibration Curve - Polynomial 3rd Degree", "Calibration Curve - Linear Model"),
           xlab = "STD..ng.", 
           ylab = "PEAK", 
           pch = 16,
           col = "green",
           cex = 0.9,        
           cex.lab = 1.1,
           xlim = ranges$x,
           ylim = ranges$y
      )
      
      points(false_rows_df$STD..ng., false_rows_df$PEAK, pch = 16, col = "red")
      points(excluded_points$STD..ng., excluded_points$PEAK, pch = 16, col = "orange")
      points(included_false_points_df$STD..ng., included_false_points_df$PEAK, pch = 16, col = "blue")
      
      curve(predict(model, data.frame(STD..ng.=x)), add = TRUE, col = "black", lty = "dashed", lwd = 1)
      mtext(bquote(R^2 == .(rsquared)), side = 3)
      
      # Border markers
      abline(v = lod_x, col = "red", lty = 2)
      abline(v = loq_x, col = "red", lty = 2)
      abline(v = lol_x, col = "red", lty = 2)
      abline(h = lod_y, col = "blue", lty = 3)
      abline(h = loq_y, col = "blue", lty = 3)
      abline(h = lol_y, col = "blue", lty = 3)
      text(lod_x, lod_y, "LoD", col = "red", pos = 4)
      text(loq_x, loq_y, "LoQ", col = "red", pos = 4)
      text(lol_x, lol_y, "LoL", col = "red", pos = 4)
      
      legend("topleft",
             legend=c("True", "Excluded True", "False", "Included False"),
             col=c("green", "orange", "red", "blue"),
             pch=16,
             bty="n"  
      )
    },
    summary = function() { capture.output(summary(model)) }
    )
  }
  
  ########################    RENDER SECTION     ########################
  # Render Plot UI
  output$plot_ui <- renderUI({
    if (zoom_mode()) {
      plotOutput("calibrationPlot",
                 brush = brushOpts(
                   id = "plot_brush",
                   resetOnNew = TRUE
                 ),
                 dblclick = "plot_dblclick",
                 width = "100%",
                 height = "100vh"
      )
    } else {
      plotOutput("calibrationPlot",
                 click = "plot_click",
                 width = "100%",
                 height = "100vh"
      )
    }
  })
  
  # Render Plot
  output$calibrationPlot <- renderPlot({ generate_plot_and_model()$plot() })
  
  # Render Filtered Statistics
  output$filteredStatsOutput <- renderPrint({ 
    cat("Filtered Model Summary (Our plot and what is clicked - included/excluded):\n")
    cat(generate_plot_and_model()$summary(), sep = "\n") 
  })
  
  # Render Original Statistics
  output$originalStatsOutput <- renderPrint({
    if (input$modelType == "poly") {
      cat("Original Model Summary (Polynomial 3rd Degree):\n")
      cat(original_model_summary_poly, sep = "\n")
    } else {
      cat("Original Model Summary (Linear):\n")
      cat(original_model_summary_linear, sep = "\n")
    }
  })
  
  ########################  DOWNLOAD SECTION    ########################
  # Download Filtered Plot and Stats
  output$downloadPlot <- downloadHandler(
    filename = function() { paste("filtered_calibration_curve_", date, ".png", sep = "") },
    content = function(file) {
      png(file)
      generate_plot_and_model()$plot()
      dev.off()
    }
  )
  output$downloadStats <- downloadHandler(
    filename = function() { paste("Filtered_stats_", date, ".txt", sep = "") },
    content = function(file) {
      capture.output({ cat(generate_plot_and_model()$summary(), sep = "\n") }, file = file)
    }
  )
  
  # Download Original Plot and Stats
  output$downloadOriginalPlot <- downloadHandler(
    filename = function() { paste("original_calibration_curve_", date, ".png", sep = "") },
    content = function(file) {
      png(file)
      
      lod_x <- input$lod_x
      loq_x <- input$loq_x
      lol_x <- input$lol_x
      
      plot(data$STD..ng.[data$T.F == "True"], data$PEAK[data$T.F == "True"], 
           main = ifelse(input$modelType == "poly", "Original Calibration Curve - Polynomial 3rd Degree", "Original Calibration Curve - Linear Model"),
           xlab = "STD..ng.", 
           ylab = "PEAK", 
           pch = 16,
           col = "green",
           cex = 0.9,        
           cex.lab = 1.1,
           xlim = ranges$x,
           ylim = ranges$y
      )
      
      points(data$STD..ng.[data$T.F == "False"], data$PEAK[data$T.F == "False"], pch = 16, col = "red")
      
      if (input$modelType == "poly") {
        curve(predict(poly_original, data.frame(STD..ng.=x)), add = TRUE, col = "black", lty = "dashed", lwd = 1)
        mtext(bquote(R^2 == .(rsquared_original_poly)), side = 3)
      } else {
        abline(linear_original, col = "black", lty = "dashed", lwd = 1)
        mtext(bquote(R^2 == .(rsquared_original_linear)), side = 3)
      }
      
      # Add user-defined markers
      abline(v = lod_x, col = "red", lty = 2)
      abline(v = loq_x, col = "red", lty = 2)
      abline(v = lol_x, col = "red", lty = 2)
      
      lod_y <- predict(if (input$modelType == "poly") poly_original else linear_original, data.frame(STD..ng. = lod_x))
      loq_y <- predict(if (input$modelType == "poly") poly_original else linear_original, data.frame(STD..ng. = loq_x))
      lol_y <- predict(if (input$modelType == "poly") poly_original else linear_original, data.frame(STD..ng. = lol_x))
      
      abline(h = lod_y, col = "blue", lty = 3)
      abline(h = loq_y, col = "blue", lty = 3)
      abline(h = lol_y, col = "blue", lty = 3)
      text(lod_x, lod_y, "LoD", col = "red", pos = 4)
      text(loq_x, loq_y, "LoQ", col = "red", pos = 4)
      text(lol_x, lol_y, "LoL", col = "red", pos = 4)
      
      legend("topleft", legend = c("True", "False"), col = c("green", "red"), pch = 16, bty = "n")
      dev.off()
    }
  )
  
  output$downloadOriginalStats <- downloadHandler(
    filename = function() { paste("Original_stats_", date, ".txt", sep = "") },
    content = function(file) {
      if (input$modelType == "poly") {
        title <- "Original Model Summary (Polynomial 3rd Degree):\n"
        writeLines(c(title, original_model_summary_poly), con = file)
      } else {
        title <- "Original Model Summary (Linear):\n"
        writeLines(c(title, original_model_summary_linear), con = file)
      }
    }
  )
}

# To save objects after the app closes
onStop(function() {
  # Save the filtered model and data to an RDS file  --  single object
  saveRDS(final_filtered_model, "final_filtered_model.rds")
  saveRDS(final_filtered_data, "final_filtered_data.rds")
})

# Run the application
shinyApp(ui = ui, server = server)
