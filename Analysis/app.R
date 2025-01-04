library(shiny)

########################        Define UI       ########################
ui <- fluidPage(
  titlePanel("Calibration Curve Analysis"),
  
  ########## Upload section ##########
  fluidRow(
    column(12,
    fileInput("file_input", "Upload CSV File", 
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    actionButton("process_button", "Process Data"))
    ),
  
  ########## Field section ##########
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
  
  ########## Plot section ##########
  fluidRow(
    column(12,
           div(style = "position: relative; height: 90%;",
               uiOutput("plot_ui"),
               absolutePanel(
                 top = 10, right = 10,
                 actionButton("zoom_toggle", label = "+")
               )
           )
    )
  ),
  ########## Coordinates of click ##########
  verbatimTextOutput("clickCoords"),
  
  fluidRow(
    column(6, verbatimTextOutput("originalStatsOutput")),
    column(6, verbatimTextOutput("filteredStatsOutput"))
  ),
  
  ########## 1) HISTOGRAM SECTION (PEAK) ##########
  fluidRow(
    column(12, h3("Histograms of PEAK by unique STD..ng."))
  ),
  fluidRow(
    column(4,
           actionButton("histOriginal", "Histogram (Original)"),
           actionButton("histFiltered", "Histogram (Filtered)"),
           actionButton("histRange",    "Histogram (Filtered + Range)")
    ),
    column(8,
           plotOutput("histogramOutput")  
    )
  ),
  
  ########## 2) SEKCJA HISTOGRAMÓW (RESZTY) ##########
  fluidRow(
    column(12, h3("Histograms of Residuals by unique STD..ng."))
  ),
  fluidRow(
    column(4,
           actionButton("histResidOriginal", "Histogram Residuals (Original)"),
           actionButton("histResidFiltered", "Histogram Residuals (Filtered)"),
           actionButton("histResidRange",    "Histogram Residuals (Filtered + Range)")
    ),
    column(8,
           plotOutput("histogramResidOutput")  
    )
  ),
  
  ########## Correlation section ##########
  fluidRow(
    column(12, h3("Correlation Tests"))
  ),
  fluidRow(
    column(4,
           selectInput("corMethod", "Correlation Method",
                       choices = c("pearson", "spearman"), selected = "pearson"),
           actionButton("runCorOriginal", "Corr (Original)"),
           actionButton("runCorFiltered", "Corr (Filtered)"),
           actionButton("runCorRange",    "Corr (Filtered + Range)")
    ),
    column(8,
           verbatimTextOutput("corOutput")
    )
  )
)

# Define global variables to store after app closes
final_filtered_model <- NULL
final_filtered_data <- NULL

########################   Define Server Logic  ########################
server <- function(input, output, session) {
  
  # Reactive value to store the dataset
  data_reactive <- reactiveVal(NULL)
  
  # Update data_reactive when a new file is uploaded and processed
  observeEvent(input$process_button, {
    req(input$file_input)  # Ensure a file is uploaded
    file_path <- input$file_input$datapath
    uploaded_data <- na.omit(read.csv(file_path))
    data_reactive(uploaded_data)
  })
  
  date <- format(Sys.Date(), "%d%m%Y")
  
  ########################   Model Calculations   ########################
  # Reactive expressions for original models based on current data
  poly_original <- reactive({
    req(data_reactive())
    lm(PEAK ~ poly(STD..ng., 3, raw = TRUE), data = data_reactive())
  })
  
  linear_original <- reactive({
    req(data_reactive())
    lm(PEAK ~ STD..ng., data = data_reactive())
  })
  
  # Save the summaries of the original models as reactive expressions
  original_model_summary_poly <- reactive({
    capture.output(summary(poly_original()))
  })
  
  original_model_summary_linear <- reactive({
    capture.output(summary(linear_original()))
  })
  
  # Extract R-squared values from the original models
  rsquared_original_poly <- reactive({
    summary(poly_original())$r.squared
  })
  
  rsquared_original_linear <- reactive({
    summary(linear_original())$r.squared
  })
  
  # Values for included/excluded
  excluded_true_points <- reactiveVal(integer(0))
  included_false_points <- reactiveVal(integer(0))
  
  # Reactive subsets
  true_rows <- reactive({ 
    req(data_reactive())
    data_reactive()[data_reactive()$T.F == 'True', ] 
  })
  
  false_rows <- reactive({ 
    req(data_reactive())
    data_reactive()[data_reactive()$T.F == 'False', ] 
  })
  
  # Zoom mode
  zoom_mode <- reactiveVal(FALSE)
  # Zoom ranges
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  # Toggle zoom mode
  observeEvent(input$zoom_toggle, {
    zoom_mode(!zoom_mode())
    if (zoom_mode()) {
      updateActionButton(session, "zoom_toggle", label = "–")
    } else {
      updateActionButton(session, "zoom_toggle", label = "+")
    }
  })
  
  # Click coordinates
  observe({
    click <- input$plot_click            
    if (!is.null(click)) {
      Sys.sleep(0.1)
      output$clickCoords <- renderText({ paste("X:", round(click$x, 3), "Y:", round(click$y, 3)) })
    }
  })
  
  # Logic of selecting the closest point
  observeEvent(input$plot_click, {
    if (!zoom_mode()) {
      click <- input$plot_click
      true_rows_df <- true_rows()
      false_rows_df <- false_rows()
      
      new_true_point <- which.min((true_rows_df$STD..ng. - click$x)^2 + (true_rows_df$PEAK - click$y)^2)
      new_false_point <- which.min((false_rows_df$STD..ng. - click$x)^2 + (false_rows_df$PEAK - click$y)^2)
      excluded_true <- excluded_true_points()
      included_false <- included_false_points()
      
      dist_true <- (true_rows_df$STD..ng.[new_true_point] - click$x)^2 + (true_rows_df$PEAK[new_true_point] - click$y)^2
      dist_false <- (false_rows_df$STD..ng.[new_false_point] - click$x)^2 + (false_rows_df$PEAK[new_false_point] - click$y)^2
      
      if (dist_true < dist_false) {
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
  
  # Reset
  observeEvent(input$reset, {
    excluded_true_points(integer(0))
    included_false_points(integer(0))
    ranges$x <- NULL
    ranges$y <- NULL
  })
  
  # Zooming logic
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
  
  ########################  MAIN PLOT + MODEL  ########################
  generate_plot_and_model <- reactive({
    req(data_reactive())
    excluded_true <- excluded_true_points()
    included_false <- included_false_points()
    true_rows_df <- true_rows()
    false_rows_df <- false_rows()
    
    # Mark True that are excluded
    true_rows_df$True <- TRUE
    if (length(excluded_true) > 0) {
      true_rows_df$True[excluded_true] <- FALSE
    }
    
    # Mark False that are included
    false_rows_df$Included <- FALSE
    if (length(included_false) > 0) {
      false_rows_df$Included[included_false] <- TRUE
    }
    
    true_points <- true_rows_df[true_rows_df$True, ]
    excluded_points <- true_rows_df[!true_rows_df$True, ]
    included_false_points_df <- false_rows_df[false_rows_df$Included, ]
    
    combined_data <- rbind(
      true_points[, c("STD..ng.", "PEAK")],
      included_false_points_df[, c("STD..ng.", "PEAK")]
    )
    
    # Fit the model based on user selection
    model <- if (input$modelType == "poly") {
      lm(PEAK ~ poly(STD..ng., 3, raw = TRUE), data = combined_data)
    } else {
      lm(PEAK ~ STD..ng., data = combined_data)
    }
    rsquared <- summary(model)$r.squared
    
    # Store the final filtered model and data
    final_filtered_model <<- model
    final_filtered_data <<- combined_data
    
    # User-defined LoD, LoQ, LoL
    lod_x <- input$lod_x
    loq_x <- input$loq_x
    lol_x <- input$lol_x
    
    lod_y <- predict(model, data.frame(STD..ng. = lod_x))
    loq_y <- predict(model, data.frame(STD..ng. = loq_x))
    lol_y <- predict(model, data.frame(STD..ng. = lol_x))
    
    list(
      plot = function() {
        plot(true_points$STD..ng., true_points$PEAK, 
             main = ifelse(input$modelType == "poly", 
                           "Calibration Curve - Polynomial 3rd Degree", 
                           "Calibration Curve - Linear Model"),
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
        
        curve(predict(model, data.frame(STD..ng.=x)),
              add = TRUE, col = "black", lty = "dashed", lwd = 1)
        mtext(bquote(R^2 == .(round(rsquared, 4))), side = 3)
        
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
      summary = function() {
        capture.output(summary(model))
      }
    )
  })
  
  ########################    RENDER SECTION     ########################
  output$plot_ui <- renderUI({
    if (zoom_mode()) {
      plotOutput("calibrationPlot",
                 brush = brushOpts(id = "plot_brush", resetOnNew = TRUE),
                 dblclick = "plot_dblclick",
                 width = "100%",
                 height = "600px"
      )
    } else {
      plotOutput("calibrationPlot",
                 click = "plot_click",
                 width = "100%",
                 height = "90vh"
      )
    }
  })
  
  # Main plot
  output$calibrationPlot <- renderPlot({
    req(generate_plot_and_model())
    generate_plot_and_model()$plot()
  })
  
  # Filtered Stats
  output$filteredStatsOutput <- renderPrint({ 
    req(generate_plot_and_model())
    cat("Filtered Model Summary (Our plot and what is clicked - included/excluded):\n")
    cat(generate_plot_and_model()$summary(), sep = "\n")
  })
  
  # Original Stats
  output$originalStatsOutput <- renderPrint({
    req(data_reactive())
    if (input$modelType == "poly") {
      cat("Original Model Summary (Polynomial 3rd Degree):\n")
      cat(original_model_summary_poly(), sep = "\n")
    } else {
      cat("Original Model Summary (Linear):\n")
      cat(original_model_summary_linear(), sep = "\n")
    }
  })
  
  ########################  DOWNLOAD SECTION  ########################
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
      capture.output({
        cat("Filtered Model Summary:\n")
        cat(generate_plot_and_model()$summary(), sep = "\n")
      }, file = file)
    }
  )
  
  output$downloadOriginalPlot <- downloadHandler(
    filename = function() { paste("original_calibration_curve_", date, ".png", sep = "") },
    content = function(file) {
      png(file)
      
      req(data_reactive())
      
      lod_x <- input$lod_x
      loq_x <- input$loq_x
      lol_x <- input$lol_x
      
      # Original model based on current data and model type
      original_model <- if (input$modelType == "poly") {
        poly_original()
      } else {
        linear_original()
      }
      
      plot(data_reactive()$STD..ng.[data_reactive()$T.F == "True"], 
           data_reactive()$PEAK[data_reactive()$T.F == "True"], 
           main = ifelse(input$modelType == "poly", 
                         "Original Calibration Curve - Polynomial 3rd Degree", 
                         "Original Calibration Curve - Linear Model"),
           xlab = "STD..ng.", 
           ylab = "PEAK", 
           pch = 16,
           col = "green",
           cex = 0.9,        
           cex.lab = 1.1,
           xlim = ranges$x,
           ylim = ranges$y
      )
      
      points(data_reactive()$STD..ng.[data_reactive()$T.F == "False"], 
             data_reactive()$PEAK[data_reactive()$T.F == "False"], pch = 16, col = "red")
      
      if (input$modelType == "poly") {
        curve(predict(original_model, data.frame(STD..ng.=x)), add = TRUE, col = "black", lty = "dashed", lwd = 1)
        mtext(bquote(R^2 == .(round(rsquared_original_poly(), 4))), side = 3)
      } else {
        abline(original_model, col = "black", lty = "dashed", lwd = 1)
        mtext(bquote(R^2 == .(round(rsquared_original_linear(), 4))), side = 3)
      }
      
      abline(v = lod_x, col = "red", lty = 2)
      abline(v = loq_x, col = "red", lty = 2)
      abline(v = lol_x, col = "red", lty = 2)
      
      lod_y <- predict(original_model, data.frame(STD..ng. = lod_x))
      loq_y <- predict(original_model, data.frame(STD..ng. = loq_x))
      lol_y <- predict(original_model, data.frame(STD..ng. = lol_x))
      
      abline(h = lod_y, col = "blue", lty = 3)
      abline(h = loq_y, col = "blue", lty = 3)
      abline(h = lol_y, col = "blue", lty = 3)
      text(lod_x, lod_y, "LoD", col = "red", pos = 4)
      text(loq_x, loq_y, "LoQ", col = "red", pos = 4)
      text(lol_x, lol_y, "LoL", col = "red", pos = 4)
      
      legend("topleft", legend = c("True", "False"), 
             col = c("green", "red"), pch = 16, bty = "n")
      dev.off()
    }
  )
  
  output$downloadOriginalStats <- downloadHandler(
    filename = function() { paste("Original_stats_", date, ".txt", sep = "") },
    content = function(file) {
      req(data_reactive())
      if (input$modelType == "poly") {
        title <- "Original Model Summary (Polynomial 3rd Degree):\n"
        writeLines(c(title, original_model_summary_poly()), con = file)
      } else {
        title <- "Original Model Summary (Linear):\n"
        writeLines(c(title, original_model_summary_linear()), con = file)
      }
    }
  )
  
  ########## Correlation Tests ##########
  observeEvent(input$runCorOriginal, {
    req(data_reactive())
    method_chosen <- input$corMethod
    if (nrow(data_reactive()) < 2) {
      output$corOutput <- renderPrint({ cat("Not enough data points in original.\n") })
    } else {
      cor_res <- cor.test(data_reactive()$STD..ng., data_reactive()$PEAK, method = method_chosen)
      output$corOutput <- renderPrint({
        cat("Correlation on ORIGINAL data (method =", method_chosen, ")\n\n")
        print(cor_res)
      })
    }
  })
  
  observeEvent(input$runCorFiltered, {
    method_chosen <- input$corMethod
    if (is.null(final_filtered_data) || nrow(final_filtered_data) < 2) {
      output$corOutput <- renderPrint({ cat("Not enough data in filtered.\n") })
    } else {
      cor_res <- cor.test(final_filtered_data$STD..ng., final_filtered_data$PEAK, method = method_chosen)
      output$corOutput <- renderPrint({
        cat("Correlation on FILTERED data (method =", method_chosen, ")\n\n")
        print(cor_res)
      })
    }
  })
  
  observeEvent(input$runCorRange, {
    method_chosen <- input$corMethod
    if (is.null(final_filtered_data) || nrow(final_filtered_data) < 2) {
      output$corOutput <- renderPrint({ cat("Not enough data in filtered.\n") })
      return()
    }
    fd <- final_filtered_data
    lod_val <- input$lod_x
    lol_val <- input$lol_x
    range_data <- fd[fd$STD..ng. >= lod_val & fd$STD..ng. <= lol_val, ]
    if (nrow(range_data) < 2) {
      output$corOutput <- renderPrint({ cat("No enough points in [LoD, LoL].\n") })
      return()
    }
    cor_res <- cor.test(range_data$STD..ng., range_data$PEAK, method = method_chosen)
    output$corOutput <- renderPrint({
      cat("Correlation on FILTERED data *within* [LoD, LoL] (method =", method_chosen, ")\n\n")
      print(cor_res)
    })
  })
  
  ########## 1) HISTOGRAMS (PEAK) - uniq STD..ng. ##########
  plotHistogramGroups <- function(df, main_label = "Histogram") {
    if (nrow(df) < 1) {
      plot(0, main = paste(main_label, "- no data"), xlab = "PEAK", ylab = "", col="white")
      return()
    }
    df$STDgroup <- as.factor(df$STD..ng.)
    groups <- levels(df$STDgroup)
    
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    
    nG <- length(groups)
    nrowcol <- ceiling(sqrt(nG))
    par(mfrow = c(nrowcol, nrowcol))
    
    for (g in groups) {
      subd <- subset(df, STDgroup == g)
      if (nrow(subd) > 0) {
        sw_pval <- tryCatch({
          shapiro.test(subd$PEAK)$p.value
        }, error = function(e) { NA })
        mu <- mean(subd$PEAK)
        sigma <- sd(subd$PEAK)
        if (sigma == 0) {
          ks_pval <- NA
        } else {
          ks_pval <- tryCatch({
            ks.test(subd$PEAK, "pnorm", mean = mu, sd = sigma)$p.value
          }, error = function(e) { NA })
        }
        
        hh <- hist(subd$PEAK,
                   main = paste(main_label, "\nSTD..ng. =", g),
                   xlab = "PEAK",
                   col = "skyblue",
                   border = "white"
        )
        ymax <- max(hh$counts)
        txt <- paste("SW p=", ifelse(is.na(sw_pval), "NA", formatC(sw_pval, format="e", digits=2)),
                     "\nKS p=", ifelse(is.na(ks_pval), "NA", formatC(ks_pval, format="e", digits=2)))
        
        text(x = mean(range(subd$PEAK)), y = 0.8*ymax, labels = txt, col="red")
      } else {
        plot(0, main = paste("No data for", g), xlab="", ylab="", col="white")
      }
    }
  }
  
  observeEvent(input$histOriginal, {
    output$histogramOutput <- renderPlot({
      plotHistogramGroups(data_reactive(), main_label = "Original Data")
    })
  })
  
  observeEvent(input$histFiltered, {
    output$histogramOutput <- renderPlot({
      if (is.null(final_filtered_data)) {
        plot(0, main = "No filtered data", xlab="", ylab="", col="white")
      } else {
        plotHistogramGroups(final_filtered_data, main_label = "Filtered Data")
      }
    })
  })
  
  observeEvent(input$histRange, {
    output$histogramOutput <- renderPlot({
      if (is.null(final_filtered_data)) {
        plot(0, main = "No filtered data", xlab="", ylab="", col="white")
      } else {
        fd <- final_filtered_data
        lod_val <- input$lod_x
        lol_val <- input$lol_x
        range_data <- fd[fd$STD..ng. >= lod_val & fd$STD..ng. <= lol_val, ]
        if (nrow(range_data) < 1) {
          plot(0, main = "No data in [LoD, LoL]", xlab="", ylab="", col="white")
        } else {
          plotHistogramGroups(range_data, main_label = "Filtered + Range Data")
        }
      }
    })
  })
  
  ########## 2) HISTOGRAMS (residuals) ##########
  # Function to compute residuals
  computeResiduals <- function(df, type = c("original","filtered")) {
    type <- match.arg(type)
    if (nrow(df) < 1) {
      df$resid <- numeric(0)
      return(df)
    }
    if (type == "original") {
      original_model <- if (input$modelType == "poly") {
        poly_original()
      } else {
        linear_original()
      }
      preds <- predict(original_model, newdata = df)
      df$resid <- df$PEAK - preds
    } else { # filtered
      if (is.null(final_filtered_model)) {
        df$resid <- numeric(0)
      } else {
        preds <- predict(final_filtered_model, newdata = df)
        df$resid <- df$PEAK - preds
      }
    }
    df
  }
  
  plotHistogramResiduals <- function(df, main_label = "Histogram Residuals") {
    if (nrow(df) < 1) {
      plot(0, main = paste(main_label, "- no data"), xlab = "resid", ylab = "", col="white")
      return()
    }
    df$STDgroup <- as.factor(df$STD..ng.)
    groups <- levels(df$STDgroup)
    
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    
    nG <- length(groups)
    nrowcol <- ceiling(sqrt(nG))
    par(mfrow = c(nrowcol, nrowcol))
    
    for (g in groups) {
      subd <- subset(df, STDgroup == g)
      if (nrow(subd) > 0) {
        sw_pval <- tryCatch({
          shapiro.test(subd$resid)$p.value
        }, error = function(e) { NA })
        mu <- mean(subd$resid)
        sigma <- sd(subd$resid)
        if (sigma == 0) {
          ks_pval <- NA
        } else {
          ks_pval <- tryCatch({
            ks.test(subd$resid, "pnorm", mean = mu, sd = sigma)$p.value
          }, error = function(e) { NA })
        }
        
        hh <- hist(subd$resid,
                   main = paste(main_label, "\nSTD..ng. =", g),
                   xlab = "resid",
                   col = "lightgreen",
                   border = "white"
        )
        ymax <- max(hh$counts)
        txt <- paste("SW p=", ifelse(is.na(sw_pval), "NA", formatC(sw_pval, format="e", digits=2)),
                     "\nKS p=", ifelse(is.na(ks_pval), "NA", formatC(ks_pval, format="e", digits=2)))
        
        text(x = mean(range(subd$resid)), y = 0.8*ymax, labels = txt, col="red")
      } else {
        plot(0, main = paste("No data for", g), xlab="", ylab="", col="white")
      }
    }
  }
  
  # Histogram Residuals (Original)
  observeEvent(input$histResidOriginal, {
    output$histogramResidOutput <- renderPlot({
      req(data_reactive())
      df_orig <- computeResiduals(data_reactive(), type="original")
      plotHistogramResiduals(df_orig, main_label = "Original Residuals")
    })
  })
  
  # Histogram Residuals (Filtered)
  observeEvent(input$histResidFiltered, {
    output$histogramResidOutput <- renderPlot({
      if (is.null(final_filtered_data) || nrow(final_filtered_data) < 1) {
        plot(0, main = "No filtered data", xlab="", ylab="", col="white")
      } else {
        df_filt <- computeResiduals(final_filtered_data, type="filtered")
        plotHistogramResiduals(df_filt, main_label = "Filtered Residuals")
      }
    })
  })
  
  # Histogram Residuals (Filtered + Range)
  observeEvent(input$histResidRange, {
    output$histogramResidOutput <- renderPlot({
      if (is.null(final_filtered_data) || nrow(final_filtered_data) < 1) {
        plot(0, main = "No filtered data", xlab="", ylab="", col="white")
      } else {
        fd <- final_filtered_data
        lod_val <- input$lod_x
        lol_val <- input$lol_x
        range_data <- fd[fd$STD..ng. >= lod_val & fd$STD..ng. <= lol_val, ]
        if (nrow(range_data) < 1) {
          plot(0, main = "No data in [LoD, LoL]", xlab="", ylab="", col="white")
        } else {
          df_range <- computeResiduals(range_data, type="filtered")
          plotHistogramResiduals(df_range, main_label = "Filtered + Range Residuals")
        }
      }
    })
  })
}

# Save after closing
onStop(function() {
  saveRDS(final_filtered_model, "final_filtered_model.rds")
  saveRDS(final_filtered_data, "final_filtered_data.rds")
})

# Activate
shinyApp(ui = ui, server = server)
