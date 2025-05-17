# Universal Data Profiler - Professional Grade Shiny App
# Load all required packages
library(shiny)
library(shinythemes)
library(shinyjs)
library(DT)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(scales)
library(kableExtra)
library(rmarkdown)
library(moments)
library(outliers)
library(correlationfunnel)

# Custom CSS for professional styling
custom_css <- "
  .well {
    background-color: #ffffff;
    border: 1px solid #e0e0e0;
    border-radius: 4px;
    box-shadow: 0 1px 3px rgba(0,0,0,0.12);
  }
  .nav-tabs>li>a {
    color: #555555;
    font-weight: 500;
  }
  .nav-tabs>li.active>a {
    color: #0066cc;
    border-bottom: 2px solid #0066cc;
  }
  .btn-primary {
    background-color: #0066cc;
    border-color: #005bb7;
  }
  .btn-primary:hover {
    background-color: #005bb7;
    border-color: #004b96;
  }
  .data-summary-card {
    background: white;
    border-radius: 4px;
    padding: 15px;
    margin-bottom: 15px;
    box-shadow: 0 1px 3px rgba(0,0,0,0.1);
  }
  .data-summary-card h4 {
    margin-top: 0;
    color: #333;
    font-size: 16px;
  }
  .data-summary-card p {
    margin-bottom: 0;
    font-size: 24px;
    font-weight: 500;
    color: #0066cc;
  }
  .progress-bar {
    background-color: #0066cc;
  }
  .file-input-label {
    font-weight: 500;
    color: #555;
  }
  .shiny-notification {
    background-color: #333;
    color: white;
    border-radius: 4px;
  }
"

# UI Definition
ui <- fluidPage(
  theme = shinytheme("flatly"),
  useShinyjs(),
  tags$head(tags$style(HTML(custom_css))),
  
  titlePanel(
    div(
      img(src = "https://via.placeholder.com/40x40", height = 40, style = "margin-right:10px;"),
      span("Universal Data Profiler", style = "font-weight: 300; font-size: 28px; color: #333;"),
      span("v1.0", style = "font-size: 12px; color: #999; margin-left: 5px; vertical-align: super;")
    ),
    windowTitle = "Universal Data Profiler"
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      wellPanel(
        tags$label("Upload your data file", class = "file-input-label"),
        fileInput("file_input", NULL, accept = c(".csv", ".xlsx", ".xls"), buttonLabel = "Browse..."),
        hr(),
        div(
          id = "file_info_panel",
          h5("File Info"),
          verbatimTextOutput("file_info"),
          style = "margin-bottom: 15px;"
        ),
        actionButton("generate_report", "Generate Full Report", icon = icon("file-alt"), class = "btn-primary btn-block"),
        downloadButton("download_report", "Download Report", class = "btn-block"),
        disabled(downloadButton("download_data", "Download Profiled Data", class = "btn-block"))
      ),
      
      conditionalPanel(
        condition = "output.file_loaded == true",
        wellPanel(
          h4("Data Quality Summary"),
          div(class = "data-summary-card",
              h4("Total Rows"),
              textOutput("row_count")),
          div(class = "data-summary-card",
              h4("Total Columns"),
              textOutput("col_count")),
          div(class = "data-summary-card",
              h4("Missing Values"),
              textOutput("missing_values")),
          div(class = "data-summary-card",
              h4("Duplicate Rows"),
              textOutput("duplicate_rows"))
        )
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",
        type = "tabs",
        
        tabPanel(
          "Data Preview",
          icon = icon("table"),
          br(),
          DTOutput("data_preview")
        ),
        
        tabPanel(
          "Data Quality",
          icon = icon("check-circle"),
          br(),
          h4("Missing Values Analysis"),
          plotlyOutput("missingness_plot"),
          br(),
          h4("Data Types Summary"),
          DTOutput("data_types_table"),
          br(),
          h4("Data Quality Issues"),
          DTOutput("data_issues_table")
        ),
        
        tabPanel(
          "Statistics",
          icon = icon("calculator"),
          br(),
          uiOutput("column_selector_stats"),
          conditionalPanel(
            condition = "output.numeric_selected == true",
            h4("Numeric Statistics"),
            DTOutput("numeric_stats_table"),
            br(),
            h4("Distribution"),
            plotlyOutput("numeric_dist_plot"),
            br(),
            h4("Outliers Detection"),
            plotlyOutput("outlier_plot")
          ),
          conditionalPanel(
            condition = "output.numeric_selected != true",
            h4("Categorical Statistics"),
            DTOutput("categorical_stats_table"),
            br(),
            h4("Frequency Distribution"),
            plotlyOutput("category_plot")
          )
        ),
        
        tabPanel(
          "Visual Explorer",
          icon = icon("chart-bar"),
          br(),
          fluidRow(
            column(4, selectInput("viz_x_axis", "X-Axis Variable", choices = NULL)),
            column(4, selectInput("viz_y_axis", "Y-Axis Variable", choices = NULL, selected = NULL)),
            column(4, selectInput("viz_color", "Color By", choices = NULL, selected = NULL))
          ),
          fluidRow(
            column(4, selectInput("viz_type", "Plot Type", 
                                  choices = c("Histogram", "Boxplot", "Scatterplot", "Bar Chart", "Density"))),
            column(4, numericInput("viz_bins", "Bins (for histogram)", value = 30, min = 5, max = 100)),
            column(4, actionButton("update_viz", "Update Visualization", class = "btn-primary"))
          ),
          br(),
          plotlyOutput("dynamic_plot")
        ),
        
        tabPanel(
          "Correlations",
          icon = icon("project-diagram"),
          br(),
          selectInput("corr_method", "Correlation Method",
                      choices = c("Pearson" = "pearson", "Spearman" = "spearman", "Kendall" = "kendall")),
          plotlyOutput("correlation_plot"),
          br(),
          h4("Top Correlations"),
          DTOutput("correlation_table")
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Define file_loaded first to avoid "output not found" error
  output$file_loaded <- reactive({
    !is.null(rv$processed_data)
  })
  outputOptions(output, "file_loaded", suspendWhenHidden = FALSE)
  
  # Reactive values to store data and state
  rv <- reactiveValues(
    raw_data = NULL,
    processed_data = NULL,
    data_types = NULL,
    data_issues = NULL,
    report_generated = FALSE
  )
  
  # Read and parse uploaded file
  observeEvent(input$file_input, {
    req(input$file_input)
    
    tryCatch({
      file <- input$file_input
      ext <- tools::file_ext(file$datapath)
      
      # Show loading state
      showNotification("Reading your data file...", duration = NULL, id = "loading_notif", type = "message")
      on.exit(removeNotification(id = "loading_notif"), add = TRUE)
      
      if (ext == "csv") {
        df <- readr::read_csv(file$datapath, show_col_types = FALSE)
      } else if (ext %in% c("xlsx", "xls")) {
        df <- readxl::read_excel(file$datapath)
      } else {
        stop("Unsupported file format")
      }
      
      # Store raw data
      rv$raw_data <- df
      
      # Process data
      process_data(df)
      
      # Update UI elements
      updateSelectInput(session, "viz_x_axis", choices = names(rv$processed_data))
      updateSelectInput(session, "viz_y_axis", choices = c("None", names(rv$processed_data)), selected = "None")
      updateSelectInput(session, "viz_color", choices = c("None", names(rv$processed_data)), selected = "None")
      
      # Enable download button
      shinyjs::enable("download_data")
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Process data function
  process_data <- function(df) {
    # Data type detection
    data_types <- sapply(df, function(x) {
      if (is.numeric(x)) return("Numeric")
      if (inherits(x, "Date")) return("Date")
      if (is.character(x) || is.factor(x)) {
        if (length(unique(x)) / length(x) < 0.1) return("Categorical")
        return("Text")
      }
      return(class(x)[1])
    })
    
    # Data quality checks
    missing_values <- colSums(is.na(df))
    duplicate_rows <- sum(duplicated(df))
    
    # Detect potential issues
    issues <- data.frame(
      Column = names(df),
      Type = data_types,
      Missing = missing_values,
      Missing_Pct = round(missing_values / nrow(df) * 100, 2),
      Unique = sapply(df, function(x) length(unique(x))),
      Zero = ifelse(data_types == "Numeric", sapply(df, function(x) sum(x == 0, na.rm = TRUE)), NA),
      Min = ifelse(data_types == "Numeric", sapply(df, function(x) min(x, na.rm = TRUE)), NA),
      Max = ifelse(data_types == "Numeric", sapply(df, function(x) max(x, na.rm = TRUE)), NA)
    )
    
    # Store processed data
    rv$processed_data <- df
    rv$data_types <- data.frame(Column = names(df), Type = data_types)
    rv$data_issues <- issues
  }
  
  # Data preview
  output$data_preview <- renderDT({
    req(rv$processed_data)
    datatable(
      head(rv$processed_data, 100),
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        dom = 'tip'
      ),
      rownames = FALSE,
      style = "bootstrap"
    )
  })
  
  # File info
  output$file_info <- renderText({
    req(rv$processed_data)
    paste(
      "File Name:", input$file_input$name, "\n",
      "Rows:", nrow(rv$processed_data), "\n",
      "Columns:", ncol(rv$processed_data), "\n",
      "Size:", format(object.size(rv$processed_data), units = "auto")
    )
  })
  
  # Data quality summaries
  output$row_count <- renderText({
    req(rv$processed_data)
    format(nrow(rv$processed_data), big.mark = ",")
  })
  
  output$col_count <- renderText({
    req(rv$processed_data)
    ncol(rv$processed_data)
  })
  
  output$missing_values <- renderText({
    req(rv$data_issues)
    total_missing <- sum(rv$data_issues$Missing)
    total_cells <- nrow(rv$processed_data) * ncol(rv$processed_data)
    paste0(format(total_missing, big.mark = ","), " (", 
           round(total_missing / total_cells * 100, 1), 
           "%)")
  })
    
    output$duplicate_rows <- renderText({
      req(rv$processed_data)
      dupes <- sum(duplicated(rv$processed_data))
      paste0(format(dupes, big.mark = ","), " (", round(dupes / nrow(rv$processed_data) * 100, 1), "%)")
    })
    
    # Data types table
    output$data_types_table <- renderDT({
      req(rv$data_types)
      datatable(
        rv$data_types,
        options = list(
          pageLength = 10,
          dom = 'tip'
        ),
        rownames = FALSE
      )
    })
    
    # Data issues table
    output$data_issues_table <- renderDT({
      req(rv$data_issues)
      datatable(
        rv$data_issues,
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = 'tip'
        ),
        rownames = FALSE
      ) %>%
        formatStyle(
          'Missing_Pct',
          background = styleColorBar(rv$data_issues$Missing_Pct, 'lightpink'),
          backgroundSize = '98% 88%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
    })
    
    # Missingness plot
    output$missingness_plot <- renderPlotly({
      req(rv$data_issues)
      
      issues <- rv$data_issues %>%
        arrange(desc(Missing_Pct)) %>%
        filter(Missing_Pct > 0)
      
      if (nrow(issues) == 0) {
        return(plotly_empty() %>% layout(title = "No missing values found"))
      }
      
      plot_ly(issues, x = ~Column, y = ~Missing_Pct, type = 'bar',
              marker = list(color = '#0066cc'),
              hoverinfo = 'text',
              text = ~paste0(Column, '\n', Missing, ' missing (', Missing_Pct, '%)')) %>%
        layout(
          title = "Missing Values by Column",
          xaxis = list(title = ""),
          yaxis = list(title = "Percentage Missing", ticksuffix = "%"),
          margin = list(b = 100)
        )
    })
    
    # Column selector for stats tab
    output$column_selector_stats <- renderUI({
      req(rv$processed_data)
      selectInput("stats_column", "Select Column to Analyze", choices = names(rv$processed_data))
    })
    
    # Check if selected column is numeric
    output$numeric_selected <- reactive({
      req(input$stats_column, rv$processed_data)
      is.numeric(rv$processed_data[[input$stats_column]])
    })
    outputOptions(output, "numeric_selected", suspendWhenHidden = FALSE)
    
    # Numeric stats table
    output$numeric_stats_table <- renderDT({
      req(input$stats_column, rv$processed_data)
      
      col_data <- rv$processed_data[[input$stats_column]]
      if (!is.numeric(col_data)) return(NULL)
      
      stats <- data.frame(
        Statistic = c("Mean", "Median", "Std Dev", "Min", "Max", "Range", "IQR", 
                      "Skewness", "Kurtosis", "Missing", "Zeros", "Outliers (IQR)"),
        Value = c(
          mean(col_data, na.rm = TRUE),
          median(col_data, na.rm = TRUE),
          sd(col_data, na.rm = TRUE),
          min(col_data, na.rm = TRUE),
          max(col_data, na.rm = TRUE),
          max(col_data, na.rm = TRUE) - min(col_data, na.rm = TRUE),
          IQR(col_data, na.rm = TRUE),
          skewness(col_data, na.rm = TRUE),
          kurtosis(col_data, na.rm = TRUE),
          sum(is.na(col_data)),
          sum(col_data == 0, na.rm = TRUE),
          sum(abs(scale(col_data)) > 3, na.rm = TRUE)
        )
      )
      
      stats$Value <- round(stats$Value, 3)
      
      datatable(
        stats,
        options = list(
          dom = 't',
          paging = FALSE,
          searching = FALSE
        ),
        rownames = FALSE,
        selection = 'none'
      )
    })
    
    # Numeric distribution plot
    output$numeric_dist_plot <- renderPlotly({
      req(input$stats_column, rv$processed_data)
      
      col_data <- rv$processed_data[[input$stats_column]]
      if (!is.numeric(col_data)) return(NULL)
      
      # Remove NA for plotting
      plot_data <- data.frame(value = col_data[!is.na(col_data)])
      
      p <- ggplot(plot_data, aes(x = value)) +
        geom_histogram(aes(y = ..density..), fill = "#0066cc", alpha = 0.7, bins = 30) +
        geom_density(color = "#003366", size = 1) +
        labs(title = paste("Distribution of", input$stats_column),
             x = input$stats_column, y = "Density") +
        theme_minimal()
      
      ggplotly(p) %>% config(displayModeBar = FALSE)
    })
    
    # Outlier plot
    output$outlier_plot <- renderPlotly({
      req(input$stats_column, rv$processed_data)
      
      col_data <- rv$processed_data[[input$stats_column]]
      if (!is.numeric(col_data)) return(NULL)
      
      # Calculate outlier thresholds
      qnt <- quantile(col_data, probs = c(0.25, 0.75), na.rm = TRUE)
      iqr <- IQR(col_data, na.rm = TRUE)
      lower <- qnt[1] - 1.5 * iqr
      upper <- qnt[2] + 1.5 * iqr
      
      # Identify outliers
      outliers <- which(col_data < lower | col_data > upper)
      
      plot_data <- data.frame(
        index = seq_along(col_data),
        value = col_data,
        is_outlier = ifelse(seq_along(col_data) %in% outliers, "Outlier", "Normal")
      )
      
      p <- ggplot(plot_data, aes(x = index, y = value, color = is_outlier, text = paste0("Row: ", index, "\nValue: ", value))) +
        geom_point(alpha = 0.7) +
        geom_hline(yintercept = upper, linetype = "dashed", color = "red") +
        geom_hline(yintercept = lower, linetype = "dashed", color = "red") +
        scale_color_manual(values = c("Normal" = "#0066cc", "Outlier" = "red")) +
        labs(title = paste("Outlier Detection for", input$stats_column),
             x = "Row Index", y = input$stats_column, color = "") +
        theme_minimal() +
        theme(legend.position = "top")
      
      ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
    })
    
    # Categorical stats table
    output$categorical_stats_table <- renderDT({
      req(input$stats_column, rv$processed_data)
      
      col_data <- rv$processed_data[[input$stats_column]]
      if (is.numeric(col_data)) return(NULL)
      
      freq_table <- as.data.frame(table(col_data, useNA = "ifany"))
      names(freq_table) <- c("Category", "Frequency")
      freq_table$Percentage <- round(freq_table$Frequency / sum(freq_table$Frequency) * 100, 1)
      freq_table <- freq_table[order(-freq_table$Frequency), ]
      
      datatable(
        freq_table,
        options = list(
          pageLength = 10,
          dom = 'tip'
        ),
        rownames = FALSE
      ) %>% formatStyle(
        'Percentage',
        background = styleColorBar(freq_table$Percentage, 'lightblue'),
        backgroundSize = '98% 88%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
    })
    
    # Categorical plot
    output$category_plot <- renderPlotly({
      req(input$stats_column, rv$processed_data)
      
      col_data <- rv$processed_data[[input$stats_column]]
      if (is.numeric(col_data)) return(NULL)
      
      freq_table <- as.data.frame(table(col_data, useNA = "ifany"))
      names(freq_table) <- c("Category", "Frequency")
      freq_table <- freq_table[order(-freq_table$Frequency), ]
      
      # Limit to top 20 categories for readability
      if (nrow(freq_table) > 20) {
        freq_table <- freq_table[1:20, ]
      }
      
      p <- ggplot(freq_table, aes(x = reorder(Category, Frequency), y = Frequency, 
                                  text = paste0(Category, "\nCount: ", Frequency))) +
        geom_bar(stat = "identity", fill = "#0066cc", alpha = 0.7) +
        coord_flip() +
        labs(title = paste("Frequency of Categories in", input$stats_column),
             x = "", y = "Count") +
        theme_minimal()
      
      ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
    })
    
    # Dynamic visualization
    observeEvent(input$update_viz, {
      req(rv$processed_data, input$viz_x_axis)
      
      x_var <- input$viz_x_axis
      y_var <- if (input$viz_y_axis == "None") NULL else input$viz_y_axis
      color_var <- if (input$viz_color == "None") NULL else input$viz_color
      
      plot_data <- rv$processed_data
      
      output$dynamic_plot <- renderPlotly({
        if (input$viz_type == "Histogram") {
          p <- ggplot(plot_data, aes_string(x = x_var)) +
            geom_histogram(fill = "#0066cc", alpha = 0.7, bins = input$viz_bins) +
            labs(title = paste("Histogram of", x_var), y = "Count") +
            theme_minimal()
        } else if (input$viz_type == "Boxplot") {
          if (is.numeric(plot_data[[x_var]])) {
            p <- ggplot(plot_data, aes_string(x = "1", y = x_var)) +
              geom_boxplot(fill = "#0066cc", alpha = 0.7) +
              labs(title = paste("Boxplot of", x_var), x = "") +
              theme_minimal() +
              theme(axis.text.x = element_blank())
          } else {
            p <- ggplot(plot_data, aes_string(x = x_var, y = y_var)) +
              geom_boxplot(fill = "#0066cc", alpha = 0.7) +
              labs(title = paste("Boxplot of", y_var, "by", x_var)) +
              theme_minimal()
          }
        } else if (input$viz_type == "Scatterplot" && !is.null(y_var)) {
          p <- ggplot(plot_data, aes_string(x = x_var, y = y_var, color = color_var)) +
            geom_point(alpha = 0.7) +
            labs(title = paste(y_var, "vs.", x_var)) +
            theme_minimal()
        } else if (input$viz_type == "Bar Chart") {
          if (is.null(y_var)) {
            p <- ggplot(plot_data, aes_string(x = x_var, fill = color_var)) +
              geom_bar(alpha = 0.7) +
              labs(title = paste("Bar Chart of", x_var), y = "Count") +
              theme_minimal()
          } else {
            p <- ggplot(plot_data, aes_string(x = x_var, y = y_var, fill = color_var)) +
              geom_col(alpha = 0.7) +
              labs(title = paste("Bar Chart of", y_var, "by", x_var)) +
              theme_minimal()
          }
        } else if (input$viz_type == "Density") {
          p <- ggplot(plot_data, aes_string(x = x_var, fill = color_var)) +
            geom_density(alpha = 0.4) +
            labs(title = paste("Density Plot of", x_var), y = "Density") +
            theme_minimal()
        } else {
          return(plotly_empty())
        }
        
        ggplotly(p) %>% config(displayModeBar = FALSE)
      })
    })
    
    # Correlation plot
    output$correlation_plot <- renderPlotly({
      req(rv$processed_data)
      
      numeric_cols <- sapply(rv$processed_data, is.numeric)
      if (sum(numeric_cols) < 2) return(NULL)
      
      cor_data <- rv$processed_data[, numeric_cols]
      cor_matrix <- cor(cor_data, use = "pairwise.complete.obs", method = input$corr_method)
      
      plot_ly(
        x = colnames(cor_matrix),
        y = colnames(cor_matrix),
        z = cor_matrix,
        type = "heatmap",
        colors = colorRamp(c("#0066cc", "white", "#cc3300")),
        zmin = -1,
        zmax = 1,
        hoverinfo = "text",
        text = matrix(paste0(
          "X: ", rep(colnames(cor_matrix), each = ncol(cor_matrix)), "\n",
          "Y: ", rep(colnames(cor_matrix), times = ncol(cor_matrix)), "\n",
          "Correlation: ", round(c(cor_matrix), 3)
        ), nrow = ncol(cor_matrix))
      ) %>%
        layout(
          title = "Correlation Matrix",
          xaxis = list(tickangle = 45),
          margin = list(l = 100, b = 100)
        )
    })
    
    # Correlation table
    output$correlation_table <- renderDT({
      req(rv$processed_data)
      
      numeric_cols <- sapply(rv$processed_data, is.numeric)
      if (sum(numeric_cols) < 2) return(NULL)
      
      cor_data <- rv$processed_data[, numeric_cols]
      cor_matrix <- cor(cor_data, use = "pairwise.complete.obs", method = input$corr_method)
      
      # Get upper triangle without diagonal
      cor_matrix[lower.tri(cor_matrix, diag = TRUE)] <- NA
      
      cor_df <- as.data.frame(as.table(cor_matrix)) %>%
        filter(!is.na(Freq)) %>%
        rename(Variable1 = Var1, Variable2 = Var2, Correlation = Freq) %>%
        mutate(Correlation = round(Correlation, 3)) %>%
        arrange(desc(abs(Correlation)))
      
      datatable(
        cor_df,
        options = list(
          pageLength = 10,
          dom = 'tip'
        ),
        rownames = FALSE
      ) %>% formatStyle(
        'Correlation',
        color = styleInterval(c(-0.5, 0.5), c('#cc3300', '#999999', '#0066cc')),
        fontWeight = styleInterval(c(-0.7, 0.7), c('bold', 'normal', 'bold'))
      )
    })
    
    # Generate report
    observeEvent(input$generate_report, {
      req(rv$processed_data)
      
      showNotification("Generating report...", duration = NULL, id = "report_notif", type = "message")
      
      # Create a temporary directory for report
      temp_report <- file.path(tempdir(), "report.Rmd")
      
      # Create a simple R Markdown template
      writeLines(
        c(
          "---",
          "title: 'Data Profiling Report'",
          "output: html_document",
          "params:",
          "  data: NA",
          "  data_types: NA",
          "  data_issues: NA",
          "  file_name: NA",
          "---",
          "",
          "```{r setup, include=FALSE}",
          "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
          "library(DT)",
          "library(ggplot2)",
          "library(kableExtra)",
          "```",
          "",
          "# Data Profiling Report for `r params$file_name`",
          "",
          "## Dataset Overview",
          "",
          "- **Number of rows:** `r nrow(params$data)`",
          "- **Number of columns:** `r ncol(params$data)`",
          "- **Size:** `r format(object.size(params$data), units = 'auto')`",
          "",
          "## Data Types Summary",
          "",
          "```{r data-types}",
          "DT::datatable(params$data_types, rownames = FALSE, options = list(dom = 't'))",
          "```",
          "",
          "## Data Quality Issues",
          "",
          "```{r data-issues}",
          "DT::datatable(params$data_issues, rownames = FALSE,",
          "              options = list(scrollX = TRUE, pageLength = 10)) %>%",
          "  formatStyle('Missing_Pct',",
          "              background = styleColorBar(params$data_issues$Missing_Pct, 'lightpink'),",
          "              backgroundSize = '98% 88%',",
          "              backgroundRepeat = 'no-repeat',",
          "              backgroundPosition = 'center')",
          "```",
          "",
          "## Sample Data",
          "",
          "```{r sample-data}",
          "DT::datatable(head(params$data, 100), rownames = FALSE,",
          "              options = list(scrollX = TRUE, pageLength = 10))",
          "```"
        ),
        temp_report
      )
      
      # Set up parameters to pass to Rmd document
      params <- list(
        data = rv$processed_data,
        data_types = rv$data_types,
        data_issues = rv$data_issues,
        file_name = input$file_input$name
      )
      
      # Knit the document
      rmarkdown::render(
        temp_report,
        output_file = file.path(tempdir(), "report.html"),
        params = params,
        envir = new.env(parent = globalenv())
      )
      
      removeNotification(id = "report_notif")
      rv$report_generated <- TRUE
      showNotification("Report generated successfully!", type = "message")
    })
    
    # Download report
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("data_profile_report_", Sys.Date(), ".html")
      },
      content = function(file) {
        req(rv$report_generated)
        file.copy(file.path(tempdir(), "report.html"), file)
      }
    )
    
    # Download profiled data
    output$download_data <- downloadHandler(
      filename = function() {
        paste0("profiled_data_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(rv$processed_data)
        write.csv(rv$processed_data, file, row.names = FALSE)
      }
    )
}

# Run the application
shinyApp(ui = ui, server = server)

