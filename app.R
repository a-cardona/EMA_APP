library(shiny)
library(tidyverse)
library(DT)
library(bslib)
library(shinycssloaders)

# 📁 Load EMA data
data_dir <- "/Users/andrewcardona/Desktop/CABLAB_CODE/EMA"
file_paths <- list.files(data_dir, pattern = "^cleaned_EMA.*\\.csv$", full.names = TRUE)
all_data <- file_paths %>% map_dfr(read_csv, .id = "file_index")

# Ensure 'Record ID' column exists
if (!"Record ID" %in% colnames(all_data)) {
  stop("Error: 'Record ID' column not found in your data. Please ensure each CSV has 'Record ID' column.")
}

platforms <- c("insta", "x", "fb", "snapchat", "tiktok", "yt", "reddit", "tumblr", "pin", "wa", "wc")
categories <- c("mood", "mins", "reason", "content", "activity")

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly", base_font = font_google("Lato")),
  
  titlePanel(div(style = "color:#2c3e50; font-size: 28px; font-weight: bold;", "EMA Explorer  /ᐠ – ˕ –マ  👍")),
  
  sidebarLayout(
    sidebarPanel(
      h4("Control Center"),
      checkboxGroupInput("selected_platforms", "Platforms:", choices = platforms, selected = platforms[1]),
      checkboxGroupInput("selected_categories", "Categories:", choices = categories, selected = categories),
      
      selectInput("x_var", "X-axis (numeric):", choices = NULL),
      selectInput("y_var", "Y-axis (numeric):", choices = NULL),
      
      selectInput("plot_type",
                  "Plot Type:",
                  choices = c("Scatterplot", "Histogram", "Boxplot", "Facet Scatterplot", "Facet Histogram")),
      hr(),
      actionButton("run_regression", "Run Regression", class = "btn btn-primary"),
      hr(),
      
      h4("📄 Export Options"),
      uiOutput("column_selector"),
      downloadButton("download_selected", "⬇️ Download CSV"),
      hr()
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs", id = "tabs",
                  tabPanel("CSV Preview", DT::dataTableOutput("preview_selected")),
                  tabPanel("Plots", withSpinner(plotOutput("scatter_plot", height = "500px"))),
                  tabPanel("Regression", verbatimTextOutput("regression_output")),
                  tabPanel(
                    "Summary Tables",
                    fluidRow(
                      column(4, style = "padding-right: 20px;",
                             h4("Reasons", style = "margin-bottom: 15px;"),
                             DT::dataTableOutput("reason_table") %>% withSpinner()),
                      column(4, style = "padding-left: 10px; padding-right: 10px;",
                             h4("Content", style = "margin-bottom: 15px;"),
                             DT::dataTableOutput("content_table") %>% withSpinner()),
                      column(4, style = "padding-left: 20px;",
                             h4("Activities", style = "margin-bottom: 15px;"),
                             DT::dataTableOutput("activity_table") %>% withSpinner())
                    )
                  )
      )
    )
  )
)

server <- function(input, output, session) {
  # Compute average mins and mood per Record ID
  recordid_avg_table <- reactive({
    mins_cols <- paste0(platforms, "_mins")
    mood_cols <- paste0(platforms, "_mood")
    all_cols <- c(mins_cols, mood_cols)
    existing_cols <- intersect(all_cols, colnames(all_data))
    
    all_data %>%
      group_by(`Record ID`) %>%
      summarise(across(.cols = all_of(existing_cols), ~ mean(.x, na.rm = TRUE))) %>%
      ungroup()
  })
  
  # Reactive that filters columns by platform + category and joins averages
  selected_df <- reactive({
    req(input$selected_platforms, input$selected_categories)
    
    selected <- input$selected_platforms
    selected_categories <- input$selected_categories
    df <- all_data
    
    # Join averages
    avg_tbl <- recordid_avg_table()
    new_names <- map_chr(names(avg_tbl), function(nm) {
      if (nm == "Record ID") return(nm)
      nm %>% str_replace("_mins$", "_avg_mins") %>% str_replace("_mood$", "_avg_mood")
    })
    names(avg_tbl) <- new_names
    df <- left_join(df, avg_tbl, by = "Record ID")
    
    # Ensure avg columns exist even if missing
    for (p in platforms) {
      if (!(paste0(p, "_avg_mins") %in% colnames(df))) df[[paste0(p, "_avg_mins")]] <- NA_real_
      if (!(paste0(p, "_avg_mood") %in% colnames(df))) df[[paste0(p, "_avg_mood")]] <- NA_real_
    }
    
    # Columns per category
    mood_cols <- paste0(selected, "_mood")
    mins_cols <- paste0(selected, "_mins")
    reason_cols <- unlist(lapply(selected, function(p) {
      grep(paste0("^", p, "_"), colnames(df), value = TRUE) %>%
        setdiff(grep(paste0("^", p, "_(cont_|act_|avg_)"), colnames(df), value = TRUE))
    }))
    content_cols <- grep(paste0("^(", paste0(selected, collapse = "|"), ")_cont_"), colnames(df), value = TRUE)
    activity_cols <- grep(paste0("^(", paste0(selected, collapse = "|"), ")_act_"), colnames(df), value = TRUE)
    
    all_category_cols <- list(
      mood = mood_cols,
      mins = mins_cols,
      reason = reason_cols,
      content = content_cols,
      activity = activity_cols
    )
    
    # Selected columns filtered by categories
    selected_cols <- unlist(all_category_cols[selected_categories])
    
    # Include avg columns for mood and mins if selected
    if ("mood" %in% selected_categories) {
      avg_mood_cols <- paste0(selected, "_avg_mood")
      selected_cols <- unique(c(selected_cols, avg_mood_cols))
    }
    if ("mins" %in% selected_categories) {
      avg_mins_cols <- paste0(selected, "_avg_mins")
      selected_cols <- unique(c(selected_cols, avg_mins_cols))
    }
    
    # Always include Record ID and gen_mood if available
    common_cols <- intersect(c("Record ID", "gen_mood"), colnames(df))
    selected_cols <- unique(c(common_cols, selected_cols))
    
    list(
      df = df,
      selected_cols = selected_cols,
      reason_cols = reason_cols,
      content_cols = content_cols,
      activity_cols = activity_cols,
      mood_cols = mood_cols,
      mins_cols = mins_cols
    )
  })
  
  # Update numeric variable selectors based on filtered columns
  observe({
    df <- selected_df()$df
    num_cols <- df %>% select(all_of(selected_df()$selected_cols)) %>% select(where(is.numeric)) %>% colnames()
    updateSelectInput(session, "x_var", choices = num_cols)
    updateSelectInput(session, "y_var", choices = num_cols)
  })
  
  # Dynamic UI for column selection for export
  output$column_selector <- renderUI({
    checkboxGroupInput("selected_columns", "Select variables to export:",
                       choices = selected_df()$selected_cols,
                       selected = selected_df()$selected_cols)
  })
  
  # Download handler for exporting selected columns
  output$download_selected <- downloadHandler(
    filename = function() paste("selected_columns", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      df <- selected_df()$df
      selected_cols <- input$selected_columns
      if (!is.null(selected_cols) && all(selected_cols %in% names(df))) {
        export_df <- df %>% select(all_of(selected_cols)) %>% mutate(across(everything(), as.character))
        write_csv(export_df, file, na = "")
      } else {
        write_csv(tibble(Note = "No valid columns selected or columns not found."), file)
      }
    }
  )
  
  # Preview table of selected columns
  output$preview_selected <- DT::renderDataTable({
    req(input$selected_columns)
    selected_df()$df %>% select(all_of(input$selected_columns))
  })
  
  # Plotting logic
  output$scatter_plot <- renderPlot({
    req(input$x_var, input$y_var, input$plot_type)
    df <- selected_df()$df
    x <- input$x_var
    y <- input$y_var
    
    # Prepare long dataframe for facet plots
    df_long <- df %>%
      pivot_longer(cols = matches("_mood$|_mins$"), names_to = "var", values_to = "value") %>%
      separate(var, into = c("platform", "measure"), sep = "_", extra = "merge") %>%
      pivot_wider(names_from = measure, values_from = value)
    
    base_plot <- theme_minimal(base_size = 14)
    
    if (input$plot_type == "Scatterplot") {
      ggplot(df, aes(x = .data[[x]], y = .data[[y]])) +
        geom_point(alpha = 0.6, color = "darkgreen") +
        geom_smooth(method = "lm", se = TRUE, color = "darkblue") +
        labs(title = paste(y, "vs", x), x = x, y = y) + base_plot
      
    } else if (input$plot_type == "Histogram") {
      ggplot(df, aes(x = .data[[x]])) +
        geom_histogram(bins = 30, fill = "darkred", color = "white") +
        labs(title = paste("Histogram of", x), x = x, y = y) + base_plot
      
    } else if (input$plot_type == "Boxplot") {
      ggplot(df, aes(y = .data[[y]])) +
        geom_boxplot(fill = "orange") +
        labs(title = paste("Boxplot of", y), y = y) + base_plot
      
    } else if (input$plot_type == "Facet Scatterplot") {
      ggplot(df_long, aes(x = mins, y = mood)) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = TRUE, color = "pink") +
        facet_wrap(~platform) +
        labs(title = paste("Facet", y, "vs", x), x = x, y = y) + base_plot
      
    } else if (input$plot_type == "Facet Histogram") {
      ggplot(df_long, aes(x = mins)) +
        geom_histogram(bins = 20, fill = "darkred", color = "white") +
        facet_wrap(~platform) +
        labs(title = "Facet: Minutes Distribution by Platform", x = "Minutes", y = "Count") + base_plot
    }
  })
  
  # Regression triggered by button
  observeEvent(input$run_regression, {
    df <- selected_df()$df
    req(input$x_var, input$y_var)
    model <- lm(as.formula(paste(input$y_var, "~", input$x_var)), data = df)
    output$regression_output <- renderPrint({ summary(model) })
  })
  
  # Summary tables for reasons, content, activity
  output$reason_table <- DT::renderDataTable({
    df <- selected_df()$df
    cols <- selected_df()$reason_cols
    if (length(cols) == 0) return(NULL)
    dat <- df %>% select(all_of(cols)) %>% pivot_longer(everything()) %>%
      filter(!is.na(value) & value != 0) %>% count(name, value) %>% head(20)
    
    datatable(dat, options = list(
      pageLength = 10,
      scrollY = "250px",
      scrollCollapse = TRUE,
      paging = TRUE
    ))
  })
  
  output$content_table <- DT::renderDataTable({
    df <- selected_df()$df
    cols <- selected_df()$content_cols
    if (length(cols) == 0) return(NULL)
    dat <- df %>% select(all_of(cols)) %>% pivot_longer(everything()) %>%
      filter(!is.na(value) & value != 0) %>% count(name, value) %>% head(20)
    
    datatable(dat, options = list(
      pageLength = 10,
      scrollY = "250px",
      scrollCollapse = TRUE,
      paging = TRUE
    ))
  })
  
  output$activity_table <- DT::renderDataTable({
    df <- selected_df()$df
    cols <- selected_df()$activity_cols
    if (length(cols) == 0) return(NULL)
    dat <- df %>% select(all_of(cols)) %>% pivot_longer(everything()) %>%
      filter(!is.na(value) & value != 0) %>% count(name, value) %>% head(20)
    
    datatable(dat, options = list(
      pageLength = 10,
      scrollY = "250px",
      scrollCollapse = TRUE,
      paging = TRUE
    ))
  })
}

shinyApp(ui = ui, server = server)
