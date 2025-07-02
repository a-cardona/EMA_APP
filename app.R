library(shiny)
library(tidyverse)
library(DT)
library(bslib)
library(shinycssloaders)

# --- 1. Load and preprocess data ONCE at startup ---
data_dir <- "/Users/andrewcardona/Desktop/CABLAB_CODE/EMA"
file_paths <- list.files(data_dir, pattern = "^cleaned_EMA.*\\.csv$", full.names = TRUE)

# Load all CSVs and combine
all_data <- file_paths %>% map_dfr(read_csv, .id = "file_index")

# Preprocess: convert PID numeric & filter invalid PIDs
processed_data <- all_data %>%
  mutate(PID = as.numeric(PID)) %>%
  filter(!is.na(PID))

# Define platform and categories
platforms <- c("insta", "x", "fb", "snapchat", "tiktok", "yt", "reddit", "tumblr", "pin", "wa", "wc")
categories <- c("mood", "mins", "reason", "content", "activity")

# --- 2. UI ---
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly", base_font = font_google("Lato")),
  titlePanel(div(style = "color:#2c3e50; font-size: 28px; font-weight: bold;", "EMA Explorer  /ᴠ – ˕ –マ  👍")),
  
  sidebarLayout(
    sidebarPanel(
      h4("Control Center"),
      radioButtons("cohort_filter", "Select Cohort:",
                   choices = c("All", "Kids", "Teens", "Adults"),
                   selected = "All"),
      checkboxGroupInput("selected_platforms", "Platforms:",
                         choices = c("All", platforms),
                         selected = character(0)),  # Default: all selected
      checkboxGroupInput("selected_categories", "Categories:",
                         choices = categories,
                         selected = character(0)),  # Default: all selected
      selectInput("x_var", "X-axis (numeric):", choices = NULL),
      selectInput("y_var", "Y-axis (numeric):", choices = NULL),
      selectInput("plot_type", "Plot Type:",
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
                  tabPanel("Regression", verbatimTextOutput("regression_output"))
      )
    )
  )
)

# --- 3. Server ---
server <- function(input, output, session) {
  
  # Helper: Handle "All" platforms checkbox logic efficiently
  observeEvent(input$selected_platforms, {
    selected <- input$selected_platforms
    # If "All" is selected, select all platforms
    if ("All" %in% selected && length(selected) < length(platforms) + 1) {
      updateCheckboxGroupInput(session, "selected_platforms", selected = c("All", platforms))
    }
    # If "All" is deselected, keep only platforms
    else if (!("All" %in% selected) && length(selected) == length(platforms)) {
      updateCheckboxGroupInput(session, "selected_platforms", selected = c("All", platforms))
    }
    # If "All" is selected but user deselects some platforms, remove "All"
    else if ("All" %in% selected && length(selected) < length(platforms) + 1) {
      updateCheckboxGroupInput(session, "selected_platforms", selected = setdiff(selected, "All"))
    }
  }, ignoreInit = TRUE)
  
  # --- Reactive: filtered data based on cohort ---
  filtered_data <- reactive({
    df <- processed_data
    cohort <- input$cohort_filter
    
    if (cohort == "Kids") {
      df <- df %>% filter(PID >= 1000 & PID < 2000)
    } else if (cohort == "Teens") {
      df <- df %>% filter(PID >= 2000 & PID < 3000)
    } else if (cohort == "Adults") {
      df <- df %>% filter(PID >= 3000 & PID < 4000)
    }
    df
  })
  
  # --- Reactive: subset columns based on platforms & categories ---
  selected_df <- reactive({
    req(input$selected_platforms, input$selected_categories)
    
    # Handle "All" removed by helper above, so just platforms:
    selected_platforms <- setdiff(input$selected_platforms, "All")
    if (length(selected_platforms) == 0) {
      # return minimal df with basic columns so app doesn’t break
      return(list(df = filtered_data() %>% select("Record ID", "PID", "gen_mood"), selected_cols = c("Record ID", "PID", "gen_mood")))
    }
    
    selected_categories <- input$selected_categories
    df <- filtered_data()
    
    # Prepare column names per category
    mood_cols <- paste0(selected_platforms, "_mood")
    mins_cols <- paste0(selected_platforms, "_mins")
    
    reason_cols <- unlist(lapply(selected_platforms, function(p) {
      grep(paste0("^", p, "_"), colnames(df), value = TRUE) %>%
        setdiff(grep(paste0("^", p, "_(cont_|act_|avg_)"), colnames(df), value = TRUE))
    }))
    
    content_cols <- grep(paste0("^(", paste0(selected_platforms, collapse = "|"), ")_cont_"), colnames(df), value = TRUE)
    activity_cols <- grep(paste0("^(", paste0(selected_platforms, collapse = "|"), ")_act_"), colnames(df), value = TRUE)
    
    all_category_cols <- list(
      mood = mood_cols,
      mins = mins_cols,
      reason = reason_cols,
      content = content_cols,
      activity = activity_cols
    )
    
    selected_cols <- unlist(all_category_cols[selected_categories])
    
    # Add precomputed summary columns if they exist in df
    summary_cols <- c("mood_during_use_score", "mood_use_association_score", "exposure_to_negative_score", "avg_total_duration")
    summary_cols <- intersect(summary_cols, colnames(df))
    
    # Always keep these basics
    base_cols <- intersect(c("Record ID", "PID", "gen_mood", "Mood_During_Use_Score", "Mood_Use_Association_Score", "Exposure_Negative_Score", "Avg_Total_Duration"), colnames(df))
    
    selected_cols <- unique(c(base_cols, summary_cols, selected_cols))
    
    # Return list for UI and outputs
    list(
      df = df,
      selected_cols = selected_cols
    )
  })
  
  # --- Update variable selectors dynamically ---
  observe({
    df <- selected_df()$df
    num_cols <- df %>% select(all_of(selected_df()$selected_cols)) %>% select(where(is.numeric)) %>% colnames()
    updateSelectInput(session, "x_var", choices = num_cols)
    updateSelectInput(session, "y_var", choices = num_cols)
  })
  
  # --- UI for column selection export ---
  output$column_selector <- renderUI({
    checkboxGroupInput("selected_columns", "Select variables to export:",
                       choices = selected_df()$selected_cols,
                       selected = selected_df()$selected_cols)
  })
  
  # --- Download handler ---
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
  
  # --- Preview selected data ---
  output$preview_selected <- DT::renderDataTable({
    req(input$selected_columns)
    selected_df()$df %>% select(all_of(input$selected_columns))
  })
  
  # --- Plotting ---
  output$scatter_plot <- renderPlot({
    req(input$x_var, input$y_var, input$plot_type)
    df <- selected_df()$df
    x <- input$x_var
    y <- input$y_var
    
    # Prepare long format for facet plots if needed
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
  
  # --- Regression ---
  observeEvent(input$run_regression, {
    df <- selected_df()$df
    req(input$x_var, input$y_var)
    model <- lm(as.formula(paste(input$y_var, "~", input$x_var)), data = df)
    output$regression_output <- renderPrint({ summary(model) })
  })
  
}

shinyApp(ui = ui, server = server)


# RUN THIS IN TERMINAL SO IT DOESNT CRASH >>> Rscript -e "shiny::runApp('/Users/andrewcardona/Desktop/CABLAB_CODE/EMA/EMA_App', launch.browser = TRUE)"

