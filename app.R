library(shiny)
library(tidyverse)
library(DT)
library(bslib)
library(shinycssloaders)

# 📁 Load EMA data
data_dir <- "/Users/andrewcardona/Desktop/CABLAB_CODE/EMA"
file_paths <- list.files(data_dir, pattern = "^cleaned_EMA.*\\.csv$", full.names = TRUE)
all_data <- file_paths %>% map_dfr(read_csv, .id = "file_index")

# Platforms and categories defining
platforms <- c("insta", "x", "fb", "snapchat", "tiktok", "yt", "reddit", "tumblr", "pin", "wa", "wc")
categories <- c("mood", "mins", "reason", "content", "activity")

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
                         selected = character(0)),
      checkboxGroupInput("selected_categories", "Categories:",
                         choices = categories,
                         selected = character(0)),
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

server <- function(input, output, session) {
  observeEvent(input$selected_platforms, {
    selected <- input$selected_platforms
    
    if ("All" %in% selected && !all(platforms %in% selected)) {
      updateCheckboxGroupInput(session, "selected_platforms", selected = c("All", platforms))
    } else if (!("All" %in% selected) && all(platforms %in% selected)) {
      updateCheckboxGroupInput(session, "selected_platforms", selected = c("All", platforms))
    } else if ("All" %in% selected && !all(platforms %in% selected)) {
      updateCheckboxGroupInput(session, "selected_platforms", selected = setdiff(selected, "All"))
    }
  }, ignoreInit = TRUE)
  
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
  
  participant_summary <- reactive({
    df <- all_data %>%
      mutate(PID = as.numeric(PID)) %>%
      filter(!is.na(PID))
    
    if (!"gen_mood" %in% names(df)) {
      df$gen_mood <- NA_real_
    }
    
    mood_cols <- paste0(platforms, "_mood")
    mins_cols <- paste0(platforms, "_mins")
    mood_cols_exist <- mood_cols[mood_cols %in% names(df)]
    mins_cols_exist <- mins_cols[mins_cols %in% names(df)]
    
    if (length(mood_cols_exist) == 0 || length(mins_cols_exist) == 0) {
      df <- df %>%
        mutate(
          total_use = NA_real_,
          weighted_mood = NA_real_
        )
    } else {
      df <- df %>%
        rowwise() %>%
        mutate(
          total_use = sum(c_across(all_of(mins_cols_exist)), na.rm = TRUE),
          weighted_mood = {
            moods <- c_across(all_of(mood_cols_exist))
            mins <- c_across(all_of(mins_cols_exist))
            total_mins <- sum(mins, na.rm = TRUE)
            if (total_mins == 0) NA_real_ else sum(moods * mins, na.rm = TRUE) / total_mins
          }
        ) %>%
        ungroup()
    }
    
    neg_cols <- grep("_cont_(violence|bully|scary|sexual|substance)", names(df), value = TRUE)
    
    # ✅ FIXED negative content computation
    if (length(neg_cols) > 0) {
      df <- df %>%
        rowwise() %>%
        mutate(exposed_negative = as.integer(sum(c_across(all_of(neg_cols)) == 1, na.rm = TRUE) > 0)) %>%
        ungroup()
    } else {
      df <- df %>% mutate(exposed_negative = NA_integer_)
    }
    
    corr_df <- df %>%
      filter(!is.na(gen_mood), !is.na(total_use)) %>%
      group_by(PID) %>%
      summarise(
        mood_use_corr = if (n() >= 2) cor(gen_mood, total_use, use = "complete.obs") else NA_real_,
        .groups = "drop"
      )
    
    summary_df <- df %>%
      group_by(PID) %>%
      summarise(
        mood_during_use_score = mean(weighted_mood, na.rm = TRUE),
        exposure_to_negative_score = mean(exposed_negative, na.rm = TRUE),
        avg_total_duration = mean(total_use, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(corr_df, by = "PID") %>%
      rename(mood_use_association_score = mood_use_corr)
    
    left_join(df, summary_df, by = "PID")
  })
  
  # Reactive that filters columns by platform + category and joins averages
  selected_df <- reactive({
    req(input$selected_platforms, input$selected_categories)
    
    selected <- setdiff(input$selected_platforms, "All")
    if (length(selected) == 0) selected <- platforms  # fallback if nothing selected
    
    selected_categories <- input$selected_categories
    df <- participant_summary()
    
    # Filter by cohort
    df <- df %>%
      mutate(PID = as.numeric(PID)) %>%
      filter(
        case_when(
          input$cohort_filter == "Kids" ~ PID >= 1000 & PID < 2000,
          input$cohort_filter == "Teens" ~ PID >= 2000 & PID < 3000,
          input$cohort_filter == "Adults" ~ PID >= 3000 & PID < 4000,
          TRUE ~ TRUE
        )
      )
    
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
    
    selected_cols <- unlist(all_category_cols[selected_categories])
    
    if ("mood" %in% selected_categories) {
      avg_mood_cols <- paste0(selected, "_avg_mood")
      selected_cols <- unique(c(selected_cols, avg_mood_cols))
    }
    if ("mins" %in% selected_categories) {
      avg_mins_cols <- paste0(selected, "_avg_mins")
      selected_cols <- unique(c(selected_cols, avg_mins_cols))
    }
    
    # Always include Record ID, PID, gen_mood, and summary scores
    summary_cols <- c("mood_during_use_score", "mood_use_association_score", "exposure_to_negative_score", "avg_total_duration")
    common_cols <- intersect(c("Record ID", "PID", "gen_mood", summary_cols), colnames(df))
    
    # Put Record ID, PID, then summary cols first
    order_cols <- c("Record ID", "PID", summary_cols, setdiff(common_cols, c("Record ID", "PID", summary_cols)))
    common_cols <- intersect(order_cols, common_cols)
    
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
}

shinyApp(ui = ui, server = server)
#.  RUN THIS IN TERMINAL SO IT DOESNT CRASH>>>> Rscript -e "shiny::runApp('/Users/andrewcardona/Desktop/CABLAB_CODE/EMA/EMA_App', launch.browser = TRUE)"
