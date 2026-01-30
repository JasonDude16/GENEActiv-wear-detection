library(shiny)
source("../helpers.R")

models <- list.files("../../models/")
df_modeling <- readRDS("../../data/modeling/df_modeling.RDS")
all_ids <- unique(df_modeling$id)

tmp <- df_modeling |> filter(id == all_ids[1])
min_date <- min(tmp$date_time)
max_date <- max(tmp$date_time)

ui <- fluidPage(
  titlePanel("Geneactiv Wear/Non-wear Classification"),
  sidebarLayout(
    sidebarPanel(
      selectInput("type", "Type", choices = c("All", "Train", "Test")),
      selectInput("id", "ID", choices = all_ids),
      # NEW: date filter (range updated dynamically per ID)
      dateRangeInput(
        "date_range",
        "Date range",
        start = min_date,
        end = max_date,
        min = min_date,
        max = max_date
      ),
      selectInput("model", "Model", choices = models),
      selectInput(
        "classifiers",
        "Classifiers",
        choices = c("Truth", "Model", "GGIR"),
        multiple = TRUE,
        selected = c("Truth", "Model")
      ),
      sliderInput("prob", "Probability Threshold", min = 0, max = 1, step = .05, value = 0.5),
      
      fluidRow(
        column(width = 6, sliderInput("height", "Plot height", min = 400, max = 1200, value = 800)),
        column(width = 6, sliderInput("width", "Plot width", min = 800, max = 1600, value = 1200))
      ),
      
      # NEW: save controls
      selectInput("save_type", "Save as", choices = c("png", "pdf"), selected = "png"),
      downloadButton("download_plot", "Save plot")
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$type, {
    if (input$type %in% c("Train", "Test")) {
      ids <- df_modeling |> filter(train_test == tolower(input$type)) |> pull(id) |> unique()
    } else {
      ids <- all_ids
    }
    updateSelectInput(session, "id", "ID", choices = ids)
  })
  
  mod <- reactive({
    readRDS(file.path("../../models", input$model))
  })

  df_subj <- reactive({
    df_modeling |>
      dplyr::filter(id == input$id, !is.na(ggir_is_worn)) |>
      dplyr::mutate(date = as.Date(date_time))
  })
  
  # NEW: update date range bounds whenever ID changes
  observeEvent(df_subj(), {
    rng <- range(df_subj()$date, na.rm = TRUE)
    updateDateRangeInput(
      session,
      "date_range",
      start = rng[1],
      end = rng[2],
      min = rng[1],
      max = rng[2]
    )
  }, ignoreInit = TRUE)
  
  # NEW: apply date filter (drives both predictions + metrics)
  df_subj_filt <- reactive({
    req(input$date_range)
    df_subj() |>
      dplyr::filter(date >= input$date_range[1], date <= input$date_range[2])
  })
  
  df_pred <- reactive({
    d <- df_subj_filt()
    req(nrow(d) > 0)
    
    d |>
      dplyr::mutate(
        prob = predict(mod(), new_data = d, type = "prob")$.pred_1,
        Model = as.factor(ifelse(prob > input$prob, 1, 0)),
        GGIR  = as.factor(ggir_is_worn),
        Truth = as.factor(label_is_worn)
      )
  })
  
  # NEW: build the plot once so we can both render and save it
  plot_obj <- reactive({
    d_pred <- df_pred()
    d_subj <- df_subj_filt()

    p_labs_time <- d_pred |>
      plot_labels_over_time(
        vars = c("mean_x_axis", "mean_temp"),
        var_label = c("Activity", "Temperature"),
        source_cols = input$classifiers,
        levels = c(0, 1),
        date_time_col = "date_time",
        var_height = 2
      ) +
      ggtitle(paste0(
        "Participant: ", input$id,
        " (", format(min(d_subj$date)), " to ", format(max(d_subj$date)), ")"
      ))
    
    # If validation, recompute metrics on the FILTERED window
    if (isTRUE(d_subj$is_validation[1])) {
      
      res <- d_pred |>
        plot_confusion_matrix(
          ref_col = "label_is_worn",
          class_cols = setdiff(input$classifiers, "Truth"),
          x_lab = "Predicted",
          y_lab = "Truth",
          pos_value = 1,
          neg_value = 0
        )
      
      p_metrics <- plot_metrics_bars(res$metrics, facet = FALSE)
      
      layout <- matrix(c(1, 1, 2,
                         1, 1, 3,
                         1, 1, 3), nrow = 3, byrow = TRUE)
      
      gridExtra::grid.arrange(p_labs_time, res$plot, p_metrics, layout_matrix = layout)
      
    } else {
      # still return a grob for consistent saving
      gridExtra::grid.arrange(p_labs_time)
    }
  })
  
  output$plot <- renderPlot({
    plot_obj()
  }, height = function(x) input$height, width = function(x) input$width)
  
  # NEW: download handler to save exactly what you see
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("wear_nonwear_", input$id, "_", Sys.Date(), ".", input$save_type)
    },
    content = function(file) {
      ggplot2::ggsave(
        filename = file,
        plot = plot_obj(),
        height = input$height*4,
        width = input$width*4,
        units = "px"
      )
    }
  )
}

shinyApp(ui = ui, server = server)
