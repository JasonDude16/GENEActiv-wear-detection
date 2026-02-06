library(shiny)
library(dplyr)
library(ggplot2)
library(tidymodels)
source("../helpers.R")

models <- list.files("../../models/")
models <- tools::file_path_sans_ext(models)

df_modeling <- readRDS("../../data/modeling/df_modeling.RDS")
vars <- df_modeling |> select(matches("mean|sd|temp")) |> colnames()
all_ids <- unique(df_modeling$id)

tmp <- df_modeling |> filter(id == all_ids[1])
min_date <- min(tmp$date_time)
max_date <- max(tmp$date_time)

ui <- fluidPage(
  titlePanel("Geneactiv Wear/Non-wear Classification"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "type", 
        label = "Type", 
        choices = c("All", "Train", "Test"),
        selected = c("All")
      ),
      selectInput(
        "id", 
        label = "ID", 
        choices = all_ids
      ),
      dateRangeInput(
        "date_range",
        label = "Date range",
        start = min_date,
        end = max_date,
        min = min_date,
        max = max_date
      ),
      selectInput(
        "variables",
        label = "Variables to plot",
        choices = vars,
        multiple = TRUE,
        selected = c("sd_z_axis")
      ),
      selectInput(
        "classifiers",
        label = "Classifiers",
        choices = c("Truth", "GGIR", models),
        multiple = TRUE,
        selected = c("Truth", "Model")
      ),
      sliderInput(
        "prob",
        label = "Probability Threshold",
        min = 0,
        max = 1,
        step = .05,
        value = 0.5
      ),       
      fluidRow(
        column(width = 6, sliderInput("height", label = "Plot height", min = 400, max = 1200, value = 800)),
        column(width = 6, sliderInput("width", label = "Plot width", min = 800, max = 1600, value = 1200))
      ),
      selectInput(
        "save_type", 
        label = "Save as", 
        choices = c("png", "pdf"), 
        selected = "png"
      ),
      downloadButton("download_plot", label = "Save plot")
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  
  ready <- reactiveVal(FALSE)
  
  observeEvent(input$type, {
    if (input$type %in% c("Train", "Test")) {
      ids <- df_modeling |> 
        filter(train_test == tolower(input$type)) |>
        pull(id) |> 
        unique()
    } else {
      ids <- all_ids
    }
    updateSelectInput(session, "id", "ID", choices = ids)
  })
  
  model_cache <- reactiveVal(list())
  
  mods <- reactive({
    model_names <- setdiff(input$classifiers, c("Truth", "GGIR"))
    if (length(model_names) == 0) return(list())
    
    cache <- model_cache()
    missing <- setdiff(model_names, names(cache))
    
    if (length(missing) > 0) {
      for (nm in missing) {
        cache[[nm]] <- readRDS(file.path("../../models", paste0(nm, ".RDS")))
      }
      model_cache(cache)
    }
    
    cache[model_names]
  })

  df_subj <- reactive({
    df_modeling |>
      dplyr::filter(id == input$id, !is.na(ggir_is_worn)) |>
      dplyr::mutate(
        date = as.Date(date_time),
        GGIR  = as.factor(ggir_is_worn),
        Truth = as.factor(label_is_worn)
      )
  }) |> bindCache(input$id)
  
  observeEvent(input$id, {
    d <- df_subj()
    req(nrow(d) > 0)
    
    rng <- range(d$date, na.rm = TRUE)
    
    freezeReactiveValue(input, "date_range")
    updateDateRangeInput(
      session, "date_range",
      start = rng[1], end = rng[2],
      min = rng[1], max = rng[2]
    )
    
    ready(TRUE)
  }, ignoreInit = FALSE, priority = 90)
  
  df_subj_filt <- reactive({
    req(input$date_range)
    df_subj() |>
      dplyr::filter(date >= input$date_range[1], date <= input$date_range[2])
  }) |> bindCache(input$id, input$date_range)
  
  df_prob <- reactive({
    d <- df_subj_filt()
    req(nrow(d) > 0)
    m <- mods()
    if (length(m) == 0) return(d)
    
    for (modi in seq_along(m)) {
      nm <- names(m)[modi]
      d[[paste0(nm, "_prob")]] <- predict(m[[modi]], new_data = d, type = "prob")$.pred_1
    }
    d
  }) |> bindCache(input$id, input$date_range, setdiff(input$classifiers, c("Truth","GGIR")))
  
  df_pred <- reactive({
    d <- df_prob()
    model_names <- setdiff(input$classifiers, c("Truth", "GGIR"))
    
    if (length(model_names) > 0) {
      for (nm in model_names) {
        pcol <- paste0(nm, "_prob")
        # create classifier label column with original model name
        d[[nm]] <- as.factor(ifelse(d[[pcol]] > input$prob, 1, 0))
      }
    }
    d
  })
  
  plot_obj <- reactive({
    d_pred <- df_pred()
    d_subj <- df_subj_filt()
    
    if (length(input$classifiers) == 0) {
      return(NULL)
    }

    p_labs_time <- d_pred |>
      plot_labels_over_time(
        vars = input$variables,
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
    if (isTRUE(d_subj$is_validation[1]) && length(setdiff(input$classifiers, "Truth")) > 0) {
      
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
      
      layout <- matrix(
        c(1, 1, 2,
          1, 1, 3,
          1, 1, 3), 
        nrow = 3, 
        byrow = TRUE
      )
      
      gridExtra::grid.arrange(p_labs_time, res$plot, p_metrics, layout_matrix = layout)
      
    } else {
      gridExtra::grid.arrange(p_labs_time)
    }
  })
  
  output$plot <- renderPlot({
    req(ready())
    plot_obj()
  }, height = function(x) input$height, width = function(x) input$width)
  
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
