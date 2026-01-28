library(shiny)
source("../helpers.R")

models <- list.files("../../models/")
df <- readRDS("../../data/modeling/df_modeling.RDS")
ids <- unique(df$id)

ui <- fluidPage(
    titlePanel("GGIR Wear/Non-wear Classification"),
    sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "id",
            label = "ID",
            choices = ids
          ),
          selectInput(
            inputId = "model",
            label = "Model",
            choices = models
          ),
          selectInput(
            inputId = "classifiers",
            label = "Classifiers",
            choices = c("Model", "GGIR", "Truth"),
            multiple = TRUE,
            selected = "Model"
          ),
          sliderInput(
            inputId = "prob",
            label = "Probability Threshold",
            min = 0,
            max = 1,
            step = .05,
            value = 0.5
          ),
          sliderInput(
            inputId = "height",
            label = "Height",
            min = 400,
            max = 1200,
            value = 800
          ),
          sliderInput(
            inputId = "width",
            label = "Width",
            min = 800,
            max = 1600,
            value = 1200
          )
        ),
          mainPanel(
           plotOutput("plot")
        )
    )
)

server <- function(input, output) {
  
  mod <- reactive({
    readRDS(file.path("../../models", input$model))
  })
  
  df_subj <- reactive({
    df_modeling |> filter(id == input$id, !is.na(ggir_is_worn))
  })
  
  df_pred <- reactive({
    df_subj() |> 
      mutate(
        prob = predict(mod(), new_data = df_subj(), type = "prob")$.pred_1,
        Model = as.factor(ifelse(prob > input$prob, 1, 0)),
        GGIR = as.factor(ggir_is_worn),
        Truth = as.factor(label_is_worn)
      )
  })
  
  output$plot <- renderPlot({
    
    p_labs_time <- df_pred() |> 
      plot_labels_over_time(
        vars = c("mean_x_axis", "mean_temp"), 
        var_label = c("Activity", "Temperature"), 
        source_cols = input$classifiers,
        levels = c(0, 1),
        date_time_col =  "date_time",
        var_height = 2
      ) + 
      ggtitle(paste0("Participant: ", input$id))
  
    if (df_subj()$is_validation[1]) {
      res <- df_pred() |> 
        plot_confusion_matrix(
          ref_col = "label_is_worn",
          class_cols = setdiff(input$classifiers, "Truth"),
          x_lab = "Predicted",
          y_lab = "Truth",
          pos_value = 1,
          neg_value = 0
        )
      
      p_metrics <- plot_metrics_bars(res$metrics, facet = FALSE)
      
      layout <- matrix(c(1, 1, 2, 1, 1, 3, 1, 1, 3), nrow = 3, byrow = TRUE)
      return(gridExtra::grid.arrange(p_labs_time, res$plot, p_metrics, layout_matrix = layout))
      
    } else {
      return(p_labs_time)
      
    }

  }, height = function(x) input$height, width = function(x) input$width)
}

shinyApp(ui = ui, server = server)
