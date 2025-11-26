library(shiny)
library(bslib)
library(fpp3)
library(tidyverse)
library(gt)
library(urca)  # <- added to ensure urca namespace is available and avoid NaN in metrics

# Load and wrangle data (from wine.qmd)
aus_wine <- read_csv(here::here("AustralianWines.csv"), na = "*",
                     col_types = cols(Rose = col_number()),
                     show_col_types = FALSE) |>
  fill(Rose, .direction = "down") |>
  mutate(Month = mdy(str_replace(Month, '-', '-01-')) |> yearmonth()) |>
  pivot_longer(cols = -Month, names_to = "Varietal", values_to = "Sales") |>
  as_tsibble(index = Month, key = Varietal)

min_date <- as.Date(min(aus_wine$Month))
max_date <- as.Date(max(aus_wine$Month))

# training / validation split (kept consistent with qmd)
train_end <- yearmonth("1993 Dec")
training <- aus_wine |> filter(Month <= train_end)
validation <- aus_wine |> filter(Month > train_end)

# UI
ui <- navbarPage(
  title = "Australian Wine Sales Forecast",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  tabPanel(
    "Visualization",
    fluidRow(
      column(
        width = 4,
        card(
          card_header("Controls"),
          checkboxGroupInput(
            "varietal_select", "Select varietal(s):",
            choices = sort(unique(aus_wine$Varietal)),
            selected = sort(unique(aus_wine$Varietal)),
            inline = FALSE
          ),
          dateRangeInput(
            "date_range", "Date range:",
            start = min_date, end = max_date,
            min = min_date, max = max_date,
            format = "yyyy-mm"
          ),
          numericInput("h_viz", "Forecast horizon (periods/months)", value = 12, min = 1, step = 1),
          actionButton("update_viz", "Update")
        )
      ),
      column(
        width = 8,
        card(
          card_header("Overview plot"),
          plotOutput("overview_plot", height = "420px")
        ),
        br(),
        card(
          card_header("Seasonal plot"),
          plotOutput("season_plot", height = "420px")
        )
      )
    )
  ),
  
  tabPanel(
    "Modeling",
    # Top row: display controls (left) + model spec (right)
    fluidRow(
      column(
        width = 3,
        card(
          card_header("Display controls"),
          checkboxInput("show_model_spec", "Show model specification", value = TRUE),
          checkboxInput("show_train_acc", "Show training accuracy", value = TRUE),
          actionButton("fit_models", "Fit models")
        )
      ),
      column(
        width = 9,
        conditionalPanel(
          "input.show_model_spec == true",
          card(
            card_header("Model Specifications"),
            verbatimTextOutput("model_specs")
          )
        )
      )
    ),
    br(),
    # Bottom row: accuracy tables side-by-side
    fluidRow(
      column(
        width = 6,
        conditionalPanel(
          "input.show_train_acc == true",
          card(
            card_header("Training Accuracy"),
            uiOutput("training_accuracy_out")
          )
        )
      ),
      column(
        width = 6,
        card(
          card_header("Forecast Accuracy (validation)"),
          uiOutput("forecast_accuracy_out")
        )
      )
    )
  ),
  
  tabPanel(
    "Forecast",
    fluidRow(
      column(
        width = 4,
        card(
          card_header("Forecast controls"),
          selectInput("model_choice", "Model for table/plot:", choices = c("TSLM", "ETS", "ARIMA"), selected = "ETS"),
          numericInput("h_forecast", "Horizon (periods/months)", value = 12, min = 1, step = 1),
          actionButton("run_forecast", "Run forecast")
        )
      ),
      column(
        width = 8,
        card(
          card_header("Forecast plot"),
          plotOutput("forecast_plot", height = "420px")
        ),
        br(),
        card(
          card_header("Forecast table"),
          uiOutput("forecast_table_out")
        )
      )
    )
  ),
  
  tabPanel(
    "About",
    fluidRow(
      column(
        width = 12,
        card(
          card_header("About this app"),
          tags$div(
            tags$p("Data source: AustralianWines.csv (monthly sales by varietal)."),
            tags$p("Modeling choices: three models are fit on the training window (TSLM: trend + season, ETS: exponential smoothing, ARIMA). Training/validation split uses 1993 Dec as the cutoff."),
            tags$p("Forecasting: forecasts are produced for the interactive horizon (H) and validation forecasts are computed over the held-out validation window."),
            tags$h5("Reproduce figures"),
            tags$ol(
              tags$li("Install required packages: fpp3, tidyverse, gt, bslib, shiny."),
              tags$li("Place AustralianWines.csv in the project root."),
              tags$li("Start the app (run app.R). Use the Modeling tab to inspect model specs and accuracy."),
              tags$li("Adjust horizon and varietal selections on the Visualization/Forecast tabs to regenerate plots and tables.")
            ),
            tags$p("Notes: training accuracy is computed from the fitted models (in-sample) and forecast accuracy uses validation forecasts aligned to the validation tsibble.")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Visualization reactive data
  viz_data <- eventReactive(input$update_viz, {
    req(input$varietal_select, input$date_range)
    aus_wine |>
      filter(
        Varietal %in% input$varietal_select,
        Month >= yearmonth(input$date_range[1]),
        Month <= yearmonth(input$date_range[2])
      )
  }, ignoreNULL = FALSE)
  
  output$overview_plot <- renderPlot({
    df <- viz_data()
    req(nrow(df) > 0)
    ggplot(df, aes(x = Month, y = Sales, color = Varietal, group = Varietal)) +
      geom_line(size = 0.8) +
      geom_point(size = 0.9, alpha = 0.7) +
      labs(title = "Wine sales — time series overview",
           x = "Month", y = "Sales") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  output$season_plot <- renderPlot({
    df <- viz_data()
    req(nrow(df) > 0)
    if (length(unique(df$Varietal)) > 1) {
      gg_season(df, Sales) +
        facet_wrap(vars(Varietal), scales = "free_y") +
        labs(title = "Seasonal pattern by varietal", y = "Sales") +
        theme_minimal()
    } else {
      gg_season(df, Sales) +
        labs(title = paste("Seasonality —", unique(df$Varietal)), y = "Sales") +
        theme_minimal()
    }
  })
  
  # Reactive models (fit on training) — fit automatically at startup
  wine_models <- reactive({
    training |>
      model(
        TSLM = TSLM(Sales ~ trend() + season()),
        ETS = ETS(Sales),
        ARIMA = ARIMA(Sales)
      )
  })
  
   # Model specifications (show the mable)
   output$model_specs <- renderPrint({
     wm <- wine_models()
     req(wm)
     wm
   })
 
   # Training accuracy (rendered as GT HTML)
   output$training_accuracy_out <- renderUI({
     wm <- wine_models()
     req(wm)
     # Use mable accuracy() (in-sample) as in wine.qmd
     acc_train <- wm |> accuracy()
     acc_tab <- acc_train |>
       select(Varietal, .model, RMSE, MAE, MAPE) |>
       arrange(.model, RMSE) |>
       gt() |>
       fmt_number(columns = c(RMSE, MAE, MAPE), decimals = 0)
     htmltools::HTML(gt::as_raw_html(acc_tab))
   })
 
   # Forecast accuracy on the validation window (rendered as GT HTML)
   output$forecast_accuracy_out <- renderUI({
     # Use the forecast object produced for the full horizon and compare to aus_wine,
     # matching the wine.qmd pattern: wine_fc |> accuracy(aus_wine)
     wm <- wine_models()
     req(wm)
     fc <- wine_fc()
     req(fc)
     acc_val <- fc |> accuracy(aus_wine)
     acc_val_tab <- acc_val |>
       select(Varietal, .model, RMSE, MAE, MAPE) |>
       arrange(.model, RMSE) |>
       gt() |>
       fmt_number(columns = c(RMSE, MAE, MAPE), decimals = 0)
     htmltools::HTML(gt::as_raw_html(acc_val_tab))
   })
  
  # Forecasts: run automatically (reactive) using current horizon input
  wine_fc <- reactive({
    wm <- wine_models()
    req(wm)
    h <- as.integer(input$h_forecast)
    wm |> forecast(h = h)
  })
  
  validation_fc <- reactive({
    wm <- wine_models()
    req(wm)
    wm |> forecast(new_data = validation)
  })
  
  validation_accuracy <- reactive({
    validation_fc() |> accuracy(validation)
  })
  
  # Forecast plot
  output$forecast_plot <- renderPlot({
    fc <- wine_fc()
    req(fc)
    cut_off <- yearmonth(max(training$Month)) # show from end of training
    fc |> autoplot(aus_wine |> filter(Month >= cut_off)) +
      labs(title = "Forecasted Wine Sales by Varietal", x = "Month", y = "Sales") +
      facet_wrap(~ Varietal, scales = "free_y")
  })
  
  # Forecast table (for chosen model)
  output$forecast_table_out <- renderUI({
    fc <- wine_fc()
    req(fc)
    gt_tab <- fc |>
      as_tibble() |>
      filter(.model == input$model_choice) |>
      select(Month, Varietal, .mean) |>
      mutate(.mean = map_dbl(.mean, as.numeric)) |>
      pivot_wider(names_from = Varietal, values_from = .mean) |>
      arrange(Month) |>
      mutate(Month = as.character(Month)) |>
      gt() |>
      fmt_number(columns = where(is.numeric), decimals = 0)
    htmltools::HTML(gt::as_raw_html(gt_tab))
  })
  
  # expose validation accuracy as message in console when models fitted/forecast run (optional)
  observeEvent(validation_accuracy(), {
    acc <- validation_accuracy()
    # do not print to UI; could log or make available in outputs later
    invisible(acc)
  })
}

shinyApp(ui = ui, server = server)
