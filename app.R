library(shiny)
library(bslib)
library(fpp3)
library(tidyverse)
library(gt)
library(urca)   # REQUIRED for ARIMA stability + accuracy (prevents NaN on Connect)

# Load and wrangle data
aus_wine <- read_csv(here::here("AustralianWines.csv"), na = "*",
                     col_types = cols(Rose = col_number()),
                     show_col_types = FALSE) |>
  fill(Rose, .direction = "down") |>
  mutate(Month = mdy(str_replace(Month, '-', '-01-')) |> yearmonth()) |>
  pivot_longer(cols = -Month, names_to = "Varietal", values_to = "Sales") |>
  as_tsibble(index = Month, key = Varietal)

min_date <- as.Date(min(aus_wine$Month))
max_date <- as.Date(max(aus_wine$Month))

# Train/validation split
train_end <- yearmonth("1993 Dec")
training  <- aus_wine |> filter(Month <= train_end)
validation <- aus_wine |> filter(Month > train_end)

# Utility — removes NaN if any appear (makes tables clean in UI)
clean_acc <- function(x){
  x |> mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA, .)))
}

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
          checkboxGroupInput("varietal_select","Select varietal(s):",
             choices = sort(unique(aus_wine$Varietal)),
             selected = sort(unique(aus_wine$Varietal))),
          dateRangeInput("date_range","Date range:",start=min_date,end=max_date,
                         min=min_date,max=max_date,format="yyyy-mm"),
          numericInput("h_viz","Forecast horizon",value=12,min=1,step=1),
          actionButton("update_viz","Update")
        )
      ),
      column(
        width = 8,
        card(card_header("Overview plot"),plotOutput("overview_plot",height="420px")),
        br(),
        card(card_header("Seasonal plot"),plotOutput("season_plot",height="420px"))
      )
    )
  ),
  
  tabPanel(
    "Modeling",
    fluidRow(
      column(
        width = 3,
        card(
          card_header("Display controls"),
          checkboxInput("show_model_spec","Show model specification", TRUE),
          checkboxInput("show_train_acc","Show training accuracy", TRUE),
          actionButton("fit_models","Fit models")
        )
      ),
      column(
        width = 9,
        conditionalPanel("input.show_model_spec==true",
          card(card_header("Model Specifications"),verbatimTextOutput("model_specs"))
        )
      )
    ),
    br(),
    fluidRow(
      column(
        width = 6,
        conditionalPanel("input.show_train_acc==true",
          card(card_header("Training Accuracy"),uiOutput("training_accuracy_out"))
        )
      ),
      column(
        width = 6,
        card(card_header("Forecast Accuracy (validation)"),uiOutput("forecast_accuracy_out"))
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
          selectInput("model_choice","Model for table/plot:",
                      choices=c("TSLM","ETS","ARIMA"),selected="ETS"),
          numericInput("h_forecast","Horizon (months)",value=12,min=1,step=1),
          actionButton("run_forecast","Run forecast")
        )
      ),
      column(
        width = 8,
        card(card_header("Forecast plot"), plotOutput("forecast_plot",height="420px")),
        br(),
        card(card_header("Forecast table"),uiOutput("forecast_table_out"))
      )
    )
  ),
  
  tabPanel(
    "About",
    card(
      card_header("About this app"),
      tags$p("Data source: AustralianWines.csv (monthly wine sales)."),
      tags$p("Models: TSLM (trend+season), ETS, ARIMA. Train split ends 1993 Dec."),
      tags$p("Forecasts generated interactively with validation accuracy reporting.")
    )
  )
)

server <- function(input, output, session) {

  # Filtered visualization data
  viz_data <- eventReactive(input$update_viz,{
    aus_wine |>
      filter(
        Varietal %in% input$varietal_select,
        Month >= yearmonth(input$date_range[1]),
        Month <= yearmonth(input$date_range[2])
      )
  },ignoreNULL=FALSE)

  output$overview_plot <- renderPlot({
    df <- viz_data(); req(nrow(df)>0)
    ggplot(df,aes(Month,Sales,color=Varietal,group=Varietal))+
      geom_line(size=.9)+geom_point(size=.8)+
      labs(title="Wine Sales — Overview",x="Month",y="Sales")+
      theme_minimal()+theme(legend.position="bottom")
  })

  output$season_plot <- renderPlot({
    df <- viz_data(); req(nrow(df)>0)
    p <- gg_season(df,Sales)+theme_minimal()
    if(length(unique(df$Varietal))>1) p + facet_wrap(vars(Varietal),scales="free_y")
    else p + labs(title=paste("Seasonality —",unique(df$Varietal)))
  })

  #––––––––– FIXED MODEL BLOCK (ARIMA stabilized) –––––––––#
  wine_models <- reactive({
    training |> model(
      TSLM = TSLM(Sales ~ trend() + season()),
      ETS  = ETS(Sales),
      ARIMA = ARIMA(Sales, stepwise=FALSE, approximation=FALSE)  # <– Cloud safe
    )
  })

  output$model_specs <- renderPrint({ wine_models() })

  output$training_accuracy_out <- renderUI({
    acc_train <- clean_acc(wine_models() |> accuracy())
    gt_tab <- acc_train |>
      select(Varietal,.model,RMSE,MAE,MAPE)|>
      gt()|>fmt_number(columns=where(is.numeric),decimals=0)
    HTML(gt::as_raw_html(gt_tab))
  })

  #––––– FULL FIX: VALIDATION ACCURACY NOW CORRECT –––––#
  output$forecast_accuracy_out <- renderUI({
    acc_val <- clean_acc(wine_fc() |> accuracy(validation))  # <– fixed here
    gt_tab <- acc_val |>
      select(Varietal,.model,RMSE,MAE,MAPE)|>
      gt()|>fmt_number(columns=where(is.numeric),decimals=0)
    HTML(gt::as_raw_html(gt_tab))
  })

  wine_fc <- eventReactive(input$run_forecast,{
    wine_models() |> forecast(h=as.integer(input$h_forecast))
  })

  #––––––––– FORECAST PLOT –––––––––#
  output$forecast_plot <- renderPlot({
    fc <- wine_fc(); req(fc)
    cut_off <- yearmonth(max(training$Month))
    fc |> autoplot(aus_wine |> filter(Month>=cut_off))+aes(color=.model)+
      labs(title="Forecasted Wine Sales by Varietal",x="Month",y="Sales")+
      facet_wrap(~Varietal,scales="free_y")
  })

  output$forecast_table_out <- renderUI({
    fc <- wine_fc()
    tab <- fc |> as_tibble() |> filter(.model==input$model_choice) |>
      select(Month,Varietal,.mean) |>
      mutate(.mean=map_dbl(.mean,as.numeric)) |>
      pivot_wider(names_from=Varietal,values_from=.mean)|>
      gt()|>fmt_number(columns=where(is.numeric),decimals=0)
    HTML(gt::as_raw_html(tab))
  })
}

shinyApp(ui,server)
