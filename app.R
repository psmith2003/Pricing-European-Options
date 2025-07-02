# --------------------------
# ----- 1. Load Libraries -----
# --------------------------
library(shiny)
library(tidyquant)
library(reticulate)
library(tidyverse)
library(bizdays)

# --------------------------
# ----- 2. Source External Scripts & Load Python -----
# --------------------------
source("app_functions.R") 
load_python() # Required to initialize Python environment

# --------------------------
# ----- 3. UI (User Interface) -----
# --------------------------
ui <- fluidPage(
  titlePanel("Market Selector"),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      div(style = "text-align: center;",
          h4("Ticker"),
          actionButton("spx_btn", "SPX", width = "100%"),
          br(), br(),
          actionButton("spy_btn", "SPY", width = "100%"),
          br(), br(),
          actionButton("qqq_btn", "QQQ", width = "100%"),
          br(), br(),
          actionButton("ndx_btn", "NDX", width = "100%"),
          
          hr(),
          
          uiOutput("date_selector_ui")
      )
    ),
    
    mainPanel(
      h3(textOutput("selected_ticker_title")),
      h4(textOutput("selected_date_display")),
      br(),
      tableOutput("ticker_data_table")
    )
  )
)

# --------------------------
# ----- 4. Server Logic -----
# --------------------------
server <- function(input, output, session) {
  
  selected_ticker <- reactiveVal("^SPX")
  
  observeEvent(input$spx_btn, { selected_ticker("^SPX") })
  observeEvent(input$spy_btn, { selected_ticker("SPY") })
  observeEvent(input$qqq_btn, { selected_ticker("QQQ") })
  observeEvent(input$ndx_btn, { selected_ticker("^NDX") })
  
  display_ticker <- reactive({
    req(selected_ticker())
    gsub("\\^", "", selected_ticker())
  })
  
  current_price <- reactive({
    req(display_ticker())
    tryCatch({
      symbol <- selected_ticker()
      quote <- quantmod::getQuote(symbol)
      quote$Last
    }, error = function(e) {
      NA
    })
  })
  
  
  expiration_dates <- reactive({
    ticker <- selected_ticker()
    req(ticker)
    dates <- py$get_call_exp_date(ticker = ticker)
    dates_converted <- suppressWarnings(as.Date(as.character(dates)))
    if (any(is.na(dates_converted))) {
      warning("Some dates could not be converted to Date class")
    }
    dates_converted
  })
  
  date_choices_with_days <- reactive({
    dates <- expiration_dates()
    today <- Sys.Date()
    
    # Define holidays (make sure this is defined before using)
    holidays <- timeDate::holidayNYSE(2025)
    
    # Create calendar once with valid range and holidays
    create.calendar(
      "NYSE",
      holidays = as.Date(holidays),
      weekdays = c("saturday", "sunday"),
      start.date = as.Date("2000-01-01"),
      end.date = as.Date("2030-12-31")
    )
    
    # Ensure dates vector is Date class
    dates <- as.Date(dates)
    
    formatted <- sapply(dates, function(d) {
      days_left <- bizdays(today, d, "NYSE") # calculate bizdays
      paste0(format(d, "%Y-%m-%d"), " (", days_left, ")")
    })
    
    names(dates) <- formatted
    dates
  })
  
  
  selected_date_index <- reactive({
    req(input$selected_date)
    dates <- expiration_dates()
    which(dates == as.Date(input$selected_date))
  })
  
  output$date_selector_ui <- renderUI({
    choices <- date_choices_with_days()
    selectInput(
      inputId = "selected_date",
      label = "Select Expiration Date",
      choices = choices,
      selected = choices[1],
      width = "100%"
    )
  })
  
  retrieved_data <- reactive({
    req(selected_ticker(), selected_date_index(), input$selected_date)
    tryCatch({
      chain <- py$get_calls(ticker = selected_ticker(), exp_index = selected_date_index())
      result <- expected_call(
        symbol = selected_ticker(),
        opt_chain = chain,
        expDate_month = input$selected_date
      )
      print(str(result$lastTradeDate))
      result
    }, error = function(e) {
      warning(paste("Error in expected_call:", e$message))
      NULL
    })
  })
  
  output$selected_ticker_title <- renderText({
    price <- current_price()
    ticker <- display_ticker()
    if (is.na(price)) {
      paste("Displaying Call Data for:", ticker)
    } else {
      paste0("Displaying Call Data for: ", ticker, "   $", round(price, 2))
    }
  })
  
  
  output$selected_date_display <- renderText({
    req(input$selected_date)
    paste("Selected Date:", input$selected_date)
  })
  
  output$ticker_data_table <- renderTable({
    req(retrieved_data())
    retrieved_data()
  })
}

# --------------------------
# ----- 5. Run the Application -----
# --------------------------
shinyApp(ui, server)
