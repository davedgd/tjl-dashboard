# For changing appearance, see: https://rstudio.github.io/shinydashboard/appearance.html
# For different types of input, see: https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/
# For authentication, see: https://github.com/PaulC91/shinyauthr/blob/master/README.md
# For icon options, see: https://fontawesome.com/icons?from=io

doAuth <- TRUE

user_base <- data.frame(
  user = c("david", "mike", "faculty", "msba"),
  password = c("test", "tjl", "mcintire", "demo"), 
  permissions = c("admin", "standard", "standard", "standard"),
  name = c("David", "Mike @ TJL", "McIntire", "MSBA"),
  stringsAsFactors = FALSE,
  row.names = NULL
)

ui <- dashboardPage(
  title = "JL Dashboard",
  header = dashboardHeader(title = a(href = "https://www.thejuicelaundry.com",
                                     img(src = "JL_Logo.png", width = 28, height = 28),
                                     "The Juice Laundry")),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Transactions and Sales", tabName = "visualizations", icon = icon("chart-line")),
      menuItem("Detailed Sales", tabName = "items", icon = icon("leaf")),
      menuItem("Forecasting", icon = icon("clock"),
               menuSubItem("Transaction Forecast",
                           tabName = "forecast"),
               menuSubItem("Diagnostics",
                           tabName = "forecast_diagnostics"),
               startExpanded = TRUE),
      menuItem("Locations and Weather", tabName = "map", icon = icon("map-marked-alt")),
      menuItem("Info", tabName = "info", icon = icon("info-circle"))
    ),
    if (doAuth) div(class = "pull-left", shinyauthr::logoutUI(id = "logout", label = "Log Out"))
  ),
  dashboardBody(
    useShinyjs(), # note: needed if using shinyjs (e.g., see ?runjs)
    if (doAuth) shinyauthr::loginUI(id = "login",
                                    title = "Welcome",
                                    login_title = "Log In"),
    includeScript("www/skycons-master/skycons.js"),
    includeCSS("www/css/styles.css"),
    tags$head(),
    tabItems(
      
      # transactions and sales tab
      tabItem(tabName = "visualizations",
              
              # 1st row
              fluidRow(
                
                box(title = strong("By Date"),
                    plotlyOutput("sales_plot1", height = 250),
                    width = 7),
                
                box(
                  title = "Controls",
                  width = 5,
                  
                  selectInput(
                    inputId = "sales_variable1", 
                    label = "Plot Type", 
                    choices = c("Transactions" = "Plot_Transactions",
                                "Sales (Undiscounted)" = "Plot_Sales_Undiscounted",
                                "Sales (Collected)" = "Plot_Sales_Collected")),
                  
                  checkboxGroupInput(
                    inputId = "sales_locations1", 
                    label = "Locations", 
                    choices = JL_Locations$id %>% 
                      map(list()) %>% 
                      set_names(JL_Locations$name),
                    selected = initialLocation),
                  
                  dateRangeInput(
                    inputId = "sales_dateRange1", 
                    label = "Date Range",
                    start = (max(JL_Transactions_V1$datetime) - days(6)) %>% as_date(),
                    end = max(JL_Transactions_V1$datetime) %>% as_date())
                  
                )
              ),
              
              # 2nd row
              fluidRow(
                
                box(title = strong("By Day of Week"),
                    plotlyOutput("sales_plot2", height = 250),
                    width = 7),
                
                box(
                  title = "Controls",
                  width = 5,
                  
                  selectInput(
                    inputId = "sales_variable2", 
                    label = "Plot Type", 
                    choices = c("Transactions" = "Plot_Transactions_ByWeekday",
                                "Sales (Undiscounted)" = "Plot_Sales_ByWeekday_Undiscounted",
                                "Sales (Collected)" = "Plot_Sales_ByWeekday_Collected")),
                  
                  checkboxGroupInput(
                    inputId = "sales_locations2", 
                    label = "Locations", 
                    choices = JL_Locations$id %>% 
                      map(list()) %>% 
                      set_names(JL_Locations$name),
                    selected = initialLocation),
                  
                  dateRangeInput(
                    inputId = "sales_dateRange2", 
                    label = "Date Range",
                    start = (max(JL_Transactions_V1$datetime) - days(6)) %>% as_date(),
                    end = max(JL_Transactions_V1$datetime) %>% as_date())
                  
                )
              )
      ),
      
      # items tab
      tabItem(tabName = "items",
              
              fluidRow(
                
                box(plotlyOutput("items_plot1", height = 450),
                    width = 7),
              
                box(
                  title = "Controls",
                  width = 5,
                  
                  selectInput(
                    inputId = "items_variable1", 
                    label = "Item Category", 
                    choices = JL_Item_Categories,
                    selected = "SMOOTHIES"),
                  
                  selectInput(
                    inputId = "items_variable2", 
                    label = "Plot Type", 
                    choices = c("Units Sold" = "count",
                                "Sales" = "price")),
                  
                  checkboxGroupInput(
                    inputId = "items_locations1", 
                    label = "Locations", 
                    choices = JL_Locations$id %>% 
                      map(list()) %>% 
                      set_names(JL_Locations$name),
                    selected = initialLocation),
                  
                  dateRangeInput(
                    inputId = "items_dateRange1", 
                    label = "Date Range",
                    start = max(JL_Transactions_V1$datetime) - days(6),
                    end = max(JL_Transactions_V1$datetime))
                  
                  )
                )
      ),
      
      # forecast tab
      tabItem(tabName = "forecast",
              fluidRow(
                box(title = strong("Forecast"),
                    plotlyOutput("forecast_plot"),
                    width = 7),
                box(
                  title = strong("Controls"),
                  selectInput(
                    inputId = "forecast_location", 
                    label = "Location", 
                    choices = JL_Locations$id %>% 
                      map(list()) %>% 
                      set_names(JL_Locations$name),
                    selected = forecast$location),
                  sliderInput(
                    inputId = "forecast_plot_horizon",
                    label = strong("Horizon (Days)"),
                    min = 1, 
                    max = 7, 
                    value = 7),
                  width = 5))
              
      ),
              
      
      # forecast diagnostics tab
      tabItem(tabName = "forecast_diagnostics",
              
              fluidRow(
                box(title = strong("Benchmarks"),
                    plotOutput("forecast_diag_plot1"),
                    width = 7),
                box(title = strong("Controls"),
                    selectInput(
                      inputId = "forecast_diag_type",
                      label = "Plot Type",
                      choices = c("Predictions" = "pred",
                                  "Summary" = "metrics"),
                      selected = "pred"),
                    selectInput(
                      inputId = "forecast_diag_metric", 
                      label = "Metric", 
                      choices = c("MSE" = "mse",
                                  "RMSE" = "rmse",
                                  "MAE" = "mae",
                                  "MAPE" = "mape",
                                  "Coverage" = "coverage"),
                      selected = "mape"),
                    sliderInput(
                      inputId = "forecast_diag_horizon",
                      label = strong("Horizon (Days)"),
                      min = 3, 
                      max = 28, 
                      value = 28),
                    selectInput(
                      inputId = "forecast_diag_location", 
                      label = "Location", 
                      choices = JL_Locations$id %>% 
                        map(list()) %>% 
                        set_names(JL_Locations$name),
                      selected = forecast$location) %>% disabled(),
                    HTML(paste(strong("Note:"), "To change location, use the", em("Transaction Forecast"), "tab (updates to this page may be slightly delayed)")),
                    width = 5)
                ),
              fluidRow(
                box(title = strong("Forecast Fit"),
                    plotOutput("forecast_diag_plot2"),
                    width = 12)),
              fluidRow(
                box(title = strong("Forecast Components"),
                    plotOutput("forecast_diag_plot3"),
                    width = 12)
              ),
              fluidRow(
                box(htmlOutput("forecast_diag_logos"),
                    width = 12))
      ),
      
      # locations tab
      tabItem(tabName = "map",
              
              fluidRow(
                box(title = strong("Location Map"),
                    plotlyOutput("map"),
                    width = 7),
                box(title = strong("Forecast"),
                    htmlOutput("forecast"),
                    width = 5)
              ),
              fluidRow(
                box(title = strong(paste0("Today's Weather (", JL_WeatherTable$time %>% unique() %>% min(), ")")),
                    tableOutput("weather"),
                    HTML(paste(strong("Note:"), "You may need to scroll to see all details")),
                    width = 12)),
              fluidRow(
                box(htmlOutput("map_logos"),
                    width = 12)
              )
      ),
      
      # info tab
      tabItem(tabName = "info",
              
              fluidRow(
                box(title = strong("Info"),
                    HTML(paste("<p>Designed and developed by David Dobolyi @ UVa McIntire</p>
                                <p>A big THANK YOU to Mike Keenan @ The Juice Laundry for his support on this project!</p>
                                <p><strong>Latest Square Transaction Timestamp in Data Set:</strong> ", max(JL_Transactions_V1$datetime),"</p>")),
                    width = 12)
              )
      )
      
    )
  )
)

server <- function(input, output, session) {
  
  cdata <- session$clientData
  
  if (doAuth) {
  
    logout_init <- callModule(shinyauthr::logout, 
                              id = "logout", 
                              active = reactive(credentials()$user_auth))
    
    credentials <- callModule(shinyauthr::login, 
                              id = "login", 
                              data = user_base,
                              user_col = user,
                              pwd_col = password,
                              log_out = reactive(logout_init()))
    
    user_data <- reactive({credentials()$info})
    
    observeEvent(credentials(), {
      
      if (doAuth) req(credentials()$user_auth)
      
      show(id = "sidebarCollapsed")
      show(selector = ".tab-content")
      
    })
    
    observeEvent(logout_init(), {
      
      hide(id = "sidebarCollapsed")
      hide(selector = ".tab-content")
      
    })
    
    runjs('document.querySelector("#login-password").addEventListener("keyup", event => {
            if (event.key !== "Enter") return;
            document.querySelector("#login-button").click();
            event.preventDefault();
          });') # allow hitting enter to submit a password
  
  } else {
    
    show(id = "sidebarCollapsed")
    show(selector = ".tab-content")
    
  }
  
  output$sales_plot1 <- renderPlotly({
    
    if (doAuth) req(credentials()$user_auth)
    
    if (input$sales_dateRange1[2] - input$sales_dateRange1[1] > 2 * 7) pointsize = 1 else pointsize = 2.5
    
    if (input$sales_variable1 == "Plot_Transactions")
      p <- Plot_Transactions(
        locations = input$sales_locations1, 
        start = input$sales_dateRange1[1], 
        end = input$sales_dateRange1[2],
        pointsize = pointsize,
        linesize = 1)

    else if (input$sales_variable1 == "Plot_Sales_Undiscounted")
      p <- Plot_Sales(
        type = "undiscounted", 
        locations = input$sales_locations1,
        start = input$sales_dateRange1[1], 
        end = input$sales_dateRange1[2],
        pointsize = pointsize,
        linesize = 1)
    
    else if (input$sales_variable1 == "Plot_Sales_Collected")
      p <- Plot_Sales(
        type = "collected",
        locations = input$sales_locations1,
        start = input$sales_dateRange1[1], 
        end = input$sales_dateRange1[2],
        pointsize = pointsize,
        linesize = 1)
    
    ggplotly(p, tooltip = "text") %>% layout(legend = list(y = 1.13, orientation = "h", font = list(size = 10)), showlegend = FALSE)
    
  })
    
  output$sales_plot2 <- renderPlotly({
    
    if (doAuth) req(credentials()$user_auth)
        
    if (input$sales_variable2 == "Plot_Transactions_ByWeekday")
      p <- Plot_Transactions_ByWeekday(
        locations = input$sales_locations2,
        start = input$sales_dateRange2[1], 
        end = input$sales_dateRange2[2],
        pointsize = 2.5,
        linesize = 1)
    
    else if (input$sales_variable2 == "Plot_Sales_ByWeekday_Undiscounted")
      p <- Plot_Sales_ByWeekday(
        type = "undiscounted", 
        locations = input$sales_locations2,
        start = input$sales_dateRange2[1], 
        end = input$sales_dateRange1[2],
        pointsize = 2.5,
        linesize = 1)
    
    else if (input$sales_variable2 == "Plot_Sales_ByWeekday_Collected")
      p <- Plot_Sales_ByWeekday(
        type = "collected", 
        locations = input$sales_locations2,
        start = input$sales_dateRange2[1], 
        end = input$sales_dateRange2[2],
        pointsize = 2.5,
        linesize = 1)
    
    ggplotly(p, tooltip = "text") %>% layout(legend = list(y = 1.13, orientation = "h", font = list(size = 10)), showlegend = FALSE)
    
  })
  
  output$items_plot1 <- renderPlotly({
    
    if (doAuth) req(credentials()$user_auth)

    p <- Plot_Items(
      type = input$items_variable2,
      locations = input$items_locations1, 
      category = input$items_variable1,
      start = input$items_dateRange1[1], 
      end = input$items_dateRange1[2],
      base_size = 13)
  
    ggplotly(p, tooltip = "text") %>% layout(legend = list(y = 1.13, orientation = "h", font = list(size = 10)), showlegend = FALSE)
      
  })
  
  # lock forecast diag location to forecast location
  observeEvent(input$forecast_location, ignoreInit = TRUE, {

    # note the use of deep assignment (see http://adv-r.had.co.nz/Environments.html)
    df <<- Build_Forecast_DF(Weather_Daily = JL_Weather_Daily, locations = input$forecast_location)
    m <<- Build_Prophet_Model(df)
    forecast <<- Build_Forecast(m, Weather_Daily = JL_Weather_Daily, Weather_Daily_Forecast = JL_Weather_Daily_Forecast)
    
    updateSelectInput(session,
                      inputId = "forecast_diag_location",
                      selected = input$forecast_location)
    
  })
  
  output$forecast_plot <- renderPlotly({
    
    if (doAuth) req(credentials()$user_auth)
    
    ggplotly(Plot_Forecast(Plot_Transactions(locations = input$forecast_location), horizon = input$forecast_plot_horizon), tooltip = "text") %>% layout(legend = list(y = 1.13, orientation = "h", font = list(size = 10)), showlegend = FALSE)
    
  })
  
  output$forecast_diag_plot1 <- renderPlot({
    
    if (doAuth) req(credentials()$user_auth)
    
    diagLocations <- input$forecast_diag_location
    horizonLength <- input$forecast_diag_horizon
    
    df.cv <- suppressWarnings(suppressMessages(cross_validation(m, initial = nrow(df) - horizonLength - 1, period = nrow(df) - horizonLength, horizon = horizonLength, units = "days")))
    
    if (input$forecast_diag_type == "metrics") {
      
      df.p <- performance_metrics(df.cv)
      df.p.full <- performance_metrics(df.cv, rolling_window = 1)
      
      metrics <- tibble(
        Metric = c("mse", "rmse", "mae", "mape", "coverage"),
        Description = c("Mean Square Error", "Root Mean Square Error", "Mean Absolute Error", "Mean Absolute Percentage Error", "Coverage")
      )
      
      p <- plot_cross_validation_metric(df.cv, metric = input$forecast_diag_metric) + 
        labs(subtitle = metrics$Description[which(metrics$Metric == input$forecast_diag_metric)],
        caption = paste0("Overall Mean: ", df.p.full %>% pull(input$forecast_diag_metric) %>% round(2))) + theme_bw(base_size = 16) + labs(y = input$forecast_diag_metric %>% str_to_upper()) 
      
    } else if (input$forecast_diag_type == "pred") {
      
      p <- Plot_Forecast_Pred(df.cv, metric = input$forecast_diag_metric, base_size = 16, locations = JL_Locations %>% filter(id  == diagLocations) %>% pull(name))
      
    }
    
    p
    
  })
  
  output$forecast_diag_plot2 <- renderPlot({
    
    if (doAuth) req(credentials()$user_auth)
    
    diagLocations <- input$forecast_diag_location
    
    plot(m, forecast) + theme_bw(base_size = 16) + labs(y = "Transactions", x = "Date")
    
  })
  
  output$forecast_diag_plot3 <- renderPlot({
    
    if (doAuth) req(credentials()$user_auth)
    
    diagLocations <- input$forecast_diag_location
    
    prophet_plot_components(m, forecast)
    
  })
  
  map <- ggplotly(JL_LocationMap, tooltip = c("text"), source = "map") %>% # width = cdata$output_pid_width, height = cdata$output_pid_height, 
    layout(xaxis = list(scaleanchor = "y", 
                        scaleratio = .815, 
                        showline = FALSE),
           yaxis = list(showline = FALSE)) %>%
    #margin = list(t = 0, b = 0, l = 0, r = 0)) %>%
    config(displayModeBar = TRUE) %>%
    style(hoverinfo = "none", traces = 2) %>%
    style(hoverlabel = list(bgcolor = toRGB("aliceblue"), font = list(color = toRGB("black"))))
  
  output$map <- renderPlotly({
    
    if (doAuth) req(credentials()$user_auth)
    
    map %>% event_register("plotly_click")
    
  })
  
  output$weather <- renderTable({
    
    if (doAuth) req(credentials()$user_auth)
    
    JL_WeatherTable %>% filter(time == min(time)) %>% select(-icon, -zip, -time)
    
  })
  
  output$forecast <- renderUI({
    
    if (doAuth) req(credentials()$user_auth)
    
    JL_ForecastTable(JL_WeatherTable, location = JL_WeatherTable$Location %>% sample(1))
    
    })
  
  output$forecast_diag_logos <- renderUI ({
    
    if (doAuth) req(credentials()$user_auth)
    
    div(a(img(src = "logos/poweredby-oneline.png", height = 50),
          href = "https://darksky.net/poweredby/"), 
        img(src = "logos/square.png", height = 50),
        class = "darksky-oneline-logo")
    
  })
  
  output$map_logos <- renderUI ({
    
    if (doAuth) req(credentials()$user_auth)
    
    div(a(img(src = "logos/poweredby-oneline.png", height = 50),
          href = "https://darksky.net/poweredby/"),
        img(src = "logos/square.png", height = 50),
        class = "darksky-oneline-logo")
    
  })
  
  obsmap <- observe({
    
    map_click <- event_data("plotly_click", source = "map")
    
    if (!is.null(map_click$pointNumber)) {
      
      #print(map_click)
      
      clickedPoint <- map_click$pointNumber + 1
      
      mapMarkerStyle <- map$x$data[[3]]$marker
      mapMarkerStyle$color <- rep(toRGB("turquoise"), length(map$x$data[[3]]$x))
      mapMarkerStyle$color[clickedPoint] <- toRGB("yellow")
      
      # note: mobile touch is a bit buggy (e.g., hoverlabel colors do not update correctly [fixing color above as a workaround]; see https://github.com/plotly/plotly.js/issues/1858)
      #mapHoverlabelStyle <- map$x$data[[3]]$hoverlabel$bgcolor
      
      plotlyProxy("map", session) %>%
        plotlyProxyInvoke("restyle", list(marker = mapMarkerStyle))
      
      locationWeather <- JL_WeatherTable %>% filter(Location == JL_Locations %>% slice(clickedPoint) %>% pull(name))
      location <- locationWeather %>% pull(Location) %>% unique()
      
      output$forecast <- renderUI({
      
        JL_ForecastTable(JL_WeatherTable, location, visibility = "visibile")
      
      })
      
    }

  }, suspended = TRUE) # to avoid warnings before the map is loaded
  
  obstabs <- observe({
    
    #print(input$tabs)
    
    if (input$tabs == "map")
      obsmap$resume()
    
  })
  
}

shinyApp(ui, server)
