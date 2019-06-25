# -----------------------
# Visualization Functions
# -----------------------

# Transactions by Date Range
Plot_Transactions <- function (Transactions_V1_DF = JL_Transactions_V1, locations = NULL, start = NULL, end = NULL, pointsize = 4, linesize = 1.2, linetype = 1, linealpha = .25, base_size = 16) {
  
  locations <- if (is.null(locations)) unique(Transactions_V1_DF$merchant_id) else locations
  end <- if (is.null(end)) max(Transactions_V1_DF$datetime) else end
  start <- if (is.null(start)) (max(Transactions_V1_DF$datetime) - days(6)) else start
    
  plotDat <- Transactions_V1_DF %>% 
    filter(datetime >= floor_date(start, unit = "day"), datetime <= ceiling_date(end, unit = "day"), merchant_id %in% locations) %>%
    mutate(week = week(datetime), 
           week_floor = floor_date(datetime, unit = "week"),
           day_floor = floor_date(datetime, unit = "day")) %>% 
    mutate(weekday = wday(day_floor, label = TRUE) %>% factor(ordered = FALSE)) %>% 
    group_by(day_floor, weekday) %>%
    summarise(day_count = n()) %>%
    rename(Date = day_floor, Transactions = day_count, DayOfWeek = weekday)
  
  suppressWarnings(
    
    ggplot(plotDat, aes(x = Date, y = Transactions)) + 
      geom_line(linetype = linetype, size = linesize, alpha = linealpha) +
      geom_point(
        size = pointsize, 
        aes(color = DayOfWeek, 
            text = paste("Day: ", DayOfWeek, "<br>",
                         "Date: ", Date, "<br>",
                         "Transactions: ", Transactions, sep = ""))) + 
      scale_x_datetime(date_labels = "%b %d", timezone = "EST5EDT") +
      expand_limits(y = c(0, max(plotDat$Transactions) * 1.1)) +
      labs(x = "Date",
           y = "Transactions",
           color = "Day of Week") +
      theme_bw(base_size = base_size) + 
      theme(legend.position = "top",
            legend.title = element_blank()) + 
      guides(color = guide_legend(nrow = 1))
    
  )

  # plot_ly(plotDat %>% data.frame()) %>%
  #   add_lines(x = ~Date, y = ~Transactions, line = list(color = "lightgrey", width = 3, dash = "dash"), name = "Trend") %>%
  #   add_markers(x = ~Date, y = ~Transactions, color = ~DayOfWeek, marker = list(alpha = 1, size = 15, line = list(color = 'black', width = 2)))
    
}

# Transactions by Weekday
Plot_Transactions_ByWeekday <- function (Transactions_V1_DF = JL_Transactions_V1, locations = NULL, start = NULL, end = NULL, pointsize = 4, linesize = 1.2, linetype = 1, linealpha = .25, base_size = 16) {
  
  locations <- if (is.null(locations)) unique(Transactions_V1_DF$merchant_id) else locations
  end <- if (is.null(end)) max(Transactions_V1_DF$datetime) else end
  start <- if (is.null(start)) (max(Transactions_V1_DF$datetime) - days(6)) else start
  
  plotDat <- Transactions_V1_DF %>% 
    filter(datetime >= floor_date(start, unit = "day"), datetime <= ceiling_date(end, unit = "day"), merchant_id %in% locations) %>%
    mutate(week = week(datetime), 
           week_floor = floor_date(datetime, unit = "week"),
           day_floor = floor_date(datetime, unit = "day")) %>% 
    group_by(day_floor) %>%
    summarise(day_count = n()) %>%
    mutate(weekday = wday(day_floor, label = TRUE) %>% factor(ordered = FALSE)) %>%
    group_by(weekday) %>%
    summarise(weekday_count = mean(day_count)) %>%
    rename(DayOfWeek = weekday, Transactions = weekday_count)
  
  suppressWarnings(
  
    ggplot(plotDat, aes(x = DayOfWeek, y = Transactions, group = 1)) + 
      geom_line(linetype = linetype, size = linesize, alpha = linealpha) +
      geom_point(
        size = pointsize, 
        aes(color = DayOfWeek, 
            text = paste("Day: ", DayOfWeek, "<br>",
                         "Transactions: ", Transactions %>% round(2), sep = ""))) + 
      expand_limits(y = c(0, max(plotDat$Transactions) * 1.1)) +
      labs(x = "Day of Week",
           y = "Transactions",
           color = "Day of Week") +
      guides(color = FALSE) +
      theme_bw(base_size = base_size) + 
      theme(legend.position = "top",
            legend.title = element_blank()) + 
      guides(color = guide_legend(nrow = 1))
    
  )
  
}

# Sales by Date Range: undiscounted or collected
Plot_Sales <- function (type = "undiscounted", Transactions_V1_DF = JL_Transactions_V1, locations = NULL, start = NULL, end = NULL, pointsize = 4, linesize = 1.2, linetype = 1, linealpha = .25, base_size = 16) {
  
  # note: see !!sym(var) below
  if (type == "undiscounted")
    var <- "raw_total"
  else if (type == "collected")
    var <- "total_collected_money"
  
  locations <- if (is.null(locations)) unique(Transactions_V1_DF$merchant_id) else locations
  end <- if (is.null(end)) max(Transactions_V1_DF$datetime) else end
  start <- if (is.null(start)) (max(Transactions_V1_DF$datetime) - days(6)) else start

  plotDat <- Transactions_V1_DF %>% 
    filter(datetime >= floor_date(start, unit = "day"), datetime <= ceiling_date(end, unit = "day"), merchant_id %in% locations) %>%
    mutate(week = week(datetime), 
           week_floor = floor_date(datetime, unit = "week"),
           day_floor = floor_date(datetime, unit = "day")) %>% 
    group_by(day_floor) %>%
    summarise(day_count = sum(!!sym(var), na.rm = TRUE)/100) %>%
    mutate(weekday = wday(day_floor, label = TRUE) %>% factor(ordered = FALSE)) %>%
    rename(Date = day_floor, Sales = day_count, DayOfWeek = weekday)
  
  suppressWarnings(
  
    ggplot(plotDat, aes(x = Date, y = Sales)) + 
      geom_line(linetype = linetype, size = linesize, alpha = linealpha) +
      geom_point(
        size = pointsize, 
        aes(color = DayOfWeek, 
            text = paste("Day: ", DayOfWeek, "<br>",
                         "Date: ", Date, "<br>",
                         "Sales: ", Sales, sep = ""))) + 
      scale_x_datetime(date_labels = "%b %d", timezone = "EST5EDT") +
      expand_limits(y = c(0, max(plotDat$Sales) * 1.1)) +
      labs(x = "Date",
           y = "Sales (Dollars)",
           color = "Day of Week") +
      theme_bw(base_size = base_size) + 
      theme(legend.position = "top",
            legend.title = element_blank()) + 
      guides(color = guide_legend(nrow = 1))
  )
  
}

# Sales by Weekday: undiscounted or collected
Plot_Sales_ByWeekday <- function (type = "undiscounted", Transactions_V1_DF = JL_Transactions_V1, locations = NULL, start = NULL, end = NULL, pointsize = 4, linesize = 1.2, linetype = 1, linealpha = .25, base_size = 16) {
  
  # note: see !!sym(var) below
  if (type == "undiscounted")
    var <- "raw_total"
  else if (type == "collected")
    var <- "total_collected_money"
  
  locations <- if (is.null(locations)) unique(Transactions_V1_DF$merchant_id) else locations
  end <- if (is.null(end)) max(Transactions_V1_DF$datetime) else end
  start <- if (is.null(start)) (max(Transactions_V1_DF$datetime) - days(6)) else start
  
  plotDat <- Transactions_V1_DF %>% 
    filter(datetime >= floor_date(start, unit = "day"), datetime <= ceiling_date(end, unit = "day"), merchant_id %in% locations) %>%
    mutate(week = week(datetime), 
           week_floor = floor_date(datetime, unit = "week"),
           day_floor = floor_date(datetime, unit = "day")) %>% 
    group_by(day_floor) %>%
    summarise(day_count = sum(!!sym(var), na.rm = TRUE)/100) %>%
    mutate(weekday = wday(day_floor, label = TRUE) %>% factor(ordered = FALSE)) %>%
    group_by(weekday) %>%
    summarise(weekday_count = mean(day_count)) %>%
    rename(DayOfWeek = weekday, Sales = weekday_count)
  
  suppressWarnings(
    
    ggplot(plotDat, aes(x = DayOfWeek, y = Sales, group = 1)) + 
      geom_line(linetype = linetype, size = linesize, alpha = linealpha) +
      geom_point(
        size = pointsize, 
        aes(color = DayOfWeek, 
            text = paste("Day: ", DayOfWeek, "<br>",
                         "Sales: ", Sales %>% round(2), sep = ""))) + 
      expand_limits(y = c(0, max(plotDat$Sales) * 1.1)) +
      labs(x = "Day of Week",
           y = "Sales (Dollars)",
           color = "Day of Week") +
      guides(color = FALSE) +
      theme_bw(base_size = base_size) +
      theme(legend.position = "top",
            legend.title = element_blank()) + 
      guides(color = guide_legend(nrow = 1))
    
  )
  
}

# Item Comparison
Plot_Items <- function (type = "count", category = "SMOOTHIE BOWLS", JL_Items_DF = JL_Items, locations = NULL, start = NULL, end = NULL, base_size = 14, dropMissing = TRUE) {
  
  locations <- if (is.null(locations)) unique(JL_Items_DF$merchant_id) else locations
  end <- if (is.null(end)) max(JL_Items_DF$datetime) else end
  start <- if (is.null(start)) (max(JL_Items_DF$datetime) - days(6)) else start
  
  allItems <- JL_Items_DF %>% 
    filter(item_detail_category_name_multival == category) %>% 
    pull(item_name_multival) %>% unique()
  
  # NOTE: items can be multiple in qty...
  # JL_Items_DF %>% filter(item_detail_category_name_multival == category) %>% slice(5) %>% data.frame()
  
  plotDat <- JL_Items_DF %>% 
    filter(item_detail_category_name_multival == category,
           datetime >= floor_date(start, unit = "day"), 
           datetime <= ceiling_date(end, unit = "day"),
           merchant_id %in% locations) %>% 
    complete(item_name_multival = allItems, fill = list(y = 0)) %>%
    group_by(item_name_multival) %>%
    summarise(
      TotalPrice = sum(item_qty_multival %>% as.numeric() * item_single_qty_money_multival %>% as.numeric()) / 100, 
      TotalCount = sum(item_qty_multival %>% as.numeric())) %>% 
    mutate(item_name = item_name_multival %>% str_replace_all("BOWL|SMOOTHIE", "") %>% str_trim())
  
  plotDat <- if (type == "price") plotDat %>% rename(Outcome = TotalPrice) else plotDat %>% rename(Outcome = TotalCount)
  outcomeTitle <- if (type == "price") "Sales (Full Price in Dollars)" else "Count"
  
  if (dropMissing)
    plotDat <- plotDat %>% na.omit()
  
  if (plotDat %>% nrow() == 0)
    stop("No data to plot in select time period...")
  
  #JL_Items_DF %>% filter(item_detail_category_name_multival == "SMOOTHIE BOWLS") %>% pull(item_qty_multival) %>% table(useNA = "always")
  
  suppressWarnings(
    
    ggplot(plotDat) +
      geom_bar(stat = "identity", aes(x = item_name, 
                                      y = Outcome, 
                                      fill = item_name,
                                      text = paste(outcomeTitle, ": ", Outcome, "<br>",
                                                   "Name: ", item_name, sep = ""))) + 
      theme_base(base_size = base_size) +
      theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
      guides(fill = FALSE) + 
      labs(x = category %>% str_to_title(),
           y = outcomeTitle)
    
  )
  
}

# forecast visual
Plot_Forecast <- function (Transactions_Plot = Plot_Transactions(), locations = NULL, start = NULL, end = NULL, pointsize = 4, linesize = 1.2, linetype = 1, linealpha = .25, base_size = 16, horizon = 7) {
  
  plotDat <- forecast %>%
    filter(!(ds %in% m$history.dates)) %>%
    mutate(ds = force_tz(ds, tzone = "EST5EDT")) %>%
    mutate(week = week(ds), 
           week_floor = floor_date(ds, unit = "week"),
           day_floor = floor_date(ds, unit = "day")) %>% 
    mutate(weekday = wday(day_floor, label = TRUE) %>% factor(ordered = FALSE)) %>%
    rename(DayOfWeek = weekday, Date = day_floor, Transactions = yhat, Upper = yhat_upper, Lower = yhat_lower) %>%
    slice(1:horizon)
  
  # fix colors
  weekdayColors <- tibble(
    weekday = wday(Sys.Date(), label = TRUE) %>% levels() %>% factor(levels = wday(Sys.Date(), label = TRUE) %>% levels()),
    color = hue_pal()(7)
  )
  theColors <- plotDat %>% select(DayOfWeek) %>% left_join(weekdayColors, by = c("DayOfWeek" = "weekday"))
  plotDat$DayOfWeek <- plotDat$DayOfWeek %>% factor(levels = plotDat$DayOfWeek)
  
  suppressMessages(suppressWarnings(
  
    newPlot <- Transactions_Plot +
      geom_errorbar(plotDat, mapping = aes(ymax = Upper, ymin = Lower), width = 0, alpha = .25) + 
      geom_line(Transactions_Plot$data %>% tail(1) %>% bind_rows(plotDat), mapping = aes(x = Date, y = Transactions), linetype = 1, size = linesize, alpha = .1) + 
      geom_point(
        plotDat, 
        show.legend = FALSE, 
        size = pointsize, 
        shape = 23,
        alpha = 1,
        color = "grey55",
        mapping = aes(x = Date, 
                      y = Transactions,
                      fill = DayOfWeek %>% factor(),
                      text = paste("Day: ", DayOfWeek, "<br>",
                                   "Date: ", Date, "<br>",
                                   "Transactions: ", Transactions %>% round(), "<br>",
                                   "Upper Est.: ", Upper %>% round(), "<br>",
                                   "Lower Est.: ", Lower %>% round(), sep = ""))) + 
      expand_limits(y = c(0, max(plotDat$Transactions) * 1.1)) +
      labs(x = "Date",
           y = "Transactions",
           color = "Day of Week") +
      theme_bw(base_size = base_size) + 
      theme(legend.position = "top",
            legend.title = element_blank()) + 
      guides(color = guide_legend(nrow = 1)) + 
      scale_fill_manual(values = brightness(theColors %>% pull(color), 1.2)) + 
      scale_x_datetime(date_labels = "%b %d", timezone = "EST5EDT", date_breaks = "3 days")
    
  ))
  
  finalPlot <- newPlot
  
  # swap layer orders to avoid line overlapping point
  finalPlot$layers[[2]] <- newPlot$layers[[5]]
  finalPlot$layers[[5]] <- newPlot$layers[[2]]
  
  finalPlot

}

# forecast predictions
Plot_Forecast_Pred <- function (df.cv, metric = "mape", base_size = 14, pointsize = 3, linesize = 1.2, locations) {
  
  metrics <- tibble(
    Metric = c("mse", "rmse", "mae", "mape", "coverage"),
    Description = c("Mean Square Error", "Root Mean Square Error", "Mean Absolute Error", "Mean Absolute Percentage Error", "Coverage")
  )
  
  #df.p <- performance_metrics(df.cv)
  df.p.full <- performance_metrics(df.cv, rolling_window = 1)
  
  plotDat <- df.cv %>% 
    select(ds, y, yhat) %>% 
    mutate(ds = with_tz(ds, tzone = "EST5EDT")) %>%
    gather(Type, Transactions, c(y, yhat)) %>% 
    mutate(Type = recode(Type, "y" = "Actual", "yhat" = "Predicted"),
           day_floor = floor_date(ds, unit = "day")) %>%
    mutate(DayOfWeek = wday(day_floor, label = TRUE) %>% factor(ordered = FALSE)) %>%
    rename(Date = day_floor)
  
  suppressWarnings(
    
    p <- ggplot(plotDat,
                aes(x = ds, y = Transactions, color = Type, group = Type)) + 
      geom_point(size = pointsize,
                 mapping = aes(text = paste("Type: ", Type, "<br>",
                                            "Day: ", DayOfWeek, "<br>",
                                            "Date: ", Date, "<br>",
                                            "Transactions: ", Transactions %>% round(), sep = ""))) + 
      geom_line(size = linesize) + 
      theme_bw(base_size = base_size) +
      theme(legend.position = "bottom") + #,
           #legend.title = element_blank()) + 
      labs(caption = paste0(metrics$Description[which(metrics$Metric == metric)], ": ", df.p.full %>% pull(metric) %>% round(2)),
           x = "Date",
           title = locations) +
      guides(color = guide_legend(
        keywidth = 1.5,
        default.unit = "cm")
      ) + 
      scale_x_datetime(date_labels = "%b %d", timezone = "EST5EDT")
    
  )
  
  return(p)
  
  #p <- ggplotly(p, tooltip = "text") %>% layout(legend = list(y = 1.13, orientation = "h"), showlegend = TRUE, margin = list(t = 80))
  
}