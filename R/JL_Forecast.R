# ---------------------
# Forecasting Functions
# ---------------------

Summarize_Transactions_DF <- function (Transactions_V1_DF = JL_Transactions_V1, unit = "day") {
  
  res <- Transactions_V1_DF %>% 
    mutate(date = ymd_hms(created_at, tz = "EST5EDT", quiet = TRUE)) %>% 
    mutate(week = week(date), 
           week_floor = floor_date(date, unit = "week"),
           day_floor = floor_date(date, unit = "day"),
           hour_floor = floor_date(date, unit = "hours")) %>% {
    if (unit == "day") {
      group_by(., day_floor) %>%
        summarise(day_count = n()) %>%
        rename(ds = day_floor, y = day_count) %>%
        select(ds, y)
    } else if (unit == "hours") {
      group_by(., hour_floor) %>%
        summarise(hour_count = n()) %>%
        rename(ds = hour_floor, y = hour_count) %>%
        select(ds, y)
    }
  }
  
  return(res)
  
}

Build_Forecast_DF <- function(Transactions_V1_DF = JL_Transactions_V1, Weather_Daily = JL_Weather_Daily, locations) {
  
  locations <- if (is.null(locations)) unique(Transactions_V1_DF$merchant_id) else locations
  
  res <- Summarize_Transactions_DF(Transactions_V1_DF %>% filter(merchant_id %in% locations), unit = "day") %>% 
    mutate(ds = as_date(ds)) %>%
    complete(ds = seq(min(ds), max(ds), by = "days"), fill = list(y = 0)) %>%
    mutate(ds = force_tz(as_datetime(ds))) %>%
    left_join(Weather_Daily, by = c("ds" = "time")) %>% 
    replace_na(list(precipProbability = 0, 
                    temperatureHigh = 0,
                    apparentTemperatureHigh = 0,
                    cloudCover = 0,
                    humidity = 0,
                    temperatureLow = 0,
                    apparentTemperatureLow = 0)) %>%
    mutate(isRain = ifelse(precipType == "rain", 1, 0) %>% replace_na(0),
           isSnow = ifelse(precipType == "snow", 1, 0) %>% replace_na(0)) %>%
    slice(1:(n() - 0))
  
  res$locations <- paste(locations, sep = "; ")
  
  return(res)
  
}

Build_Prophet_Model <- function (df) {
  
  m <- prophet(yearly.seasonality = FALSE, weekly.seasonality = TRUE, daily.seasonality = FALSE)
  m <- add_seasonality(m, name = "yearly", fourier.order = 10, period = 365, mode = "additive") # just an example
  m <- add_country_holidays(m, country_name = "US")
  m <- add_regressor(m, "precipProbability")
  m <- add_regressor(m, "isRain")
  m <- add_regressor(m, "isSnow")
  m <- add_regressor(m, "cloudCover")
  m <- add_regressor(m, "apparentTemperatureHigh")
  m <- add_regressor(m, "temperatureHigh")
  m <- add_regressor(m, "humidity")
  m <- add_regressor(m, "apparentTemperatureLow")
  m <- add_regressor(m, "temperatureLow")
  m <- fit.prophet(m, df)
  
  m$locations <- df$locations
  
  return(m)
  
}

Build_Forecast <- function (m, Weather_Daily = JL_Weather_Daily, Weather_Daily_Forecast = JL_Weather_Daily_Forecast, periods = 7) {
  
  future <- make_future_dataframe(m, periods = periods) %>% 
    left_join(Weather_Daily %>% bind_rows(Weather_Daily_Forecast), by = c("ds" = "time")) %>% 
    replace_na(list(precipProbability = 0, 
                    temperatureHigh = 0,
                    apparentTemperatureHigh = 0,
                    cloudCover = 0,
                    humidity = 0,
                    temperatureLow = 0,
                    apparentTemperatureLow = 0)) %>%
    mutate(isRain = ifelse(precipType == "rain", 1, 0) %>% replace_na(0),
           isSnow = ifelse(precipType == "snow", 1, 0) %>% replace_na(0))
  
  tail(future)
  dim(future)
  
  forecast <- predict(m, future)
  
  forecast$locations <- unique(m$locations)
  
  return(forecast)
  
}

# -----------------
# Daily Forecasting
# -----------------

doForecast <- FALSE

if (doForecast) {

  df <- Build_Forecast_DF(Weather_Daily = JL_Weather_Daily, locations = JL_locations %>% filter(name == "The Corner (C'ville)") %>% pull(id))
  
  m <- Build_Prophet_Model(df)
  
  forecast <- Build_Forecast(m, Weather_Daily = JL_Weather_Daily, Weather_Daily_Forecast = JL_Weather_Daily_Forecast)
  
  horizonLength <- 3 * 7
  
  df.cv <- cross_validation(m, initial = nrow(df) - horizonLength - 1, period = nrow(df) - horizonLength, horizon = horizonLength, units = "days")
  df.cv
  
  df.p <- performance_metrics(df.cv, rolling_window = .1)
  df.p
  
  df.p.all <- performance_metrics(df.cv, rolling_window = 1)
  df.p.all
  
  mean(abs(df.cv$y - df.cv$yhat)/df.cv$y) # mape
  
}

createPlots <- FALSE

if (createPlots) {
  
  plot(m, forecast)
  prophet_plot_components(m, forecast)
  plot_cross_validation_metric(df.cv, metric = 'mape')
  
}

# ---------------------------------
# Benchmarking baseline:      .111
# ---------------------------------
# Sequential additions to baseline:
# ---------------------------------
# Add holidays:               .114
# Add precip prob:            .087
# Add precip type:            .089
# Add cloud cover:            .083
# Add app. temp high:         .067
# Add temp high:              .067
# Add humidity:               .065
# Add app. temp low:          .065
# Add temp low:               .065
# ---------------------------------
  
# need event calendar; explored weekend is foxfield
# thursdays used to have a discount at corner JL
# corner juice more popular than JL? opened around the same time in late october
# need semester term calendars to not mess up seasonal effects (really bad if looking around spring break for instance)

# NOTE! Make sure rawWeather is grabbing first day correctly!