# -----------------
# Collect Locations
# -----------------

rawLocations <- FetchLocations(save = TRUE, trySaved = TRUE)
JL_Locations <- LocationsDF(rawLocations)

# -------------
# Set Variables
# -------------

theLocations <- JL_Locations %>% pull(id)

#theLocation <- JL_Locations %>% filter(name == "Preston Ave. (C'ville)") %>% pull(id)
theZip <- JL_Locations %>% filter(name == "Preston Ave. (C'ville)") %>% pull(zip)

endTime <- as_datetime("2019-07-25 00:00:00", tz = "EST5EDT") # Sys.time()
#beginTime <- as_datetime("2019-05-03 00:00:00", tz = "EST5EDT")
updateTime <- as_datetime("2019-07-25 00:00:00", tz = "EST5EDT") # Sys.time()

numYears <- 2

save <- TRUE
trySaved <- TRUE

if (!exists("doFull"))
  doFull <- TRUE

if (!exists("doUpdate"))
  doFull <- FALSE

# -------------------------
# Collect Transactions (V2)
# -------------------------

# rawTransactionsV2 <- FetchTransactionsV2(
#   theLocation = theLocation, 
#   beginTime = beginTime,
#   endTime = endTime,
#   save = save,
#   trySaved = trySaved)

if (doFull)
  rawTransactionsV2 <- theLocations %>%
  map(~ FetchYearlyTransactions(
    fetchFunction = FetchTransactionsV2,
    theLocation = .,
    endTime = endTime,
    numYears = numYears, 
    save = save,
    trySaved = trySaved)) %>%
  flatten() else
    rawTransactionsV2 <- NULL

JL_Transactions_V2 <- TransactionsV2_DF(
  rawTransactionsV2,
  save = save,
  trySaved = trySaved,
  saveFile = "cache/JL_Transactions_V2.RData")

if (doUpdate)
  JL_Transactions_V2 <- Update_Transactions(
    JL_Transactions_V2, 
    fetchFunction = FetchTransactionsV2,
    processFunction = TransactionsV2_DF,
    endTime = updateTime,
    save = save,
    trySaved = trySaved,
    saveFile = "cache/JL_Transactions_V2.RData")

# -------------------------
# Collect Transactions (V1)
# -------------------------

# rawTransactionsV1 <- FetchTransactionsV1(
#  theLocation = theLocation,
#  beginTime = beginTime,
#  endTime = endTime,
#  save = save,
#  trySaved = trySaved)

if (doFull)
  rawTransactionsV1 <- theLocations %>%
  map(~ FetchYearlyTransactions(
    fetchFunction = FetchTransactionsV1,
    theLocation = .,
    endTime = endTime,
    numYears = numYears, 
    save = save,
    trySaved = trySaved)) %>%
  flatten() else
    rawTransactionsV1 <- NULL

JL_Transactions_V1 <- TransactionsV1_DF(
  rawTransactionsV1,
  save = save,
  trySaved = trySaved,
  saveFile = "cache/JL_Transactions_V1.RData")

if (doUpdate)
  JL_Transactions_V1 <- Update_Transactions(
    JL_Transactions_V1, 
    fetchFunction = FetchTransactionsV1,
    processFunction = TransactionsV1_DF,
    endTime = updateTime,
    save = save,
    trySaved = trySaved,
    saveFile = "cache/JL_Transactions_V1.RData")

# ---------------------------------------------------------------
# Convert Transactions (V1) to Items (Nested within Transactions)
# ---------------------------------------------------------------

JL_Items <- JL_Transactions_V1 %>% select(-starts_with("inclusive_tax")) %>% separate_rows(contains("multival"), sep = ";")

JL_Item_Categories <- JL_Items %>% 
  pull(item_detail_category_name_multival) %>% 
  unique()

# ---------------
# Collect Weather
# ---------------

# Historical

timeRange <- range(ymd_hms(JL_Transactions_V1$created_at, tz = "EST5EDT", quiet = TRUE))

rawWeather <- FetchWeather(
  zip = theZip,
  beginTime = floor_date(timeRange[1], unit = "day"),
  endTime = ceiling_date(timeRange[2], unit = "day")
)

JL_Weather_Hourly <- Weather_DF(rawWeather, type = "hourly")
JL_Weather_Daily <- Weather_DF(rawWeather, type = "daily")

# Forecast

rawWeatherForecast <- FetchWeather(
  zip = theZip,
  beginTime = ceiling_date(timeRange[2], unit = "day") + days(1),
  endTime = ceiling_date(timeRange[2], unit = "day") + days(7)
)

JL_Weather_Hourly_Forecast <- Weather_DF(rawWeatherForecast, type = "hourly")
JL_Weather_Daily_Forecast <- Weather_DF(rawWeatherForecast, type = "daily")

rawWeatherCurrent <- 
  JL_Locations %>% 
  pull(zip) %>% 
  unique() %>% 
  map(~ FetchWeather(zip = ., beginTime = ceiling_date(Sys.time(), unit = "day"), endTime = ceiling_date(Sys.time(), unit = "day") + days(2))) %>%
  flatten()

JL_Current_Weather_Hourly <- Weather_DF(rawWeatherCurrent, type = "hourly")
JL_Current_Weather_Day <- Weather_DF(rawWeatherCurrent, type = "daily")

JL_WeatherTable <- 
  JL_Locations %>% 
  left_join(JL_Current_Weather_Day, by = "zip") %>% 
  select(name, city, zip, time, summary, icon, temperatureHigh, temperatureLow, precipProbability) %>%
  rename(High = temperatureHigh, Low = temperatureLow, Precip = precipProbability, Location = name, City = city, Summary = summary) %>%
  mutate(Precip = paste0(as.numeric(Precip) * 100, "%") %>% str_pad(3, "left"), 
         time = as_date(time) %>% as.character(),
         High = paste0(High %>% round(), "\u00B0"),
         Low = paste0(Low %>% round(), "\u00B0"))

# -------------------------------
# Establish Initial Forecast Data
# -------------------------------

initialLocation <- JL_Locations %>% 
  filter(name == "Preston Ave. (C'ville)") %>% 
  pull(id)

df <- Build_Forecast_DF(Weather_Daily = JL_Weather_Daily, locations = initialLocation)
m <- Build_Prophet_Model(df)
forecast <- Build_Forecast(m, Weather_Daily = JL_Weather_Daily, Weather_Daily_Forecast = JL_Weather_Daily_Forecast)
