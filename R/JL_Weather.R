# ----------------
# Helper Functions
# ----------------

coordFromZip <- function (x = 22902) {
  data(zipcode)
  return(zipcode %>% filter(zip == x) %>% select(zip, latitude, longitude))
}

# -----------------
# Weather Functions
# -----------------

FetchWeather <- function (zip = NULL, latitude = NULL, longitude = NULL, beginTime, endTime, save = TRUE, trySaved = TRUE) {
  
  if ((is.null(zip) & is.null(latitude)) | (!is.null(zip) & !is.null(latitude)))
    stop("You must specify either a zip or a latitude/longitude combination, but not both!")
  else if ((is.null(zip) & is.null(longitude)) | (is.null(zip) & is.null(longitude)))
    stop("You have not specified a zip nor a complete latitude/longitude combination!")
  
  if (!is.null(zip)) {
    latitude <- coordFromZip(zip)$latitude
    longitude <- coordFromZip(zip)$longitude
  }
  
  beginToEnd <- gsub(":", ".", paste(beginTime, endTime, sep = "_"))
  rawSave <- paste0("cache/rawWeather_", zip, "_", beginToEnd, ".RData")
  
  if (trySaved & file.exists(rawSave) & !is.null(zip)) {
    load(rawSave)
    
    message(paste("Loaded weather for", length(res), "days..."))
    
    return(res)
  } else {
    period <- seq(ymd(beginTime, tz = "EST5EDT", quiet = TRUE), ymd(endTime, tz = "EST5EDT", quiet = TRUE), "1 day") %>% 
      with_tz(tzone = "GMT") %>%
      date() # note: inclusion of timezone will result in date duplication issues due to UTC/GMT and EST5EDT conversions
    
    message(paste("Fetching weather for", length(period), "days..."))
    
    res <- period %>% 
      map(~get_forecast_for(latitude, longitude, .x, add_json = TRUE))

    if (!is.null(zip))
      res <- map(res, ~ list(weather = ., zip = zip, lat = NA, lon = NA)) else
      res <- map(res, ~ list(weather = ., 
                             zip = revgeocode(location = c(longitude, latitude)) %>% rm_zip(extract = TRUE) %>% extract2(1), 
                             lat = latitude, 
                             lon = longitude))
    
    if (save & !is.null(zip))
      save(res, file = rawSave)
    
    message(paste("Fetched weather for", length(res), "days..."))
    
    return(res)
  }
  
}

# Convert raw weather to a data.frame
Weather_DF <- function (rawWeather, type = "hourly") {
  
  res <- rawWeather %>%
    map_depth(1, ~ extract(., "weather")) %>% 
    map_depth(2, ~ extract2(., type)) %>% 
    map2(rawWeather %>% map_chr("zip"), ~ mutate(.x$weather, zip = .y)) %>%
    map2(rawWeather %>% map_chr("lat"), ~ mutate(.x, lat = .y)) %>%
    map2(rawWeather %>% map_chr("lon"), ~ mutate(.x, lon = .y)) %>%
    bind_rows()
  
  return(res)
    
  }

# Create weather forecast table
JL_ForecastTable <- function (JL_WeatherTable, location = "Cville", visibility = "hidden") {
  
  day3_name <- JL_WeatherTable$time %>% max() %>% wday(label = TRUE) %>% as.character()
  
  day1 <- JL_WeatherTable %>% 
    filter(Location == location) %>% 
    filter(time == min(ymd(time)))
  
  day2 <- JL_WeatherTable %>% 
    filter(Location == location) %>% 
    filter(time == min(ymd(time) + days(1)))
  
  day3 <- JL_WeatherTable %>% 
    filter(Location == location) %>% 
    filter(time == min(ymd(time) + days(2)))
  
  icon1 <- day1 %>% 
    pull(icon)
  
  icon2 <- day2 %>% 
    pull(icon)
  
  icon3 <- day3 %>% 
    pull(icon)
  
  HL1 <- day1 %>%
    mutate(HighLow = paste(High, Low, sep = "/")) %>%
    pull(HighLow)
  
  HL2 <- day2 %>% 
    mutate(HighLow = paste(High, Low, sep = "/")) %>%
    pull(HighLow)
  
  HL3 <- day3 %>% 
    mutate(HighLow = paste(High, Low, sep = "/")) %>%
    pull(HighLow)
  
  HTML(paste0('
    
        <p>Click a location on the map to get a 3-day forecast (drag a box around an area of the map to zoom in)</p>
        
        <div style = "visibility: ', visibility, '">
        
          <p><strong>Selected Location: </strong>', location, '</p>
          
          <table id = "weather-forecast">
            <thead>
              <td>Today</td>
              <td>Tomorrow</td>
              <td>', day3_name,'</td>
            </tr>
            <tr>
              <td><canvas id="icon1" width="40" height="40" class = "skycon"></canvas></td>
              <td><canvas id="icon2" width="40" height="40" class = "skycon"></canvas></td>
              <td><canvas id="icon3" width="40" height="40" class = "skycon"></canvas></td>
            </tr>
            <tr>
              <td>', HL1,'</td>
              <td>', HL2,'</td>
              <td>', HL3,'</td>
            </tr>
          </table>
          
        </div>
    
        <script>
          var skycons = new Skycons({"color": "#3c8dbc"});
          skycons.add("icon1", Skycons.', icon1 %>% toupper() %>% str_replace_all("-", "_"),');
          skycons.add("icon2", Skycons.', icon2 %>% toupper() %>% str_replace_all("-", "_"),');
          skycons.add("icon3", Skycons.', icon3 %>% toupper() %>% str_replace_all("-", "_"),');
          skycons.play();
        </script>
        
      '))
  
}
