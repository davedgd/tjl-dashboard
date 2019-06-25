# -----
# To-Do
# -----

# ADD VISUAL FOR TYPES OF TOPPINGS
# ADD DISTRIBUTION (HISTOGRAM) FOR ORDER SIZES ON A TIME PERIOD
# ADD A BUTTON TO REFRESH DATA (UPDATE: FUNCTION CREATED, BUTTON NOT ADDED)
# NEED TO WORK AROUND RATE_LIMIT_ERROR (https://docs.connect.squareup.com/api/oauth#navsection-enum)
# UPDATE RUNS TWICE IF DONE BACK-TO-BACK DUE TO TIME ROUNDING SAFETY WINDOW: COULD BE IMPROVED...
# ADD A FORECAST WINDOW OF UP TO TWO WEEKS
# 2.20190612.0 ADDED LAT/LONG TO LOCATION API (STRIP GMAPS FUNCTIONALITY?)
# AUTO HIDE/SHOW SIDEBAR RELATIVE TO LOGIN AND DISABLE SIDEBAR BUTTON AS NEEDED

# ---------------------
# Set Working Directory
# ---------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# -----------------
# Load Requirements
# -----------------

start <- proc.time() # start clock to measure load time

# To refresh all data, set to TRUE (and adjust endTime in JL_Data.R accordingly):
doFull <- FALSE

# To try to update, set to TRUE:
doUpdate <- TRUE

# To load/reload packages, set to TRUE:
doLoad <- TRUE

if (doLoad) {
  
  # Load Packages, Languages, and Setup APIs:
  source("R/JL_Setup.R")
  cat("\014") # clear console
  
  # Load Required Functions
  source("R/JL_Square.R")
  source("R/JL_Weather.R")
  source("R/JL_Visualizations.R")
  source("R/JL_Forecast.R")
  source("R/JL_Misc.R")
  
  # Load/Generate Data
  source("R/JL_Data.R")
  
  # Build Location Map
  source("R/JL_Map.R")
  
}

LoadTime(start) # load time

# -------
# Run App
# -------

options(shiny.host = "0.0.0.0") # to make available online (or on LAN)
options(shiny.port = 2552)

runApp("shiny") # run in RStudio
