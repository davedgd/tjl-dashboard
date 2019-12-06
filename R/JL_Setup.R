# ------------
# Installation
# ------------

doInstall <- FALSE

if (doInstall) {
  system("pip install squareconnect -y")
  devtools::install_github("paulc91/shinyauthr")
}

# ---------
# Libraries
# ---------

library(rstudioapi, quietly = TRUE)
library(devtools, quietly = TRUE)
library(reticulate, quietly = TRUE)

library(prophet, quietly = TRUE)

#library(jsonlite, quietly = TRUE)

library(qdapRegex, quietly = TRUE)

library(ggmap, quietly = TRUE)
library(maps, quietly = TRUE)
library(zipcode, quietly = TRUE)

library(dplyr, quietly = TRUE)
library(tidyr, quietly = TRUE)
library(magrittr, quietly = TRUE)
library(purrr, quietly = TRUE)
library(lubridate, quietly = TRUE) # need 1.7.4.9000 for format_ISO8601 (using rfc_3339 from anytime instead)
library(anytime, quietly = TRUE)
library(stringr, quietly = TRUE)
library(tools, quietly = TRUE)

library(ggplot2, quietly = TRUE)
library(ggthemes, quietly = TRUE)
library(shades, quietly = TRUE)
library(scales, quietly = TRUE)

library(plotly, quietly = TRUE)
library(htmltools, quietly = TRUE)
library(htmlwidgets, quietly = TRUE)

library(darksky, quietly = TRUE)

library(shiny, quietly = TRUE)
library(shinydashboard, quietly = TRUE)
library(shinyjs, quietly = TRUE)
library(shinyauthr, quietly = TRUE)

# --------
# API Keys
# --------

# Google Maps:
register_google(key = "...")

# Square:
square_access_token <- "..."

# Darksky:
Sys.setenv(DARKSKY_API_KEY = "...")
darksky_api_key(force = FALSE)

# ------------------
# Load Built-In Data
# ------------------

data(zipcode)

# --------------------------------
# Python Configuration and Imports
# --------------------------------

# Set Python location relative to OS (code runs across Linux, MacOS, and Windows):

if (Sys.info()["sysname"] == "Linux")
  use_python("~/.pyenv/shims/python3", required = TRUE) else 
if (Sys.info()["sysname"] == "Darwin")
  use_python("/usr/local/bin/python3", required = TRUE)

py_discover_config()

squareconnect <- import("squareconnect")
