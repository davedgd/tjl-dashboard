# ------------
# Location Map
# ------------

if (!exists("JL_LatLon"))
  JL_LatLon <- JL_Locations %>%
    mutate(full_address = paste(.$street_address, .$city, .$zip, sep = ", ")) %>%
    pull(full_address) %>%
    map(~ geocode(., output = "latlona", source = "google")) %>% bind_rows()


if (!exists("gg1"))
  gg1 <- get_googlemap("Culpeper, VA", zoom = 8, maptype = "roadmap", size = c(640, 640), scale = 2) %>% ggmap

JL_LocationMap <- 
  suppressWarnings(
    gg1 + 
    geom_point(data = JL_Locations %>% bind_cols(JL_LatLon), 
               aes(x = lon, y = lat, text = paste(name, street_address, city, zip, sep = "<br>")), 
               color = "black", 
               size = 4, 
               pch = 21, 
               fill = "turquoise") + 
    theme_void() # position = position_jitter(.2)
  )