# Example leaflet

require(leaflet)
require(viridis)
source("add_gps.R")

# Take Ecomm file and zip code file to grab lat/longs. Set whse as factor for colors in leaflet
ecomm = add_gps("E Generated.csv", "us_zip_code.csv")
ecomm[, whse := factor(whse)]

# Create color palette based on whse
factpal = colorFactor(palette = "viridis", domain = ecomm$whse)

# Create leaflet, color by warehouse, and set tiles to CartoDB Dark Matter
leaflet(data = ecomm) %>% 
  addTiles() %>% 
  addCircles(~lng, ~lat, color = ~factpal(whse)) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter)
