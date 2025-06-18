# Geocode CCBHC locations in Kansas City

library(tidygeocoder)
library(tidyverse)

ccbhc_sites <- read.csv("data/1-source/ccbhc-locations.csv")

ccbhc_sites_geo <- geocode(
  ccbhc_sites,
  street = street,
  city = city,
  state = state,
  postalcode = zip,
  method = "census"
)

ccbhc_sites_geo <- sf::st_as_sf(
  ccbhc_sites_geo,
  coords = c("long", "lat"),
  crs = 4269
)

ggplot(kcData::sf_city_2024) +
  geom_sf(linewidth = 1) +
  geom_sf(data = ccbhc_sites_geo)

saveRDS(ccbhc_sites_geo, "data/2-final/ccbhc_sites.rds")
