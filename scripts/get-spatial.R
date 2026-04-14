# Get spatial and population data

library(kcData)

options(tigris_use_cache = TRUE)

# Spatial datasets

kc_zctas_2024 <- get_kc_sf(
  geo = "zcta",
  year = 2024,
  intersect = "city",
  geometry = "clipped"
)

kc_city_2024 <- get_kc_sf(
  geo = "place",
  year = 2024
)

kc_tracts_2023 <- get_kc_sf(
  geo = "tract",
  year = 2023,
  intersect = "city",
  geometry = "clipped"
)

# Population datasets

acs1_city_2024 <- get_kc_pop(
  dataset = "acs1",
  geo = "place",
  year = 2024,
  vars = "^B01",
  var_match = "regex",
  geoids = geoid$place,
  key = keyring::key_get("census-api-key")
)

kc_zctas_2023 <- get_kc_sf(
  geo = "zcta",
  year = 2023,
  intersect = "city",
  geometry = "full"
)

acs5_zcta_2023 <- get_kc_pop(
  dataset = "acs5",
  geo = "zcta",
  year = 2023,
  vars = "B01001_001|^B01003",
  var_match = "regex",
  geoids = kc_zctas_2023$GEOID20,
  key = keyring::key_get("census-api-key")
)

acs5_zcta_2024 <- get_kc_pop(
  dataset = "acs5",
  geo = "zcta",
  year = 2024,
  vars = "^B01003",
  var_match = "regex",
  geoids = kc_zctas_2024$GEOID20,
  key = keyring::key_get("census-api-key")
)

kc_tracts_2023 <- get_kc_sf(
  geo = "tract",
  year = 2023,
  intersect = "city",
  geometry = "full"
)

acs5_tract_2023 <- get_kc_pop(
  dataset = "acs5",
  geo = "tract",
  year = 2023,
  vars = "^B01003",
  var_match = "regex",
  geoids = kc_tracts_2023$GEOID,
  key = keyring::key_get("census-api-key")
)

# Save
saveRDS(kc_zctas_2024, "data/1-source/kc_zctas_2024.rds")
saveRDS(kc_city_2024, "data/1-source/kc_city_2024.rds")
saveRDS(kc_tracts_2023, "data/1-source/kc_tracts_2023.rds")
saveRDS(acs1_city_2024, "data/1-source/acs1_city_2024.rds")
saveRDS(acs5_zcta_2023, "data/1-source/acs5_zcta_2023.rds")
saveRDS(acs5_zcta_2024, "data/1-source/acs5_zcta_2024.rds")
saveRDS(acs5_tract_2023, "data/1-source/acs5_tract_2023.rds")

