
saveRDS(mhsurv2, "../data/2-final/mh_survey_results.rds")

mh0 <- readRDS("data/2-final/mh_survey_results.rds")





library(tidyverse)
library(kcData)

# ZCTAs starting with "640" (2024)

ggplot() +
  geom_sf(
    data = sf_city_2024,
    color = NA,
    fill = "red"
  ) +
  geom_sf(
    data = sf_zcta_2024 |>
      filter(grepl("^640", ZCTA5CE20)),
    color = NA,
    fill = "black",
    alpha = 1
  )

# With population labels (2023)

zcta23 <- sf_zcta_2023 |>
  left_join(
    acs5_zcta_2023 |>
      filter(variable == "B01001_001") |>
      select(GEOID, estimate),
    by = c("GEOID20" = "GEOID")
  ) |>
  filter(grepl("^640", ZCTA5CE20))

ggplot(sf_city_2023) +
  geom_sf(
    aes(fill = GEOID),
    show.legend = FALSE
  ) +
  geom_sf(
    data = zcta23,
    aes(fill = GEOID20)
  ) +
  geom_sf_text(
    data = zcta23,
    aes(label = estimate)
  )








# Summarize results

mhsurv <- readRDS("data/2-final/mh_survey_results.rds")

source("scripts/fn.R")

fvars <- list(
  all = NULL, sex = "birth_sex", age = "age",
  "race_ethnicity" = "race_ethn_bwo",
  "hispanic_origin" = "hispanic_origin",
  "hoi_region" = "hoi_region"
)

sum_all_factors <- function(df, var, factors) {
  lapply(factors, \(f) {
    summarize_results(df, var, f)
  })
}

vars <- colnames(mhsurv)
qvars <- vars[which(vars == "q4_1"):length(vars)]

mhsum <- lapply(qvars, \(x) {
  sum_all_factors(mhsurv, x, fvars)
})
names(mhsum) <- qvars












