library(httr2)

# Measure: "Sleeping less than 7 hours among adults aged >=18 years"

# Kansas City -------------------------------------------------------------

# PLACES, 2024 release, geo = place
endpoint <- "https://data.cdc.gov/resource/eav7-hnsx.csv"

params <- paste(
  "?stateabbr=MO",
  "locationid=2938000",
  "measureid=SLEEP",
  sep = "&"
)

url <- URLencode(paste0(endpoint, params))

req <- request(url) |>
  req_headers_redacted("X-App-Token" = keyring::key_get("cdc-app-token"))

req_dry_run(req)

resp <- req_perform(req, verbosity = 2) # perform request

resp_status(resp) # get HTTP status code

resp2 <- resp_body_string(resp) # convert body of response to text

slpkc <- read.csv(text = resp2) # convert text to dataframe


# United States -----------------------------------------------------------

# PLACES, 2024 release, geo = county
endpoint <- "https://data.cdc.gov/resource/swc5-untb.csv"

params <- paste(
  "?stateabbr=US",
  "measureid=SLEEP",
  sep = "&"
)

url <- URLencode(paste0(endpoint, params))

req <- request(url) |>
  req_headers_redacted("X-App-Token" = keyring::key_get("cdc-app-token"))

req_dry_run(req)

resp <- req_perform(req, verbosity = 2) # perform request

resp_status(resp) # get HTTP status code

resp2 <- resp_body_string(resp) # convert body of response to text

slpus <- read.csv(text = resp2) # convert text to dataframe


# MO ----------------------------------------------------------------------

# Data obtained from https://www.cdc.gov/sleep/data-research/facts-stats/adults-sleep-facts-and-stats.html

slpmo <- read.csv("data/1-source/AdultState.csv")

colnames(slpmo) <- setmeup::fix_colnames(colnames(slpmo))

slpmo <- slpmo |>
  filter(state == "Missouri")

slpdur <- list(
  kc = slpkc,
  mo = slpmo,
  us = slpus
)

saveRDS(slpdur, "data/2-final/sleep_duration_stats.rds")
