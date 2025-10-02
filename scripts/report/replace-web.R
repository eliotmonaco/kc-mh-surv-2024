# MHS codebook
cb <- readRDS("../../data/2-final/mhs_codebook.rds")

# Captions for figures & tables
caps1 <- readRDS("../../data/2-final/figure_captions_sample.rds")
caps2 <- readRDS("../../data/2-final/figure_captions_questions.rds")
caps3 <- readRDS("../../data/2-final/figure_caption_ccbhc.rds")
caps4 <- readRDS("../../data/2-final/figure_captions_questions_by_age.rds")
caps5 <- readRDS("../../data/2-final/figure_captions_sleep_tables.rds")
caps <- c(caps1, caps2, caps3, caps4, caps5)

replacements <- c(
  "^(#)(\\s.*\\S)$" = "\\1#\\2 ",
  "^(##)(\\s.*\\S)$" = "\\1#\\2 ",
  "^(###)(\\s.*\\S)$" = "\\1#\\2 ",
  "^(####)(\\s.*\\S)$" = "\\1#\\2 ",
  "^(#####)(\\s.*\\S)$" = "\\1#\\2 ",
  "@@fig-map-resp" = stringr::fixed(paste0(
    "![", caps$map_resp, "](map-resp.png){#fig-map-resp}"
  )),
  "@@tbl-dist" = stringr::fixed(paste0(
    "![", caps$tbl_dist, "](tbl-dist.png){#tbl-dist width=300px}"
  )),
  "@@fig-sex" = stringr::fixed(paste0(
    "![", caps$birth_sex, "](sex.png){#fig-sex}"
  )),
  "@@fig-age1" = stringr::fixed(paste0(
    "![", caps$age, "](age.png){#fig-age}"
  )),
  "@@fig-age-str" = stringr::fixed(paste0(
    "![", caps$age_str, "](age-str.png){#fig-age-str}"
  )),
  "@@fig-race-eth" = stringr::fixed(paste0(
    "![", caps$race_ethnicity, "](race-eth.png){#fig-race-eth}"
  )),
  "@@fig-race-col" = stringr::fixed(paste0(
    "![", caps$race_collapsed, "](race-col.png){#fig-race-col}"
  )),
  "@@fig-hisp" = stringr::fixed(paste0(
    "![", caps$hispanic_origin, "](hisp.png){#fig-hisp}"
  )),
  "@@fig-q4-1" = stringr::fixed(paste0(
    "![", caps$q4_1, "](q4-1.png){#fig-q4-1}"
  )),
  "@@fig-q4-2" = stringr::fixed(paste0(
    "![", caps$q4_2, "](q4-2.png){#fig-q4-2}"
  )),
  "@@fig-q4-3" = stringr::fixed(paste0(
    "![", caps$q4_3, "](q4-3.png){#fig-q4-3}"
  )),
  "@@fig-q4-4" = stringr::fixed(paste0(
    "![", caps$q4_4, "](q4-4.png){#fig-q4-4}"
  )),
  "@@fig-q5" = stringr::fixed(paste0(
    "![", caps$q5, "](q5.png){#fig-q5}"
  )),
  "@@fig-q6" = stringr::fixed(paste0(
    "![", caps$q6, "](q6.png){#fig-q6}"
  )),
  "@@fig-q7" = stringr::fixed(paste0(
    "![", caps$q7, "](q7.png){#fig-q7}"
  )),
  "@@fig-q8a" = stringr::fixed(paste0(
    "![", caps$q8a, "](q8a.png){#fig-q8a}"
  )),
  "@@fig-q9" = stringr::fixed(paste0(
    "![", caps$q9, "](q9.png){#fig-q9}"
  )),
  "@@fig-q10" = stringr::fixed(paste0(
    "![", caps$q10, "](q10.png){#fig-q10}"
  )),
  "@@fig-q11" = stringr::fixed(paste0(
    "![", caps$q11, "](q11.png){#fig-q11}"
  )),
  "@@fig-q12" = stringr::fixed(paste0(
    "![", caps$q12, "](q12.png){#fig-q12}"
  )),
  "@@fig-map-ccbhc" = stringr::fixed(paste0(
    "![", caps$map_ccbhc, "](map-ccbhc.png){#fig-map-ccbhc}"
  )),
  "@@fig-lon-emo-bin" = stringr::fixed(paste0(
    "![", caps$lon_emo_bin, "](lon-emo-bin.png){#fig-lon-emo-bin}"
  )),
  "@@fig-lon-soc-bin" = stringr::fixed(paste0(
    "![", caps$lon_soc_bin, "](lon-soc-bin.png){#fig-lon-soc-bin}"
  )),
  "@@fig-lon-total-fac" = stringr::fixed(paste0(
    "![", caps$lon_total_fac, "](lon-total-fac.png){#fig-lon-total-fac}"
  )),
  "@@fig-lon-total-bin" = stringr::fixed(paste0(
    "![", caps$lon_total_bin, "](lon-total-bin.png){#fig-lon-total-bin}"
  )),
  "@@fig-sm-add-bin" = stringr::fixed(paste0(
    "![", caps$sm_add_bin, "](sm-add-bin.png){#fig-sm-add-bin}"
  )),
  "@@fig-sm-use-fac" = stringr::fixed(paste0(
    "![", caps$sm_use_fac, "](sm-use-fac.png){#fig-sm-use-fac}"
  )),
  "@@fig-sm-use-bin-age" = stringr::fixed(paste0(
    "![", caps$sm_use_bin_age, "](sm-use-bin-age.png){#fig-sm-use-bin-age}"
  )),
  "@@fig-hse1" = stringr::fixed(paste0(
    "![", caps$hse, "](hse.png){#fig-hse}"
  )),
  "@@fig-hse-age" = stringr::fixed(paste0(
    "![", caps$hse_age, "](hse-age.png){#fig-hse-age}"
  )),
  "@@fig-slp-dur-fac" = stringr::fixed(paste0(
    "![", caps$slp_dur_fac, "](slp-dur-fac.png){#fig-slp-dur-fac}"
  )),
  "@@tbl-slp-dur-stats" = stringr::fixed(paste0(
    "![", caps$slp_dur_stats, "](tbl-slp-dur-stats.png){#tbl-slp-dur-stats width=500px}"
  )),
  "@@fig-slp-lat" = stringr::fixed(paste0(
    "![", caps$slp_lat, "](slp-lat.png){#fig-slp-lat}"
  )),
  "@@tbl-slp-lat" = stringr::fixed(paste0(
    "![", caps$slp_lat_stats, "](tbl-slp-lat.png){#tbl-slp-lat width=440px}"
  ))
)
