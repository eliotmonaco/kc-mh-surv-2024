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
  "\\\\@\\\\@fig-map-resp" = stringr::fixed(paste0(
    "#figure(
      image(\"img/map-resp.png\", height: 6in),
      caption: [", caps$map_resp, "]
    ) <fig-map-resp>"
  )),
  "\\\\@\\\\@tbl-dist" = stringr::fixed(paste0(
    "#figure(
      table(image(\"img/tbl-dist.png\", height: 1.96in)),
      caption: [", caps$tbl_dist, "]
    )"
  )),
  "\\\\@\\\\@fig-sex" = stringr::fixed(paste0(
    "#figure(
      image(\"img/demo-sex2.png\", height: 2.5in),
      caption: [", caps$birth_sex, "]
    )"
  )),
  "\\\\@\\\\@fig-age1" = stringr::fixed(paste0(
    "#figure(
      image(\"img/demo-age.png\", height: 3in),
      caption: [", caps$age, "]
    )"
  )),
  "\\\\@\\\\@fig-age-str" = stringr::fixed(paste0(
    "#figure(
      image(\"img/demo-age-str.png\", height: 3in),
      caption: [", caps$age_str, "]
    )"
  )),
  "\\\\@\\\\@fig-race-eth" = stringr::fixed(paste0(
    "#figure(
      image(\"img/demo-race-eth.png\", height: 4in),
      caption: [", caps$race_ethnicity, "]
    )"
  )),
  "\\\\@\\\\@fig-race-col" = stringr::fixed(paste0(
    "#figure(
      image(\"img/demo-race-col.png\", height: 4in),
      caption: [", caps$race_collapsed, "]
    )"
  )),
  "\\\\@\\\\@fig-hisp" = stringr::fixed(paste0(
    "#figure(
      image(\"img/demo-hisp.png\", height: 4in),
      caption: [", caps$hispanic_origin, "]
    )"
  )),
  "\\\\@\\\\@fig-q4-1" = stringr::fixed(paste0(
    "#figure(
      image(\"img/q4-1.png\", height: 3.5in),
      caption: [", caps$q4_1, "]
    )"
  )),
  "\\\\@\\\\@fig-q4-2" = stringr::fixed(paste0(
    "#figure(
      image(\"img/q4-2.png\", height: 3.5in),
      caption: [", caps$q4_2, "]
    )"
  )),
  "\\\\@\\\\@fig-q4-3" = stringr::fixed(paste0(
    "#figure(
      image(\"img/q4-3.png\", height: 3.5in),
      caption: [", caps$q4_3, "]
    )"
  )),
  "\\\\@\\\\@fig-q4-4" = stringr::fixed(paste0(
    "#figure(
      image(\"img/q4-4.png\", height: 3.5in),
      caption: [", caps$q4_4, "]
    )"
  )),
  "\\\\@\\\\@fig-q5" = stringr::fixed(paste0(
    "#figure(
      image(\"img/q5.png\", height: 3.5in),
      caption: [", caps$q5, "]
    )"
  )),
  "\\\\@\\\\@fig-q6" = stringr::fixed(paste0(
    "#figure(
      image(\"img/q6.png\", height: 3.5in),
      caption: [", caps$q6, "]
    )"
  )),
  "\\\\@\\\\@fig-q7" = stringr::fixed(paste0(
    "#figure(
      image(\"img/q7.png\", height: 3.5in),
      caption: [", caps$q7, "]
    )"
  )),
  "\\\\@\\\\@fig-q8a" = stringr::fixed(paste0(
    "#figure(
      image(\"img/q8a.png\", height: 3.5in),
      caption: [", caps$q8a, "]
    )"
  )),
  "\\\\@\\\\@fig-q9" = stringr::fixed(paste0(
    "#figure(
      image(\"img/q9.png\", height: 3in),
      caption: [", caps$q9, "]
    )"
  )),
  "\\\\@\\\\@fig-q10" = stringr::fixed(paste0(
    "#figure(
      image(\"img/q10.png\", height: 3in),
      caption: [", caps$q10, "]
    )"
  )),
  "\\\\@\\\\@fig-q11" = stringr::fixed(paste0(
    "#figure(
      image(\"img/q11-pdf.png\", height: 4.5in),
      caption: [", caps$q11, "]
    )"
  )),
  "\\\\@\\\\@fig-q12" = stringr::fixed(paste0(
    "#figure(
      image(\"img/q12.png\", height: 3in),
      caption: [", caps$q12, "]
    )"
  )),
  "\\\\@\\\\@fig-map-ccbhc" = stringr::fixed(paste0(
    "#figure(
      image(\"img/map-ccbhc.png\", height: 6in),
      caption: [", caps$map_ccbhc, "]
    )"
  )),
  "\\\\@\\\\@fig-lon-emo-bin" = stringr::fixed(paste0(
    "#figure(
      image(\"img/lon-emo-bin.png\", height: 3.5in),
      caption: [", caps$lon_emo_bin, "]
    )"
  )),
  "\\\\@\\\\@fig-lon-soc-bin" = stringr::fixed(paste0(
    "#figure(
      image(\"img/lon-soc-bin.png\", height: 3.5in),
      caption: [", caps$lon_soc_bin, "]
    )"
  )),
  "\\\\@\\\\@fig-lon-total-fac" = stringr::fixed(paste0(
    "#figure(
      image(\"img/lon-total-fac-pdf.png\", height: 4in),
      caption: [", caps$lon_total_fac, "]
    )"
  )),
  "\\\\@\\\\@fig-lon-total-bin" = stringr::fixed(paste0(
    "#figure(
      image(\"img/lon-total-bin.png\", height: 3.5in),
      caption: [", caps$lon_total_bin, "]
    )"
  )),
  "\\\\@\\\\@fig-sm-add-bin" = stringr::fixed(paste0(
    "#figure(
      image(\"img/sm-add-bin.png\", height: 3.5in),
      caption: [", caps$sm_add_bin, "]
    )"
  )),
  "\\\\@\\\\@fig-sm-use-fac" = stringr::fixed(paste0(
    "#figure(
      image(\"img/sm-use-fac2-pdf.png\", height: 3.5in),
      caption: [", caps$sm_use_fac, "]
    )"
  )),
  "\\\\@\\\\@fig-sm-use-bin-age" = stringr::fixed(paste0(
    "#figure(
      image(\"img/sm-use-bin-age-pdf.png\", height: 3.5in),
      caption: [", caps$sm_use_bin_age, "]
    )"
  )),
  "\\\\@\\\\@fig-hse1" = stringr::fixed(paste0(
    "#figure(
      image(\"img/hse.png\", height: 3.5in),
      caption: [", caps$hse, "]
    )"
  )),
  "\\\\@\\\\@fig-hse-age" = stringr::fixed(paste0(
    "#figure(
      image(\"img/hse-age-pdf.png\", height: 3.5in),
      caption: [", caps$hse_age, "]
    )"
  )),
  "\\\\@\\\\@fig-slp-dur-fac" = stringr::fixed(paste0(
    "#figure(
      image(\"img/slp-dur-fac-pdf.png\", height: 3.5in),
      caption: [", caps$slp_dur_fac, "]
    )"
  )),
  "\\\\@\\\\@tbl-slp-dur-stats" = stringr::fixed(paste0(
    "#figure(
      table(image(\"img/tbl-slp-dur-stats.png\", height: 1.4in)),
      caption: [", caps$slp_dur_stats, "]
    )"
  )),
  "\\\\@\\\\@fig-slp-lat" = stringr::fixed(paste0(
    "#figure(
      image(\"img/slp-lat-pdf.png\", height: 3.5in),
      caption: [", caps$slp_lat, "]
    )"
  )),
  "\\\\@\\\\@tbl-slp-lat" = stringr::fixed(paste0(
    "#figure(
      table(image(\"img/tbl-slp-lat.png\", height: .84in)),
      caption: [", caps$slp_lat_stats, "]
    )"
  )),
  "\\\\@fig-" = stringr::fixed("@fig-")
)
