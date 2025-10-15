# MHS codebook
cb <- readRDS("../../data/2-final/mhs_codebook.rds")

# Captions for figures & tables
caps1 <- readRDS("../../data/2-final/figure_captions_sample.rds")
caps2 <- readRDS("../../data/2-final/figure_captions_questions.rds")
caps3 <- readRDS("../../data/2-final/figure_caption_ccbhc.rds")
caps4 <- readRDS("../../data/2-final/figure_captions_questions_by_age.rds")
caps5 <- readRDS("../../data/2-final/figure_captions_sleep_tables.rds")
caps <- c(caps1, caps2, caps3, caps4, caps5)

hd <- as.list(cb$viz$header)
names(hd) <- cb$viz$var

replacements <- c(
  # Methodology
  "\\\\@\\\\@fig-map-resp" = stringr::fixed(paste(
    "#figblock[",
    "#figure(", "image(\"img/map-resp.png\", height: 6in),",
    paste0("caption: [", caps$map_resp, "]"), ") <fig-map-resp>", "]",
    sep = "\n"
  )),
  # Sample
  "\\\\@\\\\@tbl-dist" = stringr::fixed(paste(
    "#figblock[",
    "#figure(", "table(image(\"img/tbl-dist.png\", height: 1.96in)),",
    paste0("caption: [", caps$tbl_dist, "]"), ")", "]",
    sep = "\n"
  )),
  "\\\\@\\\\@fig-sex" = stringr::fixed(paste(
    "#figblock[",
    "#figure(", "image(\"img/sex-pdf.png\", height: 2.5in),",
    paste0("caption: [", caps$birth_sex, "]"), ")", "]",
    sep = "\n"
  )),
  "\\\\@\\\\@fig-age1" = stringr::fixed(paste(
    "#figblock[",
    "#figure(", "image(\"img/age.png\", height: 3in),",
    paste0("caption: [", caps$age, "]"), ")", "]",
    sep = "\n"
  )),
  "\\\\@\\\\@fig-age-str" = stringr::fixed(paste(
    "#figblock[",
    "#figure(", "image(\"img/age-str.png\", height: 3in),",
    paste0("caption: [", caps$age_str, "]"), ")", "]",
    sep = "\n"
  )),
  "\\\\@\\\\@fig-race-eth" = stringr::fixed(paste(
    "#figblock[",
    "#figure(", "image(\"img/race-eth.png\", height: 4in),",
    paste0("caption: [", caps$race_ethnicity, "]"), ")", "]",
    sep = "\n"
  )),
  "\\\\@\\\\@fig-race-col" = stringr::fixed(paste(
    "#figblock[",
    "#figure(", "image(\"img/race-col.png\", height: 4in),",
    paste0("caption: [", caps$race_collapsed, "]"), ")", "]",
    sep = "\n"
  )),
  "\\\\@\\\\@fig-hisp" = stringr::fixed(paste(
    "#figblock[",
    "#figure(", "image(\"img/hisp.png\", height: 4in),",
    paste0("caption: [", caps$hispanic_origin, "]"), ")", "]",
    sep = "\n"
  )),
  # Personal mental health
  "\\\\@\\\\@fig-q4-1" = stringr::fixed(paste(
    "#figblock[", paste0("==== ", hd$q4_1),
    "#figure(", "image(\"img/q4-1.png\", height: 3.5in),",
    paste0("caption: [", caps$q4_1, "]"), ")", "]",
    sep = "\n"
  )),
  "\\\\@\\\\@fig-q4-2" = stringr::fixed(paste(
    "#figblock[", paste0("==== ", hd$q4_2),
    "#figure(", "image(\"img/q4-2.png\", height: 3.5in),",
    paste0("caption: [", caps$q4_2, "]"), ")", "]",
    sep = "\n"
  )),
  "\\\\@\\\\@fig-q4-3" = stringr::fixed(paste(
    "#figblock[", paste0("==== ", hd$q4_3),
    "#figure(", "image(\"img/q4-3.png\", height: 3.5in),",
    paste0("caption: [", caps$q4_3, "]"), ")", "]",
    sep = "\n"
  )),
  "\\\\@\\\\@fig-q4-4" = stringr::fixed(paste(
    "#figblock[", paste0("==== ", hd$q4_4),
    "#figure(", "image(\"img/q4-4.png\", height: 3.5in),",
    paste0("caption: [", caps$q4_4, "]"), ")", "]",
    sep = "\n"
  )),
  "\\\\@\\\\@fig-q5" = stringr::fixed(paste(
    "#figblock[", paste0("==== ", hd$q5),
    "#figure(", "image(\"img/q5.png\", height: 3.5in),",
    paste0("caption: [", caps$q5, "]"), ")", "]",
    sep = "\n"
  )),
  "\\\\@\\\\@fig-q6" = stringr::fixed(paste(
    "#figblock[", paste0("==== ", hd$q6),
    "#figure(", "image(\"img/q6.png\", height: 3.5in),",
    paste0("caption: [", caps$q6, "]"), ")", "]",
    sep = "\n"
  )),
  "\\\\@\\\\@fig-q7" = stringr::fixed(paste(
    "#figblock[", paste0("==== ", hd$q7),
    "#figure(", "image(\"img/q7.png\", height: 3.5in),",
    paste0("caption: [", caps$q7, "]"), ")", "]",
    sep = "\n"
  )),
  "\\\\@\\\\@fig-q8a" = stringr::fixed(paste(
    "#figblock[", paste0("==== ", hd$q8a),
    "#figure(", "image(\"img/q8a.png\", height: 3.5in),",
    paste0("caption: [", caps$q8a, "]"), ")", "]",
    sep = "\n"
  )),
  # Mental health treatment
  "\\\\@\\\\@fig-q9" = stringr::fixed(paste(
    "#figblock[", paste0("==== ", hd$q9),
    "#figure(", "image(\"img/q9.png\", height: 3in),",
    paste0("caption: [", caps$q9, "]"), ")", "]",
    sep = "\n"
  )),
  "\\\\@\\\\@fig-q10" = stringr::fixed(paste(
    "#figblock[", paste0("==== ", hd$q10),
    "#figure(", "image(\"img/q10.png\", height: 3in),",
    paste0("caption: [", caps$q10, "]"), ")", "]",
    sep = "\n"
  )),
  "\\\\@\\\\@fig-q11" = stringr::fixed(paste(
    "#figblock[", paste0("==== ", hd$q11),
    "#figure(", "image(\"img/q11-pdf.png\", height: 4.5in),",
    paste0("caption: [", caps$q11, "]"), ")", "]",
    sep = "\n"
  )),
  "\\\\@\\\\@fig-q12" = stringr::fixed(paste(
    "#figblock[", paste0("==== ", hd$q12),
    "#figure(", "image(\"img/q12.png\", height: 3in),",
    paste0("caption: [", caps$q12, "]"), ")", "]",
    sep = "\n"
  )),
  "\\\\@\\\\@fig-map-ccbhc" = stringr::fixed(paste(
    "#figblock[",
    "#figure(", "image(\"img/map-ccbhc.png\", height: 6in),",
    paste0("caption: [", caps$map_ccbhc, "]"), ")", "]",
    sep = "\n"
  )),
  # Emotional & social loneliness
  "\\\\@\\\\@fig-lon-emo-bin" = stringr::fixed(paste(
    "#figblock[", paste0("==== ", hd$lon_emo_bin),
    "#figure(", "image(\"img/lon-emo-bin.png\", height: 3.5in),",
    paste0("caption: [", caps$lon_emo_bin, "]"), ")", "]",
    sep = "\n"
  )),
  "\\\\@\\\\@fig-lon-soc-bin" = stringr::fixed(paste(
    "#figblock[", paste0("==== ", hd$lon_soc_bin),
    "#figure(", "image(\"img/lon-soc-bin.png\", height: 3.5in),",
    paste0("caption: [", caps$lon_soc_bin, "]"), ")", "]",
    sep = "\n"
  )),
  "\\\\@\\\\@fig-lon-total-fac" = stringr::fixed(paste(
    "#figblock[", paste0("==== ", hd$lon_total_fac),
    "#figure(", "image(\"img/lon-total-fac-pdf.png\", height: 4in),",
    paste0("caption: [", caps$lon_total_fac, "]"), ")", "]",
    sep = "\n"
  )),
  "\\\\@\\\\@fig-lon-total-bin" = stringr::fixed(paste(
    "#figblock[", paste0("==== ", hd$lon_total_bin),
    "#figure(", "image(\"img/lon-total-bin.png\", height: 3.5in),",
    paste0("caption: [", caps$lon_total_bin, "]"), ")", "]",
    sep = "\n"
  )),
  # Social media use
  "\\\\@\\\\@fig-sm-add-bin" = stringr::fixed(paste(
    "#figblock[", paste0("==== ", hd$sm_add_bin),
    "#figure(", "image(\"img/sm-add-bin.png\", height: 3.5in),",
    paste0("caption: [", caps$sm_add_bin, "]"), ")", "]",
    sep = "\n"
  )),
  "\\\\@\\\\@fig-sm-use-fac" = stringr::fixed(paste(
    "#figblock[", paste0("==== ", hd$sm_use_fac),
    "#figure(", "image(\"img/sm-use-fac2-pdf.png\", height: 3.5in),",
    paste0("caption: [", caps$sm_use_fac, "]"), ")", "]",
    sep = "\n"
  )),
  "\\\\@\\\\@fig-sm-use-bin-age" = stringr::fixed(paste(
    "#figblock[",
    "#figure(", "image(\"img/sm-use-bin-age-pdf.png\", height: 3.5in),",
    paste0("caption: [", caps$sm_use_bin_age, "]"), ")", "]",
    sep = "\n"
  )),
  # Sleep quality
  "\\\\@\\\\@fig-hse1" = stringr::fixed(paste(
    "#figblock[", paste0("==== ", hd$hse),
    "#figure(", "image(\"img/hse.png\", height: 3.5in),",
    paste0("caption: [", caps$hse, "]"), ")", "]",
    sep = "\n"
  )),
  "\\\\@\\\\@fig-hse-age" = stringr::fixed(paste(
    "#figblock[",
    "#figure(", "image(\"img/hse-age-pdf.png\", height: 3.5in),",
    paste0("caption: [", caps$hse_age, "]"), ")", "]",
    sep = "\n"
  )),
  "\\\\@\\\\@fig-slp-dur-fac" = stringr::fixed(paste(
    "#figblock[", paste0("==== ", hd$slp_dur_fac),
    "#figure(", "image(\"img/slp-dur-fac-pdf.png\", height: 3.5in),",
    paste0("caption: [", caps$slp_dur_fac, "]"), ")", "]",
    sep = "\n"
  )),
  "\\\\@\\\\@tbl-slp-dur-stats" = stringr::fixed(paste(
    "#figblock[",
    "#figure(", "table(image(\"img/tbl-slp-dur-stats.png\", height: 1.4in)),",
    paste0("caption: [", caps$slp_dur_stats, "]"), ")", "]",
    sep = "\n"
  )),
  "\\\\@\\\\@fig-slp-lat" = stringr::fixed(paste(
    "#figblock[", paste0("==== ", hd$slp_lat),
    "#figure(", "image(\"img/slp-lat-pdf.png\", height: 3.5in),",
    paste0("caption: [", caps$slp_lat, "]"), ")", "]",
    sep = "\n"
  )),
  "\\\\@\\\\@tbl-slp-lat" = stringr::fixed(paste(
    "#figblock[",
    "#figure(", "table(image(\"img/tbl-slp-lat.png\", height: .84in)),",
    paste0("caption: [", caps$slp_lat_stats, "]"), ")", "]",
    sep = "\n"
  )),
  # In-text cross refs
  "\\\\@fig-" = stringr::fixed("@fig-")
)
