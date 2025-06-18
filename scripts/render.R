# Data prep ---------------------------------------------------------------

# RMD (input) file name, without extension
rmd_name <- "1-data-prep"

# HTML (output) file name, if different, without extension
html_name <- ""

rmarkdown::render(
  input = paste0("scripts/", rmd_name, ".rmd"),
  output_file = paste(
    ifelse(html_name != "", html_name, rmd_name),
    Sys.Date(),
    sep = "-"
  ),
  output_dir = "output"
)


# Sample ------------------------------------------------------------------

# RMD (input) file name, without extension
rmd_name <- "2-sample"

# HTML (output) file name, if different, without extension
html_name <- "survey-sample-characteristics"

rmarkdown::render(
  input = paste0("scripts/", rmd_name, ".rmd"),
  output_file = paste(
    ifelse(html_name != "", html_name, rmd_name),
    Sys.Date(),
    sep = "-"
  ),
  output_dir = "output"
)


# Basic plots -------------------------------------------------------------

# RMD (input) file name, without extension
rmd_name <- "3-basic-plots"

# HTML (output) file name, if different, without extension
html_name <- ""

rmarkdown::render(
  input = paste0("scripts/", rmd_name, ".rmd"),
  output_file = paste(
    ifelse(html_name != "", html_name, rmd_name),
    Sys.Date(),
    sep = "-"
  ),
  output_dir = "output"
)


# # treatment-plots ---------------------------------------------------------
#
# # RMD (input) file name, without extension
# rmd_name <- "treatment-plots"
#
# # HTML (output) file name, if different, without extension
# html_name <- ""
#
# rmarkdown::render(
#   input = paste0("scripts/", rmd_name, ".rmd"),
#   output_file = paste(
#     ifelse(html_name != "", html_name, rmd_name),
#     Sys.Date(),
#     sep = "-"
#   ),
#   output_dir = "output"
# )
#
# # social-media-plots ------------------------------------------------------
#
# # RMD (input) file name, without extension
# rmd_name <- "social-media-plots"
#
# # HTML (output) file name, if different, without extension
# html_name <- ""
#
# rmarkdown::render(
#   input = paste0("scripts/", rmd_name, ".rmd"),
#   output_file = paste(
#     ifelse(html_name != "", html_name, rmd_name),
#     Sys.Date(),
#     sep = "-"
#   ),
#   output_dir = "output"
# )
#
