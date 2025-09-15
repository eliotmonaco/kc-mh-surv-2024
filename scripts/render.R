

rmarkdown::render("scripts/1-data-prep.rmd")
rmarkdown::render("scripts/2-sample.rmd")
rmarkdown::render("scripts/plots.rmd")
rmarkdown::render("scripts/plots-age.rmd")
# rmarkdown::render("scripts/analyses.rmd")

quarto::quarto_render("scripts/report/web-report.qmd")




