
# Standalone report -------------------------------------------------------

# Print questions in standalone report
print_questions_to_report <- function(group) {
  x <- cb$viz$var[cb$viz$group == group]
  x <- names(fig)[names(fig) %in% x]
  f <- fig[x]

  for (i in 1:length(x)) {
    q <- names(f)[i]
    q2 <- gsub("_", "-", q)
    nm <- paste0("fig-std-", q2)
    title <- cb$viz$title[cb$viz$var == q]
    d <- dims[[cb$viz$dims[cb$viz$var == q]]]
    s <- paste0("fig$", q)

    chunk <- sprintf(
      "```{r %s, fig.dim=%s}
      print(
        %s$plot |>
          add_caption(%s$cap)
      )
      ```",
      nm, d, s, s
    )

    if (!is.na(title)) {
      print_md_text(paste("###", title))
    }

    print_md_chunk(chunk)

    print_from_md_file(md, q2, 3)

    cat("\n\n")
  }
}
