
# Report draft ------------------------------------------------------------

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
    f2 <- paste0("fig$", q)
    t <- paste0("tbl$", q)

    if (q %in% names(tbl)) {
      chunk2 <- sprintf("print(%s)", t)
    } else {
      chunk2 <- ""
    }

    chunk <- sprintf(
      "```{r %s, fig.dim=%s}
      print(
        %s$plot |>
          add_caption(%s$cap)
      )

      %s
      ```",
      nm, d, f2, f2, chunk2
    )

    if (!is.na(title)) {
      print_md_text(paste("###", title))
    }

    print_md_chunk(chunk)

    print_from_md_file(md, q2, 3)

    cat("\n\n")
  }
}
