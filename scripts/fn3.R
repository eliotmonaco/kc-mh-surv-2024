
# Text wrangling ----------------------------------------------------------

# Print sections of markdown text:
# - "section": full section, including the matched heading and all text until
#   the next heading at the same level
# - "head+para": partial section, including the matched heading and all text
#   until the next heading of any level
# - "para": paragraph text only, including anything between the matched heading
#   and the next heading of any level
print_from_md_file <- function(md, head, type, silent = FALSE) {
  # Find the index of the heading containing `head`
  i <- which(grepl(paste0("(?i)^#.*(?=", head, ")"), md, perl = TRUE))

  if (length(i) > 1) {
    i <- which(grepl(paste0("(?i)^#+\\s", head, "$"), md, perl = TRUE))

    if (length(i) == 1) {
      if (!silent) {
        message(paste0(
          "More than one heading matched the text in `head`. ",
          "An exact match was found and used."
        ))
      }
    } else {
      stop(paste0(
        "More than one heading matched the text in `head`, ",
        "and no exact matches were found"
      ))
    }
  } else if (length(i) == 0) {
    stop("No headings matched the text in `head`")
  }

  if (type %in% 1:2) {
    # Find the heading level from the position of the first space character
    lvl <- gregexpr("\\s", md[i])[[1]] - 1

    lvl <- lvl[1]

    if (type == 1) { # full section
      # Find all lines with headings equal to or higher than `lvl`
      p <- unlist(lapply(1:lvl, \(x) paste(rep("#", x), collapse = "")))

      p <- paste0("^(", paste(p, collapse = "|"), ")\\s")

      m <- which(grepl(p, md))
    } else if (type == 2) { # partial section
      # Find all lines containing a heading
      p <- "^#"

      m <- which(grepl(p, md))
    }

    # Find indices from the target heading to the next heading in `m`
    i <- which(m == i)

    if (length(m) > i) {
      rng <- m[i]:(m[i + 1] - 1)
    } else {
      p <- "^\\[\\^\\d+\\]:"

      if (any(grepl(p, md))) {
        ft <- which(grepl(p, md))[1]

        rng <- m[i]:(ft - 1)
      } else {
        rng <- m[i]:length(md)
      }
    }
  } else if (type == 3) { # paragraph text only
    # Find all lines containing a heading
    p <- "^#"

    m <- which(grepl(p, md))

    # Find indices from after the target heading to just before the next heading
    i <- which(m == i)

    if (length(m) > i) {
      rng <- (m[i] + 1):(m[i + 1] - 1)
    } else {
      p <- "^\\[\\^\\d+\\]:"

      if (any(grepl(p, md))) {
        ft <- which(grepl(p, md))[1]

        rng <- (m[i] + 1):(ft - 1)
      } else {
        rng <- (m[i] + 1):length(md)
      }
    }
  }

  text <- trimws(paste(md[rng], collapse = "\n"))

  # Get footnote reference, if present
  p <- "\\[\\^\\d+\\]"

  if (grepl(p, text)) {
    ft_all <- regmatches(text, m = gregexpr(p, text))[[1]]

    ft <- lapply(ft_all, \(x) {
      ft <- regmatches(x, m = regexpr("\\d+", x))

      p <- paste0("\\[\\^", ft, "\\]:")

      i <- which(grepl(p, md))

      p <- "^\\[\\^\\d+\\]:|^#"

      m <- which(grepl(p, md))

      i <- which(m == i)

      if (length(m) > i) {
        rng <- m[i]:(m[i + 1] - 1)
      } else {
        rng <- m[i]:length(md)
      }

      ft <- trimws(paste(md[rng], collapse = "\n"))
    })

    ft <- paste(ft, collapse = "\n\n")

    text <- paste0(text, "\n\n", ft)
  }

  # Replace "\\[" and "\\]" which renders as math expression
  text <- text |>
    gsub(pattern = "\\\\\\[", replacement = "[", perl = TRUE) |>
    gsub(pattern = "\\\\\\]", replacement = "]", perl = TRUE)

  cat(text, "\n\n")
}

extract_text <- function(
    doc,  # the document to extract text from
    head_text, # heading text to search for in `doc`
    type = c("section", "head+para", "para"),
    format = c("markdown", "typst"),
    silent = FALSE) {
  type <- match.arg(type)
  format <- match.arg(format)

  h <- switch(format, markdown = "#", typst = "=")

  # Find the index of the heading containing `head_text`
  p <- paste0("(?i)^", h, ".*(?=", head_text, ")")

  i <- which(grepl(p, doc, perl = TRUE))

  if (length(i) > 1) {
    p <- paste0("(?i)^", h, "+\\s", head_text, "$")

    i <- which(grepl(p, doc, perl = TRUE))

    if (length(i) == 1) {
      if (!silent) {
        message(paste0(
          "More than one heading matched the text in `head_text`. ",
          "An exact match was found and used."
        ))
      }
    } else {
      stop(paste0(
        "More than one heading matched the text in `head_text`, ",
        "and no exact matches were found"
      ))
    }
  } else if (length(i) == 0) {
    stop("No headings matched the text in `head_text`")
  }

  if (type %in% c("section", "head+para")) {
    # Find the heading level from the position of the first space character
    lvl <- gregexpr("\\s", doc[i])[[1]] - 1

    lvl <- lvl[1]

    if (type == "section") { # full section
      # Find all lines with headings equal to or higher than `lvl`
      p <- unlist(lapply(1:lvl, \(x) paste(rep(h, x), collapse = "")))

      p <- paste0("^(", paste(p, collapse = "|"), ")\\s")

      m <- which(grepl(p, doc))
    } else if (type == "head+para") { # header + paragraph
      # Find all lines containing a heading
      m <- which(grepl(paste0("^", h), doc))
    }

    # Find indices from the target heading to the next heading in `m`
    i <- which(m == i)

    if (length(m) > i) {
      rng <- m[i]:(m[i + 1] - 1)
    } else {
      p <- "^\\[\\^\\d+\\]:"

      if (any(grepl(p, doc))) {
        ft <- which(grepl(p, doc))[1]

        rng <- m[i]:(ft - 1)
      } else {
        rng <- m[i]:length(doc)
      }
    }
  } else if (type == "para") { # paragraph text only
    # Find all lines containing a heading
    m <- which(grepl(paste0("^", h), doc))

    # Find indices from after the target heading to just before the next heading
    i <- which(m == i)

    if (length(m) > i) {
      rng <- (m[i] + 1):(m[i + 1] - 1)
    } else {
      p <- "^\\[\\^\\d+\\]:"

      if (any(grepl(p, doc))) {
        ft <- which(grepl(p, doc))[1]

        rng <- (m[i] + 1):(ft - 1)
      } else {
        rng <- (m[i] + 1):length(doc)
      }
    }
  }

  text <- trimws(paste(doc[rng], collapse = "\n"))

  if (format == "markdown") {
    # Get footnote reference, if present
    # p <- "\\[\\^\\d+\\]"
    #
    # if (grepl(p, text)) {
    #   ft_all <- regmatches(text, m = gregexpr(p, text))[[1]]
    #
    #   ft <- lapply(ft_all, \(x) {
    #     ft <- regmatches(x, m = regexpr("\\d+", x))
    #
    #     i <- which(grepl(paste0("\\[\\^", ft, "\\]:"), doc))
    #
    #     m <- which(grepl(paste0("^\\[\\^\\d+\\]:|^", h), doc))
    #
    #     i <- which(m == i)
    #
    #     if (length(m) > i) {
    #       rng <- m[i]:(m[i + 1] - 1)
    #     } else {
    #       rng <- m[i]:length(doc)
    #     }
    #
    #     ft <- trimws(paste(doc[rng], collapse = "\n"))
    #   })
    #
    #   ft <- paste(ft, collapse = "\n\n")
    #
    #   text <- paste0(text, "\n\n", ft)
    # }
    #
    # # Replace "\\[" and "\\]" which renders as math expression
    # text <- text |>
    #   gsub(pattern = "\\\\\\[", replacement = "[", perl = TRUE) |>
    #   gsub(pattern = "\\\\\\]", replacement = "]", perl = TRUE)
  }

  text <- paste0(text, "\n\n")

  # cat(text)

  text
}

print_md_text <- function(text) {
  cat(paste0(text, "\n\n"))
}

print_md_chunk <- function(chunk) {
  cat(knitr::knit(text = knitr::knit_expand(text = chunk)))
  cat("\n\n")
}

get_md_footnote <- function(doc, text) {
  p <- "\\[\\^\\d+\\]"

  if (grepl(p, text)) {
    ft_all <- regmatches(text, m = gregexpr(p, text))[[1]]

    ft <- lapply(ft_all, \(x) {
      ft <- regmatches(x, m = regexpr("\\d+", x))

      i <- which(grepl(paste0("\\[\\^", ft, "\\]:"), doc))

      m <- which(grepl(paste0("^\\[\\^\\d+\\]:|^", h), doc))

      i <- which(m == i)

      if (length(m) > i) {
        rng <- m[i]:(m[i + 1] - 1)
      } else {
        rng <- m[i]:length(doc)
      }

      ft <- trimws(paste(doc[rng], collapse = "\n"))
    })

    ft <- paste(ft, collapse = "\n\n")

    text <- paste0(text, "\n\n", ft)
  }

  # Replace "\\[" and "\\]" which renders as math expression
  text |>
    gsub(pattern = "\\\\\\[", replacement = "[", perl = TRUE) |>
    gsub(pattern = "\\\\\\]", replacement = "]", perl = TRUE)
}
