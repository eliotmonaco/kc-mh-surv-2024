
# Score survey data -------------------------------------------------------

number_to_time <- function(x) {
  paste0(
    substr(sprintf("%04d", x), 1, 2),
    ":",
    substr(sprintf("%04d", x), 3, 4)
  )
}

validate_time <- function(x) {
  h <- paste(sprintf("%02d", 0:23), collapse = "|")
  m <- paste(sprintf("%02d", 0:59), collapse = "|")
  p <- paste0("^(", h, "):(", m, ")$")
  grepl(pattern = p, x = x)
}

# Convert bedtime (`x`) and wake time (`y`) pairs to date objects
times_to_dates <- function(x, y) {
  # If y > x, assign x and y to the same day
  # If x > y, assign y to one day later than x
  xmin <- as.numeric(substr(x, 1, 2)) * 60 + as.numeric(substr(x, 4, 5))
  ymin <- as.numeric(substr(y, 1, 2)) * 60 + as.numeric(substr(y, 4, 5))
  xmin > ymin
  d1 <- as.POSIXct(paste(Sys.Date(), x), format = "%Y-%m-%d %H:%M")
  d2 <- dplyr::if_else(
    ymin > xmin,
    as.POSIXct(paste(Sys.Date(), y), format = "%Y-%m-%d %H:%M"),
    as.POSIXct(paste(Sys.Date() + 1, y), format = "%Y-%m-%d %H:%M")
  )
  list(
    date1 = d1,
    date2 = d2
  )
}

score_hse <- function(x) {
  score <- cut(
    x,
    breaks = c(0, 65, 75, 85, Inf),
    labels = c(3, 2, 1, 0),
    include.lowest = TRUE,
    right = FALSE
  )

  # Reverse factor order
  factor(score, levels = 0:3)
}

minmax_seq <- function(x) {
  if (!is.numeric(x)) stop("`x` must be a numeric vector")
  min <- min(x, na.rm = TRUE)
  max <- max(x, na.rm = TRUE)
  min:max
}


# Summarize data ----------------------------------------------------------

summarize_results <- function(
    df,
    var,
    gp_fctr = NULL, # variables to disaggregate the values in `var`
    drop = TRUE, # if FALSE, factors with a count of 0 are included
    na_rm = TRUE) { # remove rows where `var` and `gp_fctr` are NA
  # Drop rows where `var` is NA
  if (na_rm) {
    df <- df |>
      tidyr::drop_na(tidyselect::all_of(var))
  }

  if (!is.null(gp_fctr)) {
    # Drop rows where `gp_fctr` is NA
    if (na_rm) {
      df <- df |>
        tidyr::drop_na(tidyselect::all_of(gp_fctr))
    }

    total <- nrow(df)

    # Get denominators for each `gp_fctr` level
    subtotals <- df |>
      dplyr::count(
        dplyr::pick(tidyselect::all_of(gp_fctr)),
        .drop = drop
      )

    df |>
      dplyr::count(
        dplyr::pick(tidyselect::all_of(c(var, gp_fctr))),
        .drop = drop
      ) |>
      dplyr::left_join(
        subtotals,
        by = gp_fctr,
        suffix = c("", "_gp_subtotal")
      ) |>
      dplyr::rename(gp_subtotal = n_gp_subtotal) |>
      dplyr::mutate(total = total, .after = gp_subtotal) |>
      dplyr::mutate(
        pct = setmeup::pct(n, gp_subtotal, digits = 5),
        pct_fmt = setmeup::pct(n, gp_subtotal, format = TRUE)
      )
  } else {
    total <- nrow(df)

    df |>
      dplyr::count(
        dplyr::pick(tidyselect::all_of(var)),
        .drop = drop
      ) |>
      dplyr::mutate(total = total) |>
      dplyr::mutate(
        pct = setmeup::pct(n, total, digits = 5),
        pct_fmt = setmeup::pct(n, total, format = TRUE)
      )
  }
}


# Plotting ----------------------------------------------------------------

mhs_pie_chart <- function(
    plot,
    plot_title = NULL,
    legend = TRUE,
    legend_title = TRUE,
    base_font_size = 11) {
  plot <- plot +
    ggplot2::geom_bar(
      stat = "identity",
      width = 1,
      color = "white"
    ) +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::theme_void(base_size = base_font_size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = NA, color = NA),
      panel.background = ggplot2::element_rect(fill = NA, color = NA),
      legend.background = ggplot2::element_rect(fill = NA, color = NA),
      plot.title = ggtext::element_textbox_simple(),
      plot.title.position = "plot",
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank()
    )

  if (!is.null(plot_title)) {
    plot <- plot +
      ggplot2::ggtitle(plot_title)
  }

  if (!legend) {
    plot <- plot +
      ggplot2::guides(fill = "none")
  }

  if (!legend_title) {
    plot <- plot +
      ggplot2::theme(legend.title = ggplot2::element_blank())
  }

  plot
}

mhs_barplot <- function(
    plot,
    bar_position = c("stack", "dodge"),
    bar_reverse = FALSE,
    plot_title = NULL,
    legend = TRUE,
    legend_title = TRUE,
    legend_reverse = FALSE,
    base_font_size = 11,
    ...) {
  bar_position <- match.arg(bar_position)

  pos <- function(x, rev) {
    if (x == "stack") {
      ggplot2::position_stack(reverse = rev)
    } else if (x == "dodge") {
      ggplot2::position_dodge()
    }
  }

  plot <- plot +
    ggplot2::geom_bar(
      stat = "identity",
      position = pos(bar_position, bar_reverse),
      ...
    ) +
    ggplot2::theme_classic(base_size = base_font_size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = NA, color = NA),
      panel.background = ggplot2::element_rect(fill = NA, color = NA),
      legend.background = ggplot2::element_rect(fill = NA, color = NA),
      plot.title = ggtext::element_textbox_simple(),
      plot.title.position = "plot",
      axis.line = ggplot2::element_line(color = "#555555"),
      axis.ticks = ggplot2::element_line(color = "#555555")
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = legend_reverse))

  if (!is.null(plot_title)) {
    plot <- plot +
      ggplot2::ggtitle(plot_title)
  }

  if (!legend) {
    plot <- plot +
      ggplot2::guides(fill = "none")
  }

  if (!legend_title) {
    plot <- plot +
      ggplot2::theme(legend.title = ggplot2::element_blank())
  }

  plot
}

add_seg_vars <- function(df, var, factor) {
  df$x0 <- df[[var]]
  df$x1 <- df[[var]]
  df$y0 <- as.numeric(df[[factor]]) - .45
  df$y1 <- as.numeric(df[[factor]]) + .45

  df
}

pop_segment <- function() {
  ggplot2::geom_segment(
    aes(x = x0, xend = x1, y = y0, yend = y1),
    color = "black",
    alpha = .5,
    linewidth = 1
  )
}

add_caption <- function(plot, cap_text, size = 11, width = 80) {
  plot +
    ggplot2::labs(caption = stringr::str_wrap(cap_text, width = width)) +
    ggplot2::theme(
      plot.caption.position = "plot",
      plot.caption = ggplot2::element_text(
        hjust = 0,
        vjust = -5,
        size = size
      )
    )
}

contrast_color <- function(color) {
  hcl <- farver::decode_colour(
    colour = color,
    to = "hcl"
  )

  ifelse(hcl[, "l"] > 60, "#000000", "#FFFFFF")
}

mhs_palette <- function(pal, n = NULL) {
  if (pal == "candy") {
    # clr <- c("#0aa81b", "#a7ca4c", "#ffec93", "#ff9c46", "#ff1442")
    clr <- c("#00876c", "#89c079", "#fff492", "#f69c56", "#d43d51")
    grad <- colorRampPalette(clr)
    grad(n)
  } else if (pal == "blueorange") {
    clr <- c("#4267a8", "#a998d5", "#ffcfff", "#ff90a9", "#f27413")
    grad <- colorRampPalette(clr)
    grad(n)
  } else if (pal == "mellow") {
    clr <- c("#365a7a", "#30aac2", "#72ffe7", "#afef90", "#ffcd54")
    grad <- colorRampPalette(clr)
    grad(n)
  } else if (pal == "sunburst") {
    clr <- c("#5d3cab", "#ca79b2", "#ffcfd7", "#ffc297", "#ffd738")
    grad <- colorRampPalette(clr)
    grad(n)
  } else if (pal == "ocean") {
    # clr <- c("#0f9aab", "#92c1de", "#e6ecff", "#daaddd", "#e06581")
    clr <- c("#1f75a1", "#a2a7d5", "#ffe0ff", "#f1a3ca", "#e06581")
    grad <- colorRampPalette(clr)
    grad(n)
  } else if (pal == "icecream") {
    clr <- c("#4f0a6b", "#bb6588", "#ffd1cb", "#a9866c", "#4f461f")
    grad <- colorRampPalette(clr)
    grad(n)
  } else if (pal == "kc") {
    c("#318CCC", "#F04C43")
  }
}
