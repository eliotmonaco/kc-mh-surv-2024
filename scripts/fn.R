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


# Summarize data ----------------------------------------------------------

summarize_results <- function(
    df,
    var,
    gp_fctr = NULL,
    drop = TRUE,
    na_rm = TRUE) {
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
      dplyr::mutate(pct = setmeup::pct(n, gp_subtotal, digits = 5))
  } else {
    total <- nrow(df)

    df |>
      dplyr::count(
        dplyr::pick(tidyselect::all_of(var)),
        .drop = drop
      ) |>
      dplyr::mutate(total = total) |>
      dplyr::mutate(pct = setmeup::pct(n, total, digits = 5))
  }
}


# Plot --------------------------------------------------------------------

mh_pie_chart <- function(
    plot,
    fill_colors = NULL,
    title = NULL,
    caption = NULL,
    annotation_color = NULL,
    text_scaling_annotation = 1,
    text_scaling_legend = 1,
    text_scaling_caption = 1) {
  label_colors <- function(colors) {
    hcl <- farver::decode_colour(colour = colors, to = "hcl")
    ifelse(hcl[, "l"] > 50, "black", "white")
  }

  # Get fill colors from gg object
  if (is.null(fill_colors)) {
    g <- ggplot2::ggplot_build(plot)
    fill_colors <- unique(g$data[[1]]$fill)
    fill_colors <- viridisLite::viridis(length(fill_colors))
  }

  lc <- label_colors(fill_colors)

  if (!is.null(annotation_color)) {
    lc <- annotation_color
  }

  plot <- plot +
    ggplot2::geom_bar(stat = "identity", width = 1, color = "white") +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::geom_text(
      ggplot2::aes(fontface = "bold"),
      color = lc,
      position = ggplot2::position_stack(vjust = .5),
      show.legend = FALSE,
      size = text_scaling_annotation * .2,
      size.unit = "in"
    ) +
    ggplot2::scale_fill_manual(values = fill_colors) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggtext::element_textbox_simple(),
      plot.title.position = "plot",
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(
        size = ggplot2::rel(text_scaling_legend)
      ),
      plot.caption = ggplot2::element_text(
        hjust = .5,
        size = ggplot2::rel(text_scaling_caption)
      ),
      plot.caption.position = "panel"
    )

  if (!is.null(title)) {
    plot <- plot +
      ggplot2::ggtitle(title)
  }

  if (!is.null(caption)) {
    plot <- plot +
      ggplot2::labs(caption = caption)
  }

  plot
}

mh_barplot <- function(
    plot,
    bar_position = c("stack", "dodge"),
    label_position = c("identity", "stack", "dodge"),
    title = NULL,
    caption = NULL,
    legend = TRUE,
    legend_title = TRUE,
    annotation_color = NULL,
    text_scaling_axes = 1.2,
    text_scaling_legend = 1,
    text_scaling_caption = 1,
    ...) {
  bar_position <- match.arg(bar_position)

  label_position <- match.arg(label_position)

  plot <- plot +
    ggplot2::geom_bar(
      position = bar_position,
      stat = "identity",
      ...
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = NA, color = NA),
      plot.background = ggplot2::element_rect(fill = NA, color = NA),
      plot.title = ggtext::element_textbox_simple(),
      plot.title.position = "plot",
      axis.line = ggplot2::element_line(color = "#555"),
      axis.ticks = ggplot2::element_line(color = "#555"),
      axis.text.x = ggplot2::element_text(
        size = ggplot2::rel(text_scaling_axes)
      ),
      axis.text.y = ggplot2::element_text(
        size = ggplot2::rel(text_scaling_axes)
      ),
      legend.background = ggplot2::element_rect(fill = NA, color = NA),
      legend.title = ggplot2::element_text(
        size = ggplot2::rel(text_scaling_legend)
      ),
      legend.text = ggplot2::element_text(
        size = ggplot2::rel(text_scaling_legend)
      ),
      plot.caption = ggplot2::element_text(
        hjust = .5,
        size = ggplot2::rel(text_scaling_caption)
      ),
      plot.caption.position = "panel"
    ) +
    ggplot2::labs(caption = caption)

  if (is.null(annotation_color)) {
    plot <- plot +
      ggfittext::geom_bar_text(
        position = label_position,
        outside = TRUE,
        contrast = TRUE,
        fontface = "bold"
      )
  } else {
    plot <- plot +
      ggfittext::geom_bar_text(
        position = label_position,
        outside = TRUE,
        color = annotation_color,
        fontface = "bold"
      )
  }

  if (!is.null(title)) {
    plot <- plot +
      ggplot2::ggtitle(title)
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

kc_pop_segment <- function(x0, x1, y0, y1) {
  ggplot2::geom_segment(
    aes(
      x = {{ x0 }},
      xend = {{ x1 }},
      y = {{ y0 }},
      yend = {{ y1 }}
    ),
    color = "black",
    alpha = .5,
    linewidth = 1
  )
}
