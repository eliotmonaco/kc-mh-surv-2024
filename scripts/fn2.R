
# Meta-summary and plotting -----------------------------------------------

label_wrap <- function(x, w) {
  str_wrap(x, width = w, whitespace_only = FALSE)
}

cap_text <- function(df, var) {
  cap <- cb$viz$caption[cb$viz$var == var]

  sampsize <- unique(df$total)

  if (is.na(cap)) {
    cap <- paste0("N = ", sampsize, ".")
  } else {
    cap <- paste0(cap, " N = ", sampsize, ".")
  }

  cap
}

summarize_dataset <- function(fctr = NULL) {
  # Summarize MH questions grouped by a factor
  vars <- cb$viz$var[which(cb$viz$data == "factor")]

  vars <- vars[vars %in% colnames(mhsurv)]

  ls <- lapply(vars, \(x) {
    summarize_results(mhsurv, var = x, gp_fctr = fctr)
  })
  names(ls) <- vars

  # Combine Q11 components
  p <- "^q11_\\d"

  if (any(grepl(p, colnames(mhsurv)))) {
    n <- which(grepl(p, names(ls)))

    ls$q11 <- lapply(names(ls)[n], \(x) {
      ls[[x]] |>
        mutate(reason = cb$viz$prompt_abbr[cb$viz$var == x]) |>
        rename(q11 = {{ x }})
    }) |>
      list_rbind() |>
      filter(q11 == "Yes")

    # Order components by percentage of "Yes" responses
    if (!is.null(fctr)) {
      lvl <- ls$q11 |>
        filter({{ fctr }} == "Worse")
    } else {
      lvl <- ls$q11
    }

    lvl <- lvl |>
      arrange(pct) |>
      pull(reason)

    ls$q11 <- ls$q11 |>
      mutate(reason = factor(reason, levels = lvl))

    # Reorder `ls`
    vars[n[1]] <- "q11"
    vars <- vars[!grepl("^q11_", vars)]

    ls <- ls[vars]
  }

  ls
}

plot_sample <- function(df, var) {
  plot <- cb$viz$plot[cb$viz$var == var]
  axlab <- cb$viz$axis[cb$viz$var == var]

  if (grepl("bar", plot)) {
    if (plot == "vbargp") {
      pal <- c("#F04C43", "#666666")
      pal <- pal[as.numeric(df$pop)]
    } else {
      pal <- "#F04C43"
    }

    if (plot == "hbar") {
      lnudge <- 1.1
    } else {
      lnudge <- 1.5
    }

    df <- df |>
      mutate(fcolor = pal) |>
      mutate(fcolor = factor(fcolor, levels = unique(fcolor))) |>
      mutate(lpos = if_else(pct > max(pct) / 8, lnudge, -.25)) |>
      mutate(lcolor = if_else(lpos >= 1, "white", "black"))
  } else {
    pal <- c("#318CCC", "#F04C43")

    df <- df |>
      mutate(
        lbl = abs(n),
        fcolor = pal[as.numeric(df$birth_sex)],
        lpos = if_else(birth_sex == "Male", 1.2, -.2)
      )
  }

  if (plot == "hbar") {
    p <- df |>
      ggplot(aes(x = pct, y = .data[[var]], label = pct_fmt, fill = fcolor)) |>
      mhs_barplot(
        legend = FALSE,
        bar_reverse = TRUE,
        base_font_size = 11
      ) +
      scale_fill_identity() +
      geom_text(
        aes(label = pct_fmt, hjust = lpos),
        size = 11,
        size.unit = "pt",
        color = df$lcolor,
        fontface = "bold"
      ) +
      scale_x_continuous(expand = c(0, NA)) +
      scale_y_discrete(labels = ~ label_wrap(.x, 24)) +
      xlab("\nPercentage of respondents") +
      ylab(paste0(axlab, "\n"))
  } else if (plot == "vbar") {
    p <- df |>
      ggplot(aes(x = .data[[var]], y = pct, label = pct_fmt, fill = fcolor)) |>
      mhs_barplot(
        legend = FALSE,
        base_font_size = 11
      ) +
      scale_fill_identity() +
      geom_text(
        aes(label = pct_fmt, vjust = lpos),
        size = 11,
        size.unit = "pt",
        color = df$lcolor,
        fontface = "bold"
      ) +
      scale_x_discrete(labels = ~ label_wrap(.x, 16)) +
      scale_y_continuous(expand = c(0, NA)) +
      xlab(paste0("\n", axlab)) +
      ylab("Percentage of respondents\n")
  } else if (plot == "vbargp") {
    p <- df |>
      ggplot(aes(x = .data[[var]], y = pct, label = pct_fmt, fill = fcolor)) |>
      mhs_barplot(
        bar_position = "dodge",
        legend_title = FALSE,
        base_font_size = 11
      ) +
      scale_fill_identity(
        name = "Population",
        guide = "legend",
        label = levels(df$pop)
      ) +
      geom_text(
        aes(label = pct_fmt, vjust = lpos),
        position = position_dodge(width = .9),
        size = 11,
        size.unit = "pt",
        color = df$lcolor,
        fontface = "bold"
      ) +
      scale_x_discrete(labels = ~ label_wrap(.x, 16)) +
      scale_y_continuous(expand = c(0, NA)) +
      xlab(paste0("\n", axlab)) +
      ylab("Percentage of population\n") +
      theme(
        legend.position = "bottom"
      )
  } else if (plot == "pyr") {
    rngmax <- max(abs(df$n), na.rm = TRUE) + 5
    rng <- c(-rngmax, rngmax)

    p <- df |>
      ggplot(aes(x = n, y = age, label = lbl, fill = fcolor)) |>
      mhs_barplot(
        width = .8,
        legend_title = FALSE,
        base_font_size = 11
      ) +
      scale_fill_identity(
        name = "Birth sex",
        labels = df$birth_sex,
        breaks = df$fcolor,
        guide = "legend"
      ) +
      geom_text(
        aes(label = lbl, hjust = lpos),
        size = 11,
        size.unit = "pt",
        color = "white",
        fontface = "bold"
      ) +
      scale_x_continuous(labels = abs, limits = rng) +
      xlab("\nNumber of respondents") +
      ylab(paste0(axlab, "\n")) +
      theme(
        panel.grid.major.y = element_line(color = "grey92"),
        legend.position = "bottom"
      )
  }

  p
}

plot_data <- function(df, var, base_text_size = 11, label_text_size = 11) {
  plot <- cb$viz$plot[cb$viz$var == var]
  gp <- cb$viz$group[cb$viz$var == var]
  pal <- switch(
    gp,
    gen = "ocean", tr = "blueorange",
    si = "sunburst", sm = "candy", slp = "icecream"
  )
  msr <- cb$key$measure[cb$key$var2 == var]
  ncolors <- length(unique(df[[var]]))
  pal <- mhs_palette(pal, ncolors)

  if (plot == "vbar") {
    pal <- pal[seq_along(df[[var]])]
  } else if (plot == "hbar") {
    pal <- rev(pal)
  } else if (plot == "pie") {
    pal <- pal[as.numeric(df[[var]])]
  }

  if (plot == "hbar") {
    lnudge <- 1.1
  } else {
    lnudge <- 1.5
  }

  df <- df |>
    mutate(fcolor = pal) |>
    mutate(fcolor = factor(fcolor, levels = unique(fcolor))) |>
    mutate(lpos = if_else(pct > max(pct) / 10, lnudge, -.25)) |>
    mutate(lcolor = if_else(lpos >= 1, contrast_color(fcolor), "black"))

  if (plot == "vbar") {
    p <- df |>
      ggplot(aes(x = .data[[var]], y = pct, label = pct_fmt, fill = fcolor)) |>
      mhs_barplot(
        legend = FALSE,
        base_font_size = base_text_size
      ) +
      scale_fill_identity(
        name = label_wrap(msr, 16),
        labels = df[[var]],
        breaks = df$fcolor,
        guide = "legend"
      ) +
      geom_text(
        aes(label = pct_fmt, vjust = lpos),
        size = label_text_size,
        size.unit = "pt",
        color = df$lcolor,
        fontface = "bold"
      ) +
      scale_x_discrete(labels = ~ label_wrap(.x, 14)) +
      scale_y_continuous(expand = c(0, NA)) +
      xlab(paste0("\n", msr)) +
      ylab("Percentage of respondents\n")
  } else if (plot == "hbar") {
    p <- df |>
      ggplot(aes(x = pct, y = reason, label = pct_fmt, fill = fcolor)) |>
      mhs_barplot(
        legend = FALSE,
        base_font_size = base_text_size
      ) +
      scale_fill_identity() +
      geom_text(
        aes(label = pct_fmt, hjust = lpos),
        size = label_text_size,
        size.unit = "pt",
        color = df$lcolor,
        fontface = "bold"
      ) +
      scale_x_continuous(expand = c(0, NA)) +
      scale_y_discrete(labels = ~ label_wrap(.x, 38)) +
      xlab("\nPercentage of respondents") +
      ylab("Reason\n")
  } else if (plot == "pie") {
    df <- df |>
      mutate(lcolor = contrast_color(fcolor))

    p <- df |>
      ggplot(aes(x = "", y = pct, label = pct_fmt, fill = fcolor)) |>
      mhs_pie_chart(base_font_size = base_text_size) +
      scale_fill_identity(
        name = label_wrap(msr, 16),
        labels = df[[var]],
        breaks = df$fcolor,
        guide = "legend"
      ) +
      geom_text(
        aes(label = pct_fmt),
        position = position_stack(vjust = .5),
        size = label_text_size,
        size.unit = "pt",
        color = df$lcolor,
        fontface = "bold"
      )
  }

  p
}

plot_grouped_data <- function(
    df,
    var,
    fctr,
    label_min, # minimum value for normal labels
    offset = 0, # offset value for framed labels
    dir = c("h", "v"),
    text_size = 11) {
  plot <- cb$viz$plot[cb$viz$var == var]
  axis <- cb$viz$axis[cb$viz$var == fctr]
  gp <- cb$viz$group[cb$viz$var == var]
  pal <- switch(
    gp,
    gen = "ocean", tr = "blueorange",
    si = "sunburst", sm = "candy", slp = "icecream"
  )

  if (plot %in% c("vbar", "pie")) {
    msr <- cb$key$measure[cb$key$var2 == var]
    ncolors <- length(unique(df[[var]]))
    pal <- mhs_palette(pal, ncolors)

    df <- df |>
      # normal labels
      mutate(lbl = if_else(pct > label_min, pct_fmt, "")) |>
      mutate(fcolor = pal[as.numeric(df[[var]])]) |>
      mutate(fcolor = factor(fcolor, levels = unique(fcolor))) |>
      mutate(lcolor = contrast_color(fcolor)) |>
      # framed labels
      mutate(lbl2 = if_else(pct <= label_min, pct_fmt, "")) |>
      mutate(fcolor2 = if_else(pct > label_min, NA, fcolor)) |>
      mutate(lcolor2 = if_else(pct > label_min, NA, contrast_color(fcolor)))

    if (dir == "h") {
      p <- df |>
        ggplot(aes(x = pct, y = .data[[fctr]], label = lbl, fill = fcolor))
    } else if (dir == "v") {
      p <- df |>
        ggplot(aes(x = .data[[fctr]], y = pct, label = lbl, fill = fcolor))
    }

    p <- p |>
      mhs_barplot(
        base_font_size = text_size,
        bar_position = "stack",
        bar_reverse = TRUE
      ) +
      scale_fill_identity(
        name = label_wrap(msr, 16),
        labels = df[[var]],
        breaks = df$fcolor,
        guide = "legend"
      ) +
      geom_text(
        aes(label = lbl),
        position = position_stack(
          vjust = .5,
          reverse = TRUE
        ),
        color = df$lcolor,
        fontface = "bold"
      ) +
      geom_label(
        aes(label = lbl2),
        position = position_stack(
          vjust = .5 + offset,
          reverse = TRUE
        ),
        text.color = df$lcolor2,
        fill = df$fcolor2,
        border.color = colorspace::darken(df$fcolor2, .25),
        fontface = "bold",
        show.legend = FALSE
      )

    if (dir == "h") {
      p <- p +
        scale_x_continuous(expand = c(0, NA)) +
        scale_y_discrete(labels = ~ label_wrap(.x, 10)) +
        coord_cartesian(xlim = c(NA, 104)) +
        xlab(paste0("\nPercentage of respondents")) +
        ylab(paste0(label_wrap(axis, 24), "\n"))
    } else if (dir == "v") {
      p <- p +
        scale_y_continuous(expand = c(0, NA)) +
        scale_x_discrete(labels = ~ label_wrap(.x, 10)) +
        coord_cartesian(ylim = c(NA, 104)) +
        xlab(paste0("\n", label_wrap(axis, 24))) +
        ylab(paste0("Percentage of respondents\n"))
    }
  } else if (plot == "hbar") {
    ncolors <- length(unique(df[[fctr]]))
    pal <- mhs_palette("mellow", ncolors)

    df <- df |>
      mutate(fcolor = pal[as.numeric(df[[fctr]])]) |>
      mutate(fcolor = factor(fcolor, levels = unique(fcolor))) |>
      mutate(lcolor = contrast_color(fcolor))

    p <- df |>
      ggplot(aes(
        x = pct,
        y = reason,
        label = pct_fmt,
        fill = fcolor,
        group = .data[[fctr]]
      )) |>
      mhs_barplot(
        bar_position = "dodge",
        base_font_size = text_size,
        legend_reverse = TRUE
      ) +
      scale_fill_identity(
        name = label_wrap(axis, 16),
        labels = df[[fctr]],
        breaks = df$fcolor,
        guide = "legend"
      ) +
      geom_text(
        aes(label = pct_fmt, hjust = 1),
        position = position_dodge(width = .9),
        color = df$lcolor,
        fontface = "bold"
      ) +
      scale_x_continuous(expand = c(0, NA)) +
      xlab("\nPercentage of respondents") +
      ylab("Reason\n") +
      scale_y_discrete(labels = ~ label_wrap(.x, 32))
  }

  p
}

plot_hist <- function(df, x, y) {
  if (is.factor(df[[x]])) {
    nbins <- length(levels(df[[x]]))
    xbreaks <- 1:length(levels(df[[x]]))
    xlabs <- label_wrap(levels(df[[x]]), 8)
  } else if (is.numeric(df[[x]])) {
    nbins <- 10
    xbreaks <- waiver()
    xlabs <- waiver()
  }

  df |>
    ggplot(aes(
      x = as.numeric(.data[[x]]),
      y = .data[[y]],
      fill = .data[[y]]
    )) +
    geom_density_ridges(
      stat = "binline",
      bins = nbins,
      scale = .9,
      show.legend = FALSE
    ) +
    scale_x_continuous(
      breaks = xbreaks,
      labels = xlabs,
      expand = c(0, NA)
    ) +
    scale_y_discrete(
      expand = c(0, NA),
      labels = label_wrap(levels(df[[y]]), 8)
    ) +
    labs(x = "\nResponses", y = "Post-COVID-19 MH group\n") +
    theme_minimal(base_size = 16)
}

plot_box <- function(df, x, y) {
  df |>
    ggplot(aes(
      x = as.numeric(.data[[x]]),
      y = .data[[y]],
      fill = .data[[y]],
      color = .data[[y]]
    )) +
    geom_boxplot(color = "black") +
    geom_jitter(alpha = .4) +
    scale_x_continuous(
      labels = label_wrap(levels(df[[x]]), 8),
      breaks = sort(unique(as.numeric(df[[x]])))
    ) +
    scale_y_discrete(
      expand = c(0, NA),
      labels = label_wrap(levels(df[[y]]), 8)
    ) +
    labs(x = "\nResponses", y = "Post-COVID-19 MH group\n") +
    theme_minimal(base_size = 16) +
    theme(legend.position = "none")
}

plot_histobox <- function(df, x, y) {
  if (is.factor(df[[x]])) {
    nbins <- length(levels(df[[x]]))
    xbreaks <- 1:length(levels(df[[x]]))
    xlabs <- label_wrap(levels(df[[x]]), 8)
  } else if (is.numeric(df[[x]])) {
    nbins <- 10
    xbreaks <- waiver()
    xlabs <- waiver()
  }

  ymax <- max(table(df[[x]], df[[y]]), na.rm = TRUE)

  df |>
    ggplot(aes(
      x = as.numeric(.data[[x]]),
      fill = .data[[y]]
    )) +
    geom_histogram(
      bins = nbins
    ) +
    geom_boxplot(
      aes(y = ymax),
      width = ymax / 2,
      color = "black",
      fill = "yellow",
      alpha = .4
    ) +
    scale_x_continuous(
      breaks = xbreaks,
      labels = xlabs
    ) +
    facet_wrap(vars(.data[[y]]), ncol = 1) +
    labs(x = "\nResponses") +
    theme_minimal(base_size = 16) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      legend.position = "none"
    )
}

plot_density <- function(df, x, y) {
  # x-axis
  if (is.factor(df[[x]])) {
    xlabs <- label_wrap(levels(df[[x]]), 8)
    xpts <- waiver()
  } else if (is.numeric(df[[x]])) {
    xlabs <- waiver()
    xpts <- 10
  }

  # y-axis
  if (is.factor(df[[y]])) {
    ybreaks <- 1:length(levels(df[[y]]))
    ylabs <- ybreaks
    xpts <- length(unique(df[[x]]))
    ypts <- length(unique(df[[y]]))
  } else if (is.numeric(df[[y]])) {
    ybreaks <- waiver()
    ylabs <- waiver()
    ypts <- 10
  }

  df |>
    ggplot(aes(
      x = as.numeric(.data[[x]]),
      y = as.numeric(.data[[y]])
    )) +
    stat_density_2d(
      aes(size = after_stat(density)),
      geom = "point",
      contour = FALSE,
      n = c(xpts, ypts)
    ) +
    scale_x_continuous(
      labels = xlabs
    ) +
    scale_y_continuous(
      breaks = ybreaks,
      labels = ylabs
    ) +
    labs(
      x = "\nPost-COVID-19 MH group",
      y = "Responses\n"
    ) +
    theme_minimal(base_size = 16)
}

plot_points <- function(df, x, y) {
  # x-axis
  if (is.factor(df[[x]])) {
    xlabs <- label_wrap(levels(df[[x]]), 8)
  } else if (is.numeric(df[[x]])) {
    xlabs <- waiver()
    # xpts <- 10
  }

  df |>
    ggplot(aes(
      x = .data[[x]],
      y = .data[[y]]
    )) +
    geom_jitter(alpha = .2) +
    scale_x_discrete(
      labels = xlabs
    ) +
    labs(
      x = "\nPost-COVID-19 MH group",
      y = "Responses\n"
    ) +
    theme_minimal(base_size = 16)
}

postcov_tests <- function(df, var) {
  # Question responses to numeric
  df <- df |>
    select(starts_with("postcov"), all_of(var)) |>
    mutate(q = as.numeric(.data[[var]])) |>
    drop_na(q, postcov5)

  # Levene test for equal variances
  lev <- car::leveneTest(q ~ postcov5, df)
  levp <- lev$`Pr(>F)`[1]

  # T-test
  df_ttest <- df |>
    filter(postcov3 %in% c("Better", "Worse"))

  if (levp < .05) { # parametric
    t <- t.test(q ~ postcov3, df_ttest, var.equal = TRUE, alternative = "less")
  } else if (levp >= .05) { # nonparametric
    t <- wilcox.test(q ~ postcov3, df_ttest, alternative = "less")
  }

  # Predictor to numeric
  df2 <- df |>
    mutate(postcov5 = as.numeric(postcov5))

  # Correlation test
  q_cortest <- purrr::quietly(cor.test)
  cor <- q_cortest(~ q + postcov5, df2, method = "spearman")

  # Histogram + boxplot
  p1 <- plot_histobox(df, var, "postcov5")

  # 2D density plots
  p2 <- plot_density(df, "postcov5", var)

  p2a <- p2 +
    geom_smooth(method = "lm")

  p2b <- p2 +
    geom_smooth(method = "loess")

  pw <- wrap_plots(
    p2a, plot_spacer(), p2b,
    ncol = 1, heights = c(5, 1, 5),
    axes = "collect", guides = "collect"
  )

  pw <- wrap_plots(
    free(p1), plot_spacer(), pw,
    nrow = 1, widths = c(6, .5, 4)
  )

  list(
    lev_test = lev,
    t_test = t,
    cor_test = cor,
    plot = pw
  )
}

cor_tests <- function(df, x, y) {
  # Drop NAs
  df <- df |>
    drop_na(all_of(c(x, y)))

  # Histograms
  set_binwidth <- function(values) {
    rng <- max(values, na.rm = TRUE) - min(values, na.rm = TRUE)
    if (rng / 15 < 1) 1 else ceiling(rng / 15)
  }

  p1a <- df |>
    ggplot(aes(x = .data[[x]])) +
    geom_histogram(binwidth = set_binwidth(df[[x]]))

  p1b <- df |>
    ggplot(aes(x = .data[[y]])) +
    geom_histogram(binwidth = set_binwidth(df[[y]]))

  p1 <- wrap_plots(p1a, p1b)

  # Scatter plots
  p2 <- df |>
    ggplot(aes(
      x = .data[[x]],
      y = .data[[y]]
    )) +
    geom_jitter(alpha = .2)

  p2a <- p2 +
    geom_smooth(method = "lm")

  p2b <- p2 +
    geom_smooth(method = "loess")

  p2 <- wrap_plots(p2a, p2b)

  # Correlation tests
  ct <- quietly(cor.test)

  cor_p <- ct(df[[x]], df[[y]], method = "pearson")
  cor_s <- ct(df[[x]], df[[y]], method = "spearman")

  rem <- discard(cor_p[2:4], \(x) length(x) == 0 || x == "")
  cor_p <- list(broom::tidy(cor_p$result), rem)

  rem <- discard(cor_s[2:4], \(x) length(x) == 0 || x == "")
  cor_s <- list(broom::tidy(cor_s$result), rem)

  list(
    hist = p1,
    scatter = p2,
    pearson = cor_p,
    spearman = cor_s
  )
}
