suppressPackageStartupMessages({
  library(optparse)
  library(ggplot2)
  library(ggrepel)
  library(dplyr)
  library(stringr)
  library(codex.rplot)
})

option_list <- list(
  make_option("--data_input", type = "character", default = NULL, help = "Input table path"),
  make_option("--x_col", type = "character", default = NULL, help = "Column for X axis"),
  make_option("--y_col", type = "character", default = NULL, help = "Column for Y axis"),
  make_option("--color_col", type = "character", default = "", help = "Optional grouping/color column"),
  make_option("--size_col", type = "character", default = "", help = "Optional bubble size column"),
  make_option("--label_col", type = "character", default = "", help = "Optional label column"),
  make_option("--label_values", type = "character", default = "", help = "Labels to show, separated by comma or semicolon"),
  make_option("--label_top_n", type = "numeric", default = 0, help = "Show top N labels ranked by absolute Y"),
  make_option("--facet_col", type = "character", default = "", help = "Optional facet column"),
  make_option("--add_smooth", type = "logical", default = FALSE, help = "Add lm smooth line"),
  make_option("--point_size", type = "numeric", default = 2.8, help = "Default point size"),
  make_option("--point_alpha", type = "numeric", default = 0.82, help = "Point alpha"),
  make_option("--label_text_size", type = "numeric", default = 3, help = "Label text size"),
  make_option("--palette", type = "character", default = "okabe_ito", help = "Palette: okabe_ito, editorial, calm, vivid"),
  make_option("--theme", type = "character", default = "auto", help = "Theme: auto, rplot, minimal, classic, bw, light, grey, void"),
  make_option("--title", type = "character", default = "", help = "Plot title"),
  make_option("--x_title", type = "character", default = "", help = "X axis title"),
  make_option("--y_title", type = "character", default = "", help = "Y axis title"),
  make_option("--base_font_size", type = "numeric", default = 12, help = "Base font size"),
  make_option("--fig_width", type = "numeric", default = 7.5, help = "Figure width"),
  make_option("--fig_height", type = "numeric", default = 5.5, help = "Figure height"),
  make_option("--plot_name", type = "character", default = "scatter_bubble_plot", help = "Output prefix without extension")
)

opt <- parse_args(OptionParser(option_list = option_list))
if (is.null(opt$data_input) || is.null(opt$x_col) || is.null(opt$y_col)) {
  print_help(OptionParser(option_list = option_list))
  stop("--data_input, --x_col and --y_col are required.")
}

data <- read_input_table(opt$data_input)
required <- c(opt$x_col, opt$y_col, opt$color_col, opt$size_col, opt$label_col, opt$facet_col)
require_columns(data, required)
data <- coerce_numeric(data, c(opt$x_col, opt$y_col, opt$size_col))

p <- ggplot(data, aes(x = .data[[opt$x_col]], y = .data[[opt$y_col]]))
if (nzchar(opt$color_col) && nzchar(opt$size_col)) {
  p <- p + geom_point(aes(color = .data[[opt$color_col]], size = .data[[opt$size_col]]), alpha = opt$point_alpha)
} else if (nzchar(opt$color_col)) {
  p <- p + geom_point(aes(color = .data[[opt$color_col]]), size = opt$point_size, alpha = opt$point_alpha)
} else if (nzchar(opt$size_col)) {
  p <- p + geom_point(aes(size = .data[[opt$size_col]]), color = "#3B6EA8", alpha = opt$point_alpha)
} else {
  p <- p + geom_point(size = opt$point_size, color = "#3B6EA8", alpha = opt$point_alpha)
}

if (opt$add_smooth) {
  p <- p + geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.55, color = "grey25", fill = "grey75")
}

if (nzchar(opt$color_col)) {
  p <- add_discrete_scale(p, "color", opt$palette)
}

if (nzchar(opt$label_col)) {
  label_values <- parse_list(opt$label_values)
  label_data <- data[0, , drop = FALSE]
  if (length(label_values) > 0) {
    label_data <- data[data[[opt$label_col]] %in% label_values, , drop = FALSE]
  }
  if (opt$label_top_n > 0) {
    ranked <- data %>%
      filter(!is.na(.data[[opt$y_col]])) %>%
      arrange(desc(abs(.data[[opt$y_col]]))) %>%
      slice_head(n = opt$label_top_n)
    label_data <- bind_rows(label_data, ranked) %>% distinct(.data[[opt$label_col]], .keep_all = TRUE)
  }
  if (nrow(label_data) > 0) {
    p <- p + geom_text_repel(
      data = label_data,
      aes(label = .data[[opt$label_col]]),
      size = opt$label_text_size,
      color = "grey10",
      min.segment.length = 0,
      box.padding = 0.35,
      point.padding = 0.2,
      max.overlaps = Inf,
      show.legend = FALSE
    )
  }
}

if (nzchar(opt$facet_col)) {
  p <- p + facet_wrap(stats::as.formula(paste("~", opt$facet_col)))
}

p <- p +
  scale_x_continuous(expand = expansion(mult = c(0.06, 0.16))) +
  scale_y_continuous(expand = expansion(mult = c(0.08, 0.12))) +
  coord_cartesian(clip = "off") +
  theme_rplot(opt$base_font_size, opt$theme)

label_args <- list(
  title = opt$title,
  x = ifelse(nzchar(opt$x_title), opt$x_title, opt$x_col),
  y = ifelse(nzchar(opt$y_title), opt$y_title, opt$y_col)
)
if (nzchar(opt$color_col)) {
  label_args$color <- opt$color_col
}
if (nzchar(opt$size_col)) {
  label_args$size <- opt$size_col
}
p <- p + do.call(labs, label_args)

save_ggplot(p, opt$plot_name, opt$fig_width, opt$fig_height)
