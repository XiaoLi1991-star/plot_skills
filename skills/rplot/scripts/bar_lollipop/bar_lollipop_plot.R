suppressPackageStartupMessages({
  library(optparse)
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(codex.rplot)
})

option_list <- list(
  make_option("--data_input", type = "character", default = NULL, help = "Input table path"),
  make_option("--category_col", type = "character", default = NULL, help = "Category column"),
  make_option("--value_col", type = "character", default = NULL, help = "Numeric value column"),
  make_option("--color_col", type = "character", default = "", help = "Optional color/group column"),
  make_option("--mode", type = "character", default = "lollipop", help = "bar or lollipop"),
  make_option("--top_n", type = "numeric", default = 30, help = "Top N rows by value, 0 means all"),
  make_option("--descending", type = "logical", default = TRUE, help = "Sort descending"),
  make_option("--label_values", type = "logical", default = TRUE, help = "Show value labels"),
  make_option("--label_digits", type = "numeric", default = 2, help = "Digits in labels"),
  make_option("--wrap_width", type = "numeric", default = 42, help = "Category label wrap width"),
  make_option("--palette", type = "character", default = "editorial", help = "Palette"),
  make_option("--theme", type = "character", default = "auto", help = "Theme: auto, rplot, minimal, classic, bw, light, grey, void"),
  make_option("--title", type = "character", default = "", help = "Plot title"),
  make_option("--x_title", type = "character", default = "", help = "X axis title"),
  make_option("--y_title", type = "character", default = "", help = "Y axis title"),
  make_option("--base_font_size", type = "numeric", default = 12, help = "Base font size"),
  make_option("--fig_width", type = "numeric", default = 8, help = "Figure width"),
  make_option("--fig_height", type = "numeric", default = 6, help = "Figure height"),
  make_option("--plot_name", type = "character", default = "bar_lollipop_plot", help = "Output prefix without extension")
)

opt <- parse_args(OptionParser(option_list = option_list))
if (is.null(opt$data_input) || is.null(opt$category_col) || is.null(opt$value_col)) {
  print_help(OptionParser(option_list = option_list))
  stop("--data_input, --category_col and --value_col are required.")
}

data <- read_input_table(opt$data_input)
require_columns(data, c(opt$category_col, opt$value_col, opt$color_col))
data <- coerce_numeric(data, opt$value_col)
data <- data[!is.na(data[[opt$category_col]]) & !is.na(data[[opt$value_col]]), , drop = FALSE]

if (opt$descending) {
  data <- data %>% arrange(desc(.data[[opt$value_col]]))
} else {
  data <- data %>% arrange(.data[[opt$value_col]])
}
if (opt$top_n > 0 && nrow(data) > opt$top_n) {
  data <- data %>% slice_head(n = opt$top_n)
}
data$.category_wrapped <- wrap_labels(as.character(data[[opt$category_col]]), opt$wrap_width)
data$.category_wrapped <- factor(data$.category_wrapped, levels = rev(unique(data$.category_wrapped)))
if (opt$label_values) {
  data$.label <- round(data[[opt$value_col]], opt$label_digits)
}

p <- ggplot(data, aes(x = .data[[opt$value_col]], y = .category_wrapped))
if (nzchar(opt$color_col)) {
  if (opt$mode == "bar") {
    p <- p + geom_col(aes(fill = .data[[opt$color_col]]), width = 0.72)
    p <- add_discrete_scale(p, "fill", opt$palette)
  } else {
    p <- p + geom_segment(aes(x = 0, xend = .data[[opt$value_col]], yend = .category_wrapped, color = .data[[opt$color_col]]), linewidth = 0.65)
    p <- p + geom_point(aes(color = .data[[opt$color_col]]), size = 3.1)
    p <- add_discrete_scale(p, "color", opt$palette)
  }
} else {
  if (opt$mode == "bar") {
    p <- p + geom_col(width = 0.72, fill = "#3B6EA8")
  } else {
    p <- p + geom_segment(aes(x = 0, xend = .data[[opt$value_col]], yend = .category_wrapped), linewidth = 0.65, color = "grey55")
    p <- p + geom_point(size = 3.1, color = "#3B6EA8")
  }
}

if (opt$label_values) {
  p <- p + geom_text(aes(label = .label), hjust = -0.12, size = 3.2, color = "grey15")
}

label_args <- list(
  title = opt$title,
  x = ifelse(nzchar(opt$x_title), opt$x_title, opt$value_col),
  y = ifelse(nzchar(opt$y_title), opt$y_title, opt$category_col)
)
if (nzchar(opt$color_col)) {
  if (opt$mode == "bar") {
    label_args$fill <- opt$color_col
  } else {
    label_args$color <- opt$color_col
  }
}

p <- p +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  theme_rplot(opt$base_font_size, opt$theme) +
  do.call(labs, label_args)

save_ggplot(p, opt$plot_name, opt$fig_width, opt$fig_height)
