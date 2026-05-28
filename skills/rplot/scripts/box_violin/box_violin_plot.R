suppressPackageStartupMessages({
  library(optparse)
  library(dplyr)
  library(ggplot2)
  library(codex.rplot)
})

option_list <- list(
  make_option("--data_input", type = "character", default = NULL, help = "Input table path"),
  make_option("--x_col", type = "character", default = NULL, help = "Category column"),
  make_option("--y_col", type = "character", default = NULL, help = "Numeric value column"),
  make_option("--fill_col", type = "character", default = "", help = "Optional fill/group column"),
  make_option("--facet_col", type = "character", default = "", help = "Optional facet column"),
  make_option("--mode", type = "character", default = "box_violin", help = "box, violin, or box_violin"),
  make_option("--show_points", type = "logical", default = TRUE, help = "Overlay jittered points"),
  make_option("--point_size", type = "numeric", default = 1.3, help = "Point size"),
  make_option("--point_alpha", type = "numeric", default = 0.55, help = "Point alpha"),
  make_option("--box_width", type = "numeric", default = 0.18, help = "Box width"),
  make_option("--violin_alpha", type = "numeric", default = 0.68, help = "Violin alpha"),
  make_option("--sort_by_median", type = "logical", default = FALSE, help = "Sort categories by median"),
  make_option("--palette", type = "character", default = "okabe_ito", help = "Palette"),
  make_option("--theme", type = "character", default = "auto", help = "Theme: auto, rplot, minimal, classic, bw, light, grey, void"),
  make_option("--title", type = "character", default = "", help = "Plot title"),
  make_option("--x_title", type = "character", default = "", help = "X axis title"),
  make_option("--y_title", type = "character", default = "", help = "Y axis title"),
  make_option("--base_font_size", type = "numeric", default = 12, help = "Base font size"),
  make_option("--fig_width", type = "numeric", default = 7.5, help = "Figure width"),
  make_option("--fig_height", type = "numeric", default = 5.5, help = "Figure height"),
  make_option("--plot_name", type = "character", default = "box_violin_plot", help = "Output prefix without extension")
)

opt <- parse_args(OptionParser(option_list = option_list))
if (is.null(opt$data_input) || is.null(opt$x_col) || is.null(opt$y_col)) {
  print_help(OptionParser(option_list = option_list))
  stop("--data_input, --x_col and --y_col are required.")
}

data <- read_input_table(opt$data_input)
require_columns(data, c(opt$x_col, opt$y_col, opt$fill_col, opt$facet_col))
data <- coerce_numeric(data, opt$y_col)
data <- data[!is.na(data[[opt$x_col]]) & !is.na(data[[opt$y_col]]), , drop = FALSE]

if (opt$sort_by_median) {
  order_df <- data %>%
    group_by(.data[[opt$x_col]]) %>%
    summarize(.median = median(.data[[opt$y_col]], na.rm = TRUE), .groups = "drop") %>%
    arrange(.median)
  data[[opt$x_col]] <- factor(data[[opt$x_col]], levels = order_df[[opt$x_col]])
}

fill_col <- ifelse(nzchar(opt$fill_col), opt$fill_col, opt$x_col)
p <- ggplot(data, aes(x = .data[[opt$x_col]], y = .data[[opt$y_col]], fill = .data[[fill_col]]))

if (opt$mode %in% c("violin", "box_violin")) {
  p <- p + geom_violin(width = 0.9, trim = FALSE, alpha = opt$violin_alpha, color = NA)
}
if (opt$mode %in% c("box", "box_violin")) {
  p <- p + geom_boxplot(width = opt$box_width, outlier.shape = NA, alpha = 0.9, color = "grey20")
}
if (opt$show_points) {
  p <- p + geom_jitter(width = 0.13, size = opt$point_size, alpha = opt$point_alpha, color = "grey15")
}
if (nzchar(opt$facet_col)) {
  p <- p + facet_wrap(stats::as.formula(paste("~", opt$facet_col)), scales = "free_x")
}

p <- add_discrete_scale(p, "fill", opt$palette) +
  theme_rplot(opt$base_font_size, opt$theme) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1), legend.position = ifelse(nzchar(opt$fill_col), "right", "none")) +
  labs(
    title = opt$title,
    x = ifelse(nzchar(opt$x_title), opt$x_title, opt$x_col),
    y = ifelse(nzchar(opt$y_title), opt$y_title, opt$y_col),
    fill = fill_col
  )

save_ggplot(p, opt$plot_name, opt$fig_width, opt$fig_height)
