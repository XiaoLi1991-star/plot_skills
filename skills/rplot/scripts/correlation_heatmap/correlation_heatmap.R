suppressPackageStartupMessages({
  library(optparse)
  library(ggplot2)
  library(reshape2)
  library(codex.rplot)
})

option_list <- list(
  make_option("--data_input", type = "character", default = NULL, help = "Input table path"),
  make_option("--columns", type = "character", default = "", help = "Columns to correlate, comma/semicolon separated; empty means all numeric columns"),
  make_option("--method", type = "character", default = "pearson", help = "pearson, spearman, or kendall"),
  make_option("--cluster", type = "logical", default = TRUE, help = "Cluster rows and columns"),
  make_option("--show_numbers", type = "logical", default = TRUE, help = "Show correlation values"),
  make_option("--number_size", type = "numeric", default = 3, help = "Number text size"),
  make_option("--low_color", type = "character", default = "#2166AC", help = "Low color"),
  make_option("--mid_color", type = "character", default = "#F7F7F7", help = "Mid color"),
  make_option("--high_color", type = "character", default = "#B2182B", help = "High color"),
  make_option("--theme", type = "character", default = "auto", help = "Theme: auto, rplot, minimal, classic, bw, light, grey, void"),
  make_option("--title", type = "character", default = "", help = "Plot title"),
  make_option("--base_font_size", type = "numeric", default = 12, help = "Base font size"),
  make_option("--fig_width", type = "numeric", default = 7, help = "Figure width"),
  make_option("--fig_height", type = "numeric", default = 6.5, help = "Figure height"),
  make_option("--plot_name", type = "character", default = "correlation_heatmap", help = "Output prefix without extension")
)

opt <- parse_args(OptionParser(option_list = option_list))
if (is.null(opt$data_input)) {
  print_help(OptionParser(option_list = option_list))
  stop("--data_input is required.")
}

data <- read_input_table(opt$data_input)
cols <- numeric_columns(data, opt$columns)
if (length(cols) < 2) {
  stop("At least two numeric columns are required for correlation heatmap.")
}
data <- coerce_numeric(data, cols)
corr <- cor(data[, cols, drop = FALSE], use = "pairwise.complete.obs", method = opt$method)

if (opt$cluster && ncol(corr) > 2) {
  order <- hclust(as.dist(1 - corr))$order
  corr <- corr[order, order, drop = FALSE]
}

plot_data <- reshape2::melt(corr, varnames = c("row", "col"), value.name = "correlation")
plot_data$row <- factor(plot_data$row, levels = rev(rownames(corr)))
plot_data$col <- factor(plot_data$col, levels = colnames(corr))

p <- ggplot(plot_data, aes(x = col, y = row, fill = correlation)) +
  geom_tile(color = "white", linewidth = 0.45) +
  scale_fill_gradient2(low = opt$low_color, mid = opt$mid_color, high = opt$high_color, midpoint = 0, limits = c(-1, 1)) +
  coord_equal() +
  theme_rplot(opt$base_font_size, opt$theme) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank()) +
  labs(title = opt$title, x = NULL, y = NULL, fill = opt$method)

if (opt$show_numbers) {
  p <- p + geom_text(aes(label = sprintf("%.2f", correlation)), size = opt$number_size, color = "grey10")
}

save_ggplot(p, opt$plot_name, opt$fig_width, opt$fig_height)
