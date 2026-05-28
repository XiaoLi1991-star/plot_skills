suppressPackageStartupMessages({
  library(optparse)
  library(ggplot2)
  library(ggrepel)
  library(codex.rplot)
})

option_list <- list(
  make_option("--input_matrix", type = "character", default = NULL, help = "Feature x sample matrix"),
  make_option("--sample_info", type = "character", default = "", help = "Optional sample metadata"),
  make_option("--id_col", type = "character", default = "", help = "Feature ID column; empty uses first column"),
  make_option("--sample_col", type = "character", default = "sample", help = "Sample column in sample_info"),
  make_option("--group_col", type = "character", default = "group", help = "Group column in sample_info"),
  make_option("--scale", type = "logical", default = TRUE, help = "Scale features before PCA"),
  make_option("--add_ellipse", type = "logical", default = TRUE, help = "Add confidence ellipse when groups exist"),
  make_option("--ellipse_level", type = "numeric", default = 0.95, help = "Ellipse confidence level"),
  make_option("--show_label", type = "logical", default = TRUE, help = "Show sample labels"),
  make_option("--point_size", type = "numeric", default = 3, help = "Point size"),
  make_option("--label_text_size", type = "numeric", default = 3, help = "Label text size"),
  make_option("--palette", type = "character", default = "okabe_ito", help = "Palette"),
  make_option("--theme", type = "character", default = "auto", help = "Theme: auto, rplot, minimal, classic, bw, light, grey, void"),
  make_option("--title", type = "character", default = "PCA", help = "Plot title"),
  make_option("--base_font_size", type = "numeric", default = 12, help = "Base font size"),
  make_option("--fig_width", type = "numeric", default = 7.5, help = "Figure width"),
  make_option("--fig_height", type = "numeric", default = 5.8, help = "Figure height"),
  make_option("--plot_name", type = "character", default = "pca_plot", help = "Output prefix without extension")
)

opt <- parse_args(OptionParser(option_list = option_list))
if (is.null(opt$input_matrix)) {
  print_help(OptionParser(option_list = option_list))
  stop("--input_matrix is required.")
}

data <- read_input_table(opt$input_matrix)
id_col <- ifelse(nzchar(opt$id_col), opt$id_col, colnames(data)[1])
require_columns(data, id_col)
sample_cols <- setdiff(numeric_columns(data), id_col)
if (length(sample_cols) < 3) {
  stop("PCA requires at least three numeric sample columns.")
}
data <- coerce_numeric(data, sample_cols)
mat <- as.matrix(data[, sample_cols, drop = FALSE])
rownames(mat) <- make.unique(as.character(data[[id_col]]))
mat <- mat[apply(mat, 1, function(x) all(is.finite(x))), , drop = FALSE]

pca <- prcomp(t(mat), center = TRUE, scale. = opt$scale)
var_exp <- (pca$sdev^2) / sum(pca$sdev^2) * 100
plot_data <- data.frame(
  sample = rownames(pca$x),
  PC1 = pca$x[, 1],
  PC2 = pca$x[, 2],
  stringsAsFactors = FALSE
)

group_col <- ""
if (nzchar(opt$sample_info)) {
  sample_info <- read_input_table(opt$sample_info)
  require_columns(sample_info, c(opt$sample_col, opt$group_col))
  plot_data <- merge(plot_data, sample_info, by.x = "sample", by.y = opt$sample_col, all.x = TRUE, sort = FALSE)
  group_col <- opt$group_col
}

p <- ggplot(plot_data, aes(x = PC1, y = PC2))
if (nzchar(group_col)) {
  p <- p + geom_point(aes(color = .data[[group_col]]), size = opt$point_size)
  p <- add_discrete_scale(p, "color", opt$palette)
  if (opt$add_ellipse && length(unique(stats::na.omit(plot_data[[group_col]]))) > 1) {
    p <- p + stat_ellipse(aes(color = .data[[group_col]]), level = opt$ellipse_level, linewidth = 0.5, show.legend = FALSE)
  }
} else {
  p <- p + geom_point(size = opt$point_size, color = "#3B6EA8")
}
if (opt$show_label) {
  p <- p + geom_text_repel(aes(label = sample), size = opt$label_text_size, max.overlaps = Inf, show.legend = FALSE)
}

p <- p +
  theme_rplot(opt$base_font_size, opt$theme)

label_args <- list(
  title = opt$title,
  x = sprintf("PC1 (%.1f%%)", var_exp[1]),
  y = sprintf("PC2 (%.1f%%)", var_exp[2])
)
if (nzchar(group_col)) {
  label_args$color <- group_col
}
p <- p + do.call(labs, label_args)

save_ggplot(p, opt$plot_name, opt$fig_width, opt$fig_height)
