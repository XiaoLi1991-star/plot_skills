suppressPackageStartupMessages({
  library(optparse)
  library(pheatmap)
  library(codex.rplot)
})

option_list <- list(
  make_option("--input_matrix", type = "character", default = NULL, help = "Feature x sample matrix"),
  make_option("--sample_info", type = "character", default = "", help = "Optional sample annotation table"),
  make_option("--id_col", type = "character", default = "", help = "Feature ID column; empty uses first column"),
  make_option("--sample_col", type = "character", default = "sample", help = "Sample column in sample_info"),
  make_option("--group_col", type = "character", default = "group", help = "Group column in sample_info"),
  make_option("--columns", type = "character", default = "", help = "Sample columns to keep; empty means all numeric columns after id_col"),
  make_option("--top_n_variable", type = "numeric", default = 80, help = "Top N variable rows, 0 means all"),
  make_option("--scale", type = "character", default = "row", help = "none, row, or column"),
  make_option("--cluster_rows", type = "logical", default = TRUE, help = "Cluster rows"),
  make_option("--cluster_cols", type = "logical", default = TRUE, help = "Cluster columns"),
  make_option("--show_rownames", type = "logical", default = FALSE, help = "Show row names"),
  make_option("--show_colnames", type = "logical", default = TRUE, help = "Show column names"),
  make_option("--low_color", type = "character", default = "#2166AC", help = "Low color"),
  make_option("--mid_color", type = "character", default = "#F7F7F7", help = "Mid color"),
  make_option("--high_color", type = "character", default = "#B2182B", help = "High color"),
  make_option("--theme", type = "character", default = "auto", help = "Accepted for RPlot CLI consistency; pheatmap rendering is controlled by heatmap-specific options"),
  make_option("--fig_width", type = "numeric", default = 8, help = "Figure width"),
  make_option("--fig_height", type = "numeric", default = 7, help = "Figure height"),
  make_option("--plot_name", type = "character", default = "matrix_heatmap", help = "Output prefix without extension")
)

opt <- parse_args(OptionParser(option_list = option_list))
if (is.null(opt$input_matrix)) {
  print_help(OptionParser(option_list = option_list))
  stop("--input_matrix is required.")
}

data <- read_input_table(opt$input_matrix)
id_col <- ifelse(nzchar(opt$id_col), opt$id_col, colnames(data)[1])
require_columns(data, id_col)

cols <- parse_list(opt$columns)
if (length(cols) == 0) {
  cols <- setdiff(numeric_columns(data), id_col)
}
require_columns(data, cols)
data <- coerce_numeric(data, cols)
mat <- as.matrix(data[, cols, drop = FALSE])
rownames(mat) <- make.unique(as.character(data[[id_col]]))
mat <- mat[apply(mat, 1, function(x) any(!is.na(x))), , drop = FALSE]

if (opt$top_n_variable > 0 && nrow(mat) > opt$top_n_variable) {
  vars <- apply(mat, 1, stats::var, na.rm = TRUE)
  keep <- names(sort(vars, decreasing = TRUE))[seq_len(opt$top_n_variable)]
  mat <- mat[keep, , drop = FALSE]
}

annotation_col <- NULL
if (nzchar(opt$sample_info)) {
  sample_info <- read_input_table(opt$sample_info)
  require_columns(sample_info, c(opt$sample_col, opt$group_col))
  rownames(sample_info) <- sample_info[[opt$sample_col]]
  matched <- intersect(colnames(mat), rownames(sample_info))
  if (length(matched) > 0) {
    mat <- mat[, matched, drop = FALSE]
    annotation_col <- sample_info[matched, opt$group_col, drop = FALSE]
    colnames(annotation_col) <- opt$group_col
  }
}

paths <- output_paths(opt$plot_name)
colors <- colorRampPalette(c(opt$low_color, opt$mid_color, opt$high_color))(100)
scale_mode <- ifelse(opt$scale %in% c("row", "column", "none"), opt$scale, "row")
pheatmap::pheatmap(
  mat,
  color = colors,
  scale = scale_mode,
  cluster_rows = opt$cluster_rows,
  cluster_cols = opt$cluster_cols,
  show_rownames = opt$show_rownames,
  show_colnames = opt$show_colnames,
  annotation_col = annotation_col,
  filename = paths$pdf,
  width = opt$fig_width,
  height = opt$fig_height
)
pheatmap::pheatmap(
  mat,
  color = colors,
  scale = scale_mode,
  cluster_rows = opt$cluster_rows,
  cluster_cols = opt$cluster_cols,
  show_rownames = opt$show_rownames,
  show_colnames = opt$show_colnames,
  annotation_col = annotation_col,
  filename = paths$png,
  width = opt$fig_width,
  height = opt$fig_height
)
