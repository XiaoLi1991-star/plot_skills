suppressPackageStartupMessages({
  library(optparse)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(codex.rplot)
})

options(lifecycle_verbosity = "quiet")
suppressPackageStartupMessages(library(ComplexUpset))

option_list <- list(
  make_option("--data_input", type = "character", default = NULL, help = "Input table path"),
  make_option("--item_col", type = "character", default = "item", help = "Item ID column"),
  make_option("--set_col", type = "character", default = "", help = "Long-format set/group column"),
  make_option("--set_cols", type = "character", default = "", help = "Wide-format set columns, comma/semicolon separated"),
  make_option("--min_size", type = "numeric", default = 1, help = "Minimum intersection size"),
  make_option("--sort_intersections", type = "character", default = "descending", help = "descending, ascending, or none"),
  make_option("--theme", type = "character", default = "auto", help = "Theme: auto, rplot, minimal, classic, bw, light, grey, void"),
  make_option("--base_font_size", type = "numeric", default = 12, help = "Base font size"),
  make_option("--fig_width", type = "numeric", default = 8.5, help = "Figure width"),
  make_option("--fig_height", type = "numeric", default = 5.5, help = "Figure height"),
  make_option("--plot_name", type = "character", default = "upset_plot", help = "Output prefix without extension")
)

opt <- parse_args(OptionParser(option_list = option_list))
if (is.null(opt$data_input)) {
  print_help(OptionParser(option_list = option_list))
  stop("--data_input is required.")
}

data <- read_input_table(opt$data_input)
sets <- parse_list(opt$set_cols)

if (length(sets) > 0) {
  require_columns(data, c(opt$item_col, sets))
  wide <- data[, c(opt$item_col, sets), drop = FALSE]
  for (set in sets) {
    if (!is.logical(wide[[set]])) {
      wide[[set]] <- as.numeric(wide[[set]]) > 0 | tolower(as.character(wide[[set]])) %in% c("true", "yes", "y", "present")
    }
  }
} else if (nzchar(opt$set_col)) {
  require_columns(data, c(opt$item_col, opt$set_col))
  long <- unique(data[, c(opt$item_col, opt$set_col), drop = FALSE])
  long$.present <- TRUE
  wide <- long %>%
    tidyr::pivot_wider(names_from = all_of(opt$set_col), values_from = .present, values_fill = FALSE)
  sets <- setdiff(colnames(wide), opt$item_col)
} else {
  stop("Provide either --set_col for long input or --set_cols for wide input.")
}

if (length(sets) < 2) {
  stop("UpSet plot requires at least two sets.")
}

p <- ComplexUpset::upset(
  wide,
  intersect = sets,
  min_size = opt$min_size,
  sort_intersections = opt$sort_intersections,
  base_annotations = list("Intersection size" = intersection_size(text = list(size = 3))),
  width_ratio = 0.18
) +
  theme_rplot(opt$base_font_size, opt$theme)

suppressWarnings(save_ggplot(p, opt$plot_name, opt$fig_width, opt$fig_height))
