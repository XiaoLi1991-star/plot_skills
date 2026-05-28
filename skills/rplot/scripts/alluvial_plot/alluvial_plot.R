suppressPackageStartupMessages({
  library(optparse)
  library(dplyr)
  library(ggplot2)
  library(ggalluvial)
  library(stringr)
  library(codex.rplot)
})

option_list <- list(
  make_option("--data_input", type = "character", default = NULL, help = "Input table path"),
  make_option("--axis_cols", type = "character", default = NULL, help = "Categorical columns, comma/semicolon separated"),
  make_option("--weight_col", type = "character", default = "", help = "Optional numeric weight column"),
  make_option("--fill_col", type = "character", default = "", help = "Optional fill column; default first axis column"),
  make_option("--top_n_paths", type = "numeric", default = 80, help = "Top N aggregated paths by weight, 0 means all"),
  make_option("--label_wrap_width", type = "numeric", default = 22, help = "Stratum label wrap width"),
  make_option("--palette", type = "character", default = "calm", help = "Palette"),
  make_option("--theme", type = "character", default = "auto", help = "Theme: auto, rplot, minimal, classic, bw, light, grey, void"),
  make_option("--title", type = "character", default = "", help = "Plot title"),
  make_option("--base_font_size", type = "numeric", default = 12, help = "Base font size"),
  make_option("--fig_width", type = "numeric", default = 9, help = "Figure width"),
  make_option("--fig_height", type = "numeric", default = 5.8, help = "Figure height"),
  make_option("--plot_name", type = "character", default = "alluvial_plot", help = "Output prefix without extension")
)

opt <- parse_args(OptionParser(option_list = option_list))
if (is.null(opt$data_input) || is.null(opt$axis_cols)) {
  print_help(OptionParser(option_list = option_list))
  stop("--data_input and --axis_cols are required.")
}

data <- read_input_table(opt$data_input)
axis_cols <- parse_list(opt$axis_cols)
if (length(axis_cols) < 2) {
  stop("--axis_cols must contain at least two categorical columns.")
}
fill_col <- ifelse(nzchar(opt$fill_col), opt$fill_col, axis_cols[1])
require_columns(data, c(axis_cols, opt$weight_col, fill_col))

if (nzchar(opt$weight_col)) {
  data <- coerce_numeric(data, opt$weight_col)
  data$.weight <- data[[opt$weight_col]]
} else {
  data$.weight <- 1
}
for (col in axis_cols) {
  data[[col]] <- wrap_labels(as.character(data[[col]]), opt$label_wrap_width)
}
if (fill_col %in% axis_cols) {
  data[[fill_col]] <- wrap_labels(as.character(data[[fill_col]]), opt$label_wrap_width)
}
data$.fill_value <- as.character(data[[fill_col]])

plot_data <- data %>%
  group_by(across(all_of(c(axis_cols, ".fill_value")))) %>%
  summarize(.weight = sum(.weight, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(.weight))

if (opt$top_n_paths > 0 && nrow(plot_data) > opt$top_n_paths) {
  plot_data <- plot_data %>% slice_head(n = opt$top_n_paths)
}

plot_data$.alluvium_id <- seq_len(nrow(plot_data))
lodes <- ggalluvial::to_lodes_form(
  plot_data,
  axes = axis_cols,
  id = ".alluvium_id",
  key = ".axis",
  value = ".stratum"
)
lodes$.axis <- factor(lodes$.axis, levels = axis_cols)

p <- ggplot(lodes, aes(x = .axis, stratum = .stratum, alluvium = .alluvium_id, y = .weight, fill = .fill_value)) +
  geom_flow(stat = "alluvium", alpha = 0.78, color = "white", linewidth = 0.15) +
  geom_stratum(width = 0.16, color = "grey30", fill = "grey96", show.legend = FALSE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3, color = "grey10", show.legend = FALSE) +
  scale_x_discrete(expand = c(0.05, 0.05)) +
  theme_rplot(opt$base_font_size, opt$theme) +
  theme(panel.grid = element_blank()) +
  labs(title = opt$title, x = NULL, y = ifelse(nzchar(opt$weight_col), opt$weight_col, "Count"), fill = fill_col)
p <- add_discrete_scale(p, "fill", opt$palette)

save_ggplot(p, opt$plot_name, opt$fig_width, opt$fig_height)
