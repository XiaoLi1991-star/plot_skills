suppressPackageStartupMessages({
  library(optparse)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(scales)
  library(codex.rplot)
})

option_list <- list(
  make_option("--data_input", type = "character", default = NULL, help = "Input table path"),
  make_option("--mode", type = "character", default = "concept_heatmap", help = "Article-inspired chart mode"),
  make_option("--x_col", type = "character", default = "x", help = "X column"),
  make_option("--y_col", type = "character", default = "y", help = "Y column"),
  make_option("--value_col", type = "character", default = "value", help = "Primary numeric value column"),
  make_option("--value2_col", type = "character", default = "value2", help = "Secondary numeric value column"),
  make_option("--category_col", type = "character", default = "category", help = "Category column"),
  make_option("--group_col", type = "character", default = "group", help = "Group/fill column"),
  make_option("--facet_col", type = "character", default = "facet", help = "Facet column"),
  make_option("--label_col", type = "character", default = "label", help = "Label column"),
  make_option("--time_col", type = "character", default = "time", help = "Time/order column"),
  make_option("--size_col", type = "character", default = "size", help = "Point size column"),
  make_option("--top_n", type = "numeric", default = 12, help = "Top N categories where applicable"),
  make_option("--wrap_width", type = "numeric", default = 24, help = "Label wrap width"),
  make_option("--palette", type = "character", default = "nature", help = "Discrete palette"),
  make_option("--theme", type = "character", default = "rplot", help = "Theme"),
  make_option("--title", type = "character", default = "", help = "Plot title"),
  make_option("--x_title", type = "character", default = "", help = "X axis title"),
  make_option("--y_title", type = "character", default = "", help = "Y axis title"),
  make_option("--base_font_size", type = "numeric", default = 11, help = "Base font size"),
  make_option("--fig_width", type = "numeric", default = 7.6, help = "Figure width"),
  make_option("--fig_height", type = "numeric", default = 5.2, help = "Figure height"),
  make_option("--plot_name", type = "character", default = "article_gallery_plot", help = "Output prefix without extension")
)

opt <- parse_args(OptionParser(option_list = option_list))
mode <- tolower(gsub("-", "_", opt$mode))

axis_label <- function(custom, fallback) ifelse(nzchar(custom), custom, fallback)

has_cols <- function(df, cols) {
  cols <- cols[nzchar(cols)]
  length(cols) == 0 || all(cols %in% colnames(df))
}

read_or_empty <- function() {
  if (!is.null(opt$data_input) && nzchar(opt$data_input) && file.exists(opt$data_input)) {
    read_input_table(opt$data_input)
  } else {
    data.frame()
  }
}

article_palette <- function(n) {
  base <- discrete_palette(opt$palette)
  base[round(seq(1, length(base), length.out = n))]
}

demo_data <- function(mode) {
  set.seed(42)
  if (mode == "concept_heatmap") {
    expand.grid(
      category = c("Cell cycle", "Immune response", "Metabolism", "DNA repair", "Stress"),
      x = c("Evidence", "Effect", "Confidence", "Actionability"),
      KEEP.OUT.ATTRS = FALSE
    ) %>%
      mutate(value = c(0.82, 0.58, 0.71, 0.44, 0.63, 0.76, 0.31, 0.68, 0.53, 0.87, 0.49, 0.55, 0.79, 0.36, 0.61, 0.72, 0.47, 0.66, 0.38, 0.81),
             label = ifelse(value > 0.7, "High", ifelse(value > 0.5, "Mid", "Low")),
             facet = "Concept map")
  } else if (mode == "raincloud_compact") {
    expand.grid(group = c("Control", "Drug A", "Drug B"), id = seq_len(28), KEEP.OUT.ATTRS = FALSE) %>%
      mutate(value = rnorm(n(), rep(c(4.8, 5.7, 6.3), each = 28), 0.55),
             category = group)
  } else if (mode == "estimation_pvalue") {
    expand.grid(group = c("Baseline", "Responder", "Non-responder"), id = seq_len(24), KEEP.OUT.ATTRS = FALSE) %>%
      mutate(value = rnorm(n(), rep(c(2.8, 4.1, 3.2), each = 24), 0.42),
             category = "Score",
             label = c("P = 0.004", "P = 0.038", "P = 0.21")[match(group, c("Baseline", "Responder", "Non-responder"))])
  } else if (mode == "phase_portrait") {
    data.frame(time = seq(0, 2 * pi, length.out = 90)) %>%
      mutate(x = cos(time) * (1 + 0.10 * time),
             y = sin(time) * (1 + 0.08 * time),
             group = cut(time, breaks = 4, labels = c("Initiation", "Expansion", "Transition", "Recovery")),
             label = ifelse(row_number() %in% c(1, 30, 60, 90), as.character(group), ""))
  } else if (mode == "grouped_dot_matrix") {
    expand.grid(
      category = c("T cell", "B cell", "Myeloid", "Stromal", "Tumor"),
      x = c("IFNG", "GZMB", "CXCL9", "MKI67", "COL1A1"),
      group = c("Pre", "Post"),
      KEEP.OUT.ATTRS = FALSE
    ) %>%
      mutate(value = runif(n(), 0.1, 1),
             size = runif(n(), 0.2, 1))
  } else if (mode == "cluster_layer_heatmap") {
    expand.grid(category = paste0("Gene ", LETTERS[1:12]), x = paste0("S", 1:8), KEEP.OUT.ATTRS = FALSE) %>%
      mutate(group = rep(c("Module 1", "Module 2", "Module 3"), each = 32),
             value = rnorm(n(), rep(c(1.0, -0.4, 0.6), each = 32), 0.45))
  } else if (mode == "pseudo_3d_heatmap") {
    expand.grid(category = paste0("Pathway ", 1:7), x = paste0("T", 1:7), KEEP.OUT.ATTRS = FALSE) %>%
      mutate(value = sin(as.numeric(factor(category)) / 1.6) + cos(as.numeric(factor(x)) / 1.4) + rnorm(n(), 0, 0.16))
  } else if (mode == "rank_lollipop_badge") {
    data.frame(category = paste("Feature", LETTERS[1:14]), value = sort(runif(14, 0.25, 1.8), decreasing = TRUE)) %>%
      mutate(group = ifelse(row_number() <= 4, "Highlighted", "Background"),
             label = paste0(round(value, 2), "x"))
  } else if (mode == "multi_panel_distribution") {
    expand.grid(
      facet = c("Expression", "Accessibility", "Methylation"),
      group = c("Low", "Medium", "High"),
      id = seq_len(20),
      KEEP.OUT.ATTRS = FALSE
    ) %>%
      mutate(value = rnorm(n(), rep(c(3, 4.2, 5.1), each = 20), 0.45) + rep(c(0, 0.3, -0.2), each = 60),
             category = group)
  } else if (mode == "nested_donut") {
    expand.grid(category = c("Immune", "Tumor", "Stroma"), group = c("A", "B", "C", "D"), KEEP.OUT.ATTRS = FALSE) %>%
      mutate(value = c(18, 11, 8, 6, 23, 16, 7, 5, 9, 14, 12, 4))
  } else {
    data.frame(category = "A", group = "G", x = "X", y = "Y", value = 1)
  }
}

input_data <- read_or_empty()
required_by_mode <- list(
  concept_heatmap = c(opt$category_col, opt$x_col, opt$value_col),
  raincloud_compact = c(opt$group_col, opt$value_col),
  estimation_pvalue = c(opt$group_col, opt$value_col),
  phase_portrait = c(opt$x_col, opt$y_col, opt$time_col),
  grouped_dot_matrix = c(opt$category_col, opt$x_col, opt$value_col),
  cluster_layer_heatmap = c(opt$category_col, opt$x_col, opt$value_col),
  pseudo_3d_heatmap = c(opt$category_col, opt$x_col, opt$value_col),
  rank_lollipop_badge = c(opt$category_col, opt$value_col),
  multi_panel_distribution = c(opt$facet_col, opt$group_col, opt$value_col),
  nested_donut = c(opt$category_col, opt$group_col, opt$value_col)
)
df <- if (has_cols(input_data, required_by_mode[[mode]] %||% character())) input_data else demo_data(mode)
df <- coerce_numeric(df, intersect(c(opt$value_col, opt$value2_col, opt$x_col, opt$y_col, opt$time_col, opt$size_col), colnames(df)))

finish_plot <- function(p) save_ggplot(p, opt$plot_name, opt$fig_width, opt$fig_height)

if (mode == "concept_heatmap") {
  df$.category <- stringr::str_wrap(as.character(df[[opt$category_col]]), opt$wrap_width)
  p <- ggplot(df, aes(x = .data[[opt$x_col]], y = .category, fill = .data[[opt$value_col]])) +
    geom_tile(color = "white", linewidth = 1.1, width = 0.94, height = 0.86) +
    geom_text(aes(label = if (opt$label_col %in% colnames(df)) .data[[opt$label_col]] else scales::number(.data[[opt$value_col]], accuracy = 0.01)), fontface = "bold", size = 3.1, color = "#1F2937") +
    scale_fill_gradientn(colors = c("#E9F2F6", "#73B7C6", "#176B87"), labels = scales::number_format(accuracy = 0.1)) +
    theme_rplot(opt$base_font_size, opt$theme) +
    theme(panel.grid = element_blank(), axis.title = element_blank(), legend.position = "top") +
    labs(title = opt$title, fill = axis_label(opt$y_title, opt$value_col))
  finish_plot(p)
} else if (mode == "raincloud_compact") {
  df[[opt$group_col]] <- factor(as.character(df[[opt$group_col]]), levels = unique(as.character(df[[opt$group_col]])))
  pal <- setNames(article_palette(length(levels(df[[opt$group_col]]))), levels(df[[opt$group_col]]))
  p <- ggplot(df, aes(x = .data[[opt$group_col]], y = .data[[opt$value_col]], fill = .data[[opt$group_col]], color = .data[[opt$group_col]])) +
    ggdist::stat_halfeye(adjust = 0.75, width = 0.55, justification = -0.28, .width = 0, alpha = 0.72, point_colour = NA) +
    geom_boxplot(width = 0.18, outlier.shape = NA, alpha = 0.82, color = "#273341", linewidth = 0.42) +
    geom_point(position = position_jitter(width = 0.07, seed = 7), size = 1.45, alpha = 0.72) +
    scale_fill_manual(values = pal, guide = "none") +
    scale_color_manual(values = pal, guide = "none") +
    theme_rplot(opt$base_font_size, opt$theme) +
    labs(title = opt$title, x = axis_label(opt$x_title, opt$group_col), y = axis_label(opt$y_title, opt$value_col))
  finish_plot(p)
} else if (mode == "estimation_pvalue") {
  summary_df <- df %>%
    group_by(.data[[opt$group_col]]) %>%
    summarize(.mean = mean(.data[[opt$value_col]], na.rm = TRUE), .se = sd(.data[[opt$value_col]], na.rm = TRUE) / sqrt(n()), .groups = "drop")
  labels <- if (opt$label_col %in% colnames(df)) unique(as.character(df[[opt$label_col]][nzchar(as.character(df[[opt$label_col]]))])) else character()
  p_label <- if (length(labels) > 0) labels[1] else "P = 0.004"
  y_top <- max(df[[opt$value_col]], na.rm = TRUE) * 1.10
  p <- ggplot(df, aes(x = .data[[opt$group_col]], y = .data[[opt$value_col]], color = .data[[opt$group_col]])) +
    geom_point(position = position_jitter(width = 0.08, seed = 8), alpha = 0.68, size = 1.65) +
    geom_errorbar(data = summary_df, aes(x = .data[[opt$group_col]], y = .mean, ymin = .mean - 1.96 * .se, ymax = .mean + 1.96 * .se), width = 0.16, color = "#111827", linewidth = 0.58, inherit.aes = FALSE) +
    geom_point(data = summary_df, aes(x = .data[[opt$group_col]], y = .mean), color = "#111827", fill = "white", shape = 21, size = 3.2, stroke = 0.75, inherit.aes = FALSE) +
    annotate("segment", x = 1, xend = 2, y = y_top, yend = y_top, linewidth = 0.45) +
    annotate("text", x = 1.5, y = y_top * 1.025, label = p_label, size = 3.3, fontface = "bold") +
    scale_color_manual(values = article_palette(length(unique(df[[opt$group_col]]))), guide = "none") +
    coord_cartesian(clip = "off") +
    theme_rplot(opt$base_font_size, opt$theme) +
    labs(title = opt$title, x = axis_label(opt$x_title, opt$group_col), y = axis_label(opt$y_title, opt$value_col))
  finish_plot(p)
} else if (mode == "phase_portrait") {
  regimes <- data.frame(xmin = c(-Inf, 0, -Inf, 0), xmax = c(0, Inf, 0, Inf), ymin = c(0, 0, -Inf, -Inf), ymax = c(Inf, Inf, 0, 0), regime = c("Suppressed", "Activated", "Dormant", "Recovered"))
  p <- ggplot(df, aes(x = .data[[opt$x_col]], y = .data[[opt$y_col]])) +
    geom_rect(data = regimes, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = regime), inherit.aes = FALSE, alpha = 0.12) +
    geom_path(aes(color = .data[[opt$group_col]]), linewidth = 1.15, arrow = grid::arrow(type = "closed", length = unit(0.11, "inches"))) +
    geom_point(aes(color = .data[[opt$group_col]]), size = 1.9, alpha = 0.9) +
    geom_hline(yintercept = 0, linetype = "22", color = "grey55") +
    geom_vline(xintercept = 0, linetype = "22", color = "grey55") +
    scale_color_manual(values = article_palette(length(unique(df[[opt$group_col]])))) +
    scale_fill_manual(values = c("#B7D6EA", "#F2C7B8", "#D9D3EC", "#CBE7CF"), guide = "none") +
    coord_equal() +
    theme_rplot(opt$base_font_size, opt$theme) +
    labs(title = opt$title, x = axis_label(opt$x_title, opt$x_col), y = axis_label(opt$y_title, opt$y_col), color = opt$group_col)
  finish_plot(p)
} else if (mode == "grouped_dot_matrix") {
  p <- ggplot(df, aes(x = .data[[opt$x_col]], y = .data[[opt$category_col]])) +
    geom_point(aes(size = if (opt$size_col %in% colnames(df)) .data[[opt$size_col]] else abs(.data[[opt$value_col]]), fill = .data[[opt$value_col]]), shape = 21, color = "white", stroke = 0.35, alpha = 0.92) +
    facet_grid(cols = vars(.data[[opt$group_col]])) +
    scale_size_continuous(range = c(1.8, 8), guide = guide_legend(title = "Size")) +
    scale_fill_gradientn(colors = c("#EBF4F8", "#69B7C7", "#1B6B85"), labels = scales::number_format(accuracy = 0.1)) +
    theme_rplot(opt$base_font_size, opt$theme) +
    theme(panel.grid.major = element_line(color = "#EEF2F6", linewidth = 0.45), axis.title = element_blank(), legend.position = "right") +
    labs(title = opt$title, fill = axis_label(opt$y_title, opt$value_col))
  finish_plot(p)
} else if (mode == "cluster_layer_heatmap") {
  df$.category <- factor(as.character(df[[opt$category_col]]), levels = rev(unique(as.character(df[[opt$category_col]]))))
  p <- ggplot(df, aes(x = .data[[opt$x_col]], y = .category, fill = .data[[opt$value_col]])) +
    geom_tile(color = "white", linewidth = 0.35) +
    facet_grid(rows = vars(.data[[opt$group_col]]), scales = "free_y", space = "free_y", switch = "y") +
    scale_fill_gradient2(low = "#3C5488", mid = "#F7F7F7", high = "#E64B35", midpoint = 0) +
    theme_rplot(opt$base_font_size, opt$theme) +
    theme(panel.grid = element_blank(), strip.placement = "outside", axis.title = element_blank(), legend.position = "top") +
    labs(title = opt$title, fill = axis_label(opt$y_title, opt$value_col))
  finish_plot(p)
} else if (mode == "pseudo_3d_heatmap") {
  df$.x_id <- as.numeric(factor(df[[opt$x_col]], levels = unique(as.character(df[[opt$x_col]]))))
  df$.y_id <- as.numeric(factor(df[[opt$category_col]], levels = rev(unique(as.character(df[[opt$category_col]])))))
  p <- ggplot(df) +
    geom_tile(aes(x = .x_id + 0.10, y = .y_id - 0.10), fill = "#1F2937", alpha = 0.22, width = 0.86, height = 0.86) +
    geom_tile(aes(x = .x_id, y = .y_id, fill = .data[[opt$value_col]]), color = "white", linewidth = 0.55, width = 0.86, height = 0.86) +
    scale_x_continuous(breaks = sort(unique(df$.x_id)), labels = unique(as.character(df[[opt$x_col]])), expand = expansion(mult = 0.04)) +
    scale_y_continuous(breaks = sort(unique(df$.y_id)), labels = rev(unique(as.character(df[[opt$category_col]]))), expand = expansion(mult = 0.04)) +
    scale_fill_gradientn(colors = c("#2F4858", "#86BBD8", "#F6E8C3", "#D95F02")) +
    coord_fixed() +
    theme_rplot(opt$base_font_size, opt$theme) +
    theme(panel.grid = element_blank(), axis.title = element_blank(), legend.position = "top") +
    labs(title = opt$title, fill = axis_label(opt$y_title, opt$value_col))
  finish_plot(p)
} else if (mode == "rank_lollipop_badge") {
  if (!(opt$group_col %in% colnames(df))) df[[opt$group_col]] <- "Feature"
  if (!(opt$label_col %in% colnames(df))) df[[opt$label_col]] <- NA_character_
  df <- df %>%
    group_by(.data[[opt$category_col]]) %>%
    summarize(
      "{opt$value_col}" := mean(.data[[opt$value_col]], na.rm = TRUE),
      "{opt$group_col}" := dplyr::first(.data[[opt$group_col]]),
      "{opt$label_col}" := dplyr::first(.data[[opt$label_col]]),
      .groups = "drop"
    ) %>%
    mutate("{opt$label_col}" := ifelse(
      is.na(.data[[opt$label_col]]) | !nzchar(as.character(.data[[opt$label_col]])),
      scales::number(.data[[opt$value_col]], accuracy = 0.01),
      as.character(.data[[opt$label_col]])
    )) %>%
    arrange(desc(.data[[opt$value_col]])) %>%
    slice_head(n = opt$top_n)
  df$.category <- stringr::str_wrap(as.character(df[[opt$category_col]]), opt$wrap_width)
  df$.category <- factor(df$.category, levels = rev(unique(df$.category)))
  p <- ggplot(df, aes(y = .category, x = .data[[opt$value_col]])) +
    geom_segment(aes(x = 0, xend = .data[[opt$value_col]], yend = .category), color = "#C6CDD6", linewidth = 1.1) +
    geom_point(aes(fill = .data[[opt$group_col]]), shape = 21, size = 4.2, color = "white", stroke = 0.55) +
    geom_label(aes(label = if (opt$label_col %in% colnames(df)) .data[[opt$label_col]] else scales::number(.data[[opt$value_col]], accuracy = 0.01)), hjust = -0.12, size = 3, label.size = 0, fill = "#F5F7FA", color = "#1F2937") +
    scale_fill_manual(values = article_palette(length(unique(df[[opt$group_col]])))) +
    scale_x_continuous(expand = expansion(mult = c(0.02, 0.16))) +
    theme_rplot(opt$base_font_size, opt$theme) +
    theme(legend.position = "top", axis.title.y = element_blank(), panel.grid.major.y = element_blank()) +
    labs(title = opt$title, x = axis_label(opt$x_title, opt$value_col), fill = opt$group_col)
  finish_plot(p)
} else if (mode == "multi_panel_distribution") {
  p <- ggplot(df, aes(x = .data[[opt$group_col]], y = .data[[opt$value_col]], fill = .data[[opt$group_col]])) +
    geom_violin(width = 0.85, trim = FALSE, alpha = 0.55, color = NA) +
    geom_boxplot(width = 0.18, outlier.shape = NA, color = "#273341", alpha = 0.82) +
    geom_point(position = position_jitter(width = 0.08, seed = 12), size = 1.15, alpha = 0.52, color = "#273341") +
    facet_wrap(vars(.data[[opt$facet_col]]), scales = "free_y", nrow = 1) +
    scale_fill_manual(values = article_palette(length(unique(df[[opt$group_col]]))), guide = "none") +
    theme_rplot(opt$base_font_size, opt$theme) +
    labs(title = opt$title, x = axis_label(opt$x_title, opt$group_col), y = axis_label(opt$y_title, opt$value_col))
  finish_plot(p)
} else if (mode == "nested_donut") {
  df <- df %>% group_by(.data[[opt$category_col]]) %>% mutate(.cat_total = sum(.data[[opt$value_col]], na.rm = TRUE)) %>% ungroup()
  inner <- df %>% group_by(.data[[opt$category_col]]) %>% summarize(value = sum(.data[[opt$value_col]], na.rm = TRUE), .groups = "drop") %>% mutate(group = .data[[opt$category_col]], ring = "inner")
  outer <- df %>% transmute(category = .data[[opt$category_col]], group = paste(.data[[opt$category_col]], .data[[opt$group_col]], sep = ": "), value = .data[[opt$value_col]], ring = "outer")
  donut <- bind_rows(inner %>% transmute(group, value, ring), outer %>% transmute(group, value, ring)) %>% group_by(ring) %>% arrange(ring, group) %>% mutate(ymax = cumsum(value) / sum(value), ymin = lag(ymax, default = 0), x = ifelse(ring == "inner", 2, 3)) %>% ungroup()
  p <- ggplot(donut) +
    geom_rect(aes(ymin = ymin, ymax = ymax, xmin = x - 0.48, xmax = x + 0.48, fill = group), color = "white", linewidth = 0.65) +
    coord_polar(theta = "y") +
    xlim(0.7, 3.6) +
    scale_fill_manual(values = article_palette(length(unique(donut$group)))) +
    theme_rplot(opt$base_font_size, "void") +
    theme(legend.position = "right", plot.title = element_text(face = "bold", hjust = 0.5)) +
    labs(title = opt$title, fill = "")
  finish_plot(p)
} else {
  stop("Unsupported --mode: ", mode)
}
