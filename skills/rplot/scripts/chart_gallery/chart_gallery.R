suppressPackageStartupMessages({
  library(optparse)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(volcanolabel)
  library(scales)
  library(codex.rplot)
})

option_list <- list(
  make_option("--data_input", type = "character", default = NULL, help = "Input table path"),
  make_option("--mode", type = "character", default = "histogram", help = "Chart mode"),
  make_option("--x_col", type = "character", default = "", help = "X column"),
  make_option("--y_col", type = "character", default = "", help = "Y column"),
  make_option("--value_col", type = "character", default = "", help = "Primary numeric value column"),
  make_option("--value_cols", type = "character", default = "", help = "Numeric columns, comma/semicolon separated"),
  make_option("--value2_col", type = "character", default = "", help = "Secondary numeric value column"),
  make_option("--lower_col", type = "character", default = "", help = "Lower bound column"),
  make_option("--upper_col", type = "character", default = "", help = "Upper bound column"),
  make_option("--category_col", type = "character", default = "", help = "Category column"),
  make_option("--subcategory_col", type = "character", default = "", help = "Subcategory column"),
  make_option("--group_col", type = "character", default = "", help = "Group column"),
  make_option("--facet_row_col", type = "character", default = "", help = "Optional facet row column"),
  make_option("--facet_col_col", type = "character", default = "", help = "Optional facet column"),
  make_option("--from_col", type = "character", default = "", help = "Source/from column"),
  make_option("--to_col", type = "character", default = "", help = "Target/to column"),
  make_option("--weight_col", type = "character", default = "", help = "Weight/count column"),
  make_option("--date_col", type = "character", default = "", help = "Date column"),
  make_option("--time_col", type = "character", default = "", help = "Time or survival time column"),
  make_option("--status_col", type = "character", default = "", help = "Binary status/event column"),
  make_option("--actual_col", type = "character", default = "", help = "Observed binary outcome column"),
  make_option("--score_col", type = "character", default = "", help = "Prediction score/probability column"),
  make_option("--predicted_col", type = "character", default = "", help = "Predicted numeric value column"),
  make_option("--observed_col", type = "character", default = "", help = "Observed numeric value column"),
  make_option("--start_col", type = "character", default = "", help = "Start date column"),
  make_option("--end_col", type = "character", default = "", help = "End date column"),
  make_option("--label_col", type = "character", default = "", help = "Label column"),
  make_option("--response_col", type = "character", default = "", help = "Likert/response column"),
  make_option("--sequence_col", type = "character", default = "", help = "Sequence column"),
  make_option("--a_col", type = "character", default = "", help = "Ternary component A column"),
  make_option("--b_col", type = "character", default = "", help = "Ternary component B column"),
  make_option("--c_col", type = "character", default = "", help = "Ternary component C column"),
  make_option("--top_n", type = "numeric", default = 20, help = "Top N categories where applicable; 0 means all"),
  make_option("--label_top_n", type = "numeric", default = 10, help = "Top N labels per direction where applicable"),
  make_option("--fc_threshold", type = "numeric", default = 1, help = "Absolute fold-change threshold for volcano-style plots"),
  make_option("--p_threshold", type = "numeric", default = 0.05, help = "P-value/FDR threshold for volcano-style plots"),
  make_option("--bins", type = "numeric", default = 24, help = "Histogram/bin count"),
  make_option("--wrap_width", type = "numeric", default = 28, help = "Category label wrap width"),
  make_option("--show_labels", type = "character", default = "true", help = "Show in-bar labels where applicable: true/false"),
  make_option("--label_min_percent", type = "numeric", default = 0.06, help = "Minimum within-bar proportion for percent labels"),
  make_option("--palette", type = "character", default = "editorial", help = "Palette"),
  make_option("--theme", type = "character", default = "auto", help = "Theme: auto, rplot, minimal, classic, bw, light, grey, void"),
  make_option("--show_ci", type = "character", default = "false", help = "Show confidence interval ribbon for survival curves: true/false"),
  make_option("--show_censor", type = "character", default = "true", help = "Show censor tick marks for survival curves: true/false"),
  make_option("--show_pvalue", type = "character", default = "true", help = "Show log-rank P value for grouped survival curves: true/false"),
  make_option("--show_risk_table", type = "character", default = "true", help = "Show number-at-risk table under survival curves: true/false"),
  make_option("--title", type = "character", default = "", help = "Plot title"),
  make_option("--x_title", type = "character", default = "", help = "X axis title"),
  make_option("--y_title", type = "character", default = "", help = "Y axis title"),
  make_option("--base_font_size", type = "numeric", default = 12, help = "Base font size"),
  make_option("--fig_width", type = "numeric", default = 7.5, help = "Figure width"),
  make_option("--fig_height", type = "numeric", default = 5.5, help = "Figure height"),
  make_option("--plot_name", type = "character", default = "chart_gallery_plot", help = "Output prefix without extension")
)

opt <- parse_args(OptionParser(option_list = option_list))
if (is.null(opt$data_input) || !nzchar(opt$mode)) {
  print_help(OptionParser(option_list = option_list))
  stop("--data_input and --mode are required.")
}

data <- read_input_table(opt$data_input)
mode <- tolower(gsub("-", "_", opt$mode))

axis_label <- function(custom, fallback) {
  ifelse(nzchar(custom), custom, fallback)
}

label_args <- function(x = NULL, y = NULL, fill = NULL, color = NULL) {
  args <- list(title = opt$title)
  if (!is.null(x)) args$x <- axis_label(opt$x_title, x)
  if (!is.null(y)) args$y <- axis_label(opt$y_title, y)
  if (!is.null(fill) && nzchar(fill)) args$fill <- fill
  if (!is.null(color) && nzchar(color)) args$color <- color
  args
}

parse_bool <- function(value) {
  if (is.null(value) || !nzchar(trimws(as.character(value)))) {
    return(FALSE)
  }
  tolower(trimws(as.character(value))) %in% c("1", "true", "t", "yes", "y", "on")
}

finish_plot <- function(p) {
  save_ggplot(p, opt$plot_name, opt$fig_width, opt$fig_height)
}

scale_fill_palette <- function() {
  scale_fill_manual(values = discrete_palette(opt$palette), na.translate = FALSE)
}

scale_color_palette <- function() {
  scale_color_manual(values = discrete_palette(opt$palette), na.translate = FALSE)
}

top_categories <- function(df, category_col, value_col = "") {
  if (opt$top_n <= 0 || nrow(df) <= opt$top_n) {
    return(df)
  }
  if (nzchar(value_col)) {
    keep <- df %>%
      group_by(.data[[category_col]]) %>%
      summarize(.rank_value = sum(abs(.data[[value_col]]), na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(.rank_value)) %>%
      slice_head(n = opt$top_n) %>%
      pull(.data[[category_col]])
  } else {
    keep <- df %>%
      count(.data[[category_col]], sort = TRUE, name = ".n") %>%
      slice_head(n = opt$top_n) %>%
      pull(.data[[category_col]])
  }
  df[df[[category_col]] %in% keep, , drop = FALSE]
}

prepare_value <- function(required_cols, numeric_cols = NULL) {
  require_columns(data, required_cols)
  if (is.null(numeric_cols)) {
    numeric_cols <- c(opt$value_col, opt$value2_col, opt$lower_col, opt$upper_col)
  }
  numeric_cols <- intersect(numeric_cols[nzchar(numeric_cols)], colnames(data))
  coerce_numeric(data, numeric_cols)
}

with_wrapped_category <- function(df, category_col) {
  df$.category_wrapped <- wrap_labels(as.character(df[[category_col]]), opt$wrap_width)
  df
}

as_binary <- function(x) {
  if (is.numeric(x)) {
    return(as.integer(x > 0))
  }
  y <- tolower(trimws(as.character(x)))
  as.integer(y %in% c("1", "true", "yes", "y", "case", "event", "positive", "pos", "disease"))
}

rescale_to_range <- function(x, to_min, to_max) {
  from_min <- min(x, na.rm = TRUE)
  from_max <- max(x, na.rm = TRUE)
  if (!is.finite(from_min) || !is.finite(from_max) || from_max == from_min) {
    return(rep(mean(c(to_min, to_max)), length(x)))
  }
  (x - from_min) / (from_max - from_min) * (to_max - to_min) + to_min
}

auc_trapezoid <- function(fpr, tpr) {
  ord <- order(fpr, tpr)
  fpr <- fpr[ord]
  tpr <- tpr[ord]
  sum(diff(fpr) * (head(tpr, -1) + tail(tpr, -1)) / 2)
}

compute_roc <- function(df, actual_col, score_col, group_col = "") {
  groups <- if (nzchar(group_col)) unique(df[[group_col]]) else "All"
  bind_rows(lapply(groups, function(grp) {
    sub <- if (nzchar(group_col)) df[df[[group_col]] == grp, , drop = FALSE] else df
    sub$.actual <- as_binary(sub[[actual_col]])
    sub$.score <- as.numeric(sub[[score_col]])
    sub <- sub[is.finite(sub$.score) & !is.na(sub$.actual), , drop = FALSE]
    thresholds <- sort(unique(c(Inf, sub$.score, -Inf)), decreasing = TRUE)
    roc <- lapply(thresholds, function(th) {
      pred <- as.integer(sub$.score >= th)
      tp <- sum(pred == 1 & sub$.actual == 1)
      fp <- sum(pred == 1 & sub$.actual == 0)
      tn <- sum(pred == 0 & sub$.actual == 0)
      fn <- sum(pred == 0 & sub$.actual == 1)
      data.frame(
        .fpr = ifelse(fp + tn == 0, 0, fp / (fp + tn)),
        .tpr = ifelse(tp + fn == 0, 0, tp / (tp + fn))
      )
    })
    roc <- bind_rows(roc) %>% distinct(.fpr, .tpr)
    roc$.group <- as.character(grp)
    roc$.auc <- auc_trapezoid(roc$.fpr, roc$.tpr)
    roc
  }))
}

if (mode %in% c("histogram", "density", "frequency_polygon", "ecdf", "qq_plot")) {
  df <- prepare_value(c(opt$value_col, opt$group_col))
  df <- df[is.finite(df[[opt$value_col]]), , drop = FALSE]
  p <- ggplot(df, aes(x = .data[[opt$value_col]]))
  if (nzchar(opt$group_col)) {
    if (mode == "histogram") {
      p <- p + geom_histogram(aes(fill = .data[[opt$group_col]]), bins = opt$bins, alpha = 0.72, position = "identity", color = "white", linewidth = 0.15)
      p <- add_discrete_scale(p, "fill", opt$palette)
    } else if (mode == "density") {
      p <- p + geom_density(aes(fill = .data[[opt$group_col]], color = .data[[opt$group_col]]), alpha = 0.32, linewidth = 0.8)
      p <- add_discrete_scale(add_discrete_scale(p, "fill", opt$palette), "color", opt$palette)
    } else if (mode == "frequency_polygon") {
      p <- p + geom_freqpoly(aes(color = .data[[opt$group_col]]), bins = opt$bins, linewidth = 0.9)
      p <- add_discrete_scale(p, "color", opt$palette)
    } else if (mode == "ecdf") {
      p <- p + stat_ecdf(aes(color = .data[[opt$group_col]]), linewidth = 0.9)
      p <- add_discrete_scale(p, "color", opt$palette)
    } else {
      qq_data <- df %>%
        group_by(.data[[opt$group_col]]) %>%
        arrange(.data[[opt$value_col]], .by_group = TRUE) %>%
        mutate(.p = (row_number() - 0.5) / n(), .theoretical = qnorm(.p), .sample = .data[[opt$value_col]]) %>%
        ungroup()
      p <- ggplot(qq_data, aes(x = .theoretical, y = .sample, color = .data[[opt$group_col]])) +
        geom_point(size = 2, alpha = 0.8) +
        geom_abline(slope = stats::sd(df[[opt$value_col]], na.rm = TRUE), intercept = mean(df[[opt$value_col]], na.rm = TRUE), color = "grey40", linewidth = 0.4)
      p <- add_discrete_scale(p, "color", opt$palette)
    }
  } else if (mode == "histogram") {
    p <- p + geom_histogram(bins = opt$bins, fill = "#3B6EA8", color = "white", linewidth = 0.15)
  } else if (mode == "density") {
    p <- p + geom_density(fill = "#3B6EA8", color = "#254F7D", alpha = 0.35, linewidth = 0.8)
  } else if (mode == "frequency_polygon") {
    p <- p + geom_freqpoly(bins = opt$bins, color = "#3B6EA8", linewidth = 0.9)
  } else if (mode == "ecdf") {
    p <- p + stat_ecdf(color = "#3B6EA8", linewidth = 0.9)
  } else {
    qq_data <- df %>%
      arrange(.data[[opt$value_col]]) %>%
      mutate(.p = (row_number() - 0.5) / n(), .theoretical = qnorm(.p), .sample = .data[[opt$value_col]])
    p <- ggplot(qq_data, aes(x = .theoretical, y = .sample)) +
      geom_point(size = 2, alpha = 0.8, color = "#3B6EA8") +
      geom_abline(slope = stats::sd(df[[opt$value_col]], na.rm = TRUE), intercept = mean(df[[opt$value_col]], na.rm = TRUE), color = "grey40", linewidth = 0.4)
  }
  p <- p + theme_rplot(opt$base_font_size, opt$theme) + do.call(labs, label_args(x = opt$value_col, y = ifelse(mode == "ecdf", "ECDF", "Count")))
  finish_plot(p)
} else if (mode %in% c("dot_plot", "jitter_strip")) {
  df <- prepare_value(c(opt$category_col, opt$value_col, opt$group_col))
  df <- top_categories(df, opt$category_col, opt$value_col)
  df <- with_wrapped_category(df, opt$category_col)
  p <- ggplot(df, aes(x = .category_wrapped, y = .data[[opt$value_col]]))
  if (mode == "dot_plot") {
    summary_df <- df %>% group_by(.category_wrapped) %>% summarize(.value = mean(.data[[opt$value_col]], na.rm = TRUE), .groups = "drop")
    p <- ggplot(summary_df, aes(x = reorder(.category_wrapped, .value), y = .value)) +
      geom_segment(aes(xend = .category_wrapped, y = 0, yend = .value), color = "grey70", linewidth = 0.6) +
      geom_point(size = 3, color = "#3B6EA8") +
      coord_flip()
  } else if (nzchar(opt$group_col)) {
    p <- p + geom_jitter(aes(color = .data[[opt$group_col]]), width = 0.18, height = 0, size = 2.2, alpha = 0.8)
    p <- add_discrete_scale(p, "color", opt$palette)
  } else {
    p <- p + geom_jitter(width = 0.18, height = 0, size = 2.2, alpha = 0.8, color = "#3B6EA8")
  }
  p <- p + theme_rplot(opt$base_font_size, opt$theme) + do.call(labs, label_args(x = opt$category_col, y = opt$value_col, color = opt$group_col))
  finish_plot(p)
} else if (mode %in% c("hexbin", "density2d_contour", "density2d_filled")) {
  df <- prepare_value(c(opt$x_col, opt$y_col, opt$group_col), numeric_cols = c(opt$x_col, opt$y_col))
  df <- df[is.finite(df[[opt$x_col]]) & is.finite(df[[opt$y_col]]), , drop = FALSE]
  p <- ggplot(df, aes(x = .data[[opt$x_col]], y = .data[[opt$y_col]]))
  if (mode == "hexbin") {
    p <- p + geom_bin_2d(bins = opt$bins) + scale_fill_gradient(low = "#DCE9F5", high = "#2C5D91", name = "Count")
  } else if (mode == "density2d_contour") {
    p <- p + geom_point(color = "grey55", alpha = 0.45, size = 1.4) + stat_density_2d(color = "#B2182B", linewidth = 0.55)
  } else {
    p <- p + stat_density_2d_filled(alpha = 0.82) + geom_point(color = "grey20", alpha = 0.35, size = 0.9)
  }
  p <- p + theme_rplot(opt$base_font_size, opt$theme) + do.call(labs, label_args(x = opt$x_col, y = opt$y_col))
  finish_plot(p)
} else if (mode %in% c("line", "area", "stacked_area", "step", "ribbon")) {
  required <- c(opt$date_col, opt$value_col, opt$group_col)
  if (mode == "ribbon") required <- c(opt$date_col, opt$value_col, opt$lower_col, opt$upper_col, opt$group_col)
  df <- prepare_value(required)
  df$.date <- as.Date(df[[opt$date_col]])
  df <- df[!is.na(df$.date) & is.finite(df[[opt$value_col]]), , drop = FALSE]
  p <- ggplot(df, aes(x = .date, y = .data[[opt$value_col]]))
  if (mode == "line") {
    if (nzchar(opt$group_col)) {
      p <- p + geom_line(aes(color = .data[[opt$group_col]], group = .data[[opt$group_col]]), linewidth = 0.85) + geom_point(aes(color = .data[[opt$group_col]]), size = 1.8)
      p <- add_discrete_scale(p, "color", opt$palette)
    } else {
      p <- p + geom_line(linewidth = 0.85, color = "#3B6EA8") + geom_point(size = 1.8, color = "#3B6EA8")
    }
  } else if (mode == "step") {
    if (nzchar(opt$group_col)) {
      p <- p + geom_step(aes(group = .data[[opt$group_col]], color = .data[[opt$group_col]]), linewidth = 0.8)
      p <- add_discrete_scale(p, "color", opt$palette)
    } else {
      p <- p + geom_step(aes(group = 1), linewidth = 0.8, color = "#3B6EA8")
    }
  } else if (mode == "area") {
    if (nzchar(opt$group_col)) {
      p <- p + geom_area(aes(fill = .data[[opt$group_col]], group = .data[[opt$group_col]]), alpha = 0.45, position = "identity")
      p <- add_discrete_scale(p, "fill", opt$palette)
    } else {
      p <- p + geom_area(fill = "#3B6EA8", alpha = 0.55)
    }
  } else if (mode == "stacked_area") {
    p <- p + geom_area(aes(fill = .data[[opt$group_col]], group = .data[[opt$group_col]]), alpha = 0.82, position = "stack")
    p <- add_discrete_scale(p, "fill", opt$palette)
  } else {
    df <- coerce_numeric(df, c(opt$lower_col, opt$upper_col))
    p <- p + geom_ribbon(aes(ymin = .data[[opt$lower_col]], ymax = .data[[opt$upper_col]], group = if (nzchar(opt$group_col)) .data[[opt$group_col]] else 1), fill = "#9EC1DA", alpha = 0.45) +
      geom_line(aes(group = if (nzchar(opt$group_col)) .data[[opt$group_col]] else 1), color = "#2C5D91", linewidth = 0.85)
  }
  legend_fill <- ifelse(mode %in% c("area", "stacked_area") && nzchar(opt$group_col), opt$group_col, "")
  legend_color <- ifelse(mode %in% c("line", "step") && nzchar(opt$group_col), opt$group_col, "")
  p <- p + scale_x_date(date_labels = "%b %d") + theme_rplot(opt$base_font_size, opt$theme) + do.call(labs, label_args(x = opt$date_col, y = opt$value_col, fill = legend_fill, color = legend_color))
  finish_plot(p)
} else if (mode %in% c("grouped_bar", "stacked_bar", "faceted_proportion_bar", "diverging_bar", "errorbar", "pointrange", "dumbbell", "butterfly")) {
  if (mode == "faceted_proportion_bar") {
    value_source <- if (nzchar(opt$value_col)) opt$value_col else opt$weight_col
    input_cols <- unique(c(opt$category_col, opt$group_col, opt$facet_row_col, opt$facet_col_col, value_source))
    df <- prepare_value(input_cols, numeric_cols = value_source)
    facet_row_col <- opt$facet_row_col
    facet_col_col <- opt$facet_col_col
    if (!nzchar(facet_row_col)) {
      df$.facet_row <- "All"
      facet_row_col <- ".facet_row"
    }
    if (!nzchar(facet_col_col) || facet_col_col == facet_row_col) {
      df$.facet_col <- "All"
      facet_col_col <- ".facet_col"
    }
    label_uses_group <- nzchar(opt$label_col) && opt$label_col == opt$group_col
    df <- df %>%
      mutate(
        .value = if (nzchar(value_source)) as.numeric(.data[[value_source]]) else 1,
        .category_wrapped = stringr::str_wrap(as.character(.data[[opt$category_col]]), width = opt$wrap_width)
      )
    facet_row_levels <- unique(as.character(df[[facet_row_col]]))
    facet_col_levels <- unique(as.character(df[[facet_col_col]]))
    category_levels <- rev(unique(df$.category_wrapped))
    group_levels <- unique(as.character(df[[opt$group_col]]))
    df[[facet_row_col]] <- factor(as.character(df[[facet_row_col]]), levels = facet_row_levels)
    df[[facet_col_col]] <- factor(as.character(df[[facet_col_col]]), levels = facet_col_levels)
    df[[opt$group_col]] <- factor(as.character(df[[opt$group_col]]), levels = group_levels)
    df <- df %>%
      group_by(.data[[facet_row_col]], .data[[facet_col_col]], .category_wrapped, .data[[opt$group_col]]) %>%
      summarize(.count = sum(.value, na.rm = TRUE), .groups = "drop") %>%
      group_by(.data[[facet_row_col]], .data[[facet_col_col]], .category_wrapped) %>%
      mutate(
        .total = sum(.count, na.rm = TRUE),
        .prop = ifelse(.total > 0, .count / .total, 0)
      ) %>%
      ungroup()
    if (label_uses_group) {
      df <- df %>% mutate(.label = ifelse(.prop >= opt$label_min_percent, as.character(.data[[opt$group_col]]), ""))
    } else {
      df <- df %>% mutate(.label = ifelse(.prop >= opt$label_min_percent, scales::percent(.prop, accuracy = 1), ""))
    }
    df$.category_wrapped <- factor(df$.category_wrapped, levels = category_levels)
    base_palette <- discrete_palette(opt$palette)
    fill_values <- setNames(base_palette[round(seq(1, length(base_palette), length.out = length(group_levels)))], group_levels)
    p <- ggplot(df, aes(x = .category_wrapped, y = .count, fill = .data[[opt$group_col]])) +
      geom_col(position = position_fill(reverse = TRUE), width = 0.76, color = "grey30", linewidth = 0.22) +
      facet_grid(
        rows = vars(.data[[facet_row_col]]),
        cols = vars(.data[[facet_col_col]]),
        scales = "free_y",
        space = "free_y",
        switch = "y"
      ) +
      coord_flip(clip = "off") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.015))) +
      scale_fill_manual(values = fill_values, na.translate = FALSE, guide = if (label_uses_group) "none" else waiver()) +
      theme_rplot(opt$base_font_size, opt$theme) +
      theme(
        legend.position = "bottom",
        legend.justification = "left",
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = rel(0.72)),
        legend.key.size = unit(0.42, "lines"),
        panel.spacing.x = unit(0.7, "lines"),
        panel.spacing.y = unit(0.45, "lines"),
        strip.placement = "outside",
        strip.background = element_rect(fill = "#F5F7FA", color = "#D7DDE6", linewidth = 0.35),
        strip.text = element_text(face = "bold", color = "#1F2937", size = rel(0.9)),
        axis.text.y = element_text(color = "#1F2937", face = "bold", size = rel(0.78)),
        axis.text.x = element_text(color = "#4B5563", size = rel(0.78)),
        axis.title.y = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(10, 14, 10, 28)
      ) +
      labs(
        title = opt$title,
        x = NULL,
        y = axis_label(opt$y_title, "Proportion"),
        fill = opt$group_col
      )
    if (!label_uses_group) {
      p <- p + guides(fill = guide_legend(nrow = 3, byrow = TRUE, title.position = "left"))
    }
    if (parse_bool(opt$show_labels)) {
      p <- p +
        geom_text(
          aes(label = .label),
          position = position_fill(vjust = 0.5, reverse = TRUE),
          color = "#111827",
          size = max(2.4, opt$base_font_size / 4.4),
          fontface = "bold",
          show.legend = FALSE
        )
    }
    finish_plot(p)
  } else {
  required <- c(opt$category_col, opt$value_col, opt$group_col)
  if (mode %in% c("errorbar", "pointrange")) required <- c(opt$category_col, opt$value_col, opt$lower_col, opt$upper_col, opt$group_col)
  if (mode %in% c("dumbbell", "butterfly")) required <- c(opt$category_col, opt$value_col, opt$value2_col)
  df <- prepare_value(required)
  df <- top_categories(df, opt$category_col, opt$value_col)
  df <- with_wrapped_category(df, opt$category_col)
  if (mode == "grouped_bar") {
    p <- ggplot(df, aes(x = .category_wrapped, y = .data[[opt$value_col]], fill = .data[[opt$group_col]])) +
      geom_col(position = position_dodge(width = 0.72), width = 0.68)
    p <- add_discrete_scale(p, "fill", opt$palette)
  } else if (mode == "stacked_bar") {
    p <- ggplot(df, aes(x = .category_wrapped, y = .data[[opt$value_col]], fill = .data[[opt$group_col]])) +
      geom_col(width = 0.72)
    p <- add_discrete_scale(p, "fill", opt$palette)
  } else if (mode == "diverging_bar") {
    p <- ggplot(df, aes(x = reorder(.category_wrapped, .data[[opt$value_col]]), y = .data[[opt$value_col]], fill = .data[[opt$value_col]] >= 0)) +
      geom_col(width = 0.72, show.legend = FALSE) +
      scale_fill_manual(values = c("TRUE" = "#2C7BB6", "FALSE" = "#D7191C")) +
      coord_flip()
  } else if (mode == "errorbar") {
    df <- coerce_numeric(df, c(opt$lower_col, opt$upper_col))
    p <- ggplot(df, aes(x = .category_wrapped, y = .data[[opt$value_col]], fill = if (nzchar(opt$group_col)) .data[[opt$group_col]] else .category_wrapped)) +
      geom_col(width = 0.62, show.legend = nzchar(opt$group_col)) +
      geom_errorbar(aes(ymin = .data[[opt$lower_col]], ymax = .data[[opt$upper_col]]), width = 0.18, linewidth = 0.45)
    p <- add_discrete_scale(p, "fill", opt$palette)
  } else if (mode == "pointrange") {
    df <- coerce_numeric(df, c(opt$lower_col, opt$upper_col))
    p <- ggplot(df, aes(x = .data[[opt$value_col]], y = reorder(.category_wrapped, .data[[opt$value_col]]))) +
      geom_pointrange(aes(xmin = .data[[opt$lower_col]], xmax = .data[[opt$upper_col]]), linewidth = 0.55, color = "#3B6EA8")
  } else if (mode == "dumbbell") {
    df <- coerce_numeric(df, opt$value2_col)
    p <- ggplot(df, aes(y = reorder(.category_wrapped, .data[[opt$value_col]]))) +
      geom_segment(aes(x = .data[[opt$value_col]], xend = .data[[opt$value2_col]], yend = .category_wrapped), color = "grey70", linewidth = 1.1) +
      geom_point(aes(x = .data[[opt$value_col]]), color = "#3B6EA8", size = 2.8) +
      geom_point(aes(x = .data[[opt$value2_col]]), color = "#D95F02", size = 2.8)
  } else {
    df <- coerce_numeric(df, opt$value2_col)
    long_df <- bind_rows(
      data.frame(.category_wrapped = df$.category_wrapped, .side = opt$value_col, .value = -abs(df[[opt$value_col]])),
      data.frame(.category_wrapped = df$.category_wrapped, .side = opt$value2_col, .value = abs(df[[opt$value2_col]]))
    )
    p <- ggplot(long_df, aes(x = reorder(.category_wrapped, abs(.value)), y = .value, fill = .side)) +
      geom_col(width = 0.72) +
      coord_flip() +
      scale_y_continuous(labels = function(x) abs(x)) +
      scale_fill_palette()
  }
  if (mode == "butterfly") {
    p <- p + theme_rplot(opt$base_font_size, opt$theme) + labs(title = opt$title, x = opt$category_col, y = "Value", fill = "")
  } else {
    p <- p + theme_rplot(opt$base_font_size, opt$theme) + do.call(labs, label_args(x = opt$category_col, y = opt$value_col, fill = opt$group_col))
  }
  finish_plot(p)
  }
} else if (mode == "slope") {
  df <- prepare_value(c(opt$category_col, opt$x_col, opt$value_col, opt$group_col))
  df <- top_categories(df, opt$category_col, opt$value_col)
  df <- with_wrapped_category(df, opt$category_col)
  p <- ggplot(df, aes(x = .data[[opt$x_col]], y = .data[[opt$value_col]], group = .category_wrapped)) +
    geom_line(color = "grey55", linewidth = 0.65) +
    geom_point(aes(color = .category_wrapped), size = 2.6, show.legend = FALSE) +
    geom_text(aes(label = .category_wrapped), hjust = -0.08, size = 3, check_overlap = TRUE) +
    scale_x_discrete(expand = expansion(mult = c(0.08, 0.22))) +
    scale_color_palette() +
    theme_rplot(opt$base_font_size, opt$theme) +
    do.call(labs, label_args(x = opt$x_col, y = opt$value_col))
  finish_plot(p)
} else if (mode %in% c("pie", "donut", "waffle", "treemap", "sunburst", "waterfall", "pareto", "radar", "polar_bar")) {
  required <- c(opt$category_col, opt$value_col)
  if (mode == "sunburst") required <- c(opt$category_col, opt$subcategory_col, opt$value_col)
  if (mode == "radar") required <- c(opt$category_col, opt$value_col, opt$group_col)
  df <- prepare_value(required)
  df <- df[is.finite(df[[opt$value_col]]), , drop = FALSE]
  df <- top_categories(df, opt$category_col, opt$value_col)
  df <- with_wrapped_category(df, opt$category_col)
  if (mode %in% c("pie", "donut")) {
    p <- ggplot(df, aes(x = ifelse(mode == "donut", 2, 1), y = .data[[opt$value_col]], fill = .category_wrapped)) +
      geom_col(width = 1, color = "white", linewidth = 0.3, show.legend = TRUE) +
      coord_polar(theta = "y")
    if (mode == "donut") {
      p <- p + xlim(0.5, 2.5)
    }
    p <- add_discrete_scale(p, "fill", opt$palette) +
      theme_rplot(opt$base_font_size, opt$theme, default = "void") +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank()
      ) +
      labs(title = opt$title, fill = opt$category_col)
  } else if (mode == "polar_bar") {
    df <- df %>% arrange(desc(.data[[opt$value_col]]))
    df$.category_wrapped <- factor(df$.category_wrapped, levels = rev(df$.category_wrapped))
    p <- ggplot(df, aes(x = .category_wrapped, y = .data[[opt$value_col]], fill = .category_wrapped)) +
      geom_col(width = 0.72, color = "white", linewidth = 0.35, show.legend = FALSE) +
      coord_polar(theta = "x", start = -pi / length(unique(df$.category_wrapped)), clip = "off") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
      scale_fill_palette() +
      theme_rplot(opt$base_font_size, opt$theme) +
      theme(
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        plot.margin = margin(10, 18, 10, 18)
      ) +
      labs(title = opt$title)
  } else if (mode == "waffle") {
    total <- sum(abs(df[[opt$value_col]]), na.rm = TRUE)
    df$.tiles <- pmax(1, round(abs(df[[opt$value_col]]) / total * 100))
    waffle_df <- df[rep(seq_len(nrow(df)), df$.tiles), , drop = FALSE]
    waffle_df$.tile <- seq_len(nrow(waffle_df))
    waffle_df$.x <- (waffle_df$.tile - 1) %% 10 + 1
    waffle_df$.y <- floor((waffle_df$.tile - 1) / 10) + 1
    p <- ggplot(waffle_df, aes(x = .x, y = .y, fill = .category_wrapped)) +
      geom_tile(color = "white", linewidth = 0.4) +
      coord_equal() +
      scale_y_reverse() +
      scale_fill_palette() +
      theme_rplot(opt$base_font_size, opt$theme, default = "void") +
      labs(title = opt$title, fill = opt$category_col)
  } else if (mode == "treemap") {
    df <- df %>% arrange(desc(.data[[opt$value_col]]))
    df$.xmax <- cumsum(abs(df[[opt$value_col]])) / sum(abs(df[[opt$value_col]]))
    df$.xmin <- dplyr::lag(df$.xmax, default = 0)
    p <- ggplot(df, aes(xmin = .xmin, xmax = .xmax, ymin = 0, ymax = 1, fill = .category_wrapped)) +
      geom_rect(color = "white", linewidth = 0.5) +
      geom_text(aes(x = (.xmin + .xmax) / 2, y = 0.5, label = .category_wrapped), color = "white", fontface = "bold", size = 3, check_overlap = TRUE) +
      scale_fill_palette() +
      theme_rplot(opt$base_font_size, opt$theme, default = "void") +
      labs(title = opt$title, fill = opt$category_col)
  } else if (mode == "sunburst") {
    parent <- df %>% group_by(.category_wrapped) %>% summarize(.value = sum(.data[[opt$value_col]], na.rm = TRUE), .groups = "drop")
    parent$.ymax <- cumsum(parent$.value) / sum(parent$.value)
    parent$.ymin <- dplyr::lag(parent$.ymax, default = 0)
    child <- df %>% mutate(.subcategory_wrapped = wrap_labels(as.character(.data[[opt$subcategory_col]]), opt$wrap_width)) %>% arrange(.category_wrapped)
    child$.ymax <- cumsum(child[[opt$value_col]]) / sum(child[[opt$value_col]])
    child$.ymin <- dplyr::lag(child$.ymax, default = 0)
    p <- ggplot() +
      geom_rect(data = parent, aes(xmin = 1, xmax = 2, ymin = .ymin, ymax = .ymax, fill = .category_wrapped), color = "white", linewidth = 0.4) +
      geom_rect(data = child, aes(xmin = 2, xmax = 3, ymin = .ymin, ymax = .ymax, fill = .category_wrapped), color = "white", linewidth = 0.25) +
      coord_polar(theta = "y") +
      xlim(0, 3) +
      scale_fill_palette() +
      theme_rplot(opt$base_font_size, opt$theme, default = "void") +
      labs(title = opt$title, fill = opt$category_col)
  } else if (mode == "waterfall") {
    df <- df %>% mutate(.x = row_number(), .start = dplyr::lag(cumsum(.data[[opt$value_col]]), default = 0), .end = cumsum(.data[[opt$value_col]]), .direction = .data[[opt$value_col]] >= 0)
    p <- ggplot(df, aes(fill = .direction)) +
      geom_rect(aes(xmin = .x - 0.36, xmax = .x + 0.36, ymin = pmin(.start, .end), ymax = pmax(.start, .end)), show.legend = FALSE) +
      scale_fill_manual(values = c("TRUE" = "#2C7BB6", "FALSE" = "#D7191C")) +
      scale_x_continuous(breaks = df$.x, labels = df$.category_wrapped) +
      theme_rplot(opt$base_font_size, opt$theme) +
      do.call(labs, label_args(x = opt$category_col, y = "Cumulative value"))
  } else if (mode == "pareto") {
    df <- df %>% arrange(desc(.data[[opt$value_col]])) %>% mutate(.cum_pct = cumsum(.data[[opt$value_col]]) / sum(.data[[opt$value_col]]) * max(.data[[opt$value_col]], na.rm = TRUE))
    p <- ggplot(df, aes(x = reorder(.category_wrapped, -.data[[opt$value_col]]))) +
      geom_col(aes(y = .data[[opt$value_col]]), fill = "#3B6EA8", width = 0.72) +
      geom_line(aes(y = .cum_pct, group = 1), color = "#D95F02", linewidth = 0.85) +
      geom_point(aes(y = .cum_pct), color = "#D95F02", size = 2) +
      theme_rplot(opt$base_font_size, opt$theme) +
      do.call(labs, label_args(x = opt$category_col, y = opt$value_col))
  } else {
    df <- df %>% group_by(.data[[opt$group_col]]) %>% mutate(.scaled = .data[[opt$value_col]] / max(.data[[opt$value_col]], na.rm = TRUE)) %>% ungroup()
    closed <- df %>% group_by(.data[[opt$group_col]]) %>% group_modify(~ bind_rows(.x, .x[1, , drop = FALSE])) %>% ungroup()
    p <- ggplot(closed, aes(x = .category_wrapped, y = .scaled, group = .data[[opt$group_col]], color = .data[[opt$group_col]], fill = .data[[opt$group_col]])) +
      geom_polygon(alpha = 0.16, linewidth = 0.7) +
      geom_point(size = 1.8) +
      coord_polar() +
      ylim(0, 1) +
      scale_color_palette() +
      scale_fill_palette() +
      theme_rplot(opt$base_font_size, opt$theme) +
      theme(panel.grid.minor = element_blank()) +
      theme(axis.title = element_blank()) +
      do.call(labs, label_args(x = NULL, y = NULL, fill = opt$group_col, color = opt$group_col))
  }
  finish_plot(p)
} else if (mode %in% c("ridge", "half_violin", "raincloud", "beeswarm", "beanplot", "density_hist_mirror")) {
  df <- prepare_value(c(opt$category_col, opt$value_col, opt$group_col))
  df <- df[is.finite(df[[opt$value_col]]) & !is.na(df[[opt$category_col]]), , drop = FALSE]
  df <- with_wrapped_category(df, opt$category_col)
  fill_col <- if (nzchar(opt$group_col)) opt$group_col else ".category_wrapped"
  if (mode == "ridge") {
    require_rplot_package("ggridges")
    if (nzchar(opt$group_col)) {
      df$.ridge_group <- factor(
        paste(df$.category_wrapped, df[[opt$group_col]], sep = " - "),
        levels = rev(unique(paste(df$.category_wrapped, df[[opt$group_col]], sep = " - ")))
      )
      ridge_y <- ".ridge_group"
    } else {
      ridge_y <- ".category_wrapped"
    }
    p <- ggplot(df, aes(x = .data[[opt$value_col]], y = .data[[ridge_y]], fill = .data[[fill_col]])) +
      ggridges::geom_density_ridges(alpha = 0.78, scale = 1.04, color = "white", linewidth = 0.25, show.legend = nzchar(opt$group_col)) +
      scale_fill_palette() +
      theme_rplot(opt$base_font_size, opt$theme) +
      do.call(labs, label_args(x = opt$value_col, y = opt$category_col, fill = ifelse(nzchar(opt$group_col), opt$group_col, "")))
  } else if (mode %in% c("half_violin", "raincloud")) {
    require_rplot_package("ggdist")
    dodge_width <- if (nzchar(opt$group_col)) 0.72 else 0
    box_position <- if (nzchar(opt$group_col)) position_dodge(width = dodge_width) else "identity"
    p <- ggplot(df, aes(x = .category_wrapped, y = .data[[opt$value_col]], fill = .data[[fill_col]])) +
      ggdist::stat_halfeye(adjust = 0.75, width = 0.5, justification = -0.16, point_interval = NULL, point_colour = NA, alpha = 0.7, position = box_position, show.legend = FALSE) +
      geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.9, color = "grey25", position = box_position, show.legend = nzchar(opt$group_col))
    if (mode == "raincloud") {
      point_position <- if (nzchar(opt$group_col)) position_jitterdodge(jitter.width = 0.08, jitter.height = 0, dodge.width = dodge_width) else position_jitter(width = 0.08, height = 0)
      p <- p + geom_point(aes(color = .data[[fill_col]]), position = point_position, size = 1.35, alpha = 0.58, show.legend = FALSE)
      p <- add_discrete_scale(p, "color", opt$palette)
    }
    p <- add_discrete_scale(p, "fill", opt$palette) +
      theme_rplot(opt$base_font_size, opt$theme) +
      do.call(labs, label_args(x = opt$category_col, y = opt$value_col, fill = ifelse(nzchar(opt$group_col), opt$group_col, "")))
  } else if (mode == "beeswarm") {
    require_rplot_package("ggbeeswarm")
    dodge_width <- if (nzchar(opt$group_col)) 0.62 else 0
    median_position <- if (nzchar(opt$group_col)) position_dodge(width = dodge_width) else "identity"
    p <- ggplot(df, aes(x = .category_wrapped, y = .data[[opt$value_col]], color = .data[[fill_col]])) +
      ggbeeswarm::geom_quasirandom(aes(group = .data[[fill_col]]), width = 0.18, dodge.width = dodge_width, size = 2.0, alpha = 0.76, show.legend = nzchar(opt$group_col)) +
      stat_summary(aes(group = .data[[fill_col]]), fun = median, geom = "crossbar", width = 0.34, color = "grey20", linewidth = 0.35, position = median_position)
    p <- add_discrete_scale(p, "color", opt$palette) +
      theme_rplot(opt$base_font_size, opt$theme) +
      do.call(labs, label_args(x = opt$category_col, y = opt$value_col, color = ifelse(nzchar(opt$group_col), opt$group_col, "")))
  } else if (mode == "beanplot") {
    dodge_width <- if (nzchar(opt$group_col)) 0.74 else 0
    bean_position <- if (nzchar(opt$group_col)) position_dodge(width = dodge_width) else "identity"
    point_position <- if (nzchar(opt$group_col)) position_jitterdodge(jitter.width = 0.07, jitter.height = 0, dodge.width = dodge_width) else position_jitter(width = 0.09, height = 0)
    p <- ggplot(df, aes(x = .category_wrapped, y = .data[[opt$value_col]], fill = .data[[fill_col]])) +
      geom_violin(trim = FALSE, alpha = 0.58, color = "white", linewidth = 0.25, position = bean_position, show.legend = nzchar(opt$group_col)) +
      geom_point(aes(color = .data[[fill_col]]), position = point_position, size = 1.18, alpha = 0.38, show.legend = FALSE) +
      stat_summary(aes(group = .data[[fill_col]]), fun = mean, geom = "point", shape = 23, size = 2.6, fill = "white", color = "grey15", position = bean_position) +
      scale_fill_palette() +
      scale_color_palette() +
      theme_rplot(opt$base_font_size, opt$theme) +
      do.call(labs, label_args(x = opt$category_col, y = opt$value_col, fill = ifelse(nzchar(opt$group_col), opt$group_col, "")))
  } else {
    if (!nzchar(opt$group_col)) {
      stop("--group_col is required for density_hist_mirror.")
    }
    groups <- unique(as.character(df[[opt$group_col]]))
    if (length(groups) < 2) {
      stop("density_hist_mirror requires at least two groups.")
    }
    group_a <- groups[1]
    group_b <- groups[2]
    p <- ggplot() +
      geom_histogram(
        data = df[df[[opt$group_col]] == group_a, , drop = FALSE],
        aes(x = .data[[opt$value_col]], y = after_stat(count), fill = group_a),
        bins = opt$bins, alpha = 0.76, color = "white", linewidth = 0.15
      ) +
      geom_histogram(
        data = df[df[[opt$group_col]] == group_b, , drop = FALSE],
        aes(x = .data[[opt$value_col]], y = -after_stat(count), fill = group_b),
        bins = opt$bins, alpha = 0.76, color = "white", linewidth = 0.15
      ) +
      geom_hline(yintercept = 0, color = "grey35", linewidth = 0.35) +
      scale_y_continuous(labels = function(x) abs(x)) +
      scale_fill_palette() +
      theme_rplot(opt$base_font_size, opt$theme) +
      do.call(labs, label_args(x = opt$value_col, y = "Count", fill = opt$group_col))
  }
  finish_plot(p)
} else if (mode %in% c("parallel_coordinate", "bump_chart", "streamgraph", "connected_scatter", "dual_axis_line")) {
  if (mode == "parallel_coordinate") {
    value_cols <- numeric_columns(data, opt$value_cols)
    value_cols <- setdiff(value_cols, c(opt$group_col, opt$label_col))
    if (length(value_cols) < 2) {
      stop("parallel_coordinate requires at least two numeric columns via --value_cols or input numeric columns.")
    }
    require_columns(data, c(opt$group_col, opt$label_col, value_cols))
    df <- coerce_numeric(data, value_cols)
    df$.row_id <- if (nzchar(opt$label_col)) as.character(df[[opt$label_col]]) else paste0("Row", seq_len(nrow(df)))
    long <- df %>%
      select(any_of(c(".row_id", opt$group_col, value_cols))) %>%
      pivot_longer(cols = all_of(value_cols), names_to = ".variable", values_to = ".value") %>%
      group_by(.variable) %>%
      mutate(.scaled = rescale_to_range(.value, 0, 1)) %>%
      ungroup()
    p <- ggplot(long, aes(x = .variable, y = .scaled, group = .row_id)) +
      geom_line(aes(color = if (nzchar(opt$group_col)) .data[[opt$group_col]] else .row_id), alpha = 0.56, linewidth = 0.65, show.legend = nzchar(opt$group_col)) +
      geom_point(aes(color = if (nzchar(opt$group_col)) .data[[opt$group_col]] else .row_id), size = 1.5, alpha = 0.75, show.legend = FALSE) +
      scale_color_palette() +
      theme_rplot(opt$base_font_size, opt$theme) +
      do.call(labs, label_args(x = "Variable", y = "Scaled value", color = opt$group_col))
  } else if (mode == "bump_chart") {
    df <- prepare_value(c(opt$category_col, opt$x_col, opt$value_col, opt$group_col))
    df <- top_categories(df, opt$category_col, opt$value_col)
    df$.period <- as.character(df[[opt$x_col]])
    df <- df %>%
      group_by(.period) %>%
      mutate(.rank = dense_rank(desc(.data[[opt$value_col]]))) %>%
      ungroup()
    last_period <- tail(unique(df$.period), 1)
    label_df <- df[df$.period == last_period, , drop = FALSE]
    p <- ggplot(df, aes(x = .period, y = .rank, group = .data[[opt$category_col]], color = .data[[opt$category_col]])) +
      geom_line(linewidth = 0.95, alpha = 0.78, show.legend = FALSE) +
      geom_point(size = 2.3, show.legend = FALSE) +
      geom_text(data = label_df, aes(label = .data[[opt$category_col]]), hjust = -0.08, size = 3, show.legend = FALSE) +
      scale_y_reverse(breaks = sort(unique(df$.rank))) +
      scale_x_discrete(expand = expansion(mult = c(0.04, 0.18))) +
      scale_color_palette() +
      theme_rplot(opt$base_font_size, opt$theme) +
      do.call(labs, label_args(x = opt$x_col, y = "Rank"))
  } else if (mode == "streamgraph") {
    df <- prepare_value(c(opt$date_col, opt$group_col, opt$value_col))
    df$.date <- as.Date(df[[opt$date_col]])
    df <- df[!is.na(df$.date) & is.finite(df[[opt$value_col]]), , drop = FALSE]
    stream <- df %>%
      group_by(.date) %>%
      arrange(.data[[opt$group_col]], .by_group = TRUE) %>%
      mutate(.total = sum(.data[[opt$value_col]], na.rm = TRUE), .ymax = cumsum(.data[[opt$value_col]]) - .total / 2, .ymin = .ymax - .data[[opt$value_col]]) %>%
      ungroup()
    p <- ggplot(stream, aes(x = .date, ymin = .ymin, ymax = .ymax, fill = .data[[opt$group_col]], group = .data[[opt$group_col]])) +
      geom_ribbon(alpha = 0.86, color = "white", linewidth = 0.18) +
      scale_fill_palette() +
      scale_x_date(date_labels = "%b %d") +
      theme_rplot(opt$base_font_size, opt$theme) +
      do.call(labs, label_args(x = opt$date_col, y = "Centered value", fill = opt$group_col))
  } else if (mode == "connected_scatter") {
    df <- prepare_value(c(opt$x_col, opt$y_col, opt$date_col, opt$label_col), numeric_cols = c(opt$x_col, opt$y_col))
    df <- df[is.finite(df[[opt$x_col]]) & is.finite(df[[opt$y_col]]), , drop = FALSE]
    if (nzchar(opt$date_col)) {
      df <- df %>% arrange(.data[[opt$date_col]])
    }
    p <- ggplot(df, aes(x = .data[[opt$x_col]], y = .data[[opt$y_col]])) +
      geom_path(color = "#3B6EA8", linewidth = 0.75, alpha = 0.78, arrow = arrow(length = unit(0.12, "inches"), type = "closed")) +
      theme_rplot(opt$base_font_size, opt$theme)
    if (nzchar(opt$group_col)) {
      require_columns(df, opt$group_col)
      p <- p + geom_point(aes(color = .data[[opt$group_col]]), size = 2.6, alpha = 0.88)
      p <- add_discrete_scale(p, "color", opt$palette)
    } else {
      p <- p + geom_point(size = 2.6, alpha = 0.88, color = "#3B6EA8")
    }
    p <- p + do.call(labs, label_args(x = opt$x_col, y = opt$y_col, color = opt$group_col))
    if (nzchar(opt$label_col) && nrow(df) > 1) {
      p <- p + geom_text(data = df[c(1, nrow(df)), , drop = FALSE], aes(label = .data[[opt$label_col]]), nudge_y = 0.05, size = 3, color = "grey15")
    }
  } else {
    df <- prepare_value(c(opt$date_col, opt$x_col, opt$value_col, opt$value2_col))
    x_axis <- if (nzchar(opt$date_col)) ".date" else opt$x_col
    if (nzchar(opt$date_col)) {
      df$.date <- as.Date(df[[opt$date_col]])
      df <- df[!is.na(df$.date), , drop = FALSE]
    }
    df <- coerce_numeric(df, c(opt$value_col, opt$value2_col))
    y1_min <- min(df[[opt$value_col]], na.rm = TRUE)
    y1_max <- max(df[[opt$value_col]], na.rm = TRUE)
    y2_min <- min(df[[opt$value2_col]], na.rm = TRUE)
    y2_max <- max(df[[opt$value2_col]], na.rm = TRUE)
    df$.value2_scaled <- rescale_to_range(df[[opt$value2_col]], y1_min, y1_max)
    p <- ggplot(df, aes(x = .data[[x_axis]])) +
      geom_line(aes(y = .data[[opt$value_col]], color = opt$value_col, group = 1), linewidth = 0.9) +
      geom_point(aes(y = .data[[opt$value_col]], color = opt$value_col), size = 2) +
      geom_line(aes(y = .value2_scaled, color = opt$value2_col, group = 1), linewidth = 0.9) +
      geom_point(aes(y = .value2_scaled, color = opt$value2_col), size = 2) +
      scale_color_manual(values = c("#3B6EA8", "#D95F02")) +
      scale_y_continuous(
        name = axis_label(opt$y_title, opt$value_col),
        sec.axis = sec_axis(~ (. - y1_min) / (y1_max - y1_min) * (y2_max - y2_min) + y2_min, name = opt$value2_col)
      ) +
      theme_rplot(opt$base_font_size, opt$theme) +
      labs(title = opt$title, x = ifelse(nzchar(opt$date_col), opt$date_col, opt$x_col), color = "")
  }
  finish_plot(p)
} else if (mode %in% c("roc_curve", "calibration_curve", "survival_curve", "forest_plot", "funnel_plot", "taylor_diagram")) {
  if (mode == "roc_curve") {
    df <- prepare_value(c(opt$actual_col, opt$score_col, opt$group_col), numeric_cols = opt$score_col)
    roc <- compute_roc(df, opt$actual_col, opt$score_col, opt$group_col)
    roc <- roc %>% mutate(.legend = paste0(.group, " (AUC=", sprintf("%.2f", .auc), ")"))
    p <- ggplot(roc, aes(x = .fpr, y = .tpr, color = .legend)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey65") +
      geom_line(linewidth = 1) +
      coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
      scale_color_palette() +
      theme_rplot(opt$base_font_size, opt$theme) +
      labs(title = opt$title, x = "False positive rate", y = "True positive rate", color = ifelse(nzchar(opt$group_col), opt$group_col, "Model"))
  } else if (mode == "calibration_curve") {
    df <- prepare_value(c(opt$actual_col, opt$score_col, opt$group_col), numeric_cols = opt$score_col)
    df$.actual <- as_binary(df[[opt$actual_col]])
    df$.score <- pmin(pmax(as.numeric(df[[opt$score_col]]), 0), 1)
    if (!nzchar(opt$group_col)) df$.group <- "All" else df$.group <- as.character(df[[opt$group_col]])
    group_count <- max(1, length(unique(df$.group)))
    calibration_bins <- min(opt$bins, max(4, floor(nrow(df) / (group_count * 3))))
    breaks <- seq(0, 1, length.out = calibration_bins + 1)
    cal <- df %>%
      mutate(.bin = cut(.score, breaks = breaks, include.lowest = TRUE)) %>%
      group_by(.group, .bin) %>%
      summarize(.predicted = mean(.score, na.rm = TRUE), .observed = mean(.actual, na.rm = TRUE), .n = n(), .groups = "drop") %>%
      filter(is.finite(.predicted), is.finite(.observed)) %>%
      arrange(.group, .predicted)
    p <- ggplot(cal, aes(x = .predicted, y = .observed, color = .group, group = .group)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey65") +
      geom_line(linewidth = 0.9) +
      geom_point(aes(size = .n), alpha = 0.86) +
      coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
      scale_color_palette() +
      theme_rplot(opt$base_font_size, opt$theme) +
      labs(title = opt$title, x = "Mean predicted probability", y = "Observed event rate", color = ifelse(nzchar(opt$group_col), opt$group_col, ""), size = "N")
  } else if (mode == "survival_curve") {
    require_rplot_package("survival")
    df <- prepare_value(c(opt$time_col, opt$status_col, opt$group_col), numeric_cols = opt$time_col)
    df$.time <- as.numeric(df[[opt$time_col]])
    df$.status <- as_binary(df[[opt$status_col]])
    df <- df[is.finite(df$.time) & !is.na(df$.status), , drop = FALSE]
    if (nzchar(opt$group_col)) {
      df$.group <- as.character(df[[opt$group_col]])
      fit <- survival::survfit(survival::Surv(.time, .status) ~ .group, data = df)
    } else {
      df$.group <- "All"
      fit <- survival::survfit(survival::Surv(.time, .status) ~ 1, data = df)
    }
    sm <- summary(fit)
    surv <- data.frame(time = sm$time, surv = sm$surv, lower = sm$lower, upper = sm$upper)
    surv$.group <- if (is.null(sm$strata)) "All" else sub("^.*=", "", sm$strata)
    if (nrow(surv) > 0) {
      surv <- bind_rows(lapply(split(surv, surv$.group), function(x) {
        bind_rows(data.frame(time = 0, surv = 1, lower = 1, upper = 1, .group = unique(x$.group)[1]), x)
      }))
    } else {
      surv <- data.frame(time = 0, surv = 1, lower = 1, upper = 1, .group = "All")
    }
    surv$.group <- factor(surv$.group, levels = unique(surv$.group))
    group_levels <- levels(surv$.group)
    group_counts <- table(factor(df$.group, levels = group_levels))
    legend_labels <- paste0(group_levels, " (n=", as.integer(group_counts[group_levels]), ")")
    names(legend_labels) <- group_levels
    survival_palette_name <- tolower(ifelse(tolower(opt$palette) == "editorial", "nature", opt$palette))
    survival_base_palettes <- list(
      okabe_ito = c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#F0E442", "#000000"),
      editorial = c("#3B6EA8", "#D95F02", "#1B9E77", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D"),
      nature = c("#E64B35", "#4DBBD5", "#00A087", "#3C5488", "#F39B7F", "#8491B4", "#91D1C2", "#DC0000"),
      nejm = c("#BC3C29", "#0072B5", "#E18727", "#20854E", "#7876B1", "#6F99AD", "#FFDC91", "#EE4C97"),
      calm = c("#4C78A8", "#F58518", "#54A24B", "#E45756", "#72B7B2", "#B279A2", "#FF9DA6", "#9D755D"),
      vivid = c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2", "#7F7F7F")
    )
    survival_colors <- rep(survival_base_palettes[[survival_palette_name]] %||% discrete_palette(survival_palette_name), length.out = length(group_levels))
    names(survival_colors) <- group_levels
    max_time <- max(df$.time, na.rm = TRUE)
    min_time <- min(0, min(df$.time, na.rm = TRUE))
    time_breaks <- scales::breaks_pretty(n = 5)(c(min_time, max_time))
    time_breaks <- time_breaks[time_breaks >= min_time & time_breaks <= max_time]
    if (!any(time_breaks == 0) && min_time <= 0) {
      time_breaks <- sort(unique(c(0, time_breaks)))
    }
    span <- max(max_time - min_time, 1)
    left_pad <- span * ifelse(parse_bool(opt$show_risk_table), 0.13, 0.02)
    risk_depth <- ifelse(parse_bool(opt$show_risk_table), 0.22 + length(group_levels) * 0.055, 0)

    p <- ggplot(surv, aes(x = time, y = surv, color = .group, fill = .group, group = .group))
    if (parse_bool(opt$show_ci)) {
      p <- p + geom_ribbon(aes(ymin = pmax(lower, 0), ymax = pmin(upper, 1)), alpha = 0.08, color = NA, show.legend = FALSE)
    }
    p <- p + geom_step(linewidth = 0.9, lineend = "butt")

    censor_data <- data.frame()
    if (parse_bool(opt$show_censor) && !is.null(sm$n.censor)) {
      censor_data <- data.frame(time = sm$time, surv = sm$surv, n.censor = sm$n.censor)
      censor_data$.group <- if (is.null(sm$strata)) "All" else sub("^.*=", "", sm$strata)
      censor_data <- censor_data[censor_data$n.censor > 0, , drop = FALSE]
      censor_data$.group <- factor(censor_data$.group, levels = group_levels)
    }
    if (nrow(censor_data) > 0) {
      p <- p + geom_point(data = censor_data, aes(x = time, y = surv, color = .group), inherit.aes = FALSE, shape = 3, size = 2.3, stroke = 0.55, show.legend = FALSE)
    }

    if (parse_bool(opt$show_pvalue) && nzchar(opt$group_col) && length(group_levels) > 1) {
      log_rank <- survival::survdiff(survival::Surv(.time, .status) ~ .group, data = df)
      p_value <- stats::pchisq(log_rank$chisq, df = length(log_rank$n) - 1, lower.tail = FALSE)
      p_label <- ifelse(p_value < 0.001, "Log-rank P < 0.001", paste0("Log-rank P = ", formatC(p_value, format = "f", digits = 3)))
      p <- p + annotate("text", x = min_time + span * 0.04, y = 0.09, label = p_label, hjust = 0, size = 3.1, color = "grey15")
    }

    if (parse_bool(opt$show_risk_table)) {
      risk_summary <- summary(fit, times = time_breaks, extend = TRUE)
      risk_table <- data.frame(time = risk_summary$time, n.risk = risk_summary$n.risk)
      risk_table$.group <- if (is.null(risk_summary$strata)) "All" else sub("^.*=", "", risk_summary$strata)
      risk_table$.group <- factor(risk_table$.group, levels = group_levels)
      risk_y <- setNames(-0.13 - seq_along(group_levels) * 0.06, group_levels)
      risk_table$.risk_y <- risk_y[as.character(risk_table$.group)]
      group_label_data <- data.frame(.group = factor(group_levels, levels = group_levels), .risk_y = risk_y[group_levels], label = group_levels)
      p <- p +
        annotate("text", x = min_time - left_pad * 0.94, y = -0.105, label = "Number at risk", hjust = 0, size = 3.05, fontface = "bold", color = "grey15") +
        geom_text(data = group_label_data, aes(x = min_time - left_pad * 0.94, y = .risk_y, label = label, color = .group), inherit.aes = FALSE, hjust = 0, size = 2.9, show.legend = FALSE) +
        geom_text(data = risk_table, aes(x = time, y = .risk_y, label = n.risk, color = .group), inherit.aes = FALSE, size = 2.9, show.legend = FALSE)
    }

    p <- p +
      scale_color_manual(values = survival_colors, labels = legend_labels, drop = FALSE) +
      scale_fill_manual(values = survival_colors, labels = legend_labels, drop = FALSE) +
      scale_x_continuous(limits = c(min_time - left_pad, max_time), breaks = time_breaks, expand = expansion(mult = c(0, 0.025))) +
      scale_y_continuous(limits = c(-risk_depth, 1.02), breaks = seq(0, 1, by = 0.25), labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.01))) +
      coord_cartesian(clip = "off") +
      theme_rplot(opt$base_font_size, opt$theme) +
      theme(
        legend.position = ifelse(nzchar(opt$group_col), "top", "none"),
        legend.justification = "left",
        legend.title = element_blank(),
        legend.key.width = grid::unit(1.2, "lines"),
        legend.text = element_text(color = "grey10"),
        plot.title = element_text(face = "bold", hjust = 0, color = "grey10"),
        plot.margin = margin(8, 16, 8, 72)
      ) +
      labs(title = opt$title, x = axis_label(opt$x_title, opt$time_col), y = axis_label(opt$y_title, "Survival probability"), color = opt$group_col, fill = opt$group_col)
  } else if (mode == "forest_plot") {
    df <- prepare_value(c(opt$category_col, opt$value_col, opt$lower_col, opt$upper_col, opt$group_col))
    df <- coerce_numeric(df, c(opt$lower_col, opt$upper_col))
    df <- with_wrapped_category(df, opt$category_col)
    p <- ggplot(df, aes(x = .data[[opt$value_col]], y = reorder(.category_wrapped, .data[[opt$value_col]]))) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
      geom_errorbar(aes(xmin = .data[[opt$lower_col]], xmax = .data[[opt$upper_col]], color = if (nzchar(opt$group_col)) .data[[opt$group_col]] else .category_wrapped), orientation = "y", width = 0.18, linewidth = 0.62, show.legend = nzchar(opt$group_col)) +
      geom_point(aes(color = if (nzchar(opt$group_col)) .data[[opt$group_col]] else .category_wrapped), size = 2.6, show.legend = nzchar(opt$group_col)) +
      scale_color_palette() +
      theme_rplot(opt$base_font_size, opt$theme) +
      do.call(labs, label_args(x = opt$value_col, y = opt$category_col, color = opt$group_col))
  } else if (mode == "funnel_plot") {
    df <- prepare_value(c(opt$value_col, opt$value2_col, opt$label_col), numeric_cols = c(opt$value_col, opt$value2_col))
    center <- weighted.mean(df[[opt$value_col]], 1 / pmax(df[[opt$value2_col]], .Machine$double.eps)^2, na.rm = TRUE)
    se_seq <- seq(min(df[[opt$value2_col]], na.rm = TRUE), max(df[[opt$value2_col]], na.rm = TRUE), length.out = 100)
    band <- data.frame(.se = se_seq, .lower = center - 1.96 * se_seq, .upper = center + 1.96 * se_seq)
    p <- ggplot(df, aes(x = .data[[opt$value_col]], y = .data[[opt$value2_col]])) +
      geom_path(data = band, aes(x = .lower, y = .se), color = "grey55", linetype = "dashed") +
      geom_path(data = band, aes(x = .upper, y = .se), color = "grey55", linetype = "dashed") +
      geom_vline(xintercept = center, color = "grey35", linewidth = 0.45) +
      geom_point(color = "#3B6EA8", size = 2.4, alpha = 0.82) +
      scale_y_reverse() +
      theme_rplot(opt$base_font_size, opt$theme) +
      do.call(labs, label_args(x = opt$value_col, y = opt$value2_col))
  } else {
    df <- prepare_value(c(opt$observed_col, opt$predicted_col, opt$group_col), numeric_cols = c(opt$observed_col, opt$predicted_col))
    if (!nzchar(opt$group_col)) df$.group <- "Model" else df$.group <- as.character(df[[opt$group_col]])
    taylor <- df %>%
      group_by(.group) %>%
      summarize(
        .corr = stats::cor(.data[[opt$observed_col]], .data[[opt$predicted_col]], use = "complete.obs"),
        .sd_ratio = stats::sd(.data[[opt$predicted_col]], na.rm = TRUE) / stats::sd(.data[[opt$observed_col]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(.angle = acos(pmin(pmax(.corr, -1), 1)), .x = .sd_ratio * cos(.angle), .y = .sd_ratio * sin(.angle))
    arc <- data.frame(.angle = seq(0, pi / 2, length.out = 160), .r = 1)
    p <- ggplot() +
      geom_path(data = arc, aes(x = cos(.angle), y = sin(.angle)), color = "grey55", linetype = "dashed") +
      geom_segment(aes(x = 0, xend = 1.35, y = 0, yend = 0), color = "grey70") +
      geom_segment(aes(x = 0, xend = 0, y = 0, yend = 1.35), color = "grey70") +
      geom_point(data = taylor, aes(x = .x, y = .y, color = .group), size = 3.2) +
      geom_text(data = taylor, aes(x = .x, y = .y, label = .group), nudge_y = 0.06, size = 3) +
      coord_equal(xlim = c(0, 1.45), ylim = c(0, 1.25)) +
      scale_color_palette() +
      theme_rplot(opt$base_font_size, opt$theme) +
      labs(title = opt$title, x = "Correlation x SD ratio", y = "Centered RMS direction", color = opt$group_col)
  }
  finish_plot(p)
} else if (mode %in% c("likert_plot", "mosaic_plot", "rose_chart", "ma_plot", "bland_altman")) {
  if (mode == "likert_plot") {
    df <- prepare_value(c(opt$category_col, opt$response_col, opt$value_col))
    df$.response <- factor(as.character(df[[opt$response_col]]), levels = unique(as.character(df[[opt$response_col]])))
    response_text <- tolower(as.character(df$.response))
    df$.signed <- ifelse(grepl("disagree|poor|low|worse|negative", response_text), -df[[opt$value_col]], ifelse(grepl("neutral|middle", response_text), df[[opt$value_col]] * 0.5, df[[opt$value_col]]))
    df <- with_wrapped_category(df, opt$category_col)
    p <- ggplot(df, aes(x = .signed, y = .category_wrapped, fill = .response)) +
      geom_col(width = 0.72, color = "white", linewidth = 0.25) +
      geom_vline(xintercept = 0, color = "grey35", linewidth = 0.35) +
      scale_x_continuous(labels = function(x) abs(x)) +
      scale_fill_palette() +
      theme_rplot(opt$base_font_size, opt$theme) +
      labs(title = opt$title, x = opt$value_col, y = opt$category_col, fill = opt$response_col)
  } else if (mode == "mosaic_plot") {
    df <- prepare_value(c(opt$category_col, opt$subcategory_col, opt$value_col))
    df <- df %>% group_by(.data[[opt$category_col]]) %>% mutate(.cat_total = sum(.data[[opt$value_col]], na.rm = TRUE)) %>% ungroup()
    cat_widths <- df %>% distinct(.data[[opt$category_col]], .cat_total) %>% arrange(.data[[opt$category_col]]) %>% mutate(.xmax = cumsum(.cat_total) / sum(.cat_total), .xmin = lag(.xmax, default = 0))
    mos <- df %>%
      left_join(cat_widths, by = opt$category_col) %>%
      group_by(.data[[opt$category_col]]) %>%
      arrange(.data[[opt$subcategory_col]], .by_group = TRUE) %>%
      mutate(.ymax = cumsum(.data[[opt$value_col]]) / sum(.data[[opt$value_col]], na.rm = TRUE), .ymin = lag(.ymax, default = 0)) %>%
      ungroup()
    p <- ggplot(mos, aes(xmin = .xmin, xmax = .xmax, ymin = .ymin, ymax = .ymax, fill = .data[[opt$subcategory_col]])) +
      geom_rect(color = "white", linewidth = 0.4) +
      geom_text(aes(x = (.xmin + .xmax) / 2, y = 1.03, label = .data[[opt$category_col]]), size = 3, color = "grey20") +
      scale_fill_palette() +
      coord_cartesian(clip = "off") +
      theme_rplot(opt$base_font_size, opt$theme, default = "void") +
      labs(title = opt$title, fill = opt$subcategory_col)
  } else if (mode == "rose_chart") {
    df <- prepare_value(c(opt$category_col, opt$value_col, opt$group_col))
    df <- with_wrapped_category(df, opt$category_col)
    p <- ggplot(df, aes(x = .category_wrapped, y = .data[[opt$value_col]], fill = if (nzchar(opt$group_col)) .data[[opt$group_col]] else .category_wrapped)) +
      geom_col(width = 0.9, color = "white", linewidth = 0.25) +
      coord_polar(start = -pi / 12) +
      scale_fill_palette() +
      theme_rplot(opt$base_font_size, opt$theme) +
      theme(axis.title = element_blank(), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank()) +
      labs(title = opt$title, fill = ifelse(nzchar(opt$group_col), opt$group_col, opt$category_col))
  } else if (mode == "ma_plot") {
    df <- prepare_value(c(opt$value_col, opt$value2_col, opt$group_col, opt$label_col), numeric_cols = c(opt$value_col, opt$value2_col))
    p <- ggplot(df, aes(x = .data[[opt$value_col]], y = .data[[opt$value2_col]])) +
      geom_hline(yintercept = 0, color = "grey55", linewidth = 0.35) +
      theme_rplot(opt$base_font_size, opt$theme)
    if (nzchar(opt$group_col)) {
      p <- p + geom_point(aes(color = .data[[opt$group_col]]), size = 1.9, alpha = 0.76)
      p <- add_discrete_scale(p, "color", opt$palette)
    } else {
      p <- p + geom_point(size = 1.9, alpha = 0.76, color = "#3B6EA8")
    }
    p <- p + do.call(labs, label_args(x = opt$value_col, y = opt$value2_col, color = opt$group_col))
  } else {
    df <- prepare_value(c(opt$observed_col, opt$predicted_col, opt$label_col), numeric_cols = c(opt$observed_col, opt$predicted_col))
    df$.mean <- rowMeans(df[, c(opt$observed_col, opt$predicted_col)], na.rm = TRUE)
    df$.diff <- df[[opt$predicted_col]] - df[[opt$observed_col]]
    bias <- mean(df$.diff, na.rm = TRUE)
    loa <- 1.96 * stats::sd(df$.diff, na.rm = TRUE)
    p <- ggplot(df, aes(x = .mean, y = .diff)) +
      geom_hline(yintercept = bias, color = "#3B6EA8", linewidth = 0.6) +
      geom_hline(yintercept = c(bias - loa, bias + loa), color = "#D95F02", linetype = "dashed", linewidth = 0.55) +
      geom_point(color = "#3B6EA8", alpha = 0.78, size = 2.1) +
      theme_rplot(opt$base_font_size, opt$theme) +
      labs(title = opt$title, x = paste("Mean of", opt$observed_col, "and", opt$predicted_col), y = paste(opt$predicted_col, "-", opt$observed_col))
  }
  finish_plot(p)
} else if (mode %in% c("venn_diagram", "euler_diagram", "chord_diagram", "arc_network", "ternary_plot", "seqlogo_plot", "wordcloud_plot")) {
  if (mode %in% c("venn_diagram", "euler_diagram")) {
    df <- read_input_table(opt$data_input)
    require_columns(df, c(opt$label_col, opt$category_col))
    set_list <- split(as.character(df[[opt$label_col]]), as.character(df[[opt$category_col]]))
    set_sizes <- sort(vapply(set_list, function(x) length(unique(x)), numeric(1)), decreasing = TRUE)
    set_names <- names(set_sizes)
    if (length(set_names) < 2) {
      stop("venn_diagram requires at least two sets.")
    }
    if (length(set_names) > 4) {
      set_names <- set_names[seq_len(4)]
      set_list <- set_list[set_names]
    }
    centers <- switch(
      as.character(length(set_names)),
      "2" = data.frame(.set = set_names, .x = c(-0.42, 0.42), .y = c(0, 0)),
      "3" = data.frame(.set = set_names, .x = c(-0.42, 0.42, 0), .y = c(-0.12, -0.12, 0.50)),
      data.frame(.set = set_names, .x = c(-0.42, 0.42, -0.20, 0.20), .y = c(-0.12, -0.12, 0.44, 0.44))
    )
    if (nrow(centers) == 4) {
      centers$.label_x <- c(-0.82, 0.82, -0.82, 0.82)
      centers$.label_y <- c(-0.62, -0.62, 1.16, 1.16)
    } else {
      centers$.label_x <- centers$.x
      centers$.label_y <- centers$.y + ifelse(centers$.y >= 0, 0.72, -0.72)
    }
    circle_points <- function(cx, cy, r = 0.62, n = 220) {
      theta <- seq(0, 2 * pi, length.out = n)
      data.frame(.x = cx + r * cos(theta), .y = cy + r * sin(theta))
    }
    circles <- bind_rows(lapply(seq_len(nrow(centers)), function(i) {
      cbind(.set = centers$.set[i], circle_points(centers$.x[i], centers$.y[i]))
    }))
    pair_counts <- bind_rows(utils::combn(set_names, 2, simplify = FALSE, FUN = function(pair) {
      c1 <- centers[centers$.set == pair[1], ]
      c2 <- centers[centers$.set == pair[2], ]
      data.frame(
        .x = mean(c(c1$.x, c2$.x)),
        .y = mean(c(c1$.y, c2$.y)),
        .label = length(Reduce(intersect, lapply(set_list[pair], unique)))
      )
    }))
    all_count <- length(Reduce(intersect, lapply(set_list, unique)))
    center_count <- data.frame(.x = mean(centers$.x), .y = mean(centers$.y), .label = all_count)
    p <- ggplot() +
      geom_polygon(data = circles, aes(x = .x, y = .y, group = .set, fill = .set), color = "grey25", linewidth = 0.55, alpha = 0.28) +
      geom_label(data = centers, aes(x = .label_x, y = .label_y, label = paste0(.set, "\nN=", vapply(set_list[.set], function(x) length(unique(x)), numeric(1)))), size = 2.8, linewidth = 0, fill = "white", alpha = 0.9) +
      geom_text(data = pair_counts, aes(x = .x, y = .y, label = .label), size = 4, fontface = "bold", color = "grey20") +
      geom_text(data = center_count, aes(x = .x, y = .y - 0.12, label = .label), size = 4.3, fontface = "bold", color = "grey10") +
      scale_fill_palette() +
      coord_equal(xlim = c(-1.25, 1.25), ylim = c(-1.05, 1.48), clip = "off") +
      theme_rplot(opt$base_font_size, opt$theme, default = "void") +
      theme(legend.position = "none", plot.title = element_text(face = "bold")) +
      labs(title = opt$title)
    finish_plot(p)
  } else if (mode == "chord_diagram") {
    require_rplot_package("circlize")
    df <- prepare_value(c(opt$from_col, opt$to_col, opt$weight_col), numeric_cols = opt$weight_col)
    chord_df <- df[, c(opt$from_col, opt$to_col, opt$weight_col), drop = FALSE]
    colnames(chord_df) <- c("from", "to", "value")
    grid_cols <- setNames(discrete_palette(opt$palette)[seq_along(unique(c(chord_df$from, chord_df$to)))], unique(c(chord_df$from, chord_df$to)))
    draw_fn <- function() {
      circlize::circos.clear()
      circlize::chordDiagram(chord_df, grid.col = grid_cols, transparency = 0.25, annotationTrack = "grid", preAllocateTracks = 1)
      circlize::circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
        sector <- circlize::get.cell.meta.data("sector.index")
        xlim <- circlize::get.cell.meta.data("xlim")
        ylim <- circlize::get.cell.meta.data("ylim")
        circlize::circos.text(mean(xlim), ylim[1] + 0.1, sector, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex = 0.72)
      }, bg.border = NA)
      title(opt$title)
      circlize::circos.clear()
    }
    save_base_plot(opt$plot_name, draw_fn, opt$fig_width, opt$fig_height)
  } else if (mode == "arc_network") {
    df <- prepare_value(c(opt$from_col, opt$to_col, opt$weight_col), numeric_cols = opt$weight_col)
    nodes <- sort(unique(c(as.character(df[[opt$from_col]]), as.character(df[[opt$to_col]]))))
    node_df <- data.frame(.node = nodes, .x = seq_along(nodes), .y = 0)
    edges <- df %>% mutate(.from = as.character(.data[[opt$from_col]]), .to = as.character(.data[[opt$to_col]])) %>% left_join(node_df, by = c(".from" = ".node")) %>% rename(.x = .x) %>% left_join(node_df, by = c(".to" = ".node"), suffix = c("1", "2"))
    p <- ggplot() +
      geom_curve(data = edges, aes(x = .x1, xend = .x2, y = 0, yend = 0, linewidth = .data[[opt$weight_col]], color = .data[[opt$weight_col]]), curvature = 0.42, alpha = 0.72) +
      geom_point(data = node_df, aes(x = .x, y = .y), size = 3, color = "grey20") +
      geom_text(data = node_df, aes(x = .x, y = -0.08, label = .node), angle = 35, hjust = 1, vjust = 1, size = 3) +
      coord_cartesian(xlim = c(0.5, length(nodes) + 0.5), ylim = c(-0.30, 0.38), clip = "off") +
      scale_x_continuous(expand = expansion(mult = c(0.02, 0.02))) +
      scale_color_gradient(low = "#9EC1DA", high = "#B2182B") +
      scale_linewidth(range = c(0.25, 1.7), guide = "none") +
      theme_rplot(opt$base_font_size, opt$theme, default = "void") +
      theme(plot.margin = margin(8, 18, 62, 18)) +
      labs(title = opt$title, color = opt$weight_col)
    finish_plot(p)
  } else if (mode == "ternary_plot") {
    df <- prepare_value(c(opt$a_col, opt$b_col, opt$c_col, opt$group_col, opt$label_col), numeric_cols = c(opt$a_col, opt$b_col, opt$c_col))
    total <- df[[opt$a_col]] + df[[opt$b_col]] + df[[opt$c_col]]
    df$.a <- df[[opt$a_col]] / total
    df$.b <- df[[opt$b_col]] / total
    df$.c <- df[[opt$c_col]] / total
    df$.x <- df$.b + df$.c / 2
    df$.y <- sqrt(3) / 2 * df$.c
    tri <- data.frame(.x = c(0, 1, 0.5, 0), .y = c(0, 0, sqrt(3) / 2, 0))
    p <- ggplot() +
      geom_path(data = tri, aes(x = .x, y = .y), color = "grey30", linewidth = 0.7) +
      annotate("text", x = -0.04, y = -0.03, label = opt$a_col, hjust = 1, size = 3.2) +
      annotate("text", x = 1.04, y = -0.03, label = opt$b_col, hjust = 0, size = 3.2) +
      annotate("text", x = 0.5, y = sqrt(3) / 2 + 0.04, label = opt$c_col, size = 3.2) +
      coord_equal(xlim = c(-0.08, 1.08), ylim = c(-0.08, 0.95)) +
      theme_rplot(opt$base_font_size, opt$theme, default = "void") +
      labs(title = opt$title, color = opt$group_col)
    if (nzchar(opt$group_col)) {
      p <- p + geom_point(data = df, aes(x = .x, y = .y, color = .data[[opt$group_col]]), size = 2.8, alpha = 0.86)
      p <- p + scale_color_palette()
    } else {
      p <- p + geom_point(data = df, aes(x = .x, y = .y), size = 2.8, alpha = 0.86, color = "#3B6EA8")
    }
    finish_plot(p)
  } else if (mode == "seqlogo_plot") {
    require_rplot_package("ggseqlogo")
    df <- read_input_table(opt$data_input)
    require_columns(df, opt$sequence_col)
    seqs <- toupper(as.character(df[[opt$sequence_col]]))
    p <- suppressWarnings(ggseqlogo::ggseqlogo(seqs, method = "prob")) +
      theme_rplot(opt$base_font_size, opt$theme) +
      labs(title = opt$title, x = "Position", y = "Probability")
    suppressWarnings(finish_plot(p))
  } else {
    require_rplot_package("ggwordcloud")
    df <- prepare_value(c(opt$category_col, opt$value_col), numeric_cols = opt$value_col)
    df <- top_categories(df, opt$category_col, opt$value_col)
    word_max_size <- ifelse(nrow(df) <= 10, 42, 30)
    p <- ggplot(df, aes(x = 0, y = 0, label = .data[[opt$category_col]], size = .data[[opt$value_col]], color = .data[[opt$category_col]])) +
      ggwordcloud::geom_text_wordcloud_area(rm_outside = TRUE, eccentricity = 0.82, seed = 42, grid_size = 2, xlim = c(-0.95, 0.95), ylim = c(-0.52, 0.52)) +
      scale_size_area(max_size = word_max_size) +
      scale_color_palette() +
      coord_equal(xlim = c(-1, 1), ylim = c(-0.58, 0.58), clip = "off") +
      theme_rplot(opt$base_font_size, opt$theme, default = "void") +
      theme(legend.position = "none", plot.margin = margin(8, 8, 8, 8)) +
      labs(title = opt$title)
    finish_plot(p)
  }
} else if (mode %in% c("volcano_plot", "volcano", "enrichment_dotplot", "enrichment_barplot")) {
  if (mode %in% c("volcano_plot", "volcano")) {
    df <- prepare_value(c(opt$value_col, opt$value2_col, opt$label_col, opt$group_col), numeric_cols = c(opt$value_col, opt$value2_col))
    df$.effect <- as.numeric(df[[opt$value_col]])
    df$.pvalue <- pmax(as.numeric(df[[opt$value2_col]]), .Machine$double.xmin)
    if (nzchar(opt$group_col)) {
      df$.status <- as.character(df[[opt$group_col]])
    } else {
      df$.status <- ifelse(
        df$.pvalue <= opt$p_threshold & df$.effect >= opt$fc_threshold,
        "Up",
        ifelse(df$.pvalue <= opt$p_threshold & df$.effect <= -opt$fc_threshold, "Down", "NS")
      )
    }
    status_order <- c("Down", "NS", "Up")
    status_values <- unique(as.character(df$.status))
    df$.status <- factor(df$.status, levels = c(intersect(status_order, status_values), setdiff(sort(status_values), status_order)))
    legend_col <- ifelse(nzchar(opt$group_col), opt$group_col, "status")
    df[[legend_col]] <- df$.status
    df$.neg_log10_p <- -log10(df$.pvalue)
    df$.row_id <- seq_len(nrow(df))
    df$.plot_label <- NA_character_
    if (nzchar(opt$label_col) && opt$label_top_n > 0) {
      label_df <- df %>%
        filter(.status %in% c("Up", "Down")) %>%
        group_by(.status) %>%
        arrange(.pvalue, desc(abs(.effect)), .by_group = TRUE) %>%
        slice_head(n = opt$label_top_n) %>%
        ungroup()
      df$.plot_label[match(label_df$.row_id, df$.row_id)] <- as.character(label_df[[opt$label_col]])
    }
    status_levels <- levels(df$.status)
    volcano_colors <- setNames(discrete_palette(opt$palette)[seq_along(status_levels)], status_levels)
    volcano_colors[names(volcano_colors) == "Down"] <- "#2166AC"
    volcano_colors[names(volcano_colors) == "NS"] <- "grey72"
    volcano_colors[names(volcano_colors) == "Up"] <- "#B2182B"
    p <- volcanolabel::volcano_plot(
      df,
      x = ".effect",
      y = ".neg_log10_p",
      label = ".plot_label",
      color = legend_col,
      x_cutoff = opt$fc_threshold,
      y_cutoff = -log10(opt$p_threshold),
      title = opt$title,
      palette = volcano_colors,
      point_size = 1.75,
      point_alpha = 0.78,
      label_size = max(2.8, opt$base_font_size / 4),
      label_color = "#1F2937",
      label_segment_color = "grey38",
      label_segment_size = 0.34,
      label_segment_alpha = 0.9,
      base_size = opt$base_font_size,
      legend_position = "top",
      xlab = axis_label(opt$x_title, opt$value_col),
      ylab = paste0("-log10(", opt$value2_col, ")")
    )
    finish_plot(p)
  } else if (mode == "enrichment_dotplot") {
    df <- prepare_value(c(opt$category_col, opt$value_col, opt$value2_col, opt$weight_col, opt$group_col), numeric_cols = c(opt$value_col, opt$value2_col, opt$weight_col))
    df$.score <- if (nzchar(opt$value2_col)) -log10(pmax(as.numeric(df[[opt$value2_col]]), .Machine$double.xmin)) else abs(as.numeric(df[[opt$value_col]]))
    df <- df %>% arrange(desc(.score))
    if (opt$top_n > 0) df <- df %>% slice_head(n = opt$top_n)
    df <- df %>% arrange(.score)
    df$.term_wrapped <- stringr::str_wrap(as.character(df[[opt$category_col]]), width = opt$wrap_width)
    df$.term_wrapped <- factor(df$.term_wrapped, levels = unique(df$.term_wrapped))
    p <- ggplot(df, aes(x = .data[[opt$value_col]], y = .term_wrapped)) +
      geom_point(aes(size = if (nzchar(opt$weight_col)) .data[[opt$weight_col]] else .score, color = .score), alpha = 0.86) +
      scale_color_gradient(low = "#4C78A8", high = "#B2182B") +
      scale_size_continuous(range = c(2.2, 7)) +
      theme_rplot(opt$base_font_size, opt$theme) +
      labs(title = opt$title, x = axis_label(opt$x_title, opt$value_col), y = opt$category_col, color = ifelse(nzchar(opt$value2_col), paste0("-log10(", opt$value2_col, ")"), "score"), size = ifelse(nzchar(opt$weight_col), opt$weight_col, "score"))
    if (nzchar(opt$group_col)) {
      p <- p + facet_grid(rows = vars(.data[[opt$group_col]]), scales = "free_y", space = "free_y")
    }
    finish_plot(p)
  } else {
    df <- prepare_value(c(opt$category_col, opt$value_col, opt$value2_col, opt$group_col), numeric_cols = c(opt$value_col, opt$value2_col))
    df$.bar_value <- if (nzchar(opt$value2_col)) -log10(pmax(as.numeric(df[[opt$value2_col]]), .Machine$double.xmin)) else as.numeric(df[[opt$value_col]])
    df <- df %>% arrange(desc(.bar_value))
    if (opt$top_n > 0) df <- df %>% slice_head(n = opt$top_n)
    df <- df %>% arrange(.bar_value)
    df$.term_wrapped <- stringr::str_wrap(as.character(df[[opt$category_col]]), width = opt$wrap_width)
    p <- ggplot(df, aes(x = .bar_value, y = reorder(.term_wrapped, .bar_value), fill = if (nzchar(opt$group_col)) .data[[opt$group_col]] else .term_wrapped)) +
      geom_col(width = 0.72, color = "white", linewidth = 0.25, show.legend = nzchar(opt$group_col)) +
      scale_fill_palette() +
      theme_rplot(opt$base_font_size, opt$theme) +
      labs(title = opt$title, x = ifelse(nzchar(opt$value2_col), paste0("-log10(", opt$value2_col, ")"), opt$value_col), y = opt$category_col, fill = opt$group_col)
    finish_plot(p)
  }
} else if (mode %in% c("tile_heatmap", "calendar_heatmap")) {
  if (mode == "tile_heatmap") {
    df <- prepare_value(c(opt$x_col, opt$y_col, opt$value_col))
    p <- ggplot(df, aes(x = .data[[opt$x_col]], y = .data[[opt$y_col]], fill = .data[[opt$value_col]])) +
      geom_tile(color = "white", linewidth = 0.35) +
      scale_fill_gradient2(low = "#2166AC", mid = "#F7F7F7", high = "#B2182B", midpoint = median(df[[opt$value_col]], na.rm = TRUE)) +
      theme_rplot(opt$base_font_size, opt$theme) +
      do.call(labs, label_args(x = opt$x_col, y = opt$y_col, fill = opt$value_col))
  } else {
    df <- prepare_value(c(opt$date_col, opt$value_col))
    df$.date <- as.Date(df[[opt$date_col]])
    df$.week <- as.numeric(format(df$.date, "%W"))
    df$.weekday <- factor(format(df$.date, "%a"), levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
    p <- ggplot(df, aes(x = .week, y = .weekday, fill = .data[[opt$value_col]])) +
      geom_tile(color = "white", linewidth = 0.4) +
      scale_fill_gradient(low = "#DCE9F5", high = "#2C5D91") +
      theme_rplot(opt$base_font_size, opt$theme) +
      do.call(labs, label_args(x = "Week", y = "Day", fill = opt$value_col))
  }
  finish_plot(p)
} else if (mode == "gantt") {
  df <- prepare_value(c(opt$category_col, opt$start_col, opt$end_col, opt$group_col))
  df$.start <- as.Date(df[[opt$start_col]])
  df$.end <- as.Date(df[[opt$end_col]])
  df <- df[!is.na(df$.start) & !is.na(df$.end), , drop = FALSE]
  df <- with_wrapped_category(df, opt$category_col)
  p <- ggplot(df, aes(y = reorder(.category_wrapped, .start))) +
    geom_segment(aes(x = .start, xend = .end, yend = .category_wrapped, color = if (nzchar(opt$group_col)) .data[[opt$group_col]] else .category_wrapped), linewidth = 6, lineend = "round") +
    scale_x_date(date_labels = "%b %d") +
    scale_color_palette() +
    theme_rplot(opt$base_font_size, opt$theme) +
    do.call(labs, label_args(x = "Date", y = opt$category_col, color = opt$group_col))
  finish_plot(p)
} else {
  stop("Unsupported --mode: ", opt$mode)
}
