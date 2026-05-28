args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 1 && nzchar(args[1])) args[1] else file.path(".test_output", "all_examples")

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_file <- if (length(file_arg)) sub("^--file=", "", file_arg[1]) else file.path("scripts", "run_all_examples.R")
repo_root <- normalizePath(file.path(dirname(script_file), ".."), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(repo_root, "SKILL.md"))) {
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

path <- function(...) normalizePath(file.path(repo_root, ...), winslash = "/", mustWork = TRUE)
script <- function(...) path("scripts", ...)
data <- function(name) path("examples", "data", name)
plot_prefix <- function(id) file.path(out_dir, id)

cmd <- function(id, script_path, args) {
  list(id = id, script = script_path, args = args)
}

gallery <- function(id, mode, input, extra) {
  cmd(
    id,
    script("chart_gallery", "chart_gallery.R"),
    c("--data_input", data(input), "--mode", mode, extra, "--plot_name", plot_prefix(id))
  )
}

article <- function(id, mode) {
  cmd(
    id,
    script("article_gallery", "article_gallery.R"),
    c("--data_input", data("article_gallery_demo.tsv"), "--mode", mode, "--plot_name", plot_prefix(id))
  )
}

examples <- list(
  cmd("scatter_bubble_plot", script("scatter_bubble", "scatter_bubble_plot.R"), c("--data_input", data("generic_measurements.tsv"), "--x_col", "score_a", "--y_col", "abundance", "--color_col", "group", "--size_col", "score_b", "--label_col", "biomarker", "--label_top_n", "5", "--plot_name", plot_prefix("scatter_bubble_plot"))),
  cmd("box_violin_plot", script("box_violin", "box_violin_plot.R"), c("--data_input", data("generic_measurements.tsv"), "--x_col", "group", "--y_col", "abundance", "--mode", "box_violin", "--sort_by_median", "TRUE", "--plot_name", plot_prefix("box_violin_plot"))),
  cmd("bar_lollipop_plot", script("bar_lollipop", "bar_lollipop_plot.R"), c("--data_input", data("category_scores.tsv"), "--category_col", "term", "--value_col", "score", "--color_col", "category", "--mode", "lollipop", "--top_n", "12", "--plot_name", plot_prefix("bar_lollipop_plot"))),
  cmd("correlation_heatmap", script("correlation_heatmap", "correlation_heatmap.R"), c("--data_input", data("generic_measurements.tsv"), "--columns", "score_a,score_b,abundance", "--plot_name", plot_prefix("correlation_heatmap"))),
  cmd("matrix_heatmap", script("matrix_heatmap", "matrix_heatmap.R"), c("--input_matrix", data("expression_matrix.tsv"), "--sample_info", data("sample_info.tsv"), "--id_col", "feature", "--plot_name", plot_prefix("matrix_heatmap"))),
  cmd("pca_plot", script("pca_plot", "pca_plot.R"), c("--input_matrix", data("expression_matrix.tsv"), "--sample_info", data("sample_info.tsv"), "--id_col", "feature", "--plot_name", plot_prefix("pca_plot"))),
  cmd("upset_plot", script("upset_plot", "upset_plot.R"), c("--data_input", data("set_membership.tsv"), "--item_col", "item", "--set_col", "set", "--plot_name", plot_prefix("upset_plot"))),
  cmd("alluvial_plot", script("alluvial_plot", "alluvial_plot.R"), c("--data_input", data("alluvial_paths.tsv"), "--axis_cols", "source,class,outcome", "--weight_col", "count", "--fill_col", "source", "--plot_name", plot_prefix("alluvial_plot"))),

  gallery("histogram_plot", "histogram", "gallery_points.tsv", c("--value_col", "value", "--group_col", "group")),
  gallery("density_plot", "density", "gallery_points.tsv", c("--value_col", "value", "--group_col", "group")),
  gallery("frequency_polygon_plot", "frequency_polygon", "gallery_points.tsv", c("--value_col", "value", "--group_col", "group")),
  gallery("ecdf_plot", "ecdf", "gallery_points.tsv", c("--value_col", "value", "--group_col", "group")),
  gallery("qq_plot", "qq_plot", "gallery_points.tsv", c("--value_col", "value", "--group_col", "group")),
  gallery("dot_plot", "dot_plot", "gallery_category.tsv", c("--category_col", "category", "--value_col", "value", "--group_col", "group")),
  gallery("jitter_strip_plot", "jitter_strip", "gallery_points.tsv", c("--category_col", "category", "--value_col", "value", "--group_col", "group")),
  gallery("hexbin_plot", "hexbin", "gallery_points.tsv", c("--x_col", "x", "--y_col", "y")),
  gallery("density2d_contour_plot", "density2d_contour", "gallery_points.tsv", c("--x_col", "x", "--y_col", "y")),
  gallery("density2d_filled_plot", "density2d_filled", "gallery_points.tsv", c("--x_col", "x", "--y_col", "y")),
  gallery("line_plot", "line", "gallery_time.tsv", c("--date_col", "date", "--value_col", "value", "--group_col", "group")),
  gallery("area_plot", "area", "gallery_time.tsv", c("--date_col", "date", "--value_col", "value", "--group_col", "group")),
  gallery("stacked_area_plot", "stacked_area", "gallery_time.tsv", c("--date_col", "date", "--value_col", "value", "--group_col", "group")),
  gallery("step_plot", "step", "gallery_time.tsv", c("--date_col", "date", "--value_col", "value", "--group_col", "group")),
  gallery("ribbon_plot", "ribbon", "gallery_time.tsv", c("--date_col", "date", "--value_col", "value", "--lower_col", "lower", "--upper_col", "upper", "--group_col", "group")),
  gallery("grouped_bar_plot", "grouped_bar", "gallery_category.tsv", c("--category_col", "category", "--value_col", "value", "--group_col", "group")),
  gallery("stacked_bar_plot", "stacked_bar", "gallery_category.tsv", c("--category_col", "category", "--value_col", "value", "--group_col", "group")),
  gallery("faceted_proportion_bar_plot", "faceted_proportion_bar", "gallery_faceted_proportion.tsv", c("--category_col", "variable", "--group_col", "value", "--value_col", "count", "--label_col", "value", "--facet_row_col", "type", "--facet_col_col", "RCB", "--show_labels", "true", "--label_min_percent", "0.08", "--wrap_width", "24")),
  gallery("diverging_bar_plot", "diverging_bar", "gallery_change.tsv", c("--category_col", "category", "--value_col", "value")),
  gallery("errorbar_plot", "errorbar", "gallery_category.tsv", c("--category_col", "category", "--value_col", "value", "--lower_col", "lower", "--upper_col", "upper", "--group_col", "group")),
  gallery("pointrange_plot", "pointrange", "gallery_category.tsv", c("--category_col", "category", "--value_col", "value", "--lower_col", "lower", "--upper_col", "upper", "--group_col", "group")),
  gallery("dumbbell_plot", "dumbbell", "gallery_change.tsv", c("--category_col", "category", "--value_col", "value", "--value2_col", "value2")),
  gallery("slope_chart", "slope", "gallery_slope.tsv", c("--category_col", "category", "--x_col", "period", "--value_col", "value", "--group_col", "group")),
  gallery("butterfly_chart", "butterfly", "gallery_change.tsv", c("--category_col", "category", "--value_col", "value", "--value2_col", "value2")),
  gallery("pie_chart", "pie", "gallery_composition.tsv", c("--category_col", "category", "--value_col", "value")),
  gallery("donut_chart", "donut", "gallery_composition.tsv", c("--category_col", "category", "--value_col", "value")),
  gallery("waffle_chart", "waffle", "gallery_composition.tsv", c("--category_col", "category", "--value_col", "value")),
  gallery("treemap_chart", "treemap", "gallery_composition.tsv", c("--category_col", "category", "--value_col", "value")),
  gallery("sunburst_chart", "sunburst", "gallery_hierarchy.tsv", c("--category_col", "category", "--subcategory_col", "subcategory", "--value_col", "value")),
  gallery("waterfall_chart", "waterfall", "gallery_change.tsv", c("--category_col", "category", "--value_col", "value")),
  gallery("pareto_chart", "pareto", "gallery_composition.tsv", c("--category_col", "category", "--value_col", "value")),
  gallery("radar_chart", "radar", "gallery_category.tsv", c("--category_col", "category", "--value_col", "value", "--group_col", "group")),
  gallery("polar_bar_chart", "polar_bar", "gallery_composition.tsv", c("--category_col", "category", "--value_col", "value")),
  gallery("tile_heatmap", "tile_heatmap", "gallery_grid.tsv", c("--x_col", "x", "--y_col", "y", "--value_col", "value")),
  gallery("calendar_heatmap", "calendar_heatmap", "gallery_grid.tsv", c("--date_col", "date", "--value_col", "value")),
  gallery("gantt_chart", "gantt", "gallery_tasks.tsv", c("--category_col", "task", "--start_col", "start", "--end_col", "end", "--group_col", "group")),

  gallery("ridge_plot", "ridge", "gallery_distribution.tsv", c("--category_col", "category", "--value_col", "value", "--group_col", "group")),
  gallery("half_violin_plot", "half_violin", "gallery_distribution.tsv", c("--category_col", "category", "--value_col", "value", "--group_col", "group")),
  gallery("raincloud_plot", "raincloud", "gallery_distribution.tsv", c("--category_col", "category", "--value_col", "value", "--group_col", "group")),
  gallery("beeswarm_plot", "beeswarm", "gallery_distribution.tsv", c("--category_col", "category", "--value_col", "value", "--group_col", "group")),
  gallery("bean_plot", "beanplot", "gallery_distribution.tsv", c("--category_col", "category", "--value_col", "value", "--group_col", "group")),
  gallery("mirrored_histogram", "density_hist_mirror", "gallery_distribution.tsv", c("--category_col", "category", "--value_col", "value", "--group_col", "group")),
  gallery("parallel_coordinate_plot", "parallel_coordinate", "gallery_points.tsv", c("--value_cols", "value,value2,x,y", "--group_col", "group")),
  gallery("bump_chart", "bump_chart", "gallery_slope.tsv", c("--category_col", "category", "--x_col", "period", "--value_col", "value", "--group_col", "group")),
  gallery("streamgraph", "streamgraph", "gallery_time.tsv", c("--date_col", "date", "--value_col", "value", "--group_col", "group")),
  gallery("connected_scatter_plot", "connected_scatter", "gallery_points.tsv", c("--x_col", "x", "--y_col", "y", "--group_col", "group", "--date_col", "date")),
  gallery("dual_axis_line", "dual_axis_line", "gallery_time.tsv", c("--date_col", "date", "--value_col", "value", "--value2_col", "lower", "--group_col", "group")),
  gallery("roc_curve", "roc_curve", "gallery_model.tsv", c("--actual_col", "actual", "--score_col", "score", "--group_col", "group")),
  gallery("calibration_curve", "calibration_curve", "gallery_model.tsv", c("--actual_col", "actual", "--score_col", "score", "--group_col", "group")),
  gallery("survival_curve", "survival_curve", "gallery_survival.tsv", c("--time_col", "time", "--status_col", "status", "--group_col", "group")),
  gallery("forest_plot", "forest_plot", "gallery_forest.tsv", c("--category_col", "term", "--value_col", "estimate", "--lower_col", "lower", "--upper_col", "upper", "--group_col", "group")),
  gallery("funnel_plot", "funnel_plot", "gallery_model.tsv", c("--value_col", "value", "--value2_col", "value2")),
  gallery("taylor_diagram", "taylor_diagram", "gallery_model.tsv", c("--observed_col", "observed", "--predicted_col", "predicted", "--group_col", "group")),
  gallery("likert_plot", "likert_plot", "gallery_likert.tsv", c("--category_col", "question", "--response_col", "response", "--value_col", "value")),
  gallery("mosaic_plot", "mosaic_plot", "gallery_hierarchy.tsv", c("--category_col", "category", "--subcategory_col", "subcategory", "--value_col", "value")),
  gallery("rose_chart", "rose_chart", "gallery_composition.tsv", c("--category_col", "category", "--value_col", "value")),
  gallery("ma_plot", "ma_plot", "gallery_model.tsv", c("--value_col", "value", "--value2_col", "value2", "--label_col", "sample")),
  gallery("bland_altman_plot", "bland_altman", "gallery_model.tsv", c("--observed_col", "observed", "--predicted_col", "predicted", "--group_col", "group")),
  gallery("volcano_plot", "volcano", "gallery_differential.tsv", c("--value_col", "log2fc", "--value2_col", "padj", "--label_col", "feature", "--group_col", "status")),
  gallery("enrichment_dotplot", "enrichment_dotplot", "gallery_enrichment.tsv", c("--category_col", "term", "--value_col", "gene_ratio", "--value2_col", "padj", "--weight_col", "count", "--group_col", "category")),
  gallery("enrichment_barplot", "enrichment_barplot", "gallery_enrichment.tsv", c("--category_col", "term", "--value_col", "gene_ratio", "--value2_col", "padj", "--group_col", "category")),
  gallery("venn_diagram", "venn_diagram", "set_membership.tsv", c("--category_col", "item", "--group_col", "set")),
  gallery("chord_diagram", "chord_diagram", "gallery_edges.tsv", c("--from_col", "from", "--to_col", "to", "--weight_col", "weight")),
  gallery("arc_network", "arc_network", "gallery_edges.tsv", c("--from_col", "from", "--to_col", "to", "--weight_col", "weight")),
  gallery("ternary_plot", "ternary_plot", "gallery_ternary.tsv", c("--a_col", "A", "--b_col", "B", "--c_col", "C", "--group_col", "group")),
  gallery("seqlogo_plot", "seqlogo_plot", "gallery_sequences.tsv", c("--sequence_col", "sequence")),
  gallery("wordcloud_plot", "wordcloud_plot", "gallery_composition.tsv", c("--category_col", "category", "--value_col", "value")),

  article("article_concept_heatmap", "concept_heatmap"),
  article("article_raincloud_compact", "raincloud_compact"),
  article("article_estimation_pvalue", "estimation_pvalue"),
  article("article_phase_portrait", "phase_portrait"),
  article("article_grouped_dot_matrix", "grouped_dot_matrix"),
  article("article_cluster_layer_heatmap", "cluster_layer_heatmap"),
  article("article_pseudo_3d_heatmap", "pseudo_3d_heatmap"),
  article("article_rank_lollipop_badge", "rank_lollipop_badge"),
  article("article_multi_panel_distribution", "multi_panel_distribution"),
  article("article_nested_donut", "nested_donut")
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("codex.rplot", quietly = TRUE)) {
  stop("Install dependencies first: Rscript scripts/install_deps.R")
}

results <- data.frame(id = character(), status = character(), message = character(), stringsAsFactors = FALSE)

for (example in examples) {
  cat("Running ", example$id, "...\n", sep = "")
  status <- system2("Rscript", c(example$script, example$args), stdout = TRUE, stderr = TRUE)
  exit_code <- attr(status, "status") %||% 0
  png <- paste0(plot_prefix(example$id), ".png")
  pdf <- paste0(plot_prefix(example$id), ".pdf")
  ok <- identical(exit_code, 0) &&
    file.exists(png) && file.info(png)$size > 0 &&
    file.exists(pdf) && file.info(pdf)$size > 0
  results <- rbind(
    results,
    data.frame(
      id = example$id,
      status = if (ok) "ok" else "failed",
      message = if (ok) "" else paste(c(paste("exit", exit_code), tail(status, 8)), collapse = " | "),
      stringsAsFactors = FALSE
    )
  )
}

report_path <- file.path(out_dir, "validation_report.tsv")
utils::write.table(results, report_path, sep = "\t", quote = FALSE, row.names = FALSE)

failed <- results[results$status != "ok", , drop = FALSE]
cat("\nValidation report: ", normalizePath(report_path, winslash = "/", mustWork = TRUE), "\n", sep = "")
cat("Passed: ", nrow(results) - nrow(failed), " / ", nrow(results), "\n", sep = "")

if (nrow(failed) > 0) {
  print(failed, row.names = FALSE)
  quit(status = 1)
}
