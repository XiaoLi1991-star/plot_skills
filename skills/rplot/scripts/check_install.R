args <- commandArgs(trailingOnly = TRUE)
install_missing <- "--install-missing" %in% args
out_arg <- grep("^--out=", args, value = TRUE)
out_dir <- if (length(out_arg)) sub("^--out=", "", out_arg[1]) else file.path(".test_output", "health_check")

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_file <- if (length(file_arg)) sub("^--file=", "", file_arg[1]) else file.path("scripts", "check_install.R")
repo_root <- normalizePath(file.path(dirname(script_file), ".."), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(repo_root, "SKILL.md"))) {
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

path <- function(...) normalizePath(file.path(repo_root, ...), winslash = "/", mustWork = TRUE)
rel <- function(...) file.path(repo_root, ...)
log_ok <- function(msg) message("[OK] ", msg)
log_info <- function(msg) message("[INFO] ", msg)
log_fail <- function(msg) message("[FAIL] ", msg)

required_packages <- c(
  "ComplexUpset", "circlize", "data.table", "dplyr", "ggalluvial",
  "ggbeeswarm", "ggdist", "ggplot2", "ggrepel", "ggseqlogo",
  "ggwordcloud", "ggridges", "optparse", "pheatmap", "readxl",
  "reshape2", "scales", "stringr", "survival", "tidyr",
  "volcanolabel", "codex.rplot"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) && install_missing) {
  log_info(paste("Installing missing packages:", paste(missing_packages, collapse = ", ")))
  status <- system2(
    "Rscript",
    c(path("scripts", "install_deps.R")),
    stdout = TRUE,
    stderr = TRUE
  )
  cat(paste(status, collapse = "\n"), "\n")
  missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
}

if (length(missing_packages)) {
  log_fail(paste("Missing R packages:", paste(missing_packages, collapse = ", ")))
  log_info("Run: Rscript scripts/install_deps.R")
  log_info("Or run: Rscript scripts/check_install.R --install-missing")
  quit(status = 2)
}
log_ok("Required R packages are available")

required_files <- c(
  rel("SKILL.md"),
  rel("registry.yaml"),
  rel("package", "codex.rplot", "DESCRIPTION"),
  rel("examples", "data", "generic_measurements.tsv"),
  rel("examples", "data", "gallery_points.tsv"),
  rel("scripts", "scatter_bubble", "scatter_bubble_plot.R"),
  rel("scripts", "chart_gallery", "chart_gallery.R")
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files)) {
  log_fail(paste("Missing required files:", paste(missing_files, collapse = ", ")))
  quit(status = 3)
}
log_ok("Required files are present")

dir.create(file.path(repo_root, out_dir), recursive = TRUE, showWarnings = FALSE)
probe_file <- file.path(repo_root, out_dir, ".write_test")
writeLines("ok", probe_file)
if (!file.exists(probe_file)) {
  log_fail(paste("Output directory is not writable:", file.path(repo_root, out_dir)))
  quit(status = 4)
}
unlink(probe_file)
log_ok(paste("Output directory is writable:", out_dir))

run_command <- function(name, script, command_args, prefix) {
  log_info(paste("Running", name))
  old_wd <- setwd(repo_root)
  on.exit(setwd(old_wd), add = TRUE)
  output <- system2("Rscript", c(script, command_args), stdout = TRUE, stderr = TRUE)
  status <- attr(output, "status")
  if (!is.null(status) && status != 0) {
    cat(paste(output, collapse = "\n"), "\n")
    log_fail(paste(name, "failed with status", status))
    quit(status = 5)
  }
  expected <- paste0(file.path(repo_root, prefix), c(".png", ".pdf"))
  missing <- expected[!file.exists(expected)]
  if (length(missing)) {
    log_fail(paste(name, "did not create:", paste(missing, collapse = ", ")))
    quit(status = 6)
  }
  sizes <- file.info(expected)$size
  if (any(is.na(sizes) | sizes < 1000)) {
    log_fail(paste(name, "created empty or tiny output:", paste(expected, collapse = ", ")))
    quit(status = 7)
  }
  log_ok(paste(name, "created PNG/PDF outputs"))
}

scatter_prefix <- file.path(out_dir, "scatter_bubble_health")
run_command(
  "scatter_bubble_plot",
  file.path("scripts", "scatter_bubble", "scatter_bubble_plot.R"),
  c(
    "--data_input", file.path("examples", "data", "generic_measurements.tsv"),
    "--x_col", "score_a",
    "--y_col", "abundance",
    "--color_col", "group",
    "--size_col", "score_b",
    "--label_top_n", "0",
    "--plot_name", scatter_prefix
  ),
  scatter_prefix
)

hist_prefix <- file.path(out_dir, "histogram_health")
run_command(
  "chart_gallery_histogram",
  file.path("scripts", "chart_gallery", "chart_gallery.R"),
  c(
    "--data_input", file.path("examples", "data", "gallery_points.tsv"),
    "--mode", "histogram",
    "--value_col", "value",
    "--group_col", "group",
    "--plot_name", hist_prefix
  ),
  hist_prefix
)

log_ok("RPlot health check passed")
log_info(paste("Smoke-test outputs:", normalizePath(file.path(repo_root, out_dir), winslash = "/", mustWork = TRUE)))
