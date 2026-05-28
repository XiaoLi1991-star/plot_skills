args <- commandArgs(trailingOnly = TRUE)
skip_local_package <- "--skip-local-package" %in% args

repos <- Sys.getenv("R_REPOS", unset = "https://cloud.r-project.org")
options(repos = c(CRAN = repos))

cran_packages <- c(
  "ComplexUpset",
  "circlize",
  "data.table",
  "dplyr",
  "ggalluvial",
  "ggbeeswarm",
  "ggdist",
  "ggplot2",
  "ggrepel",
  "ggseqlogo",
  "ggwordcloud",
  "ggridges",
  "optparse",
  "pheatmap",
  "readxl",
  "reshape2",
  "scales",
  "stringr",
  "survival",
  "tidyr"
)

install_missing <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (!length(missing)) {
    return(invisible(TRUE))
  }
  utils::install.packages(missing, dependencies = c("Depends", "Imports", "LinkingTo"))
  still_missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(still_missing)) {
    stop("Missing R packages after install: ", paste(still_missing, collapse = ", "))
  }
}

install_missing(cran_packages)

if (!requireNamespace("volcanolabel", quietly = TRUE)) {
  utils::install.packages(
    "https://github.com/XiaoLi1991-star/volcanolabel/archive/refs/tags/v0.1.1.tar.gz",
    repos = NULL,
    type = "source"
  )
}

if (!requireNamespace("volcanolabel", quietly = TRUE)) {
  stop("Missing R package after install: volcanolabel")
}

if (!skip_local_package) {
  package_dir <- normalizePath(file.path(getwd(), "package", "codex.rplot"), mustWork = TRUE)
  package_install_dir <- package_dir
  if (.Platform$OS.type == "windows") {
    package_tmp_parent <- file.path(tempdir(), "rplot_local_package")
    package_tmp_dir <- file.path(package_tmp_parent, "codex.rplot")
    unlink(package_tmp_parent, recursive = TRUE, force = TRUE)
    dir.create(package_tmp_parent, recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(package_dir, package_tmp_parent, recursive = TRUE)) {
      stop("Failed to copy local package to temporary install directory: ", package_tmp_parent)
    }
    package_install_dir <- normalizePath(package_tmp_dir, winslash = "/", mustWork = TRUE)
  }
  utils::install.packages(package_install_dir, repos = NULL, type = "source")
  if (!requireNamespace("codex.rplot", quietly = TRUE)) {
    stop("Missing local package after install: codex.rplot")
  }
}

message("RPlot dependencies are installed.")
