script_dir <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) == 0) {
    return(getwd())
  }
  dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE))
}

read_input_table <- function(file_path) {
  if (is.null(file_path) || !nzchar(file_path)) {
    stop("Input file path is empty.")
  }
  if (!file.exists(file_path)) {
    stop("Input file does not exist: ", file_path)
  }

  ext <- tolower(tools::file_ext(file_path))
  if (ext %in% c("xlsx", "xlsm", "xltx", "xltm")) {
    return(as.data.frame(readxl::read_excel(file_path), check.names = FALSE))
  }

  if (ext == "xls") {
    con <- file(file_path, "rb")
    on.exit(close(con), add = TRUE)
    magic <- readBin(con, "raw", n = 4)
    magic_int <- as.integer(magic)
    is_ole_excel <- length(magic_int) >= 4 && identical(magic_int[1:4], c(0xD0, 0xCF, 0x11, 0xE0))
    is_zip_excel <- length(magic_int) >= 2 && identical(magic_int[1:2], c(0x50, 0x4B))
    if (is_ole_excel || is_zip_excel) {
      return(as.data.frame(readxl::read_excel(file_path), check.names = FALSE))
    }
  }

  data.table::fread(file_path, data.table = FALSE, check.names = FALSE)
}

parse_list <- function(value) {
  if (is.null(value) || !nzchar(trimws(value))) {
    return(character(0))
  }
  trimws(unlist(strsplit(value, "[,;]")))
}

require_columns <- function(data, columns) {
  columns <- columns[nzchar(columns)]
  missing <- setdiff(columns, colnames(data))
  if (length(missing) > 0) {
    stop("Input data is missing required columns: ", paste(missing, collapse = ", "))
  }
}

numeric_columns <- function(data, requested = "") {
  requested_cols <- parse_list(requested)
  if (length(requested_cols) > 0) {
    require_columns(data, requested_cols)
    return(requested_cols)
  }
  names(data)[vapply(data, is.numeric, logical(1))]
}

coerce_numeric <- function(data, columns) {
  for (col in columns) {
    if (is.null(col) || !nzchar(col) || !(col %in% colnames(data))) {
      next
    }
    if (!is.numeric(data[[col]])) {
      suppressWarnings(data[[col]] <- as.numeric(data[[col]]))
    }
  }
  data
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

theme_rplot <- function(base_size = 12, style = "auto", default = "rplot") {
  style <- tolower(trimws(style %||% "auto"))
  default <- tolower(trimws(default %||% "rplot"))
  if (!nzchar(style) || style == "auto") {
    style <- default
  }

  base_theme <- switch(
    style,
    rplot = theme_classic(base_size = base_size),
    minimal = theme_minimal(base_size = base_size),
    classic = theme_classic(base_size = base_size),
    bw = theme_bw(base_size = base_size),
    light = theme_light(base_size = base_size),
    grey = theme_grey(base_size = base_size),
    gray = theme_grey(base_size = base_size),
    void = theme_void(base_size = base_size),
    stop("Unsupported --theme value: ", style, ". Use auto, rplot, minimal, classic, bw, light, grey, or void.")
  )

  if (style != "rplot") {
    return(
      base_theme +
        theme(
          plot.title = element_text(face = "bold", hjust = 0),
          legend.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold")
        )
    )
  }

  base_theme +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey91", linewidth = 0.32),
      axis.line = element_line(color = "grey12", linewidth = 0.42),
      axis.ticks = element_line(color = "grey12", linewidth = 0.38),
      axis.ticks.length = grid::unit(2.2, "pt"),
      axis.text = element_text(color = "grey15"),
      axis.title = element_text(color = "grey10", face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0, color = "grey10", size = base_size + 1.4, margin = margin(b = 7)),
      plot.subtitle = element_text(color = "grey35", margin = margin(b = 6)),
      legend.position = "right",
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.key.height = grid::unit(0.8, "lines"),
      legend.spacing.y = grid::unit(0.12, "lines"),
      legend.title = element_text(face = "bold", color = "grey10"),
      legend.text = element_text(color = "grey15"),
      strip.background = element_rect(fill = "grey96", color = NA),
      strip.text = element_text(face = "bold", color = "grey15"),
      plot.margin = margin(10, 14, 10, 10)
    )
}

discrete_palette <- function(name = "okabe_ito") {
  palettes <- list(
    okabe_ito = c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#F0E442", "#000000"),
    editorial = c("#3B6EA8", "#D95F02", "#1B9E77", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D"),
    cns = c("#E64B35", "#4DBBD5", "#00A087", "#3C5488", "#F39B7F", "#8491B4", "#91D1C2", "#DC0000"),
    nature = c("#E64B35", "#4DBBD5", "#00A087", "#3C5488", "#F39B7F", "#8491B4", "#91D1C2", "#DC0000"),
    nejm = c("#BC3C29", "#0072B5", "#E18727", "#20854E", "#7876B1", "#6F99AD", "#FFDC91", "#EE4C97"),
    calm = c("#4C78A8", "#F58518", "#54A24B", "#E45756", "#72B7B2", "#B279A2", "#FF9DA6", "#9D755D"),
    vivid = c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2", "#7F7F7F")
  )
  base <- palettes[[name]] %||% palettes$okabe_ito
  grDevices::colorRampPalette(base)(40)
}

add_discrete_scale <- function(p, aesthetic = "color", palette = "okabe_ito") {
  values <- discrete_palette(palette)
  if (aesthetic == "fill") {
    p + scale_fill_manual(values = values, na.translate = FALSE)
  } else {
    p + scale_color_manual(values = values, na.translate = FALSE)
  }
}

output_paths <- function(plot_name) {
  if (is.null(plot_name) || !nzchar(plot_name)) {
    stop("--plot_name cannot be empty.")
  }
  root <- sub("\\.(pdf|png)$", "", plot_name, ignore.case = TRUE)
  out_dir <- dirname(root)
  if (nzchar(out_dir) && out_dir != "." && !dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  list(pdf = paste0(root, ".pdf"), png = paste0(root, ".png"))
}

save_ggplot <- function(plot, plot_name, width = 8, height = 6, dpi = 450, bg = "white") {
  paths <- output_paths(plot_name)
  ggsave(paths$pdf, plot = plot, width = width, height = height, bg = bg, device = grDevices::cairo_pdf)
  grDevices::png(paths$png, width = width, height = height, units = "in", res = dpi, bg = bg)
  on.exit(grDevices::dev.off(), add = TRUE)
  print(plot)
  invisible(paths)
}

save_base_plot <- function(plot_name, draw_fn, width = 8, height = 6, dpi = 450, bg = "white") {
  paths <- output_paths(plot_name)
  pdf_device <- if (isTRUE(capabilities("cairo"))) grDevices::cairo_pdf else grDevices::pdf
  pdf_device(paths$pdf, width = width, height = height, bg = bg)
  draw_fn()
  grDevices::dev.off()

  grDevices::png(paths$png, width = width, height = height, units = "in", res = dpi, bg = bg)
  draw_fn()
  grDevices::dev.off()
  invisible(paths)
}

require_rplot_package <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(
      "R package '", package, "' is required for this chart. ",
      "Rebuild the rplot Docker image from the skill Dockerfile."
    )
  }
}

wrap_labels <- function(values, width = 40) {
  if (is.null(width) || is.na(width) || width <= 0) {
    return(values)
  }
  stringr::str_wrap(values, width = width)
}
