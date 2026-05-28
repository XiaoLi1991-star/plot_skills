---
name: rplot
display_name: "RPlot Generic Plotting"
short_description: "Generate publication-style PNG and PDF charts from CSV/TSV/XLS/XLSX tables with reusable R templates."
description: "Use this skill when the user wants R-based plotting from tabular data: scatter/bubble, box/violin, heatmap, PCA, UpSet, alluvial, volcano, enrichment, survival, correlation, and other general chart templates."
category: "visualization"
icon: "bar-chart"
version: "0.4.0"
tags: ["R", "ggplot2", "visualization", "plotting", "charts", "heatmap", "PCA", "UpSet"]
---

# RPlot Generic Plotting

RPlot is a Codex-friendly skill for turning arbitrary tables into static, publication-style figures. Scripts are plain `Rscript` CLIs and write both `.png` and `.pdf` outputs through `--plot_name`.

## When To Use

Use this skill for general plotting requests from CSV, TSV, TXT, XLS, or XLSX data. Prefer it when the user asks for reusable R templates or publication-ready biomedical charts that are not tied to a specific analysis pipeline.

Use another skill when the user explicitly asks for Python, seaborn, matplotlib, or a pipeline-specific visualization.

## Setup Options

This repository supports two installation paths. Local R is the recommended path for Codex because it avoids Docker volume mapping and makes workspace files easy to read and write.

1. Local R installation, recommended for Codex:

```bash
Rscript scripts/install_deps.R
Rscript scripts/check_install.R
Rscript scripts/scatter_bubble/scatter_bubble_plot.R \
  --data_input examples/data/generic_measurements.tsv \
  --x_col score_a --y_col abundance --color_col group \
  --plot_name figures/scatter_bubble
```

2. Docker, optional for reproducible isolated use:

```bash
docker build -t codex-rplot:latest .
docker run --rm -v "$PWD:/work" -w /work codex-rplot:latest \
  Rscript /skills/rplot/scripts/scatter_bubble/scatter_bubble_plot.R \
  --data_input /skills/rplot/examples/data/generic_measurements.tsv \
  --x_col score_a --y_col abundance --color_col group \
  --plot_name figures/scatter_bubble
```

## Operating Rules

- Read `registry.yaml` first to choose the closest workflow and see a working example command.
- Check the input table header before choosing column parameters.
- Prefer exposed CLI parameters over editing scripts. Common controls include `--theme`, `--palette`, `--fig_width`, `--fig_height`, `--base_font_size`, `--title`, `--x_title`, `--y_title`, and `--plot_name`.
- Keep `library(codex.rplot)` in copied or customized scripts. Shared helpers provide table reading, themes, palettes, validation, and output helpers.
- Output prefixes should be relative paths such as `figures/my_plot`; scripts create parent directories when needed.
- For batch validation, run plotting commands sequentially. Some plotting packages use native libraries and can be noisy or memory-heavy when run in parallel.
- After installation, run `Rscript scripts/check_install.R` for a fast smoke test. If dependencies are missing and network access is available, run `Rscript scripts/check_install.R --install-missing`.

## Main Workflows

Dedicated scripts:

- `scatter_bubble_plot`: scatter and bubble plots with optional labels, facets, and smoothing.
- `box_violin_plot`: boxplot, violin, or combined box+violin plots.
- `bar_lollipop_plot`: ranked bar or lollipop plots.
- `correlation_heatmap`: numeric-column correlation heatmaps.
- `matrix_heatmap`: feature-by-sample matrix heatmaps with optional annotations.
- `pca_plot`: PCA plots from feature-by-sample matrices.
- `upset_plot`: set intersection plots.
- `alluvial_plot`: multi-stage alluvial/Sankey-like plots.

Gallery scripts:

- `scripts/chart_gallery/chart_gallery.R` covers histogram, density, ECDF, Q-Q, dot, hexbin, 2D density, line, area, grouped/stacked bars, error bars, dumbbell, slope, pie/donut/waffle/treemap/sunburst, waterfall, Pareto, radar, heatmap, calendar, volcano, enrichment, survival, forest, funnel, chord, arc network, ternary, sequence logo, word cloud, and more.
- `scripts/article_gallery/article_gallery.R` covers article-style templates such as concept heatmaps, compact rainclouds, estimation/P-value plots, phase portraits, grouped dot matrices, layered heatmaps, pseudo-3D heatmaps, ranked lollipop badges, multi-panel distributions, and nested donuts.

## Quick Example

```bash
Rscript scripts/chart_gallery/chart_gallery.R \
  --data_input examples/data/gallery_enrichment.tsv \
  --mode enrichment_dotplot \
  --category_col term \
  --value_col gene_ratio \
  --value2_col padj \
  --weight_col count \
  --group_col category \
  --plot_name figures/enrichment_dotplot
```

Verify success by checking that both `figures/enrichment_dotplot.png` and `figures/enrichment_dotplot.pdf` exist and are non-empty.
