# RPlot Skill

RPlot is a standalone Codex skill for generating static R figures from tabular data. It ships reusable `Rscript` templates, lightweight example datasets, preview thumbnails, and a small helper package named `codex.rplot`.

## Install

Local R is the recommended path for Codex:

```bash
Rscript scripts/install_deps.R
```

Docker is optional when you want a fully isolated runtime:

```bash
docker build -t codex-rplot:latest .
```

The local installer installs CRAN dependencies, the `volcanolabel` source package, and the bundled `package/codex.rplot` helper package.

Quick health check after installation:

```bash
Rscript scripts/check_install.R
```

If dependencies are missing and network access is available, the health check can install them first:

```bash
Rscript scripts/check_install.R --install-missing
```

## Run An Example

```bash
Rscript scripts/scatter_bubble/scatter_bubble_plot.R \
  --data_input examples/data/generic_measurements.tsv \
  --x_col score_a \
  --y_col abundance \
  --color_col group \
  --size_col score_b \
  --label_col biomarker \
  --label_top_n 5 \
  --plot_name figures/scatter_bubble_plot
```

Docker equivalent:

```bash
docker run --rm -v "$PWD:/work" -w /work codex-rplot:latest \
  Rscript /skills/rplot/scripts/scatter_bubble/scatter_bubble_plot.R \
  --data_input /skills/rplot/examples/data/generic_measurements.tsv \
  --x_col score_a \
  --y_col abundance \
  --color_col group \
  --plot_name figures/scatter_bubble_plot
```

Both commands create `figures/scatter_bubble_plot.png` and `figures/scatter_bubble_plot.pdf`.

## Validate Everything

Run all bundled workflows sequentially:

```bash
Rscript scripts/run_all_examples.R
```

Outputs and `validation_report.tsv` are written under `.test_output/all_examples` by default.

For a faster smoke test, run:

```bash
Rscript scripts/check_install.R
```

## Repository Layout

- `SKILL.md`: Codex skill instructions.
- `registry.yaml`: workflow index and example commands.
- `scripts/`: R plotting CLIs and helper install/test scripts.
- `package/codex.rplot/`: local R helper package.
- `examples/data/`: small example tables.
- `thumbnails/`: lightweight preview images generated from the bundled examples.

## Notes For Publishing

- The skill does not depend on a private runtime or private Docker image.
- The Docker image builds from a public `r-base:4.4.1` image.
- Scripts are still intended to be used through Codex as a skill, but they also run as normal command-line R scripts.
