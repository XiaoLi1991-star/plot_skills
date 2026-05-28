# Plot Skills

This repository is organized as a skills collection in the style of `anthropics/skills`: each skill lives under `skills/<skill-name>/SKILL.md`.

## Skills

- [rplot](skills/rplot/): R-based publication-style plotting from tabular data.

## Install RPlot For Codex

For a user-level Codex install, copy or install the skill folder to:

```text
$CODEX_HOME/skills/rplot
```

`CODEX_HOME` defaults to `~/.codex`.

For a project-local Codex install, use:

```text
.agents/skills/rplot
```

In this repository, use the skill folder as the working directory:

```bash
cd skills/rplot
Rscript scripts/install_deps.R
```

Then run a smoke test:

```bash
Rscript scripts/scatter_bubble/scatter_bubble_plot.R \
  --data_input examples/data/generic_measurements.tsv \
  --x_col score_a \
  --y_col abundance \
  --color_col group \
  --plot_name figures/scatter_bubble_plot
```

Docker is optional:

```bash
cd skills/rplot
docker build -t codex-rplot:latest .
```

## Validate

```bash
cd skills/rplot
Rscript scripts/run_all_examples.R
```

The validation runner covers every workflow listed in `skills/rplot/registry.yaml`.

## Codex Metadata

This is a pure skill repository, so it does not need a plugin marketplace manifest. Codex discovers skills from directories that contain `SKILL.md` under configured skill roots such as `$CODEX_HOME/skills`, project `.agents/skills`, or plugin `skills` folders.
