```bash
Rscript scripts/box_violin/box_violin_plot.R \
  --data_input examples/data/generic_measurements.tsv \
  --x_col group \
  --y_col abundance \
  --mode box_violin \
  --sort_by_median TRUE \
  --plot_name figures/box_violin_plot
```
