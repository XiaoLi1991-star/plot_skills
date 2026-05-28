```bash
Rscript scripts/scatter_bubble/scatter_bubble_plot.R \
  --data_input examples/data/generic_measurements.tsv \
  --x_col score_a \
  --y_col abundance \
  --color_col group \
  --size_col score_b \
  --label_col biomarker \
  --label_top_n 5 \
  --add_smooth TRUE \
  --plot_name figures/scatter_bubble_plot
```
