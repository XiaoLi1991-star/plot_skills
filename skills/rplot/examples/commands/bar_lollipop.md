```bash
Rscript scripts/bar_lollipop/bar_lollipop_plot.R \
  --data_input examples/data/category_scores.tsv \
  --category_col term \
  --value_col score \
  --color_col category \
  --mode lollipop \
  --top_n 12 \
  --plot_name figures/bar_lollipop_plot
```
