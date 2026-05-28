```bash
Rscript scripts/alluvial_plot/alluvial_plot.R \
  --data_input examples/data/alluvial_paths.tsv \
  --axis_cols source,class,outcome \
  --weight_col count \
  --fill_col source \
  --plot_name figures/alluvial_plot
```
