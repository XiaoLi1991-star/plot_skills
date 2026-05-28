```bash
Rscript scripts/pca_plot/pca_plot.R \
  --input_matrix examples/data/expression_matrix.tsv \
  --sample_info examples/data/sample_info.tsv \
  --id_col feature \
  --group_col group \
  --show_label TRUE \
  --plot_name figures/pca_plot
```
