```bash
Rscript scripts/matrix_heatmap/matrix_heatmap.R \
  --input_matrix examples/data/expression_matrix.tsv \
  --sample_info examples/data/sample_info.tsv \
  --id_col feature \
  --top_n_variable 30 \
  --show_rownames TRUE \
  --plot_name figures/matrix_heatmap
```
