```bash
Rscript scripts/correlation_heatmap/correlation_heatmap.R \
  --data_input examples/data/generic_measurements.tsv \
  --columns score_a,score_b,abundance \
  --method pearson \
  --show_numbers TRUE \
  --plot_name figures/correlation_heatmap
```
