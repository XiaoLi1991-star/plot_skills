```bash
Rscript scripts/chart_gallery/chart_gallery.R \
  --data_input examples/data/gallery_points.tsv \
  --mode histogram \
  --value_col value \
  --group_col group \
  --plot_name figures/histogram_plot

Rscript scripts/chart_gallery/chart_gallery.R \
  --data_input examples/data/gallery_category.tsv \
  --mode grouped_bar \
  --category_col category \
  --value_col value \
  --group_col group \
  --plot_name figures/grouped_bar_plot

Rscript scripts/chart_gallery/chart_gallery.R \
  --data_input examples/data/gallery_hierarchy.tsv \
  --mode sunburst \
  --category_col category \
  --subcategory_col subcategory \
  --value_col value \
  --plot_name figures/sunburst_chart

Rscript scripts/chart_gallery/chart_gallery.R \
  --data_input examples/data/gallery_composition.tsv \
  --mode polar_bar \
  --category_col category \
  --value_col value \
  --theme rplot \
  --plot_name figures/polar_bar_chart
```

Hiplot-inspired additions:

```bash
Rscript scripts/chart_gallery/chart_gallery.R \
  --data_input examples/data/gallery_distribution.tsv \
  --mode raincloud \
  --category_col category \
  --value_col value \
  --group_col group \
  --plot_name figures/raincloud_plot

Rscript scripts/chart_gallery/chart_gallery.R \
  --data_input examples/data/gallery_model.tsv \
  --mode roc_curve \
  --actual_col actual \
  --score_col score \
  --group_col group \
  --plot_name figures/roc_curve

Rscript scripts/chart_gallery/chart_gallery.R \
  --data_input examples/data/gallery_edges.tsv \
  --mode chord_diagram \
  --from_col from \
  --to_col to \
  --weight_col weight \
  --plot_name figures/chord_diagram
```
