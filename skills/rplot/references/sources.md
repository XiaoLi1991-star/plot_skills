# RPlot Reference Sources

RPlot uses the following open-source plotting galleries/platforms as design references. The scripts in this skill are independently implemented for Codex and are not copied from these projects.

| Source | URL | Used For |
| --- | --- | --- |
| R Graph Gallery | https://github.com/holtzy/R-graph-gallery | Broad chart taxonomy, ggplot2 gallery organization, lollipop/alluvial/upset-style examples |
| R CHARTS | https://github.com/R-CoderDotCom/R-CHARTS | General chart examples, tutorial-style parameter organization, color/style inspiration |
| plotthis | https://github.com/pwwang/plotthis | High-level ggplot2 chart taxonomy and consistent plotting API vocabulary; conceptual reference only |
| plotmics | https://github.com/amitjavilaventura/plotmics | Omics-focused plotting vocabulary such as volcano, heatmap, DE comparison, and set views; conceptual reference only |
| EasyPubPlot | https://github.com/Pharmaco-OmicsLab/EasyPubPlot | Omics-oriented plotting platform structure and publishable plot expectations |
| DataMap | https://github.com/gexijin/datamap | Table-to-plot reproducibility pattern and matrix/PCA/heatmap workflow ideas |
| xOmicsShiny | https://github.com/interactivereport/xOmicsShiny | Cross-omics visualization module coverage: QC, DE, heatmap, Venn, GSEA/pathway, network |
| Hiplot ORG plugins-open | https://github.com/hiplot/plugins-open | Broad bioinformatics plotting catalog, plugin metadata/UI parameter patterns, and candidate chart coverage |
| hiplotlib | https://github.com/hiplot/hiplotlib | Hiplot native plugin runtime design reference; not used as a runtime dependency in this skill |
| ggVolcanoR | https://github.com/KerryAM-R/ggVolcanoR | Differential-result exploration vocabulary: volcano, correlation, heatmap, UpSet-style follow-up views |
| GeneTonic | https://github.com/federicomarini/GeneTonic | Gene set / enrichment interpretation patterns and result-table plotting terminology |
| DEBrowser | https://github.com/UMMS-Biocore/debrowser | Differential expression plotting coverage such as volcano, MA, PCA, heatmap, density and QC views |
| Public article-style plotting posts | https://mp.weixin.qq.com/s/EnH4OAETEtsQK_EABW3FFg and https://blog.csdn.net/qq_21478261 | Visual ideas for article-style composition charts, heatmap variants, compact distributions, trajectory panels, and publication-style layout patterns; no article code is copied |

## Selection and Licensing Policy

- GPL-family platforms are used only as chart taxonomy, UX, parameter naming, and workflow coverage references.
- Codex RPlot scripts are project-local implementations with a uniform CLI and do not copy third-party source code.
- Example datasets in this skill are synthetic or project-local fixtures designed to make preview images reproducible.
- Public articles without an explicit compatible source-code license are used only as visual references; reimplement the chart behavior from scratch and keep code/data synthetic.
- When adding future gallery modules, prefer using upstream projects for chart intent and parameter inspiration; do not vendor code unless its license is explicitly compatible with this repository and the source is recorded here.
