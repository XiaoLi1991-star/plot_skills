from __future__ import annotations

from pathlib import Path
from textwrap import wrap

from PIL import Image, ImageDraw, ImageFont


ROOT = Path(__file__).resolve().parents[1]
THUMBNAILS = ROOT / "skills" / "rplot" / "thumbnails"
OUTPUT = ROOT / "docs" / "assets" / "rplot-thumbnail-gallery.png"


CATEGORIES: list[tuple[str, list[str]]] = [
    (
        "Core RPlot workflows",
        [
            "scatter_bubble_plot",
            "box_violin_plot",
            "bar_lollipop_plot",
            "correlation_heatmap",
            "matrix_heatmap",
            "pca_plot",
            "upset_plot",
            "alluvial_plot",
        ],
    ),
    (
        "Distribution and density",
        [
            "histogram_plot",
            "density_plot",
            "frequency_polygon_plot",
            "ecdf_plot",
            "qq_plot",
            "dot_plot",
            "jitter_strip_plot",
            "ridge_plot",
            "half_violin_plot",
            "raincloud_plot",
            "beeswarm_plot",
            "bean_plot",
            "mirrored_histogram",
        ],
    ),
    (
        "Relationships and multivariate",
        [
            "hexbin_plot",
            "density2d_contour_plot",
            "density2d_filled_plot",
            "connected_scatter_plot",
            "parallel_coordinate_plot",
            "ternary_plot",
            "radar_chart",
            "taylor_diagram",
            "bland_altman_plot",
        ],
    ),
    (
        "Time, trend, and ranking",
        [
            "line_plot",
            "area_plot",
            "stacked_area_plot",
            "step_plot",
            "ribbon_plot",
            "slope_chart",
            "bump_chart",
            "streamgraph",
            "dual_axis_line",
            "dumbbell_plot",
        ],
    ),
    (
        "Bars, intervals, and change",
        [
            "grouped_bar_plot",
            "stacked_bar_plot",
            "faceted_proportion_bar_plot",
            "diverging_bar_plot",
            "errorbar_plot",
            "pointrange_plot",
            "butterfly_chart",
            "waterfall_chart",
            "pareto_chart",
            "gantt_chart",
        ],
    ),
    (
        "Composition, hierarchy, and networks",
        [
            "pie_chart",
            "donut_chart",
            "waffle_chart",
            "treemap_chart",
            "sunburst_chart",
            "polar_bar_chart",
            "rose_chart",
            "mosaic_plot",
            "chord_diagram",
            "arc_network",
        ],
    ),
    (
        "Heatmaps and matrix layouts",
        [
            "tile_heatmap",
            "calendar_heatmap",
        ],
    ),
    (
        "Clinical, model, and enrichment",
        [
            "roc_curve",
            "calibration_curve",
            "survival_curve",
            "forest_plot",
            "funnel_plot",
            "ma_plot",
            "volcano_plot",
            "enrichment_dotplot",
            "enrichment_barplot",
            "likert_plot",
        ],
    ),
    (
        "Specialized biological views",
        [
            "venn_diagram",
            "seqlogo_plot",
            "wordcloud_plot",
        ],
    ),
    (
        "Article-style composite figures",
        [
            "article_concept_heatmap",
            "article_raincloud_compact",
            "article_estimation_pvalue",
            "article_phase_portrait",
            "article_grouped_dot_matrix",
            "article_cluster_layer_heatmap",
            "article_pseudo_3d_heatmap",
            "article_rank_lollipop_badge",
            "article_multi_panel_distribution",
            "article_nested_donut",
        ],
    ),
]


def load_font(size: int, bold: bool = False) -> ImageFont.FreeTypeFont | ImageFont.ImageFont:
    candidates = [
        Path("C:/Windows/Fonts/arialbd.ttf" if bold else "C:/Windows/Fonts/arial.ttf"),
        Path("C:/Windows/Fonts/segoeuib.ttf" if bold else "C:/Windows/Fonts/segoeui.ttf"),
    ]
    for candidate in candidates:
        if candidate.exists():
            return ImageFont.truetype(str(candidate), size)
    return ImageFont.load_default()


def display_name(plot_id: str) -> str:
    return plot_id.replace("article_", "").replace("_", " ").title()


def fit_image(image: Image.Image, size: tuple[int, int]) -> Image.Image:
    image = image.convert("RGB")
    image.thumbnail(size, Image.Resampling.LANCZOS)
    canvas = Image.new("RGB", size, "white")
    x = (size[0] - image.width) // 2
    y = (size[1] - image.height) // 2
    canvas.paste(image, (x, y))
    return canvas


def validate_categories() -> None:
    categorized = [plot_id for _, plot_ids in CATEGORIES for plot_id in plot_ids]
    duplicates = sorted({plot_id for plot_id in categorized if categorized.count(plot_id) > 1})
    available = sorted(path.stem for path in THUMBNAILS.glob("*.png"))
    missing = sorted(set(categorized) - set(available))
    uncategorized = sorted(set(available) - set(categorized))
    if duplicates or missing or uncategorized:
        details = []
        if duplicates:
            details.append(f"duplicates: {', '.join(duplicates)}")
        if missing:
            details.append(f"missing thumbnails: {', '.join(missing)}")
        if uncategorized:
            details.append(f"uncategorized thumbnails: {', '.join(uncategorized)}")
        raise SystemExit("; ".join(details))


def main() -> None:
    validate_categories()

    columns = 4
    tile_w = 260
    image_h = 170
    label_h = 46
    gap = 18
    outer = 34
    header_h = 54
    section_gap = 26
    title_h = 78
    footer_h = 36
    tile_h = image_h + label_h
    width = outer * 2 + columns * tile_w + (columns - 1) * gap

    heights = [title_h + footer_h + outer * 2]
    for _, plot_ids in CATEGORIES:
        rows = (len(plot_ids) + columns - 1) // columns
        heights.append(header_h + rows * tile_h + max(0, rows - 1) * gap + section_gap)
    height = sum(heights)

    canvas = Image.new("RGB", (width, height), "#f7f8fb")
    draw = ImageDraw.Draw(canvas)
    title_font = load_font(34, bold=True)
    header_font = load_font(22, bold=True)
    label_font = load_font(15)
    small_font = load_font(13)

    y = outer
    draw.text((outer, y), "RPlot thumbnail gallery", fill="#111827", font=title_font)
    draw.text(
        (outer, y + 44),
        "85 bundled plotting workflows grouped by chart type",
        fill="#4b5563",
        font=small_font,
    )
    y += title_h

    for title, plot_ids in CATEGORIES:
        draw.rounded_rectangle(
            (outer, y, width - outer, y + header_h - 8),
            radius=10,
            fill="#e8eef7",
            outline="#d2dbe9",
        )
        draw.text((outer + 18, y + 13), title, fill="#1f2937", font=header_font)
        y += header_h

        for index, plot_id in enumerate(plot_ids):
            row, col = divmod(index, columns)
            x = outer + col * (tile_w + gap)
            tile_y = y + row * (tile_h + gap)
            draw.rounded_rectangle(
                (x, tile_y, x + tile_w, tile_y + tile_h),
                radius=8,
                fill="#ffffff",
                outline="#d9dee8",
            )
            thumb = Image.open(THUMBNAILS / f"{plot_id}.png")
            fitted = fit_image(thumb, (tile_w - 20, image_h - 16))
            canvas.paste(fitted, (x + 10, tile_y + 8))

            label_lines = wrap(display_name(plot_id), width=24, max_lines=2, placeholder="...")
            label_y = tile_y + image_h + 8
            for line in label_lines:
                bbox = draw.textbbox((0, 0), line, font=label_font)
                draw.text(
                    (x + (tile_w - (bbox[2] - bbox[0])) / 2, label_y),
                    line,
                    fill="#374151",
                    font=label_font,
                )
                label_y += 18

        rows = (len(plot_ids) + columns - 1) // columns
        y += rows * tile_h + max(0, rows - 1) * gap + section_gap

    draw.text(
        (outer, height - outer - 20),
        "Generated from skills/rplot/thumbnails with scripts/build_thumbnail_gallery.py",
        fill="#6b7280",
        font=small_font,
    )

    OUTPUT.parent.mkdir(parents=True, exist_ok=True)
    canvas.save(OUTPUT, optimize=True)
    print(f"Wrote {OUTPUT.relative_to(ROOT)} ({width}x{height})")


if __name__ == "__main__":
    main()
