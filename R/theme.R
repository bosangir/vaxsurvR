# ---------------------------------------------------------------------------
# Visual identity for report output: a palette, a ggplot2 theme and a
# flextable theme. Everything here is cosmetic; no estimate depends on it.
#
# The default look follows the Kongo Central survey reports (green headings,
# grey rules, Century Gothic where available). Every colour is overridable
# through vcs_palette(), and every function that draws something takes a
# `palette` argument, so a different programme can restyle the output without
# touching the analysis code.
# ---------------------------------------------------------------------------

#' Colour palettes used by vaxsurvR figures and tables
#'
#' A single place where every colour used in report output is defined. Pass
#' the result, possibly modified, to any `plot_*()` or `ft_*()` function.
#'
#' @param ... Named colour overrides, e.g. `primary = "#1F497D"`.
#' @return A named list of colours and colour vectors.
#' @export
#' @examples
#' vcs_palette()$primary
#' vcs_palette(primary = "navy")$primary
vcs_palette <- function(...) {
  pal <- list(
    # identity
    primary   = "#4E7D3A",   # heading green
    secondary = "#8CB369",   # mid green
    tertiary  = "#C5DCB4",   # pale green
    accent    = "#E0A340",   # amber
    danger    = "#C0504D",   # red
    info      = "#5B7FA6",   # blue
    ink       = "#1A1A1A",
    grey_dk   = "#595959",
    grey      = "#BFBFBF",
    grey_lt   = "#E6E6E6",
    bar_fill  = "#6FCF97",   # proportional cell shading in bar tables
    # VCTC timeliness segments (VCQI convention)
    timing = c(
      "Too early"        = "#7B2C8A",
      "BCG by day 5"     = "#2E5E2A",
      "Timely (28 days)" = "#2E9E3C",
      "< 2 months late"  = "#F6A7F3",
      "2+ months late"   = "#F51FE9",
      "BCG after 1 year" = "#3C3C3C",
      "Timing unknown"   = "#E3E3E3"
    ),
    # MOSV children chart
    mosv = c(
      "Vaccinated at first eligible opportunity" = "#3C6EB0",
      "MOSV - later corrected"                   = "#F2D77A",
      "MOSV - uncorrected"                       = "#D33A2C"
    ),
    # coverage-by-stratum bars (Annex C)
    strata = c(
      overall = "#2E75B6", level1 = "#8FB8DE", level2 = "#DDEBF7",
      residence = "#FFB870", sex = "#A69B84", education = "#CFE0DA"
    ),
    # BeSD question groups (Table 9 row colours)
    besd = c(
      intention = "#FF0000", confidence = "#FF9999", norms = "#7030A0",
      motivation = "#B4A0D8", access = "#1F7A8C", practical = "#66C2E8",
      service = "#4E7D3A"
    ),
    # antigen lines for cumulative curves
    antigen = c(
      BCG = "#3C3C3C", OPV = "#E41A1C", PENTA = "#1F1FFF", PCV = "#FFC000",
      ROTA = "#808080", IPV = "#7B2C8A", MCV = "#0B7A0B", MAL = "#FF1FFF",
      YF = "#E0A340", HEPB = "#8C564B", VITA = "#17BECF", DEWORM = "#BCBD22"
    )
  )
  over <- rlang::list2(...)
  if (length(over)) {
    if (is.null(names(over)) || any(!nzchar(names(over)))) {
      vcs_abort("Palette overrides must be named.", class = "vaxsurvR_value_error")
    }
    pal <- utils::modifyList(pal, over)
  }
  pal
}

#' Font family used for figures
#'
#' Resolves the family named in `options(vaxsurvR.font)` (default
#' `"Century Gothic"`) to one the current graphics device can render, falling
#' back to `"sans"` when it is not installed. Never errors.
#'
#' @param family Font family to try first.
#' @return A single string.
#' @export
#' @examples
#' vcs_font()
vcs_font <- function(family = getOption("vaxsurvR.font", "Century Gothic")) {
  if (is.null(family) || !nzchar(family)) {
    return("sans")
  }
  ok <- tryCatch({
    if (requireNamespace("systemfonts", quietly = TRUE)) {
      any(tolower(systemfonts::system_fonts()$family) == tolower(family))
    } else {
      FALSE
    }
  }, error = function(e) FALSE)
  if (isTRUE(ok)) family else "sans"
}

#' A ggplot2 theme for vaxsurvR report figures
#'
#' Light background, horizontal grid only, no panel border, title in the
#' palette's ink colour. Built on [ggplot2::theme_minimal()].
#'
#' @param base_size Base font size in points.
#' @param base_family Font family; defaults to [vcs_font()].
#' @param palette A [vcs_palette()].
#' @param grid `"x"`, `"y"`, `"xy"` or `"none"`: which major grid lines to keep.
#' @return A ggplot2 theme object.
#' @export
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ggplot2::ggplot(data.frame(x = 1:3, y = 3:1), ggplot2::aes(x, y)) +
#'     ggplot2::geom_col() + theme_vcs()
#' }
theme_vcs <- function(base_size = 10, base_family = vcs_font(),
                      palette = vcs_palette(), grid = "x") {
  assert_installed("ggplot2", "theme_vcs()")
  grid <- match.arg(grid, c("x", "y", "xy", "none"))
  el <- ggplot2::element_line(colour = palette$grey_lt, linewidth = 0.4)
  th <- ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", colour = palette$ink,
                                         size = base_size * 1.2, hjust = 0),
      plot.subtitle = ggplot2::element_text(colour = palette$grey_dk,
                                            size = base_size * 0.95),
      plot.caption = ggplot2::element_text(colour = palette$grey_dk, hjust = 0,
                                           size = base_size * 0.8),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      axis.title = ggplot2::element_text(colour = palette$ink),
      axis.text = ggplot2::element_text(colour = palette$ink),
      axis.line = ggplot2::element_line(colour = palette$grey_dk, linewidth = 0.4),
      axis.ticks = ggplot2::element_line(colour = palette$grey_dk, linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = if (grid %in% c("x", "xy")) el else ggplot2::element_blank(),
      panel.grid.major.y = if (grid %in% c("y", "xy")) el else ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.key.size = grid::unit(0.9, "lines"),
      strip.text = ggplot2::element_text(face = "bold", colour = palette$ink,
                                         hjust = 0),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA)
    )
  th
}

#' Themed flextable for Word and HTML reports
#'
#' Applies the report look to a data frame: coloured header row with white bold
#' text, thin grey inner rules, body font, numeric columns right-aligned and
#' the first column left-aligned.
#'
#' @param x A data frame, or an existing `flextable`.
#' @param palette A [vcs_palette()].
#' @param font_size Body font size in points.
#' @param font_family Font family.
#' @param autofit Fit column widths to content.
#' @param header_fill Header background colour. Defaults to `palette$primary`.
#' @param digits Digits for numeric columns.
#' @param col_labels Optional named character vector renaming header labels.
#' @return A `flextable` object.
#' @export
#' @examples
#' if (requireNamespace("flextable", quietly = TRUE)) {
#'   ft_vcs(data.frame(Zone = c("A", "B"), Coverage = c(81.2, 64.9)))
#' }
ft_vcs <- function(x, palette = vcs_palette(), font_size = 9,
                   font_family = getOption("vaxsurvR.font", "Century Gothic"),
                   autofit = TRUE, header_fill = NULL, digits = 1,
                   col_labels = NULL) {
  assert_installed("flextable", "ft_vcs()")
  header_fill <- header_fill %||% palette$primary
  ft <- if (inherits(x, "flextable")) x else flextable::flextable(as.data.frame(x))
  if (!is.null(col_labels)) {
    ft <- flextable::set_header_labels(ft, values = as.list(col_labels))
  }
  ft <- flextable::font(ft, fontname = font_family, part = "all")
  ft <- flextable::fontsize(ft, size = font_size, part = "all")
  ft <- flextable::bg(ft, bg = header_fill, part = "header")
  ft <- flextable::color(ft, color = "white", part = "header")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::border_remove(ft)
  thin <- officer::fp_border(color = palette$grey, width = 0.5)
  ft <- flextable::border_inner_h(ft, border = thin, part = "body")
  ft <- flextable::hline_bottom(ft, border = officer::fp_border(color = palette$primary, width = 1),
                                part = "body")
  ft <- flextable::hline_top(ft, border = officer::fp_border(color = palette$primary, width = 1),
                             part = "header")
  ft <- flextable::align(ft, align = "left", part = "all", j = 1)
  if (ncol(ft$body$dataset) > 1L) {
    ft <- flextable::align(ft, align = "center", part = "header",
                           j = seq(2L, ncol(ft$body$dataset)))
    ft <- flextable::align(ft, align = "right", part = "body",
                           j = seq(2L, ncol(ft$body$dataset)))
  }
  ft <- flextable::colformat_double(ft, digits = digits, big.mark = ",")
  ft <- flextable::colformat_int(ft, big.mark = ",")
  ft <- flextable::padding(ft, padding.top = 2, padding.bottom = 2, part = "all")
  # Short tables stay on one page with their caption.
  if (nrow(ft$body$dataset) <= 30L) ft <- flextable::keep_with_next(ft, part = "all")
  if (autofit) ft <- flextable::autofit(ft)
  ft
}

#' Save a ggplot with the report's default device settings
#'
#' A thin wrapper around [ggplot2::ggsave()] that uses the `ragg` device when
#' available (for reliable font rendering on every platform) and records the
#' path on the returned object.
#'
#' @param plot A ggplot.
#' @param path Output file path (`.png`, `.pdf` or `.svg`).
#' @param width,height Size in inches.
#' @param dpi Resolution for raster output.
#' @return `path`, invisibly.
#' @export
#' @examples
#' \dontrun{
#' save_vcs_plot(p, "vctc_overall.png", width = 10, height = 7)
#' }
save_vcs_plot <- function(plot, path, width = 9, height = 6, dpi = 200) {
  assert_installed("ggplot2", "save_vcs_plot()")
  assert_string(path)
  ext <- tolower(tools::file_ext(path))
  dev <- if (ext == "png" && requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png
  } else {
    NULL
  }
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  if (is.null(dev)) {
    ggplot2::ggsave(path, plot, width = width, height = height, dpi = dpi,
                    bg = "white")
  } else {
    ggplot2::ggsave(path, plot, width = width, height = height, dpi = dpi,
                    device = dev, bg = "white")
  }
  invisible(path)
}
