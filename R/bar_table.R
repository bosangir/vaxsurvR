# ---------------------------------------------------------------------------
# Stratified "bar tables": the VCQI tabular convention in which each outcome
# cell is shaded in proportion to its value, rows are the stratifier ladder
# from estimate_stratified(), and small denominators are suppressed.
#
# Two renderers share one specification: plot_bar_table() draws the table as
# a figure (ggplot2), ft_bar_table() builds a native Word/HTML table
# (flextable) with in-cell mini bars.
# ---------------------------------------------------------------------------

#' Describe one column block of a bar table
#'
#' @param value Column holding the outcome (a proportion in 0--1 unless
#'   `scale = 1`).
#' @param n Column holding the sample size shown beside the outcome, or `NULL`.
#' @param label Header text for the outcome column; `\n` breaks lines.
#' @param n_label Header text for the sample-size column.
#' @param scale Multiplier applied before display (100 turns a proportion into
#'   a percentage).
#' @param digits Decimal places.
#' @param bar Shade the cell in proportion to `value / max`.
#' @param max Value that fills the cell completely.
#' @param suppress Replace the outcome with `(*)` when `n` is below the
#'   table's suppression threshold.
#' @param weighted_n Is `n` a weighted count (rounded for display)?
#' @return A list of class `vcs_measure`.
#' @export
#' @examples
#' bar_measure("estimate", "denominator", label = "PENTA1-PENTA3\nDropout (%)")
bar_measure <- function(value, n = NULL, label = value, n_label = "N",
                        scale = 100, digits = 1, bar = TRUE, max = 100,
                        suppress = TRUE, weighted_n = FALSE) {
  assert_string(value)
  assert_string(n, allow_null = TRUE)
  structure(list(value = value, n = n, label = label, n_label = n_label,
                 scale = scale, digits = digits, bar = bar, max = max,
                 suppress = suppress, weighted_n = weighted_n),
            class = "vcs_measure")
}

#' Lay out the rows of a bar table
#'
#' Inserts a blank spacer before every stratifier header and returns the
#' display order, so both renderers agree on row structure.
#' @noRd
bar_table_rows <- function(data) {
  if (!all(c("stratum", "row_type") %in% names(data))) {
    vcs_abort("`data` needs `stratum` and `row_type` columns (see `estimate_stratified()`).",
              class = "vaxsurvR_value_error")
  }
  data$indent <- data$indent %||% 0L
  out <- list()
  for (i in seq_len(nrow(data))) {
    if (data$row_type[i] == "header" && i > 1L) {
      sp <- data[i, , drop = FALSE]
      sp[] <- lapply(sp, function(col) col[NA_integer_])
      sp$stratum <- ""
      sp$row_type <- "spacer"
      sp$indent <- 0L
      out[[length(out) + 1L]] <- sp
    }
    out[[length(out) + 1L]] <- data[i, , drop = FALSE]
  }
  rows <- dplyr::bind_rows(out)
  rows$.row <- seq_len(nrow(rows))
  rows
}

#' Format a cell value, honouring suppression
#' @noRd
bar_cell_text <- function(value, n, m, suppress_n) {
  v <- suppressWarnings(as.numeric(value)) * m$scale
  txt <- ifelse(is.na(v), "", formatC(v, format = "f", digits = m$digits))
  if (!is.null(n) && isTRUE(m$suppress) && !is.null(suppress_n)) {
    nn <- suppressWarnings(as.numeric(n))
    txt[!is.na(nn) & nn < suppress_n & !is.na(v)] <- "(*)"
  }
  txt
}

#' Draw a stratified bar table as a figure
#'
#' Reproduces the VCQI table layout: a label column, then for every measure
#' a shaded outcome cell and a sample-size cell, with stratifier headers in
#' bold, nested levels indented, and outcomes based on fewer than
#' `suppress_n` observations shown as `(*)`.
#'
#' @param data A table from [estimate_stratified()] (needs `stratum`,
#'   `row_type`, `indent`).
#' @param measures A list of [bar_measure()] specifications.
#' @param suppress_n Suppress outcomes with `n` below this. `NULL` disables.
#' @param caution_n Mentioned in the default note; outcomes below it should be
#'   read with caution.
#' @param notes Footnote lines; `NULL` for the standard notes.
#' @param title Optional title.
#' @param palette A [vcs_palette()]; `palette$bar_fill` shades the cells.
#' @param base_size Font size in points.
#' @param label_width Width of the label column, in text units.
#' @param cell_width,n_width Widths of the outcome and sample-size cells.
#' @return A ggplot.
#' @export
#' @seealso [ft_bar_table()], [estimate_stratified()]
#' @examples
#' d <- derive_vaccination_status(vcs_example)
#' des <- vcs_design(d)
#' st <- vcs_strata(stratum = list(var = "stratum", label = "Stratum"))
#' tab <- estimate_stratified(des, estimate_dropout, st, first = "PENTA1", last = "PENTA3")
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot_bar_table(tab, list(bar_measure("estimate", "denominator",
#'                  label = "PENTA1-PENTA3\nDropout (%)", n_label = "N")))
#' }
plot_bar_table <- function(data, measures, suppress_n = 25, caution_n = 50,
                           notes = NULL, title = NULL, palette = vcs_palette(),
                           base_size = 9, label_width = 26, cell_width = 11,
                           n_width = 7) {
  assert_installed("ggplot2", "plot_bar_table()")
  if (inherits(measures, "vcs_measure")) measures <- list(measures)
  rows <- bar_table_rows(data)
  n_row <- nrow(rows)
  fam <- vcs_font()
  txt_size <- base_size / ggplot2::.pt

  # geometry: x in text units, y = row index (top = 1)
  x0 <- label_width
  blocks <- list()
  cur <- x0
  for (i in seq_along(measures)) {
    m <- measures[[i]]
    has_n <- !is.null(m$n)
    w <- cell_width + if (has_n) n_width else 0
    blocks[[i]] <- list(m = m, x_start = cur, x_val = cur + cell_width,
                        x_n = if (has_n) cur + cell_width + n_width else NA,
                        x_end = cur + w)
    cur <- cur + w + 1.2
  }
  x_end <- cur
  y_of <- function(r) n_row - r + 1
  header_h <- max(vapply(measures, function(m) {
    max(lengths(strsplit(c(m$label, m$n_label %||% ""), "\n")))
  }, numeric(1))) + 0.4
  ymax <- n_row + header_h + 0.5

  # label column
  lab_df <- rows[rows$row_type != "spacer", , drop = FALSE]
  lab_df$y <- y_of(lab_df$.row)
  lab_df$x <- 0.6 + 2.2 * lab_df$indent
  lab_df$face <- ifelse(lab_df$row_type %in% c("header", "overall"), "bold",
                        ifelse(lab_df$indent > 0, "italic", "plain"))

  p <- ggplot2::ggplot() +
    # header band
    ggplot2::annotate("rect", xmin = x0, xmax = x_end, ymin = n_row + 0.5,
                      ymax = ymax, fill = "#F2F2F2", colour = NA) +
    ggplot2::geom_text(data = lab_df,
                       ggplot2::aes(x = .data$x, y = .data$y, label = .data$stratum,
                                    fontface = .data$face),
                       hjust = 0, size = txt_size, family = fam, colour = palette$ink)

  # row rules
  rule_rows <- rows$.row[rows$row_type != "spacer"]
  p <- p + ggplot2::annotate("segment", x = 0, xend = x_end,
                             y = y_of(rule_rows) - 0.5, yend = y_of(rule_rows) - 0.5,
                             colour = palette$grey_lt, linewidth = 0.3)
  p <- p + ggplot2::annotate("segment", x = 0, xend = x_end, y = n_row + 0.5,
                             yend = n_row + 0.5, colour = palette$grey_dk, linewidth = 0.5)

  for (b in blocks) {
    m <- b$m
    v <- suppressWarnings(as.numeric(rows[[m$value]]))
    nn <- if (!is.null(m$n)) suppressWarnings(as.numeric(rows[[m$n]])) else NULL
    txt <- bar_cell_text(v, nn, m, suppress_n)
    show <- rows$row_type %in% c("overall", "parent", "level") & nzchar(txt)
    # Positions are stored in the data: ggplot2 evaluates aesthetics lazily,
    # so a reference to the loop variable would resolve to the last block.
    df <- tibble::tibble(y = y_of(rows$.row), txt = txt, v = v * m$scale,
                         show = show, sup = txt == "(*)",
                         xs = b$x_start, xv = b$x_val - 0.4, xn = b$x_n - 0.4)
    df$face <- ifelse(rows$row_type == "overall", "bold", "plain")
    if (isTRUE(m$bar)) {
      bars <- df[df$show & !df$sup & !is.na(df$v), , drop = FALSE]
      bars$w <- pmin(pmax(bars$v / m$max, 0), 1) * cell_width
      p <- p + ggplot2::geom_rect(
        data = bars,
        ggplot2::aes(xmin = .data$xs, xmax = .data$xs + .data$w,
                     ymin = .data$y - 0.45, ymax = .data$y + 0.45),
        fill = palette$bar_fill, colour = NA
      )
    }
    p <- p + ggplot2::geom_text(
      data = df[df$show, , drop = FALSE],
      ggplot2::aes(x = .data$xv, y = .data$y, label = .data$txt,
                   fontface = .data$face),
      hjust = 1, size = txt_size, family = fam, colour = palette$ink
    )
    if (!is.null(nn)) {
      ntxt <- ifelse(is.na(nn), "", format(round(nn), big.mark = ",", trim = TRUE))
      p <- p + ggplot2::geom_text(
        data = tibble::tibble(y = df$y, txt = ntxt, show = df$show,
                              face = df$face, xn = df$xn)[df$show, ],
        ggplot2::aes(x = .data$xn, y = .data$y, label = .data$txt,
                     fontface = .data$face),
        hjust = 1, size = txt_size, family = fam, colour = palette$ink
      )
    }
    # headers
    p <- p + ggplot2::annotate("text", x = b$x_val - 0.4, y = n_row + 0.7 + header_h / 2,
                               label = m$label, hjust = 1, vjust = 0.5,
                               size = txt_size, family = fam, colour = palette$ink,
                               lineheight = 0.95)
    if (!is.null(nn)) {
      p <- p + ggplot2::annotate("text", x = b$x_n - 0.4, y = n_row + 0.7 + header_h / 2,
                                 label = m$n_label, hjust = 1, vjust = 0.5,
                                 size = txt_size, family = fam, colour = palette$ink,
                                 lineheight = 0.95)
    }
    # block separators
    p <- p + ggplot2::annotate("segment", x = b$x_start - 0.6, xend = b$x_start - 0.6,
                               y = 0.5, yend = ymax, colour = palette$grey_dk,
                               linewidth = 0.4)
  }
  p <- p + ggplot2::annotate("segment", x = x_end, xend = x_end, y = 0.5, yend = ymax,
                             colour = palette$grey_dk, linewidth = 0.4)

  if (is.null(notes)) {
    notes <- c(
      "Table cells are shaded in proportion to the outcome; an outcome of 100% would fill the cell with colour.",
      if (!is.null(suppress_n)) sprintf("Outcomes based on fewer than %d observations are suppressed (*). Outcomes based on fewer than %d observations should be interpreted with caution.", suppress_n, caution_n)
    )
  }
  p <- p +
    ggplot2::scale_x_continuous(limits = c(0, x_end + 0.5), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0.4, ymax + 0.2), expand = c(0, 0)) +
    ggplot2::labs(title = title,
                  caption = if (length(notes)) paste(unlist(lapply(notes, strwrap, width = max(40, round(x_end * 1.15)))), collapse = "\n") else NULL) +
    ggplot2::theme_void(base_size = base_size, base_family = fam) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", colour = palette$ink, hjust = 0,
                                         size = base_size * 1.15,
                                         margin = ggplot2::margin(b = 6)),
      plot.caption = ggplot2::element_text(hjust = 0, colour = palette$grey_dk,
                                           size = base_size * 0.85,
                                           margin = ggplot2::margin(t = 6)),
      plot.caption.position = "plot",
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin = ggplot2::margin(4, 6, 4, 4)
    )
  attr(p, "vcs_size") <- c(width = (x_end + 2) / 12.5, height = (ymax + 3) / 5.4)
  p
}

#' Suggested figure size for a bar table, in inches
#'
#' @param p A plot from [plot_bar_table()].
#' @return A named numeric vector `width`, `height`.
#' @export
bar_table_size <- function(p) {
  s <- attr(p, "vcs_size")
  if (is.null(s)) c(width = 8, height = 6) else s
}

#' Build a stratified bar table as a flextable
#'
#' The Word/HTML counterpart of [plot_bar_table()]: a native table with a
#' proportional mini-bar and the value in each outcome cell, stratifier
#' headers in bold and nested rows indented.
#'
#' @inheritParams plot_bar_table
#' @param palette A [vcs_palette()].
#' @param font_size Body font size in points.
#' @return A `flextable`.
#' @export
#' @seealso [plot_bar_table()]
ft_bar_table <- function(data, measures, suppress_n = 25, caution_n = 50,
                         notes = NULL, palette = vcs_palette(), font_size = 8.5) {
  assert_installed(c("flextable", "officer"), "ft_bar_table()")
  if (inherits(measures, "vcs_measure")) measures <- list(measures)
  rows <- bar_table_rows(data)
  rows <- rows[rows$row_type != "spacer", , drop = FALSE]
  n_row <- nrow(rows)
  label <- ifelse(rows$indent > 0, paste0(strrep(" ", 4 * rows$indent), rows$stratum),
                  rows$stratum)
  out <- data.frame(stratum = label, stringsAsFactors = FALSE)
  keys <- character(0)
  hdr <- list(stratum = "")
  bars <- list()
  for (i in seq_along(measures)) {
    m <- measures[[i]]
    v <- suppressWarnings(as.numeric(rows[[m$value]]))
    nn <- if (!is.null(m$n)) suppressWarnings(as.numeric(rows[[m$n]])) else NULL
    txt <- bar_cell_text(v, nn, m, suppress_n)
    show <- rows$row_type %in% c("overall", "parent", "level")
    txt[!show] <- ""
    vk <- sprintf("v%d", i)
    out[[vk]] <- txt
    hdr[[vk]] <- gsub("\n", " ", m$label)
    keys <- c(keys, vk)
    share <- ifelse(show & txt != "(*)" & !is.na(v), pmin(pmax(v * m$scale / m$max, 0), 1), 0)
    bars[[vk]] <- list(share = share, on = isTRUE(m$bar))
    if (!is.null(nn)) {
      nk <- sprintf("n%d", i)
      out[[nk]] <- ifelse(show & !is.na(nn), format(round(nn), big.mark = ",", trim = TRUE), "")
      hdr[[nk]] <- m$n_label
      keys <- c(keys, nk)
    }
  }
  ft <- flextable::flextable(out)
  ft <- flextable::set_header_labels(ft, values = hdr)
  fam <- getOption("vaxsurvR.font", "Century Gothic")
  ft <- flextable::font(ft, fontname = fam, part = "all")
  ft <- flextable::fontsize(ft, size = font_size, part = "all")
  ft <- flextable::bg(ft, bg = "#F2F2F2", part = "header")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::align(ft, align = "right", part = "all", j = keys)
  ft <- flextable::align(ft, align = "left", part = "all", j = "stratum")
  ft <- flextable::valign(ft, valign = "bottom", part = "header")
  for (vk in names(bars)) {
    if (!bars[[vk]]$on) next
    for (r in seq_len(n_row)) {
      sh <- bars[[vk]]$share[r]
      if (is.na(sh) || sh <= 0) next
      ft <- flextable::compose(
        ft, i = r, j = vk,
        value = flextable::as_paragraph(
          flextable::minibar(value = sh, max = 1, barcol = palette$bar_fill,
                             bg = "transparent", width = 0.45, height = 0.12),
          " ", out[[vk]][r]
        )
      )
    }
  }
  hdr_rows <- which(rows$row_type == "header")
  if (length(hdr_rows)) {
    ft <- flextable::bold(ft, i = hdr_rows, j = "stratum")
    ft <- flextable::padding(ft, i = hdr_rows, padding.top = 6, part = "body")
  }
  ov <- which(rows$row_type == "overall")
  if (length(ov)) ft <- flextable::bold(ft, i = ov)
  par_rows <- which(rows$row_type == "parent")
  if (length(par_rows)) ft <- flextable::bold(ft, i = par_rows, j = "stratum")
  it <- which(rows$indent > 0)
  if (length(it)) ft <- flextable::italic(ft, i = it, j = "stratum")
  ft <- flextable::border_remove(ft)
  thin <- officer::fp_border(color = palette$grey_lt, width = 0.5)
  ft <- flextable::border_inner_h(ft, border = thin, part = "body")
  ft <- flextable::hline_bottom(ft, border = officer::fp_border(color = palette$grey_dk, width = 0.75), part = "header")
  ft <- flextable::hline_bottom(ft, border = officer::fp_border(color = palette$grey_dk, width = 0.75), part = "body")
  # vertical rules between measure blocks
  block_first <- vapply(seq_along(measures), function(i) sprintf("v%d", i), character(1))
  ft <- flextable::border(ft, j = block_first, border.left = officer::fp_border(color = palette$grey, width = 0.5), part = "all")
  ft <- flextable::padding(ft, padding.top = 1.5, padding.bottom = 1.5, part = "all")
  if (is.null(notes)) {
    notes <- c(
      "Table cells are shaded in proportion to the outcome; an outcome of 100% would fill the cell with colour.",
      if (!is.null(suppress_n)) sprintf("Outcomes based on fewer than %d observations are suppressed (*); fewer than %d should be interpreted with caution.", suppress_n, caution_n)
    )
  }
  if (length(notes)) {
    ft <- flextable::add_footer_lines(ft, values = notes)
    ft <- flextable::fontsize(ft, size = font_size - 1, part = "footer")
    ft <- flextable::font(ft, fontname = fam, part = "footer")
    ft <- flextable::color(ft, color = palette$grey_dk, part = "footer")
  }
  if (n_row <= 30L) ft <- flextable::keep_with_next(ft, part = "all")
  ft <- flextable::autofit(ft)
  ft <- flextable::width(ft, j = "stratum", width = 1.9)
  ft
}
