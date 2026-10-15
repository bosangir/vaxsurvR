# ---------------------------------------------------------------------------
# The pipeline, drawn rather than described.
#
# The workflow is five stages, and the thing readers most often miss is that
# the first stage is configuration: the schedule, the dictionary and the recall
# crosswalk are written once and every later call reads them. The diagram puts
# that stage at the top with the others hanging off it, so the shape of the
# pipeline is visible before any of the function names are read.
# ---------------------------------------------------------------------------

#' The vaxsurvR analysis pipeline, as data
#'
#' One row per function in the workflow, with the stage it belongs to and the
#' order it runs in. [plot_vcs_pipeline()] draws it; returning it as a table
#' means the same definition can also be tabulated, filtered or extended
#' without editing the drawing code.
#'
#' @return A [tibble][tibble::tibble] with columns `stage`, `step`, `fn`,
#'   `optional` and `caption`.
#' @export
#' @family pipeline
#' @examples
#' vcs_pipeline()
#' subset(vcs_pipeline(), stage == "Configure")
vcs_pipeline <- function() {
  tibble::tribble(
    ~stage,       ~step, ~fn,                             ~optional, ~caption,
    "Configure",  1L,    "vcs_schedule()",                FALSE,     "describe the survey once",
    "Configure",  2L,    "vcs_dictionary()",              FALSE,     "describe the survey once",
    "Configure",  3L,    "vcs_recall_map()",              TRUE,      "describe the survey once",
    "Import",     1L,    "read_vcs()",                    FALSE,     "import and standardise",
    "Import",     2L,    "apply_recall_map()",            TRUE,      "import and standardise",
    "Import",     3L,    "map_vcs_variables()",           FALSE,     "import and standardise",
    "Check",      1L,    "validate_vcs()",                FALSE,     "check, flag, log",
    "Check",      2L,    "clean_vcs()",                   FALSE,     "check, flag, log",
    "Derive",     1L,    "derive_vaccination_status()",   FALSE,     "define, then protect",
    "Derive",     2L,    "deidentify_vcs()",              FALSE,     "define, then protect",
    "Estimate",   1L,    "vcs_design()",                  FALSE,     "estimate with the design",
    "Estimate",   2L,    "estimate_coverage_by_evidence()", FALSE,   "estimate with the design",
    "Estimate",   3L,    "estimate_dropout()",            FALSE,     "estimate with the design",
    "Estimate",   4L,    "vcs_summary()",                 FALSE,     "estimate with the design"
  )
}

#' Break a long function name at the underscore nearest its middle
#' @noRd
wrap_fn_label <- function(x, max_chars = 20L) {
  if (nchar(x) <= max_chars) {
    return(x)
  }
  pos <- gregexpr("_", x, fixed = TRUE)[[1]]
  if (identical(as.integer(pos), -1L)) {
    return(x)
  }
  cut <- pos[which.min(abs(pos - nchar(x) / 2))]
  paste0(substr(x, 1, cut), "\n", substr(x, cut + 1L, nchar(x)))
}

#' Draw the vaxsurvR pipeline
#'
#' A five-stage flow diagram: configuration at the top, then import, quality
#' assurance, derivation and estimation. Optional steps are outlined rather
#' than filled, so a reader can see at a glance which parts of the workflow a
#' survey without a recall block can skip.
#'
#' @param pipeline A table in the shape of [vcs_pipeline()].
#' @param palette A [vcs_palette()].
#' @param base_size Base font size in points.
#' @param title,subtitle Labels. `NULL` for none.
#' @return A ggplot.
#' @export
#' @family pipeline
#' @seealso [vcs_pipeline()]
#' @examples
#' p <- plot_vcs_pipeline()
#' class(p)
plot_vcs_pipeline <- function(pipeline = vcs_pipeline(), palette = vcs_palette(),
                              base_size = 10, title = "The vaxsurvR pipeline",
                              subtitle = "Each stage is a function you can run, inspect and test on its own") {
  assert_installed("ggplot2", "plot_vcs_pipeline()")
  d <- as.data.frame(pipeline)
  need <- c("stage", "step", "fn", "optional", "caption")
  missing <- setdiff(need, names(d))
  if (length(missing)) {
    vcs_abort(sprintf("`pipeline` is missing column(s): %s.",
                      paste(missing, collapse = ", ")),
              class = "vaxsurvR_missing_variable")
  }
  fam <- vcs_font()
  stages <- unique(d$stage)
  n_stage <- length(stages)

  # Rows run top to bottom; boxes are centred within their row so that rows of
  # two and rows of four stay visually balanced.
  d$row <- match(d$stage, stages)
  d$y <- n_stage - d$row + 1L
  widest <- max(table(d$stage))
  d$x <- unlist(lapply(split(d, d$row), function(g) {
    n <- nrow(g)
    seq_len(n) + (widest - n) / 2
  }), use.names = FALSE)

  bw <- 0.42   # half box width; the gap left over is where the arrows go
  bh <- 0.30   # half box height
  d$xmin <- d$x - bw; d$xmax <- d$x + bw
  d$ymin <- d$y - bh; d$ymax <- d$y + bh

  stage_fill <- stats::setNames(
    rep(c(palette$primary, palette$info, palette$accent, palette$danger,
          palette$secondary), length.out = n_stage),
    stages
  )
  d$fill <- ifelse(d$optional, "white", unname(stage_fill[d$stage]))
  d$colour <- unname(stage_fill[d$stage])
  d$text_colour <- ifelse(d$optional, unname(stage_fill[d$stage]), "white")
  # A name longer than the box is broken at an underscore near its middle, so
  # that estimate_coverage_by_evidence() does not spill over its edges.
  d$label <- vapply(d$fn, wrap_fn_label, character(1), USE.NAMES = FALSE)
  d$label <- ifelse(d$optional, paste0(d$label, "\n(optional)"), d$label)

  # Arrows between consecutive boxes in a row.
  across <- do.call(rbind, lapply(split(d, d$row), function(g) {
    g <- g[order(g$step), ]
    if (nrow(g) < 2L) return(NULL)
    data.frame(x = g$xmax[-nrow(g)] + 0.02, xend = g$xmin[-1] - 0.02,
               y = g$y[-nrow(g)], yend = g$y[-1])
  }))
  # Arrows from each row to the next, dropped from the row's centre.
  down <- do.call(rbind, lapply(seq_len(n_stage - 1L), function(i) {
    a <- d[d$row == i, ]; b <- d[d$row == i + 1L, ]
    data.frame(x = mean(range(a$x)), xend = mean(range(b$x)),
               y = min(a$ymin) - 0.02, yend = max(b$ymax) + 0.02)
  }))

  caps <- do.call(rbind, lapply(split(d, d$row), function(g) {
    data.frame(x = widest + 0.85, y = g$y[1], stage = g$stage[1],
               caption = g$caption[1])
  }))

  arrow <- ggplot2::arrow(length = ggplot2::unit(5, "pt"), type = "closed")
  ggplot2::ggplot() +
    ggplot2::geom_segment(data = down,
                          ggplot2::aes(x = .data$x, xend = .data$xend,
                                       y = .data$y, yend = .data$yend),
                          arrow = arrow, colour = palette$grey_dk,
                          linewidth = 0.5) +
    {
      if (!is.null(across)) {
        ggplot2::geom_segment(data = across,
                              ggplot2::aes(x = .data$x, xend = .data$xend,
                                           y = .data$y, yend = .data$yend),
                              arrow = arrow, colour = palette$grey_dk,
                              linewidth = 0.5)
      }
    } +
    ggplot2::geom_rect(data = d,
                       ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                                    ymin = .data$ymin, ymax = .data$ymax),
                       fill = d$fill, colour = d$colour, linewidth = 0.6) +
    ggplot2::geom_text(data = d,
                       ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
                       colour = d$text_colour, size = base_size / 3.6,
                       family = fam, lineheight = 0.95, fontface = "bold") +
    ggplot2::geom_text(data = caps,
                       ggplot2::aes(x = .data$x, y = .data$y, label = .data$stage),
                       hjust = 0, family = fam, fontface = "bold",
                       size = base_size / 3.1, colour = palette$ink) +
    ggplot2::geom_text(data = caps,
                       ggplot2::aes(x = .data$x, y = .data$y - 0.18,
                                    label = .data$caption),
                       hjust = 0, family = fam, size = base_size / 4,
                       colour = palette$grey_dk) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::coord_cartesian(xlim = c(0.3, widest + 2.6), clip = "off") +
    ggplot2::labs(title = title, subtitle = subtitle, x = NULL, y = NULL) +
    ggplot2::theme_void(base_size = base_size, base_family = fam) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", colour = palette$primary,
                                         size = base_size + 3,
                                         margin = ggplot2::margin(b = 2)),
      plot.subtitle = ggplot2::element_text(colour = palette$grey_dk,
                                            size = base_size - 1,
                                            margin = ggplot2::margin(b = 8)),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
}
