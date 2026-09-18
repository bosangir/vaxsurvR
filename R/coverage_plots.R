# ---------------------------------------------------------------------------
# Coverage figures beyond the VCTC: one dose across every stratum (the Annex
# C figures), card / recall / either side by side, and the achieved-sample
# table.
# ---------------------------------------------------------------------------

#' Plot one dose's coverage across the stratifier ladder
#'
#' Horizontal bars, one per stratum in the order of [estimate_stratified()],
#' coloured by stratifier, with the confidence interval as a line and the
#' point estimate and interval printed at the right.
#'
#' @param tab Output of [estimate_stratified()] with [estimate_coverage()]
#'   for a single dose.
#' @param title Title; defaults to `"Crude coverage of <dose>"`.
#' @param palette A [vcs_palette()]; `palette$strata` colours the groups.
#' @param base_size Font size.
#' @param x_max Right limit of the axis (percent), leaving room for the text.
#' @return A ggplot.
#' @export
#' @examples
#' d <- derive_vaccination_status(vcs_example)
#' des <- vcs_design(d)
#' st <- vcs_strata(stratum = list(var = "stratum", label = "Stratum"))
#' tab <- estimate_stratified(des, estimate_coverage, st, vaccines = "PENTA1")
#' if (requireNamespace("ggplot2", quietly = TRUE)) plot_coverage_by_stratum(tab)
plot_coverage_by_stratum <- function(tab, title = NULL, palette = vcs_palette(),
                                     base_size = 10, x_max = 100) {
  assert_installed("ggplot2", "plot_coverage_by_stratum()")
  d <- tab[tab$row_type != "header", , drop = FALSE]
  dose <- unique(stats::na.omit(d$indicator))[1]
  title <- title %||% sprintf("Crude coverage of %s", dose)
  d$y <- rev(seq_len(nrow(d)))
  strat_names <- unique(d$stratifier)
  pal_keys <- c("overall", "level1", "residence", "sex", "education")
  cols <- palette$strata
  fill <- vapply(seq_len(nrow(d)), function(i) {
    if (d$row_type[i] == "overall") return(cols[["overall"]])
    k <- match(d$stratifier[i], strat_names[-1])
    if (d$row_type[i] == "parent") return(cols[["level1"]])
    if (d$indent[i] > 0) return(cols[["level2"]])
    if (!is.na(k)) {
      key <- c("level1", "residence", "sex", "education", "level2")[min(k, 5)]
      return(cols[[key]])
    }
    cols[["level1"]]
  }, character(1))
  d$fill <- fill
  d$label_txt <- ifelse(is.na(d$estimate), "",
                        sprintf("%.1f%% (%.1f, %.1f)", 100 * d$estimate, 100 * d$conf_low, 100 * d$conf_high))
  fam <- vcs_font()
  p <- ggplot2::ggplot(d) +
    ggplot2::geom_rect(ggplot2::aes(xmin = 0, xmax = 100 * .data$estimate,
                                    ymin = .data$y - 0.38, ymax = .data$y + 0.38),
                       fill = d$fill, colour = "grey40", linewidth = 0.2) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = 100 * .data$conf_low, xmax = 100 * .data$conf_high,
                                         y = .data$y), height = 0, colour = "black", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(x = x_max + 3, y = .data$y, label = .data$label_txt),
                       hjust = 0, size = base_size / ggplot2::.pt * 0.85, family = fam) +
    ggplot2::scale_y_continuous(breaks = d$y, labels = d$stratum, expand = ggplot2::expansion(add = 0.6)) +
    ggplot2::scale_x_continuous(breaks = seq(0, 100, 25), limits = c(0, x_max + 45), expand = c(0, 0)) +
    ggplot2::labs(x = "Estimated coverage %", y = NULL, title = title,
                  caption = "Text at right: point estimate (2-sided 95% confidence interval)") +
    theme_vcs(base_size = base_size, palette = palette, grid = "x")
  p
}

#' Plot coverage by source of evidence
#'
#' Dot-and-interval chart with card-documented, caregiver-recall and combined
#' coverage for every dose, as in the Kongo Central progress reports.
#'
#' @param est Output of [estimate_coverage_by_evidence()] with no `by`.
#' @param title Title.
#' @param palette A [vcs_palette()].
#' @param base_size Font size.
#' @return A ggplot.
#' @export
#' @examples
#' est <- estimate_coverage_by_evidence(vcs_example, vaccines = c("BCG", "PENTA1", "PENTA3"))
#' if (requireNamespace("ggplot2", quietly = TRUE)) plot_coverage_evidence(est)
plot_coverage_evidence <- function(est, title = "Coverage by antigen: card-documented, caregiver recall, and combined",
                                   palette = vcs_palette(), base_size = 10) {
  assert_installed("ggplot2", "plot_coverage_evidence()")
  d <- tibble::as_tibble(est)
  doses <- unique(d$indicator)
  d$indicator <- factor(d$indicator, levels = rev(doses))
  lv <- levels(factor(d$evidence))
  cols <- stats::setNames(c(palette$secondary, palette$accent, palette$primary)[seq_along(lv)], lv)
  p <- ggplot2::ggplot(d, ggplot2::aes(y = .data$indicator, x = 100 * .data$estimate,
                                       colour = .data$evidence)) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = 100 * .data$conf_low, xmax = 100 * .data$conf_high),
                            height = 0.25, position = ggplot2::position_dodge(width = 0.6),
                            linewidth = 0.5) +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.6), size = 2.3,
                        shape = 21, fill = NA, stroke = 1.1) +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.6), size = 2.3,
                        ggplot2::aes(fill = .data$evidence), shape = 21, colour = "grey20", stroke = 0.3) +
    ggplot2::scale_colour_manual(values = cols) +
    ggplot2::scale_fill_manual(values = cols) +
    ggplot2::scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    ggplot2::labs(x = "Coverage (%) with 95% confidence interval", y = NULL, title = title) +
    theme_vcs(base_size = base_size, palette = palette, grid = "xy") +
    ggplot2::theme(legend.position = c(0.98, 0.04), legend.justification = c(1, 0),
                   legend.background = ggplot2::element_rect(fill = "white", colour = palette$grey))
  p
}

#' Target versus achieved sample, by stratum
#'
#' @param x A [vcs_data][new_vcs_data] object or a child-level data frame.
#' @param by Column naming the stratum (e.g. health zone).
#' @param targets Optional data frame with the `by` column, `target_clusters`
#'   and `target_respondents`.
#' @param psu Column naming the cluster.
#' @return A tibble with one row per stratum and a total row: clusters,
#'   respondents, average respondents per cluster, and targets where given.
#' @export
#' @examples
#' sample_achieved(vcs_example, by = "stratum")
sample_achieved <- function(x, by, targets = NULL, psu = "psu") {
  ch <- if (is_vcs_data(x)) vcs_children(x) else {
    assert_data(x)
    x
  }
  assert_string(by)
  assert_columns(ch, c(by, psu))
  g <- dplyr::group_by(ch, dplyr::across(dplyr::all_of(by)))
  out <- dplyr::summarise(g, clusters = dplyr::n_distinct(.data[[psu]]),
                          respondents = dplyr::n(), .groups = "drop")
  out[[by]] <- as.character(out[[by]])
  total <- tibble::tibble(clusters = length(unique(ch[[psu]])), respondents = nrow(ch))
  total[[by]] <- "Total"
  out <- dplyr::bind_rows(out, total)
  out$per_cluster <- out$respondents / out$clusters
  if (!is.null(targets)) {
    assert_columns(targets, c(by, "target_clusters", "target_respondents"))
    t <- targets
    t[[by]] <- as.character(t[[by]])
    tt <- tibble::tibble(target_clusters = sum(t$target_clusters),
                         target_respondents = sum(t$target_respondents))
    tt[[by]] <- "Total"
    t <- dplyr::bind_rows(t[, c(by, "target_clusters", "target_respondents")], tt)
    out <- dplyr::left_join(out, t, by = by)
    out$pct_respondents <- out$respondents / out$target_respondents
  }
  out
}
