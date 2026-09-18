# ---------------------------------------------------------------------------
# Timeliness beyond categories: intradose intervals, cumulative coverage
# curves (VCQI RI_CCC_02), cumulative interval curves (RI_CIC_02) and
# organ-pipe plots of cluster-level coverage.
#
# Everything here rests on card dates. A dose known only from a tick mark or
# from recall has no date, contributes nothing to a curve, and is never
# imputed.
# ---------------------------------------------------------------------------

#' Child-level weight vector, equal weights when none are mapped
#' @noRd
child_weights <- function(ch) {
  if ("weight" %in% names(ch)) {
    w <- suppressWarnings(as.numeric(ch$weight))
    w[is.na(w) | w <= 0] <- NA_real_
    if (all(is.na(w))) w <- rep(1, nrow(ch))
    return(w)
  }
  rep(1, nrow(ch))
}

#' Intervals between consecutive dated doses
#'
#' For every dose with a `previous_dose` in the schedule, the number of days
#' between the two doses when both carry usable card dates. Nothing is
#' computed for a pair with a missing or partial date.
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param schedule A [vcs_schedule()]; defaults to the one stored in `x`.
#' @return A tibble with `child_id`, `antigen`, `from`, `to`, `interval_days`,
#'   `scheduled_interval_days` and the child-level columns of `x`.
#' @export
#' @seealso [table_dose_intervals()], [compute_cic()]
#' @examples
#' head(derive_dose_intervals(vcs_example))
derive_dose_intervals <- function(x, schedule = NULL) {
  assert_vcs_data(x)
  schedule <- schedule %||% x$schedule
  if (is.null(schedule)) {
    vcs_abort("Supply `schedule`.", class = "vaxsurvR_value_error")
  }
  vx <- x$vaccinations
  if (!nrow(vx) || !"card_date" %in% names(vx)) {
    return(tibble::tibble(child_id = character(0), antigen = character(0),
                          from = character(0), to = character(0),
                          interval_days = numeric(0),
                          scheduled_interval_days = numeric(0)))
  }
  prec <- vx$card_date_precision %||% rep("day", nrow(vx))
  usable <- !is.na(vx$card_date) & (is.na(prec) | prec %in% c("day", "complete"))
  key <- paste(vx$child_id, vx$vaccine)
  date_of <- stats::setNames(vx$card_date, key)
  date_of[!usable] <- NA
  pairs <- schedule[!is.na(schedule$previous_dose), , drop = FALSE]
  rows <- lapply(seq_len(nrow(pairs)), function(i) {
    to <- pairs$vaccine[i]
    from <- pairs$previous_dose[i]
    kids <- unique(vx$child_id[vx$vaccine == to])
    d_to <- date_of[paste(kids, to)]
    d_from <- date_of[paste(kids, from)]
    ok <- !is.na(d_to) & !is.na(d_from)
    if (!any(ok)) return(NULL)
    tibble::tibble(
      child_id = kids[ok], antigen = pairs$antigen[i], from = from, to = to,
      interval_days = as.numeric(d_to[ok] - d_from[ok]),
      scheduled_interval_days = pairs$minimum_interval_days[i]
    )
  })
  out <- dplyr::bind_rows(rows)
  if (!nrow(out)) return(out)
  ch <- vcs_children(x)
  extra <- setdiff(names(ch), names(out))
  dplyr::left_join(out, ch[, c("child_id", extra), drop = FALSE], by = "child_id")
}

#' Percent of intradose intervals that were too short or too long
#'
#' The unweighted summary VCQI reports as RI_QUAL_05: among consecutive doses
#' of a series that both have card dates, the share of intervals shorter than
#' `short_days` and longer than `long_days`. `N` counts intervals, not
#' children -- a child contributes one interval per consecutive pair with
#' dates.
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param by Domains, as a one-sided formula or character vector, over the
#'   child-level columns.
#' @param antigen Antigen series to summarise (e.g. `"PENTA"`), or `NULL` for
#'   every series with a `previous_dose`.
#' @param short_days,long_days Thresholds in days.
#' @param pairs Optional restriction to particular `to` doses.
#' @return A tibble with `antigen`, the domain columns, `n_intervals`,
#'   `n_short`, `prop_short`, `n_long`, `prop_long`, `median_days`.
#' @export
#' @examples
#' table_dose_intervals(vcs_example, antigen = "PENTA")
#' table_dose_intervals(vcs_example, antigen = "PENTA", by = ~stratum)
table_dose_intervals <- function(x, by = NULL, antigen = NULL, short_days = 28,
                                 long_days = 56, pairs = NULL) {
  iv <- derive_dose_intervals(x)
  by <- as_column_names(by)
  if (!is.null(antigen)) iv <- iv[iv$antigen %in% antigen, , drop = FALSE]
  if (!is.null(pairs)) iv <- iv[iv$to %in% pairs, , drop = FALSE]
  grp <- c("antigen", by)
  if (!nrow(iv)) {
    out <- tibble::tibble(antigen = antigen %||% NA_character_)
    for (v in by) out[[v]] <- NA_character_
    out$n_intervals <- 0L; out$n_short <- 0L; out$prop_short <- NA_real_
    out$n_long <- 0L; out$prop_long <- NA_real_; out$median_days <- NA_real_
    return(out)
  }
  if (is.null(antigen)) grp <- c("antigen", by) else grp <- by
  iv$antigen <- if (is.null(antigen)) iv$antigen else paste(antigen, collapse = "+")
  out <- dplyr::group_by(iv, dplyr::across(dplyr::all_of(c("antigen", by))))
  out <- dplyr::summarise(
    out,
    n_intervals = dplyr::n(),
    n_short = sum(.data$interval_days < short_days),
    n_long = sum(.data$interval_days > long_days),
    median_days = stats::median(.data$interval_days),
    .groups = "drop"
  )
  out$prop_short <- out$n_short / out$n_intervals
  out$prop_long <- out$n_long / out$n_intervals
  for (v in by) out[[v]] <- as.character(out[[v]])
  out
}

#' Cumulative coverage curves by age
#'
#' For each dose, the weighted percentage of all children known -- from a
#' dated card entry -- to have received the dose by each age in days. The
#' denominator is every child in the domain, so each curve plateaus at the
#' card-dated coverage of the dose, and a curve that keeps rising long after
#' the due age shows doses given late.
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param vaccines Doses to include; defaults to the schedule.
#' @param subset Optional logical expression on the child-level table.
#' @param max_age Upper age in days; defaults to the oldest observed.
#' @return A tibble of class `vcs_ccc` with `vaccine`, `antigen`, `dose`,
#'   `age_days`, `cum_pct`, and attributes `n_children`, `n_with_dates`.
#' @export
#' @seealso [plot_ccc()]
#' @examples
#' cc <- compute_ccc(vcs_example, vaccines = c("BCG", "PENTA1", "PENTA3"))
#' head(cc)
compute_ccc <- function(x, vaccines = NULL, subset = NULL, max_age = NULL) {
  assert_vcs_data(x)
  sched <- x$schedule
  vaccines <- vaccines %||% intersect(sched$vaccine, unique(x$vaccinations$vaccine))
  ch <- vcs_children(x)
  sub_q <- rlang::enquo(subset)
  if (!rlang::quo_is_null(sub_q)) {
    keep <- rlang::eval_tidy(sub_q, data = ch)
    keep[is.na(keep)] <- FALSE
    ch <- ch[keep, , drop = FALSE]
  }
  w <- child_weights(ch)
  w[is.na(w)] <- 0
  total_w <- sum(w)
  vx <- vx_with_dates(x)
  vx <- vx[vx$child_id %in% ch$child_id & vx$vaccine %in% vaccines, , drop = FALSE]
  prec <- vx$card_date_precision %||% rep("day", nrow(vx))
  age <- derive_age_at_vaccination(vx$child_dob, vx$card_date)
  ok <- !is.na(age) & age >= 0 & (is.na(prec) | prec %in% c("day", "complete"))
  vx <- vx[ok, , drop = FALSE]
  age <- age[ok]
  wv <- w[match(vx$child_id, ch$child_id)]
  max_age <- max_age %||% (if (length(age)) max(age) else 730)
  grid <- seq(0, max_age, by = 1)
  rows <- lapply(vaccines, function(v) {
    sel <- vx$vaccine == v
    a <- age[sel]
    ww <- wv[sel]
    cum <- vapply(grid, function(g) sum(ww[a <= g]), numeric(1))
    tibble::tibble(vaccine = v,
                   antigen = sched$antigen[match(v, sched$vaccine)],
                   dose = sched$dose[match(v, sched$vaccine)],
                   age_days = grid,
                   cum_pct = if (total_w > 0) 100 * cum / total_w else NA_real_)
  })
  out <- dplyr::bind_rows(rows)
  n_dates <- length(unique(vx$child_id))
  attr(out, "n_children") <- nrow(ch)
  attr(out, "n_with_dates") <- n_dates
  attr(out, "schedule") <- sched
  class(out) <- c("vcs_ccc", class(out))
  out
}

#' Plot cumulative coverage curves
#'
#' @param x A [compute_ccc()] result.
#' @param title,subtitle Titles.
#' @param antigens Optional subset of antigen series to draw.
#' @param palette A [vcs_palette()]; `palette$antigen` colours the series.
#' @param mark_ages Draw dashed vertical lines at scheduled ages. Defaults to
#'   the distinct minimum ages of the doses drawn.
#' @param y_max Upper limit of the y axis; `NULL` fits the data.
#' @return A ggplot.
#' @export
#' @examples
#' cc <- compute_ccc(vcs_example)
#' if (requireNamespace("ggplot2", quietly = TRUE)) plot_ccc(cc)
plot_ccc <- function(x, title = "Cumulative coverage by age", subtitle = NULL,
                     antigens = NULL, palette = vcs_palette(), mark_ages = NULL,
                     y_max = NULL) {
  assert_installed("ggplot2", "plot_ccc()")
  sched <- attr(x, "schedule")
  d <- tibble::as_tibble(x)
  if (!is.null(antigens)) d <- d[d$antigen %in% antigens, , drop = FALSE]
  if (!nrow(d)) {
    vcs_abort("Nothing to plot.", class = "vaxsurvR_value_error")
  }
  vaccines <- unique(d$vaccine)
  mark_ages <- mark_ages %||% sort(unique(stats::na.omit(sched$minimum_age_days[sched$vaccine %in% vaccines])))
  cols <- palette$antigen
  miss <- setdiff(unique(d$antigen), names(cols))
  if (length(miss)) cols <- c(cols, stats::setNames(grDevices::hcl.colors(length(miss), "Dark 3"), miss))
  d$dose_f <- factor(d$dose)
  n_dose <- length(unique(d$dose))
  lt <- c("solid", "22", "42", "13", "1141")[seq_len(max(n_dose, 1))]
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$age_days, y = .data$cum_pct,
                                       colour = .data$antigen, group = .data$vaccine,
                                       linetype = .data$dose_f)) +
    ggplot2::geom_vline(xintercept = mark_ages, linetype = "dashed", colour = palette$grey,
                        linewidth = 0.4) +
    ggplot2::geom_step(linewidth = 0.6) +
    ggplot2::scale_colour_manual(values = cols, name = NULL) +
    ggplot2::scale_linetype_manual(values = lt, name = "Dose", guide = if (n_dose > 1) "legend" else "none") +
    ggplot2::scale_x_continuous(breaks = unique(c(0, mark_ages, 365, 540, max(d$age_days))),
                                expand = c(0.01, 0)) +
    ggplot2::labs(x = "Age (days)", y = "Cumulative weighted % vaccinated\n(according to card)",
                  title = title, subtitle = subtitle,
                  caption = sprintf(
                    "Vertical dashed lines mark scheduled vaccination ages: %s days.\nDenominator is all eligible respondents. %s of %s respondents had card records with dates.",
                    paste(mark_ages, collapse = ", "),
                    format(attr(x, "n_with_dates"), big.mark = ","),
                    format(attr(x, "n_children"), big.mark = ","))) +
    theme_vcs(palette = palette, grid = "xy") +
    ggplot2::theme(legend.position = "bottom", legend.box = "vertical",
                   axis.text.x = ggplot2::element_text(size = 8))
  if (!is.null(y_max)) p <- p + ggplot2::coord_cartesian(ylim = c(0, y_max))
  p
}

#' Cumulative interval curves for consecutive dose pairs
#'
#' The weighted percentage of all children who received both doses of a pair,
#' by the number of days between them. Ideally the curve is a step at the
#' scheduled interval.
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param pairs Doses (the later dose of each pair) to include; defaults to
#'   every dose with a `previous_dose`.
#' @param subset Optional logical expression on the child-level table.
#' @param max_days Upper limit of the interval axis.
#' @return A tibble of class `vcs_cic` with `pair`, `from`, `to`, `days`,
#'   `cum_pct`, `n_pairs`, and attribute `n_children`.
#' @export
#' @seealso [plot_cic()]
#' @examples
#' ci <- compute_cic(vcs_example, pairs = "PENTA2")
#' head(ci)
compute_cic <- function(x, pairs = NULL, subset = NULL, max_days = NULL) {
  assert_vcs_data(x)
  ch <- vcs_children(x)
  sub_q <- rlang::enquo(subset)
  if (!rlang::quo_is_null(sub_q)) {
    keep <- rlang::eval_tidy(sub_q, data = ch)
    keep[is.na(keep)] <- FALSE
    ch <- ch[keep, , drop = FALSE]
  }
  w <- child_weights(ch)
  w[is.na(w)] <- 0
  total_w <- sum(w)
  iv <- derive_dose_intervals(x)
  iv <- iv[iv$child_id %in% ch$child_id, , drop = FALSE]
  if (!is.null(pairs)) iv <- iv[iv$to %in% pairs, , drop = FALSE]
  iv <- iv[iv$interval_days >= 0, , drop = FALSE]
  max_days <- max_days %||% (if (nrow(iv)) max(iv$interval_days) else 365)
  grid <- seq(0, max_days, by = 1)
  wv <- w[match(iv$child_id, ch$child_id)]
  rows <- lapply(unique(iv$to), function(to) {
    sel <- iv$to == to
    d <- iv$interval_days[sel]
    ww <- wv[sel]
    tibble::tibble(pair = sprintf("%s & %s", iv$from[sel][1], to),
                   from = iv$from[sel][1], to = to,
                   scheduled_interval_days = iv$scheduled_interval_days[sel][1],
                   days = grid,
                   cum_pct = if (total_w > 0) 100 * vapply(grid, function(g) sum(ww[d <= g]), numeric(1)) / total_w else NA_real_,
                   n_pairs = sum(sel))
  })
  out <- dplyr::bind_rows(rows)
  attr(out, "n_children") <- nrow(ch)
  attr(out, "schedule") <- x$schedule
  class(out) <- c("vcs_cic", class(out))
  out
}

#' Plot a cumulative interval curve
#'
#' @param x A [compute_cic()] result.
#' @param pair The later dose of the pair to draw (e.g. `"PENTA2"`).
#' @param title,subtitle Titles.
#' @param palette A [vcs_palette()].
#' @return A ggplot.
#' @export
#' @examples
#' ci <- compute_cic(vcs_example)
#' if (requireNamespace("ggplot2", quietly = TRUE) && nrow(ci)) plot_cic(ci, ci$to[1])
plot_cic <- function(x, pair, title = NULL, subtitle = NULL, palette = vcs_palette()) {
  assert_installed("ggplot2", "plot_cic()")
  d <- tibble::as_tibble(x)
  d <- d[d$to == pair, , drop = FALSE]
  if (!nrow(d)) {
    vcs_abort(sprintf("No interval data for \"%s\".", pair), class = "vaxsurvR_value_error")
  }
  sched <- attr(x, "schedule")
  from <- d$from[1]
  sched_int <- d$scheduled_interval_days[1]
  age_diff <- sched$minimum_age_days[match(pair, sched$vaccine)] -
    sched$minimum_age_days[match(from, sched$vaccine)]
  marks <- c(sched_int, age_diff)
  marks <- marks[!is.na(marks)]
  title <- title %||% sprintf("%s & %s interval", from, pair)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$days, y = .data$cum_pct)) +
    ggplot2::geom_vline(xintercept = marks, linetype = c("dashed", "solid")[seq_along(marks)],
                        colour = palette$grey_dk, linewidth = 0.4) +
    ggplot2::geom_step(colour = "#1F3B73", linewidth = 0.7) +
    ggplot2::scale_x_continuous(breaks = unique(c(0, marks, max(d$days)))) +
    ggplot2::scale_y_continuous(limits = c(0, 100)) +
    ggplot2::labs(x = "Days between doses", y = "Cumulative weighted % vaccinated\n(according to card)",
                  title = title, subtitle = subtitle,
                  caption = sprintf(
                    "Vertical lines mark (1) the scheduled interval (dashed) and (2) the difference between the minimum ages of the two doses (solid).\nDenominator is all eligible respondents. %s of %s respondents had card dates for both doses.",
                    format(d$n_pairs[1], big.mark = ","),
                    format(attr(x, "n_children"), big.mark = ","))) +
    theme_vcs(palette = palette, grid = "xy")
  p
}

#' Organ-pipe plot of cluster-level coverage
#'
#' One column per cluster, sorted from highest to lowest coverage, with the
#' column width proportional to the sum of survey weights in the cluster and
#' the shaded height equal to the unweighted share of sampled children with
#' evidence of the dose. A dashed line traces the number of children per
#' cluster on the right axis. This is the diagnostic plot of the WHO 2018
#' reference manual for looking at heterogeneity across clusters.
#'
#' @param x A [vcs_data][new_vcs_data] object with coverage derived, or a
#'   [vcs_design()].
#' @param vaccine Dose to plot.
#' @param subset Optional logical expression on the child-level table (for a
#'   single stratum).
#' @param prefix Prefix of the coverage column.
#' @param title Title; defaults to the dose and domain.
#' @param palette A [vcs_palette()].
#' @param n_axis_max Upper limit of the right-hand axis.
#' @return A ggplot.
#' @export
#' @examples
#' d <- derive_vaccination_status(vcs_example)
#' if (requireNamespace("ggplot2", quietly = TRUE)) plot_organ_pipe(d, "PENTA1")
plot_organ_pipe <- function(x, vaccine, subset = NULL, prefix = "cov_",
                            title = NULL, palette = vcs_palette(), n_axis_max = NULL) {
  assert_installed("ggplot2", "plot_organ_pipe()")
  assert_string(vaccine)
  ch <- if (is_vcs_design(x)) x$data else {
    assert_vcs_data(x)
    vcs_children(x)
  }
  col <- paste0(prefix, vaccine)
  if (!col %in% names(ch)) {
    vcs_abort(sprintf("Column \"%s\" not found; derive coverage first.", col),
              class = "vaxsurvR_value_error")
  }
  sub_q <- rlang::enquo(subset)
  dom <- NULL
  if (!rlang::quo_is_null(sub_q)) {
    keep <- rlang::eval_tidy(sub_q, data = ch)
    keep[is.na(keep)] <- FALSE
    ch <- ch[keep, , drop = FALSE]
    dom <- rlang::as_label(sub_q)
  }
  if (!"psu" %in% names(ch)) {
    vcs_abort("No `psu` column in the child table.", class = "vaxsurvR_value_error")
  }
  y <- suppressWarnings(as.numeric(ch[[col]]))
  w <- child_weights(ch)
  w[is.na(w)] <- 0
  ok <- !is.na(y)
  cl <- dplyr::group_by(tibble::tibble(psu = as.character(ch$psu)[ok], y = y[ok], w = w[ok]), .data$psu)
  cl <- dplyr::summarise(cl, n = dplyr::n(), cov = 100 * mean(.data$y), sw = sum(.data$w), .groups = "drop")
  cl <- cl[order(-cl$cov, -cl$n), , drop = FALSE]
  cl$xmax <- cumsum(cl$sw) / sum(cl$sw) * 100
  cl$xmin <- cl$xmax - cl$sw / sum(cl$sw) * 100
  cl$xmid <- (cl$xmin + cl$xmax) / 2
  n_max <- n_axis_max %||% max(5, ceiling(max(cl$n) / 5) * 5)
  est <- 100 * sum(cl$cov / 100 * cl$sw) / sum(cl$sw)
  title <- title %||% paste0(vaccine, if (!is.null(dom)) paste0(" - ", gsub('.*== *"?([^"]*)"?$', "\\1", dom)) else "")
  step <- tibble::tibble(x = c(cl$xmin, cl$xmax[nrow(cl)]), n = c(cl$n, cl$n[nrow(cl)]))
  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(data = cl, ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                                               ymin = 0, ymax = .data$cov),
                       fill = "#D4D4D4", colour = "white", linewidth = 0.15) +
    ggplot2::geom_step(data = step, ggplot2::aes(x = .data$x, y = .data$n / n_max * 100),
                       colour = "grey20", linetype = "dashed", linewidth = 0.6) +
    ggplot2::scale_y_continuous(
      name = "Percent of cluster", limits = c(0, 100), breaks = c(0, 50, 100),
      sec.axis = ggplot2::sec_axis(~ . * n_max / 100, name = "Number of children (N)")
    ) +
    ggplot2::scale_x_continuous(expand = c(0, 0), breaks = NULL) +
    ggplot2::labs(x = NULL, title = title,
                  caption = sprintf("Estimated coverage = %.1f%%. Column width is proportional to the sum of survey weights in the cluster; the dashed line is the number of children sampled.", est)) +
    theme_vcs(palette = palette, grid = "none") +
    ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA, colour = "black"))
  p
}
