# ---------------------------------------------------------------------------
# Missed opportunities for simultaneous vaccination (MOSV).
#
# A documented vaccination visit is a date on which the card shows the child
# received at least one dose. At that visit the child was eligible for every
# dose in the schedule whose minimum age had been reached, whose previous
# dose (if any) had already been given on an earlier date, and which the
# child had not yet received. Each such eligible-but-not-given dose is one
# MOSV (VCQI RI_QUAL_08 counts them per visit, RI_QUAL_09 per child).
#
# Only card-dated doses can be placed at a visit. Crude measures: early doses
# are accepted as received, and every dose is treated as valid.
# ---------------------------------------------------------------------------

#' Reconstruct documented vaccination visits and dose eligibility
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param vaccines Doses for which MOSVs are assessed. Defaults to every dose
#'   in the schedule; the South Sudan report excluded PCV and rotavirus,
#'   which were too new in the schedule to be informative.
#' @param schedule A [vcs_schedule()]; defaults to the one stored in `x`.
#' @return An object of class `vcs_mosv`: a list of tibbles `visit_dose`
#'   (one row per visit and assessed dose), `visits` (one row per visit),
#'   `child_dose` (one row per child and dose, among children with at least
#'   one eligible visit) and `child` (one row per child with an MOSV
#'   summary), each carrying the child-level columns of `x`.
#' @export
#' @seealso [estimate_mosv_visits()], [estimate_mosv_children()],
#'   [plot_mosv_children()]
#' @examples
#' m <- derive_mosv(vcs_example)
#' table(m$child_dose$status)
derive_mosv <- function(x, vaccines = NULL, schedule = NULL) {
  assert_vcs_data(x)
  schedule <- schedule %||% x$schedule
  if (is.null(schedule)) {
    vcs_abort("Supply `schedule`.", class = "vaxsurvR_value_error")
  }
  vaccines <- vaccines %||% intersect(schedule$vaccine, unique(x$vaccinations$vaccine))
  ch <- vcs_children(x)
  vx <- vx_with_dates(x)
  empty <- structure(list(visit_dose = tibble::tibble(), visits = tibble::tibble(),
                          child_dose = tibble::tibble(), child = tibble::tibble(),
                          vaccines = vaccines), class = "vcs_mosv")
  if (!nrow(vx) || !all(c("card_date", "child_dob") %in% names(vx))) {
    return(empty)
  }
  prec <- vx$card_date_precision %||% rep("day", nrow(vx))
  card_yes <- if ("card_documented" %in% names(vx)) !is.na(vx$card_documented) & vx$card_documented else !is.na(vx$card_date)
  usable <- card_yes & !is.na(vx$card_date) & !is.na(vx$child_dob) &
    (is.na(prec) | prec %in% c("day", "complete")) & vx$card_date >= vx$child_dob
  dated <- vx[usable, c("child_id", "vaccine", "card_date", "child_dob"), drop = FALSE]
  if (!nrow(dated)) {
    return(empty)
  }
  # A dose documented without a usable date is still "received" for the
  # correction question, at an unknown time.
  undated_yes <- vx[card_yes & !usable, c("child_id", "vaccine"), drop = FALSE]

  # visits: distinct (child, date) with at least one dated dose
  visits <- dplyr::distinct(dated[, c("child_id", "card_date", "child_dob")])
  names(visits)[2] <- "visit_date"
  visits$age_days <- as.numeric(visits$visit_date - visits$child_dob)
  visits <- visits[order(visits$child_id, visits$visit_date), , drop = FALSE]
  visits$visit_no <- stats::ave(seq_len(nrow(visits)), visits$child_id, FUN = seq_along)

  date_of <- function(child, v) {
    key <- paste(child, v)
    d <- stats::setNames(dated$card_date, paste(dated$child_id, dated$vaccine))
    out <- d[key]
    as.Date(unname(out), origin = "1970-01-01")
  }
  s <- schedule[match(vaccines, schedule$vaccine), , drop = FALSE]
  rows <- lapply(seq_len(nrow(s)), function(i) {
    v <- s$vaccine[i]
    m <- s$minimum_age_days[i]
    prev <- s$previous_dose[i]
    rec <- date_of(visits$child_id, v)
    prev_date <- if (!is.na(prev)) date_of(visits$child_id, prev) else as.Date(rep(NA, nrow(visits)))
    received_here <- !is.na(rec) & rec == visits$visit_date
    received_before <- !is.na(rec) & rec < visits$visit_date
    age_ok <- if (is.na(m)) rep(TRUE, nrow(visits)) else visits$age_days >= m
    prev_ok <- if (is.na(prev)) rep(TRUE, nrow(visits)) else !is.na(prev_date) & prev_date < visits$visit_date
    eligible <- age_ok & prev_ok & !received_before
    tibble::tibble(
      child_id = visits$child_id, visit_date = visits$visit_date,
      visit_no = visits$visit_no, age_days = visits$age_days, vaccine = v,
      eligible = eligible, received_here = received_here,
      received_before = received_before,
      mosv = eligible & !received_here,
      received_date = rec
    )
  })
  vd <- dplyr::bind_rows(rows)

  vs <- dplyr::group_by(vd, .data$child_id, .data$visit_date, .data$visit_no, .data$age_days)
  vs <- dplyr::summarise(vs, n_doses_received = sum(.data$received_here),
                         n_eligible = sum(.data$eligible), n_mosv = sum(.data$mosv),
                         any_mosv = any(.data$mosv), .groups = "drop")
  # A visit is a documented vaccination visit only if some dose was given;
  # every (child, date) here comes from a dated dose, so this always holds,
  # but doses outside `vaccines` may be the only ones given that day.
  vs$n_doses_received <- pmax(vs$n_doses_received, 1L)

  # per child and dose
  cd <- vd[vd$eligible | vd$received_here, , drop = FALSE]
  cd <- dplyr::group_by(cd, .data$child_id, .data$vaccine)
  cd <- dplyr::summarise(
    cd,
    n_eligible_visits = sum(.data$eligible),
    n_mosv = sum(.data$mosv),
    first_eligible_date = suppressWarnings(min(.data$visit_date[.data$eligible])),
    first_mosv_date = suppressWarnings(min(.data$visit_date[.data$mosv])),
    received_date = .data$received_date[1],
    .groups = "drop"
  )
  cd <- cd[cd$n_eligible_visits > 0, , drop = FALSE]
  cd$first_eligible_date <- as.Date(ifelse(is.finite(cd$first_eligible_date), cd$first_eligible_date, NA), origin = "1970-01-01")
  cd$first_mosv_date <- as.Date(ifelse(is.finite(cd$first_mosv_date), cd$first_mosv_date, NA), origin = "1970-01-01")
  undated_key <- paste(undated_yes$child_id, undated_yes$vaccine)
  cd$received_undated <- paste(cd$child_id, cd$vaccine) %in% undated_key
  cd$status <- ifelse(
    cd$n_mosv == 0, "first_opportunity",
    ifelse(!is.na(cd$received_date) & cd$received_date > cd$first_mosv_date, "corrected",
           ifelse(cd$received_undated, "corrected", "uncorrected"))
  )
  cd$days_to_correction <- ifelse(cd$status == "corrected" & !is.na(cd$received_date),
                                  as.numeric(cd$received_date - cd$first_mosv_date), NA_real_)

  # per child summary: NM / AC / SC / NC over assessed doses
  cs <- dplyr::group_by(cd, .data$child_id)
  cs <- dplyr::summarise(
    cs,
    n_doses_assessed = dplyr::n(),
    n_mosv_doses = sum(.data$status != "first_opportunity"),
    n_corrected = sum(.data$status == "corrected"),
    n_uncorrected = sum(.data$status == "uncorrected"),
    .groups = "drop"
  )
  cs$mosv_summary <- ifelse(
    cs$n_mosv_doses == 0, "NM",
    ifelse(cs$n_uncorrected == 0, "AC", ifelse(cs$n_corrected == 0, "NC", "SC"))
  )

  join_child <- function(tab) {
    extra <- setdiff(names(ch), names(tab))
    dplyr::left_join(tab, ch[, c("child_id", extra), drop = FALSE], by = "child_id")
  }
  structure(
    list(visit_dose = join_child(vd), visits = join_child(vs),
         child_dose = join_child(cd), child = join_child(cs),
         vaccines = vaccines, schedule = s),
    class = "vcs_mosv"
  )
}

#' @export
print.vcs_mosv <- function(x, ...) {
  cat("<vcs_mosv>\n")
  cat(sprintf("  visits      : %d documented vaccination visits, %d children\n",
              nrow(x$visits), length(unique(x$visits$child_id))))
  cat(sprintf("  doses       : %s\n", paste(x$vaccines, collapse = ", ")))
  if (nrow(x$visits)) {
    cat(sprintf("  visits with >= 1 MOSV: %.1f%%; MOSVs per visit: %.2f\n",
                100 * mean(x$visits$any_mosv), mean(x$visits$n_mosv)))
  }
  if (nrow(x$child)) {
    print(round(100 * prop.table(table(factor(x$child$mosv_summary, c("NM", "AC", "SC", "NC")))), 1))
  }
  invisible(x)
}

#' @noRd
assert_mosv <- function(x) {
  if (!inherits(x, "vcs_mosv")) {
    vcs_abort("`x` must be a `vcs_mosv` from `derive_mosv()`.", class = "vaxsurvR_type_error")
  }
  invisible(x)
}

#' Percent of vaccination visits with a missed opportunity
#'
#' For each assessed dose, the share of documented visits at which the child
#' was eligible for the dose but did not receive it (VCQI RI_QUAL_08), plus
#' the share of visits with at least one MOSV for any dose and the mean
#' number of MOSVs per visit. Unweighted; `N` counts visits.
#'
#' @param x A [vcs_mosv][derive_mosv] object, or a [vcs_data][new_vcs_data]
#'   object (then [derive_mosv()] is called with `vaccines`).
#' @param by Domains over the child-level columns.
#' @param vaccines Doses; defaults to those in `x`.
#' @return A tibble, one row per domain, with `mosv_<dose>` and `n_<dose>`
#'   columns for every dose, plus `mosv_any`, `n_visits`, `mosv_per_visit`.
#' @export
#' @examples
#' estimate_mosv_visits(vcs_example)
estimate_mosv_visits <- function(x, by = NULL, vaccines = NULL) {
  if (is_vcs_data(x)) x <- derive_mosv(x, vaccines = vaccines)
  assert_mosv(x)
  by <- as_column_names(by)
  vaccines <- vaccines %||% x$vaccines
  vd <- x$visit_dose
  vs <- x$visits
  if (!nrow(vd)) {
    return(tibble::tibble())
  }
  key <- function(tab) if (length(by)) do.call(paste, c(lapply(by, function(v) as.character(tab[[v]])), sep = " | ")) else rep("<overall>", nrow(tab))
  vd$.key <- key(vd)
  vs$.key <- key(vs)
  groups <- unique(vs$.key)
  rows <- lapply(groups, function(g) {
    r <- tibble::tibble(domain = g)
    for (i in seq_along(by)) {
      r[[by[i]]] <- vapply(strsplit(g, " | ", fixed = TRUE), `[`, character(1), i)
    }
    for (v in vaccines) {
      sel <- vd$.key == g & vd$vaccine == v & vd$eligible
      r[[paste0("mosv_", v)]] <- if (any(sel)) mean(vd$mosv[sel]) else NA_real_
      r[[paste0("n_", v)]] <- sum(sel)
    }
    sv <- vs$.key == g
    r$mosv_any <- mean(vs$any_mosv[sv])
    r$mosv_per_visit <- mean(vs$n_mosv[sv])
    r$n_visits <- sum(sv)
    r
  })
  out <- dplyr::bind_rows(rows)
  out
}

#' Children with missed opportunities, by dose
#'
#' Among children with at least one documented visit at which they were
#' eligible for the dose: the share vaccinated at the first eligible
#' opportunity, the share with an MOSV later corrected, and the share with an
#' uncorrected MOSV (VCQI RI_QUAL_09). Also the child-level summary: no MOSVs
#' (NM), all corrected (AC), some corrected (SC), none corrected (NC).
#'
#' @inheritParams estimate_mosv_visits
#' @return A list with `by_dose` (one row per domain and dose: `n`,
#'   `first_opportunity`, `corrected`, `uncorrected`) and `summary` (one row
#'   per domain: `n`, `NM`, `AC`, `SC`, `NC`).
#' @export
#' @examples
#' estimate_mosv_children(vcs_example)$summary
estimate_mosv_children <- function(x, by = NULL, vaccines = NULL) {
  if (is_vcs_data(x)) x <- derive_mosv(x, vaccines = vaccines)
  assert_mosv(x)
  by <- as_column_names(by)
  vaccines <- vaccines %||% x$vaccines
  cd <- x$child_dose
  cs <- x$child
  if (!nrow(cd)) {
    return(list(by_dose = tibble::tibble(), summary = tibble::tibble()))
  }
  key <- function(tab) if (length(by)) do.call(paste, c(lapply(by, function(v) as.character(tab[[v]])), sep = " | ")) else rep("<overall>", nrow(tab))
  cd$.key <- key(cd)
  cs$.key <- key(cs)
  groups <- unique(cs$.key)
  split_key <- function(r, g) {
    for (i in seq_along(by)) r[[by[i]]] <- vapply(strsplit(g, " | ", fixed = TRUE), `[`, character(1), i)
    r
  }
  by_dose <- dplyr::bind_rows(lapply(groups, function(g) {
    dplyr::bind_rows(lapply(vaccines, function(v) {
      sel <- cd$.key == g & cd$vaccine == v
      st <- cd$status[sel]
      r <- tibble::tibble(domain = g, vaccine = v, n = sum(sel),
                          first_opportunity = if (any(sel)) mean(st == "first_opportunity") else NA_real_,
                          corrected = if (any(sel)) mean(st == "corrected") else NA_real_,
                          uncorrected = if (any(sel)) mean(st == "uncorrected") else NA_real_)
      split_key(r, g)
    }))
  }))
  summary <- dplyr::bind_rows(lapply(groups, function(g) {
    sel <- cs$.key == g
    ms <- factor(cs$mosv_summary[sel], levels = c("NM", "AC", "SC", "NC"))
    pr <- prop.table(table(ms))
    r <- tibble::tibble(domain = g, n = sum(sel), NM = pr[["NM"]], AC = pr[["AC"]],
                        SC = pr[["SC"]], NC = pr[["NC"]])
    split_key(r, g)
  }))
  list(by_dose = by_dose, summary = summary)
}

#' Time from first MOSV to correction
#'
#' @inheritParams estimate_mosv_visits
#' @return A tibble with one row per corrected child-dose: the domain columns,
#'   `vaccine`, `days_to_correction`.
#' @export
#' @examples
#' head(mosv_time_to_correction(vcs_example))
mosv_time_to_correction <- function(x, by = NULL, vaccines = NULL) {
  if (is_vcs_data(x)) x <- derive_mosv(x, vaccines = vaccines)
  assert_mosv(x)
  by <- as_column_names(by)
  vaccines <- vaccines %||% x$vaccines
  cd <- x$child_dose
  cd <- cd[cd$status == "corrected" & !is.na(cd$days_to_correction) & cd$vaccine %in% vaccines,
           c("child_id", "vaccine", "days_to_correction", by), drop = FALSE]
  cd
}

#' Plot children with missed opportunities, by stratum and dose
#'
#' The block chart of the South Sudan report: one row per stratum, one column
#' per dose, each block a horizontal 0--100% bar split into vaccinated at the
#' first eligible opportunity, MOSV later corrected and MOSV uncorrected.
#' Blocks with fewer than `suppress_n` children are drawn in faded colours,
#' and the four summary columns (NM, AC, SC, NC) are printed at the right.
#'
#' @param x A [vcs_mosv][derive_mosv] object or a [vcs_data][new_vcs_data].
#' @param by Stratifier column (one), or `NULL` for the whole sample.
#' @param vaccines Doses (columns).
#' @param suppress_n Blocks with fewer children are faded.
#' @param show_n Print the sample size on every block.
#' @param palette A [vcs_palette()]; `palette$mosv` gives the three colours.
#' @param stratum_labels Optional named vector relabelling strata.
#' @param base_size Font size.
#' @return A ggplot.
#' @export
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot_mosv_children(vcs_example, by = "stratum")
#' }
plot_mosv_children <- function(x, by = NULL, vaccines = NULL, suppress_n = 25,
                               show_n = FALSE, palette = vcs_palette(),
                               stratum_labels = NULL, base_size = 9) {
  assert_installed("ggplot2", "plot_mosv_children()")
  if (is_vcs_data(x)) x <- derive_mosv(x, vaccines = vaccines)
  assert_mosv(x)
  by <- as_column_names(by)
  if (length(by) > 1L) {
    vcs_abort("`by` must name at most one column.", class = "vaxsurvR_value_error")
  }
  vaccines <- vaccines %||% x$vaccines
  est <- estimate_mosv_children(x, by = by, vaccines = vaccines)
  bd <- est$by_dose
  sm <- est$summary
  if (!nrow(bd)) {
    vcs_abort("No MOSV data to plot.", class = "vaxsurvR_value_error")
  }
  strata <- unique(bd$domain)
  if (!length(by)) strata <- "<overall>"
  lab <- if (is.null(stratum_labels)) stats::setNames(strata, strata) else stratum_labels
  lab[strata == "<overall>"] <- "Entire study area"
  bd$row <- match(bd$domain, strata)
  bd$col <- match(bd$vaccine, vaccines)
  long <- tidyr_pivot(bd)
  long$faded <- long$n < suppress_n
  long$part <- factor(long$part, levels = c("first_opportunity", "corrected", "uncorrected"),
                      labels = names(palette$mosv))
  long <- long[!is.na(long$value), , drop = FALSE]
  long <- long[order(long$row, long$col, long$part), , drop = FALSE]
  long <- dplyr::group_by(long, .data$row, .data$col)
  long <- dplyr::mutate(long, xmax = cumsum(.data$value) * 100, xmin = .data$xmax - .data$value * 100)
  long <- dplyr::ungroup(long)
  n_col <- length(vaccines)
  n_row <- length(strata)
  block_w <- 100
  gap <- 22
  long$x0 <- (long$col - 1) * (block_w + gap)
  long$y0 <- -(long$row - 1) * 1.6
  long$fill <- as.character(palette$mosv[as.character(long$part)])
  faded <- grDevices::adjustcolor(long$fill, alpha.f = 0.35)
  long$fill_final <- ifelse(long$faded, faded, long$fill)
  fam <- vcs_font()
  txt <- base_size / ggplot2::.pt
  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(data = long,
                       ggplot2::aes(xmin = .data$x0 + .data$xmin, xmax = .data$x0 + .data$xmax,
                                    ymin = .data$y0 - 0.5, ymax = .data$y0 + 0.5,
                                    fill = .data$part, alpha = .data$faded),
                       colour = "white", linewidth = 0.2) +
    ggplot2::scale_fill_manual(values = palette$mosv, name = NULL) +
    ggplot2::scale_alpha_manual(values = c(`FALSE` = 1, `TRUE` = 0.3), guide = "none") +
    ggplot2::annotate("text", x = (seq_len(n_col) - 1) * (block_w + gap) + block_w / 2,
                      y = 0.95, label = vaccines, size = txt, family = fam, fontface = "bold") +
    ggplot2::annotate("text", x = -8, y = -(seq_len(n_row) - 1) * 1.6, label = lab[strata],
                      hjust = 1, size = txt, family = fam)
  # axis ticks 0 and 100 under each block
  p <- p + ggplot2::annotate("text", x = c((seq_len(n_col) - 1) * (block_w + gap),
                                           (seq_len(n_col) - 1) * (block_w + gap) + block_w),
                             y = -(n_row - 1) * 1.6 - 0.85, label = rep(c("0", "100"), each = n_col),
                             size = txt * 0.85, family = fam, colour = palette$grey_dk)
  if (show_n) {
    nn <- unique(bd[, c("row", "col", "n")])
    p <- p + ggplot2::geom_label(data = nn,
                                 ggplot2::aes(x = (.data$col - 1) * (block_w + gap) + 20,
                                              y = -(.data$row - 1) * 1.6, label = .data$n),
                                 size = txt * 0.85, family = fam, fill = "white", alpha = 0.8,
                                 label.size = 0, label.padding = grid::unit(0.1, "lines"))
  }
  # summary columns at the right
  sm$row <- match(sm$domain, strata)
  sx <- n_col * (block_w + gap) + c(15, 65, 115, 165)
  for (i in seq_along(c("NM", "AC", "SC", "NC"))) {
    nm <- c("NM", "AC", "SC", "NC")[i]
    p <- p + ggplot2::annotate("text", x = sx[i], y = 0.95, label = nm, size = txt,
                               family = fam, fontface = "bold") +
      ggplot2::geom_text(data = sm, ggplot2::aes(x = sx[i], y = -(.data$row - 1) * 1.6,
                                                 label = sprintf("%.1f", 100 * .data[[nm]])),
                         size = txt, family = fam)
  }
  p <- p +
    ggplot2::coord_cartesian(xlim = c(-60, n_col * (block_w + gap) + 190),
                             ylim = c(-(n_row - 1) * 1.6 - 1.2, 1.4), clip = "off") +
    ggplot2::labs(caption = sprintf(
      paste0("Crude measures of MOSV shown; only children with dated doses on a home-based record and at least one documented visit when eligible for the dose are included.\n",
             "Blocks with fewer than %d children are faded.%s\n",
             "MOSV summary by stratum, %% of children with: NM = no MOSVs; AC = all MOSVs corrected; SC = some corrected; NC = none corrected. Percentages in each row sum to 100%%.\n",
             "Doses assessed: %s."),
      suppress_n, if (show_n) " Sample size is shown on each bar." else "",
      paste(vaccines, collapse = ", "))) +
    ggplot2::theme_void(base_size = base_size, base_family = fam) +
    ggplot2::theme(legend.position = "bottom",
                   plot.caption = ggplot2::element_text(hjust = 0, colour = palette$grey_dk,
                                                        size = base_size * 0.85),
                   plot.caption.position = "plot",
                   plot.background = ggplot2::element_rect(fill = "white", colour = NA),
                   plot.margin = ggplot2::margin(6, 6, 6, 6))
  attr(p, "vcs_size") <- c(width = 1.3 + n_col * 0.95 + 2.2, height = 0.9 + n_row * 0.55 + 1.1)
  p
}

#' Minimal long pivot of the three MOSV proportions
#' @noRd
tidyr_pivot <- function(bd) {
  parts <- c("first_opportunity", "corrected", "uncorrected")
  dplyr::bind_rows(lapply(parts, function(pn) {
    tibble::tibble(domain = bd$domain, vaccine = bd$vaccine, row = bd$row, col = bd$col,
                   n = bd$n, part = pn, value = bd[[pn]])
  }))
}

#' Plot time to MOSV correction
#'
#' Small multiples, one per dose: the cumulative share of corrected MOSVs by
#' the number of days between the first MOSV and receipt of the dose, with the
#' median marked in red. Panels with fewer than `suppress_n` corrections are
#' faded.
#'
#' @param x A [vcs_mosv][derive_mosv] object or a [vcs_data][new_vcs_data].
#' @param by Optional stratifier column (one): one row of panels per stratum.
#' @param vaccines Doses.
#' @param suppress_n Faded below this many corrected MOSVs.
#' @param max_days Upper limit of the x axis.
#' @param palette A [vcs_palette()].
#' @param base_size Font size.
#' @return A ggplot.
#' @export
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) plot_mosv_correction(vcs_example)
plot_mosv_correction <- function(x, by = NULL, vaccines = NULL, suppress_n = 25,
                                 max_days = 150, palette = vcs_palette(), base_size = 8) {
  assert_installed("ggplot2", "plot_mosv_correction()")
  if (is_vcs_data(x)) x <- derive_mosv(x, vaccines = vaccines)
  assert_mosv(x)
  by <- as_column_names(by)
  vaccines <- vaccines %||% x$vaccines
  tt <- mosv_time_to_correction(x, by = by, vaccines = vaccines)
  tt$stratum <- if (length(by)) as.character(tt[[by[1]]]) else "Entire study area"
  grid <- seq(0, max_days, by = 1)
  strata <- unique(tt$stratum)
  if (!length(strata)) strata <- "Entire study area"
  curves <- dplyr::bind_rows(lapply(strata, function(s) {
    dplyr::bind_rows(lapply(vaccines, function(v) {
      d <- tt$days_to_correction[tt$stratum == s & tt$vaccine == v]
      n_corr <- length(d)
      tibble::tibble(stratum = s, vaccine = factor(v, levels = vaccines), days = grid,
                     cum = if (n_corr) 100 * vapply(grid, function(g) mean(d <= g), numeric(1)) else NA_real_,
                     n = n_corr, median = if (n_corr) stats::median(d) else NA_real_)
    }))
  }))
  curves$faded <- curves$n < suppress_n
  meds <- unique(curves[, c("stratum", "vaccine", "n", "median", "faded")])
  meds <- meds[!is.na(meds$median), , drop = FALSE]
  fam <- vcs_font()
  p <- ggplot2::ggplot(curves[!is.na(curves$cum), , drop = FALSE],
                       ggplot2::aes(x = .data$days, y = .data$cum, alpha = .data$faded)) +
    ggplot2::geom_step(colour = "#1F3B73", linewidth = 0.5) +
    ggplot2::geom_vline(data = meds, ggplot2::aes(xintercept = pmin(.data$median, max_days),
                                                  alpha = .data$faded),
                        colour = "red", linewidth = 0.5) +
    ggplot2::geom_text(data = meds, ggplot2::aes(x = pmin(.data$median, max_days) + 4, y = 12,
                                                 label = round(.data$median), alpha = .data$faded),
                       colour = "red", hjust = 0, size = base_size / ggplot2::.pt, family = fam) +
    ggplot2::scale_alpha_manual(values = c(`FALSE` = 1, `TRUE` = 0.25), guide = "none") +
    ggplot2::facet_grid(rows = ggplot2::vars(.data$stratum), cols = ggplot2::vars(.data$vaccine),
                        drop = FALSE) +
    ggplot2::scale_y_continuous(breaks = c(0, 100), labels = c("0%", "100%"), limits = c(0, 100)) +
    ggplot2::scale_x_continuous(breaks = c(0, 50, 100, 150), limits = c(0, max_days)) +
    ggplot2::labs(x = "Days between first MOSV and receiving dose", y = NULL,
                  caption = sprintf("Red vertical lines and numerals indicate the median (50th percentile). Crude measures of MOSV shown; panels with fewer than %d corrected MOSVs are faded.", suppress_n)) +
    theme_vcs(base_size = base_size, palette = palette, grid = "y") +
    ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0, hjust = 0),
                   panel.spacing = grid::unit(0.5, "lines"),
                   panel.border = ggplot2::element_rect(fill = NA, colour = palette$grey_lt),
                   axis.text = ggplot2::element_text(size = base_size * 0.8))
  attr(p, "vcs_size") <- c(width = 1.5 + length(vaccines) * 0.85, height = 1 + length(strata) * 1.05)
  p
}
