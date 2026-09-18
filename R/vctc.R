# ---------------------------------------------------------------------------
# Dose timing and the Vaccination Coverage and Timeliness Chart (VCTC).
#
# A VCTC (VCQI indicator RI_VCTC_01) shows, for every dose in the schedule,
# estimated crude coverage as a horizontal bar whose coloured segments say what
# share of the dose was given too early, on time, a little late or very late
# according to card dates, with a grey tip for doses whose timing is unknown
# (tick marks and caregiver recall). Point estimate, confidence interval,
# sample size, effective sample size, design effect and ICC are printed at
# the right of each bar.
# ---------------------------------------------------------------------------

#' Timing rules used to classify dated doses
#'
#' A dose received at age `a` days, due at `m` days, is classified as
#' too early (`a < m`), timely (`m <= a < m + timely_days`), a little late
#' (`m + timely_days <= a < m + late_days`) or very late (`a >= m + late_days`).
#' `special` overrides the windows for individual vaccines; the default gives
#' BCG the categories used in VCQI charts (by day 5, and after one year).
#'
#' @param timely_days Width of the timely window in days after the minimum age.
#' @param late_days Days after the minimum age at which a dose becomes
#'   "very late".
#' @param special A named list of per-vaccine overrides. Each element may set
#'   `timely_days`, `timely_label`, `very_late_days` and `very_late_label`.
#' @return A list of class `vcs_timing_rules`.
#' @export
#' @examples
#' vcs_timing_rules()
#' vcs_timing_rules(special = list())   # no BCG-specific categories
vcs_timing_rules <- function(timely_days = 28, late_days = 60,
                             special = list(BCG = list(
                               timely_days = 6, timely_label = "BCG by day 5",
                               very_late_days = 365,
                               very_late_label = "BCG after 1 year"))) {
  assert_number(timely_days, lower = 1)
  assert_number(late_days, lower = 1)
  if (late_days < timely_days) {
    vcs_abort("`late_days` must not be smaller than `timely_days`.",
              class = "vaxsurvR_value_error")
  }
  structure(list(timely_days = timely_days, late_days = late_days,
                 special = special %||% list()),
            class = "vcs_timing_rules")
}

#' Timing categories, in the order they stack on a VCTC bar
#'
#' @param rules A [vcs_timing_rules()].
#' @return A character vector of category labels.
#' @export
#' @examples
#' vcs_timing_levels()
vcs_timing_levels <- function(rules = vcs_timing_rules()) {
  sp <- rules$special
  extra_timely <- unique(unlist(lapply(sp, function(s) s$timely_label)))
  extra_late <- unique(unlist(lapply(sp, function(s) s$very_late_label)))
  c("Too early", extra_timely, sprintf("Timely (%d days)", as.integer(rules$timely_days)),
    "< 2 months late", "2+ months late", extra_late, "Timing unknown")
}

#' Classify every dose by timing
#'
#' Adds `age_at_vaccination_days` and `timing_category` to the long
#' vaccination table. A dose is classified only when the child has a date of
#' birth and the card carries a complete, logically usable date; every other
#' dose that counts as received under `evidence` is `"Timing unknown"`, and a
#' dose that does not count as received is `NA`.
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param evidence Definition of "received"; see [vcs_evidence_definitions()].
#' @param rules A [vcs_timing_rules()].
#' @param schedule A [vcs_schedule()]; defaults to the one stored in `x`.
#' @return `x`, with columns added to `x$vaccinations` and `vaccinated` set
#'   under `evidence`.
#' @export
#' @seealso [estimate_vctc()], [plot_vctc()]
#' @examples
#' d <- derive_dose_timing(vcs_example)
#' table(d$vaccinations$timing_category, useNA = "ifany")
derive_dose_timing <- function(x, evidence = "card_or_recall",
                               rules = vcs_timing_rules(), schedule = NULL) {
  assert_vcs_data(x)
  if (!inherits(rules, "vcs_timing_rules")) {
    vcs_abort("`rules` must come from `vcs_timing_rules()`.",
              class = "vaxsurvR_type_error")
  }
  schedule <- schedule %||% x$schedule
  if (is.null(schedule)) {
    vcs_abort("Supply `schedule`: `x` carries no schedule.", class = "vaxsurvR_value_error")
  }
  x <- derive_vaccination_status(x, evidence = evidence)
  vx <- vx_with_dates(x)
  n <- nrow(vx)
  levels <- vcs_timing_levels(rules)
  out <- rep(NA_character_, n)
  if (!n) {
    x$vaccinations$timing_category <- factor(out, levels = levels)
    return(x)
  }
  vacc <- !is.na(vx$vaccinated) & vx$vaccinated
  age <- if (all(c("child_dob", "card_date") %in% names(vx))) {
    derive_age_at_vaccination(vx$child_dob, vx$card_date)
  } else {
    rep(NA_integer_, n)
  }
  # Only a complete date can place a dose in time; a partial date is a tick.
  prec <- vx$card_date_precision %||% rep("day", n)
  usable <- !is.na(age) & age >= 0 & (is.na(prec) | prec %in% c("day", "complete"))
  card_yes <- if ("card_documented" %in% names(vx)) !is.na(vx$card_documented) & vx$card_documented else rep(FALSE, n)
  dated <- vacc & card_yes & usable

  idx <- match(vx$vaccine, schedule$vaccine)
  m <- schedule$minimum_age_days[idx]
  timely_lab <- sprintf("Timely (%d days)", as.integer(rules$timely_days))
  t_days <- rep(rules$timely_days, n)
  t_lab <- rep(timely_lab, n)
  vl_days <- rep(NA_real_, n)
  vl_lab <- rep(NA_character_, n)
  for (nm in names(rules$special)) {
    s <- rules$special[[nm]]
    hit <- vx$vaccine == nm | vx$antigen == nm
    if (!is.null(s$timely_days)) t_days[hit] <- s$timely_days
    if (!is.null(s$timely_label)) t_lab[hit] <- s$timely_label
    if (!is.null(s$very_late_days)) vl_days[hit] <- s$very_late_days
    if (!is.null(s$very_late_label)) vl_lab[hit] <- s$very_late_label
  }
  out[vacc] <- "Timing unknown"
  d <- dated & !is.na(m)
  out[d & age < m] <- "Too early"
  out[d & age >= m & age < m + t_days] <- t_lab[d & age >= m & age < m + t_days]
  out[d & age >= m + t_days & age < m + rules$late_days] <- "< 2 months late"
  out[d & age >= m + rules$late_days] <- "2+ months late"
  vl <- d & !is.na(vl_days) & age >= vl_days
  out[vl] <- vl_lab[vl]
  # A schedule without a minimum age cannot classify; treat as unknown.
  out[dated & is.na(m)] <- "Timing unknown"

  x$vaccinations$age_at_vaccination_days <- mark_derived(
    age, "days between child_dob and card_date"
  )
  x$vaccinations$timing_category <- mark_derived(
    factor(out, levels = levels),
    sprintf("timing vs schedule minimum age: timely < %d days, very late >= %d days; undated doses = Timing unknown",
            as.integer(rules$timely_days), as.integer(rules$late_days))
  )
  x$meta$timing_rules <- rules
  x
}

#' Age of each child at interview, in days
#'
#' From `interview_date - child_dob` where both exist, otherwise from
#' `age_months` at 30.4375 days per month.
#' @noRd
child_age_days <- function(ch) {
  n <- nrow(ch)
  out <- rep(NA_real_, n)
  if (all(c("child_dob", "interview_date") %in% names(ch))) {
    out <- as.numeric(as_date_safe(ch$interview_date) - as_date_safe(ch$child_dob))
  }
  if ("age_months" %in% names(ch)) {
    am <- suppressWarnings(as.numeric(ch$age_months))
    out[is.na(out) & !is.na(am)] <- am[is.na(out) & !is.na(am)] * 30.4375
  }
  out
}

#' Rebuild a design over new child data using the specification of another
#' @noRd
design_like <- function(x, design = NULL, design_args = list()) {
  if (!is.null(design)) {
    assert_design(design)
    sp <- design$spec
    args <- list(ids = sp$ids, strata = if (length(sp$strata)) sp$strata else NULL,
                 weights = if (isTRUE(sp$weighted)) sp$weights else NULL,
                 fpc = if (length(sp$fpc)) sp$fpc else NULL,
                 nest = sp$nest, lonely_psu = sp$lonely_psu)
    args <- utils::modifyList(args, design_args)
    args <- args[!vapply(args, is.null, logical(1))]
    return(suppressWarnings(do.call(vcs_design, c(list(x), args))))
  }
  suppressWarnings(do.call(vcs_design, c(list(x), design_args)))
}

#' Weighted share of each level of a factor, over a design
#' @noRd
svy_shares <- function(des, variable, levels) {
  fml <- stats::as.formula(paste0("~", variable))
  rule <- options(survey.lonely.psu = des$spec$lonely_psu %||% "adjust")
  on.exit(options(rule), add = TRUE)
  est <- try_quiet(survey::svymean(fml, des$design, na.rm = TRUE))
  out <- stats::setNames(rep(0, length(levels)), levels)
  if (is.null(est)) {
    return(out)
  }
  nm <- sub(paste0("^", variable), "", names(est))
  vals <- as.numeric(est)
  out[intersect(nm, levels)] <- vals[match(intersect(nm, levels), nm)]
  out
}

#' Estimate a Vaccination Coverage and Timeliness Chart
#'
#' Computes everything a VCTC displays: crude coverage of every dose with a
#' confidence interval, sample size, effective sample size, design effect and
#' ICC; the weighted share of each timing category; the proportion of
#' children who showed a home-based record; and the fully-vaccinated and
#' not-vaccinated proportions for the footnote.
#'
#' Crude coverage uses every child in the denominator (a child with no
#' evidence counts as not vaccinated), which is the convention of published
#' coverage tables and of VCQI's RI_COVG_01. Doses due after the youngest age
#' in the survey window are estimated among age-eligible children only when
#' `age_eligible = TRUE`.
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param design Optional [vcs_design()] whose specification (clusters,
#'   strata, weights) is reused. Otherwise `design_args` are passed to
#'   [vcs_design()].
#' @param vaccines Doses to chart; defaults to the schedule order.
#' @param evidence Definition of "received"; see [vcs_evidence_definitions()].
#' @param subset An optional logical expression on the child-level table
#'   selecting one domain, e.g. `health_zone == "Boma"`.
#' @param fully_vaccinated Doses that define "fully vaccinated" for the
#'   footnote. Defaults to [schedule_final_doses()].
#' @param rules A [vcs_timing_rules()].
#' @param age_eligible Restrict each dose's denominator to children whose age
#'   at interview is at least the dose's minimum age.
#' @param ci_method `"wilson"` (default here, as in VCQI) or `"normal"`.
#' @param level Confidence level.
#' @param design_args Passed to [vcs_design()] when `design` is `NULL`.
#' @return An object of class `vcs_vctc`: a list with `coverage` (one row
#'   per dose), `timing` (long, one row per dose and category), `card_seen`,
#'   `fully`, `not_vaccinated`, `n_children`, `schedule`, `evidence` and
#'   `domain`.
#' @export
#' @seealso [plot_vctc()], [derive_dose_timing()]
#' @examples
#' v <- estimate_vctc(vcs_example)
#' v$coverage[, c("vaccine", "estimate", "conf_low", "conf_high", "deff")]
estimate_vctc <- function(x, design = NULL, vaccines = NULL,
                          evidence = "card_or_recall", subset = NULL,
                          fully_vaccinated = NULL, rules = vcs_timing_rules(),
                          age_eligible = TRUE, ci_method = "wilson",
                          level = 0.95, design_args = list()) {
  assert_vcs_data(x)
  ci_method <- match.arg(ci_method, c("wilson", "normal"))
  assert_flag(age_eligible)
  sched <- x$schedule
  if (is.null(sched)) {
    vcs_abort("`x` carries no schedule.", class = "vaxsurvR_value_error")
  }
  vaccines <- vaccines %||% intersect(sched$vaccine, unique(x$vaccinations$vaccine))
  fully_vaccinated <- fully_vaccinated %||% intersect(schedule_final_doses(sched), vaccines)
  if (!length(fully_vaccinated)) fully_vaccinated <- vaccines
  levels <- vcs_timing_levels(rules)

  x <- derive_dose_timing(x, evidence = evidence, rules = rules)
  x <- derive_vaccination_status(x, evidence = evidence, vaccines = vaccines,
                                 missing_as_unvaccinated = TRUE, prefix = "vctc_")
  vx <- x$vaccinations
  ch <- vcs_children(x)
  age_days <- child_age_days(ch)

  # Per-child timing factor for every dose, "Not vaccinated" as an explicit
  # level so that shares are over the same denominator as coverage.
  tlev <- c(levels, "Not vaccinated")
  for (v in vaccines) {
    sel <- vx$vaccine == v
    idx <- match(ch$child_id, vx$child_id[sel])
    tc <- as.character(vx$timing_category[sel][idx])
    cov <- ch[[paste0("vctc_", v)]]
    tc[is.na(tc) & !is.na(cov) & cov == 0] <- "Not vaccinated"
    tc[is.na(tc) & !is.na(cov) & cov == 1] <- "Timing unknown"
    if (age_eligible) {
      m <- sched$minimum_age_days[match(v, sched$vaccine)]
      too_young <- !is.na(m) & !is.na(age_days) & age_days < m
      tc[too_young] <- NA_character_
      cov[too_young] <- NA_integer_
    }
    ch[[paste0("tim_", v)]] <- factor(tc, levels = tlev)
    ch[[paste0("vctc_", v)]] <- cov
  }
  fv <- derive_fully_vaccinated(x, vaccines = fully_vaccinated, evidence = evidence,
                                name = ".fully")$children$.fully
  # Fully vaccinated with an "all children" denominator; unknown = not fully.
  ch$.fully <- as.integer(!is.na(fv) & fv == 1L)
  covm <- as.matrix(ch[, paste0("vctc_", fully_vaccinated), drop = FALSE])
  ch$.none <- as.integer(rowSums(covm == 1, na.rm = TRUE) == 0)
  ch$.card <- as.integer(!is.na(ch$card_seen) & ch$card_seen)

  x$children <- ch
  des <- design_like(x, design, design_args)

  domain_label <- "<overall>"
  sub_q <- rlang::enquo(subset)
  if (!rlang::quo_is_null(sub_q)) {
    keep <- rlang::eval_tidy(sub_q, data = des$data)
    keep[is.na(keep)] <- FALSE
    if (!any(keep)) {
      vcs_abort("`subset` selects no children.", class = "vaxsurvR_value_error")
    }
    des$design <- des$design[keep, ]
    des$data <- des$data[keep, , drop = FALSE]
    domain_label <- rlang::as_label(sub_q)
  }

  cov <- estimate_coverage(des, vaccines = vaccines, prefix = "vctc_",
                           evidence = evidence, ci_method = ci_method,
                           level = level)
  cov <- tibble::as_tibble(cov)
  cov$vaccine <- cov$indicator
  cov <- cov[, c("vaccine", "estimate", "conf_low", "conf_high", "numerator",
                 "denominator", "n_unweighted", "deff", "neff", "icc", "se"), drop = FALSE]

  timing <- dplyr::bind_rows(lapply(vaccines, function(v) {
    sh <- svy_shares(des, paste0("tim_", v), tlev)
    tibble::tibble(vaccine = v, category = factor(names(sh), levels = tlev),
                   share = as.numeric(sh))
  }))
  timing <- timing[timing$category != "Not vaccinated", , drop = FALSE]

  card <- estimate_indicator(des, ".card", type = "card_availability",
                             ci_method = ci_method, level = level)
  fully <- estimate_indicator(des, ".fully", type = "fully_vaccinated",
                              ci_method = ci_method, level = level)
  none <- estimate_indicator(des, ".none", type = "not_vaccinated",
                             ci_method = ci_method, level = level)
  pick <- function(e) c(estimate = e$estimate[1], conf_low = e$conf_low[1],
                        conf_high = e$conf_high[1], n = e$denominator[1])

  structure(
    list(
      coverage = cov,
      timing = timing,
      card_seen = pick(card),
      fully = pick(fully),
      not_vaccinated = pick(none),
      fully_vaccinated_doses = fully_vaccinated,
      n_children = nrow(des$data),
      schedule = sched[sched$vaccine %in% vaccines, , drop = FALSE],
      evidence = evidence,
      rules = rules,
      domain = domain_label,
      weighted = des$spec$weighted,
      ci_method = ci_method,
      level = level
    ),
    class = "vcs_vctc"
  )
}

#' @export
print.vcs_vctc <- function(x, ...) {
  cat(sprintf("<vcs_vctc: %d dose(s), %d children, domain %s>\n",
              nrow(x$coverage), x$n_children, x$domain))
  cat(sprintf("  evidence : %s | CI: %s%s\n", x$evidence, x$ci_method,
              if (!isTRUE(x$weighted)) " | UNWEIGHTED" else ""))
  cat(sprintf("  HBR seen : %.1f%%   fully vaccinated: %.1f%%   not vaccinated: %.1f%%\n",
              100 * x$card_seen[["estimate"]], 100 * x$fully[["estimate"]],
              100 * x$not_vaccinated[["estimate"]]))
  print(x$coverage[, c("vaccine", "estimate", "conf_low", "conf_high",
                       "n_unweighted", "neff", "deff", "icc")], ...)
  invisible(x)
}

#' Convert a VCTC to a flat table
#'
#' One row per dose with coverage, its interval, sample sizes and the share of
#' each timing category as columns -- the table behind the chart.
#'
#' @param x A [vcs_vctc][estimate_vctc] object.
#' @param ... Unused.
#' @return A tibble.
#' @export
as.data.frame.vcs_vctc <- function(x, ...) {
  wide <- stats::reshape(as.data.frame(x$timing), idvar = "vaccine",
                         timevar = "category", direction = "wide")
  names(wide) <- sub("^share\\.", "", names(wide))
  out <- dplyr::left_join(x$coverage, tibble::as_tibble(wide), by = "vaccine")
  out$card_seen <- x$card_seen[["estimate"]]
  out
}

#' Vertical positions for VCTC bars: schedule order, grouped by due age
#' @noRd
vctc_positions <- function(schedule, vaccines, gap = 0.7) {
  s <- schedule[match(vaccines, schedule$vaccine), , drop = FALSE]
  s$order <- seq_len(nrow(s))
  age <- ifelse(is.na(s$minimum_age_days), Inf, s$minimum_age_days)
  ord <- order(age, s$order)
  s <- s[ord, , drop = FALSE]
  age <- age[ord]
  y <- numeric(nrow(s))
  pos <- 0
  for (i in seq_len(nrow(s))) {
    if (i > 1L && age[i] != age[i - 1L]) pos <- pos + gap
    pos <- pos + 1
    y[i] <- pos
  }
  tibble::tibble(vaccine = s$vaccine, y = y, due = age)
}

#' Plot a Vaccination Coverage and Timeliness Chart
#'
#' Draws the chart estimated by [estimate_vctc()]: one horizontal bar per
#' dose, ordered by due age from the bottom up and grouped by age, coloured by
#' timing category, with the confidence interval as an error bar, a dashed
#' line at the proportion who showed a home-based record, and the summary
#' statistics printed at the right.
#'
#' @param x A [vcs_vctc][estimate_vctc] object.
#' @param title,subtitle Chart titles.
#' @param palette A [vcs_palette()]; `palette$timing` colours the segments.
#' @param show_stats Print the statistics columns at the right.
#' @param show_ci Draw the confidence interval.
#' @param show_hbr Draw the home-based record line.
#' @param footnote Add the fully-vaccinated and abbreviations footnote.
#' @param base_size Base font size.
#' @param labels Optional named vector of display labels for doses.
#' @return A ggplot.
#' @export
#' @examples
#' v <- estimate_vctc(vcs_example)
#' if (requireNamespace("ggplot2", quietly = TRUE)) plot_vctc(v)
plot_vctc <- function(x, title = "Vaccination coverage and timeliness",
                      subtitle = NULL, palette = vcs_palette(),
                      show_stats = TRUE, show_ci = TRUE, show_hbr = TRUE,
                      footnote = TRUE, base_size = 10, labels = NULL) {
  assert_installed("ggplot2", "plot_vctc()")
  if (!inherits(x, "vcs_vctc")) {
    vcs_abort("`x` must be a `vcs_vctc` from `estimate_vctc()`.",
              class = "vaxsurvR_type_error")
  }
  vaccines <- x$coverage$vaccine
  pos <- vctc_positions(x$schedule, vaccines)
  cov <- dplyr::left_join(x$coverage, pos, by = "vaccine")
  levels <- vcs_timing_levels(x$rules)
  tim <- dplyr::left_join(x$timing, pos, by = "vaccine")
  tim$category <- factor(as.character(tim$category), levels = levels)
  tim <- tim[order(tim$y, tim$category), , drop = FALSE]
  tim <- dplyr::group_by(tim, .data$vaccine)
  tim <- dplyr::mutate(tim, xmax = cumsum(.data$share) * 100,
                       xmin = .data$xmax - .data$share * 100)
  tim <- dplyr::ungroup(tim)
  tim <- tim[tim$share > 0, , drop = FALSE]

  cols <- palette$timing
  missing_cols <- setdiff(levels, names(cols))
  if (length(missing_cols)) {
    cols <- c(cols, stats::setNames(rep(palette$grey, length(missing_cols)), missing_cols))
  }
  lab <- if (is.null(labels)) vaccines else ifelse(vaccines %in% names(labels), labels[vaccines], vaccines)
  names(lab) <- vaccines
  half <- 0.42
  fmt1 <- function(v) ifelse(is.na(v), "", sprintf("%.1f", 100 * v))
  fmtn <- function(v) ifelse(is.na(v), "", format(round(v), big.mark = ","))

  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = tim,
      ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                   ymin = .data$y - half, ymax = .data$y + half,
                   fill = .data$category),
      colour = "grey35", linewidth = 0.15
    ) +
    ggplot2::scale_fill_manual(values = cols, breaks = levels, drop = FALSE,
                               name = NULL) +
    ggplot2::scale_y_continuous(breaks = cov$y, labels = lab[cov$vaccine],
                                expand = ggplot2::expansion(add = c(0.9, 1.6))) +
    ggplot2::scale_x_continuous(breaks = seq(0, 100, 20), expand = c(0, 0)) +
    ggplot2::labs(x = "Estimated coverage (%)", y = NULL, title = title,
                  subtitle = subtitle) +
    theme_vcs(base_size = base_size, palette = palette, grid = "none") +
    ggplot2::theme(legend.position = "bottom",
                   legend.key.size = grid::unit(1, "lines"),
                   axis.line.y = ggplot2::element_line(colour = palette$grey_dk),
                   plot.margin = ggplot2::margin(6, 12, 6, 6)) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow = TRUE))

  if (show_ci) {
    p <- p + ggplot2::geom_errorbarh(
      data = cov,
      ggplot2::aes(xmin = 100 * .data$conf_low, xmax = 100 * .data$conf_high,
                   y = .data$y),
      height = 0.35, colour = "grey30", linewidth = 0.35
    )
  }
  ytop <- max(cov$y) + 1.3
  if (show_hbr) {
    hbr <- 100 * x$card_seen[["estimate"]]
    p <- p + ggplot2::annotate("segment", x = hbr, xend = hbr, y = 0.3,
                               yend = ytop - 0.3, linetype = "dashed",
                               colour = "grey45", linewidth = 0.5) +
      ggplot2::annotate("text", x = hbr + 1, y = 0.05, hjust = 0, vjust = 0.5,
                        size = base_size / ggplot2::.pt * 0.85, colour = "grey30",
                        label = sprintf("<-- Showed HBR (%.1f%%)", hbr),
                        family = vcs_font())
  }
  xmax <- 100
  if (show_stats) {
    stat_x <- c(118, 136, 152, 165, 177, 189)
    hdr <- c("Coverage (%)", "95% CI", "N", "NEFF", "DEFF", "ICC")
    vals <- list(
      fmt1(cov$estimate),
      ifelse(is.na(cov$conf_low), "",
             sprintf("(%.1f, %.1f)", 100 * cov$conf_low, 100 * cov$conf_high)),
      fmtn(cov$denominator), fmtn(cov$neff),
      ifelse(is.na(cov$deff), "", sprintf("%.1f", cov$deff)),
      ifelse(is.na(cov$icc), "", sprintf("%.3f", cov$icc))
    )
    for (i in seq_along(stat_x)) {
      p <- p + ggplot2::annotate("text", x = stat_x[i], y = cov$y, label = vals[[i]],
                                 size = base_size / ggplot2::.pt * 0.85, hjust = 0.5,
                                 family = vcs_font(), colour = palette$ink) +
        ggplot2::annotate("text", x = stat_x[i], y = ytop, label = hdr[i],
                          size = base_size / ggplot2::.pt * 0.85, hjust = 0.5,
                          family = vcs_font(), colour = palette$ink)
    }
    xmax <- 198
  }
  p <- p + ggplot2::coord_cartesian(xlim = c(0, xmax), clip = "off")
  if (footnote) {
    cap <- sprintf(
      paste0("Fully vaccinated: %.1f%% (95%% CI: %.1f - %.1f%%). Fully vaccinated dose list: %s.\n",
             "Not vaccinated: %.1f%% (95%% CI: %.1f - %.1f%%). Not vaccinated means the child did not receive any of the doses from the fully vaccinated dose list.\n",
             "Abbreviations: HBR - home-based record; CI - confidence interval; N - sample size; NEFF - effective sample size; DEFF - design effect; ICC - intracluster correlation coefficient.%s"),
      100 * x$fully[["estimate"]], 100 * x$fully[["conf_low"]], 100 * x$fully[["conf_high"]],
      paste(x$fully_vaccinated_doses, collapse = " "),
      100 * x$not_vaccinated[["estimate"]], 100 * x$not_vaccinated[["conf_low"]],
      100 * x$not_vaccinated[["conf_high"]],
      if (!isTRUE(x$weighted)) "\nUnweighted: equal weights were used, so these describe the sample, not the population." else ""
    )
    p <- p + ggplot2::labs(caption = cap)
  }
  p
}
