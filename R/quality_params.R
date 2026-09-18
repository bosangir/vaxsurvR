# ---------------------------------------------------------------------------
# Date data quality, observed sample-size planning parameters, and survey
# weights following Annex J of the WHO 2018 Vaccination Coverage Cluster
# Survey Reference Manual.
# ---------------------------------------------------------------------------

#' Summarise logical problems in card dates
#'
#' Counts, over every dated card dose, the problems VCQI's data-quality
#' report lists: a date before the earliest possible vaccination date for
#' the age window, a date after the interview, doses of a series out of order,
#' the same date on two doses of a series, and a dated later dose with an
#' undated earlier dose. A dose can have more than one problem; the last row
#' counts distinct affected dates.
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param min_age_months Youngest eligible age; sets the earliest possible
#'   vaccination date as `interview_date - (max_age_months + 1) * 30.4375`.
#' @param max_age_months Oldest eligible age.
#' @param schedule A [vcs_schedule()].
#' @return A tibble with `problem`, `n_dates`, `prop_of_dates`, and the
#'   attribute `n_dates_total`.
#' @export
#' @examples
#' date_quality_summary(vcs_example)
date_quality_summary <- function(x, min_age_months = 12, max_age_months = 23,
                                 schedule = NULL) {
  assert_vcs_data(x)
  schedule <- schedule %||% x$schedule
  vx <- vx_with_dates(x)
  if (!nrow(vx) || !"card_date" %in% names(vx)) {
    return(tibble::tibble(problem = character(0), n_dates = integer(0), prop_of_dates = numeric(0)))
  }
  prec <- vx$card_date_precision %||% rep("day", nrow(vx))
  dated <- !is.na(vx$card_date) & (is.na(prec) | prec %in% c("day", "complete"))
  n_total <- sum(dated)
  d <- vx$card_date
  key <- paste(vx$child_id, vx$vaccine)
  date_of <- stats::setNames(d, key)
  date_of[!dated] <- NA

  # 1. before earliest possible date (child cannot be older than max age)
  earliest <- if ("interview_date" %in% names(vx)) {
    as_date_safe(vx$interview_date) - round((max_age_months + 1) * 30.4375)
  } else {
    as.Date(rep(NA, nrow(vx)))
  }
  if ("child_dob" %in% names(vx)) {
    dob <- as_date_safe(vx$child_dob)
    earliest <- as.Date(ifelse(!is.na(dob), pmax(dob, earliest, na.rm = TRUE), earliest), origin = "1970-01-01")
  }
  p1 <- dated & !is.na(earliest) & d < earliest
  # 2. after interview
  p2 <- if ("interview_date" %in% names(vx)) dated & !is.na(vx$interview_date) & d > as_date_safe(vx$interview_date) else rep(FALSE, nrow(vx))
  # 3/4/5 series problems
  p3 <- p4 <- p5 <- rep(FALSE, nrow(vx))
  if (!is.null(schedule)) {
    prev <- schedule$previous_dose[match(vx$vaccine, schedule$vaccine)]
    has_prev <- !is.na(prev)
    pd <- as.Date(rep(NA, nrow(vx)))
    pd[has_prev] <- as.Date(unname(date_of[paste(vx$child_id[has_prev], prev[has_prev])]), origin = "1970-01-01")
    p3 <- dated & has_prev & !is.na(pd) & d < pd
    p4 <- dated & has_prev & !is.na(pd) & d == pd
    p5 <- dated & has_prev & is.na(pd)
    # the earlier dose of an out-of-order / same-date pair is affected too
    for (i in which(p3 | p4)) {
      j <- which(key == paste(vx$child_id[i], prev[i]))
      if (length(j)) { if (p3[i]) p3[j] <- TRUE else p4[j] <- TRUE }
    }
  }
  any_p <- p1 | p2 | p3 | p4 | p5
  out <- tibble::tibble(
    problem = c(
      sprintf("Vaccination date is before the earliest possible vaccination date for a child %d-%d months", min_age_months, max_age_months),
      "Vaccination date is after the date of the interview",
      "Vaccination dates in a dose series are out of order",
      "The same date is given for two or more doses in a dose series",
      "Date present for a later dose in a series but missing for earlier dose(s) in that same series",
      "Total dates with at least one problem (to be replaced with tick-mark evidence)"
    ),
    n_dates = c(sum(p1), sum(p2), sum(p3), sum(p4), sum(p5), sum(any_p))
  )
  out$prop_of_dates <- if (n_total > 0) out$n_dates / n_total else NA_real_
  attr(out, "n_dates_total") <- n_total
  attr(out, "flag") <- any_p
  out
}

#' Replace logically impossible card dates with tick-mark evidence
#'
#' The VCQI convention: a dated dose whose date fails a logical check keeps
#' its "documented on the card" status but loses the date, so that it counts
#' toward crude coverage but not toward timeliness, intervals, curves or
#' MOSVs. Every change is written to the audit trail.
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param max_passes Erasing a date can expose a new problem (a later dose
#'   now dated with its earlier dose undated), so the checks are re-run until
#'   nothing is flagged or this many passes have been made.
#' @param ... Passed to [date_quality_summary()].
#' @return `x` with offending `card_date` set to `NA` and
#'   `card_date_precision` set to `"tick"`. `x$meta$date_quality` holds the
#'   before-review counts with an `n_dates_after` column, and
#'   `x$meta$date_erasures` the audit log of every erased date.
#' @export
#' @examples
#' d <- erase_illogical_dates(vcs_example)
#' d$meta$date_quality
erase_illogical_dates <- function(x, max_passes = 5L, ...) {
  assert_vcs_data(x)
  first <- date_quality_summary(x, ...)
  logs <- list()
  for (pass in seq_len(max_passes)) {
    summ <- if (pass == 1L) first else date_quality_summary(x, ...)
    flag <- attr(summ, "flag")
    if (is.null(flag) || !any(flag)) break
    vx <- x$vaccinations
    changed <- vx[flag, , drop = FALSE]
    if (!"card_date_raw" %in% names(vx)) vx$card_date_raw <- as.character(vx$card_date)
    vx$card_date[flag] <- as.Date(NA)
    if ("card_date_precision" %in% names(vx)) vx$card_date_precision[flag] <- "tick"
    x$vaccinations <- vx
    logs[[pass]] <- tibble::tibble(
      record_id = paste(changed$child_id, changed$vaccine),
      variable = "card_date",
      original = as.character(changed$card_date),
      new = NA_character_,
      rule = "VCQI_DATE_LOGIC",
      action = sprintf("erase date, keep tick (pass %d)", pass),
      time = vcs_timestamp()
    )
  }
  log <- dplyr::bind_rows(logs)
  after <- date_quality_summary(x, ...)
  first$n_dates_after <- after$n_dates
  first$n_erased_total <- c(rep(NA_integer_, nrow(first) - 1L), nrow(log))
  x$meta$date_erasures <- log
  x$meta$date_quality <- first
  x
}

#' Observed values of sample-size planning parameters
#'
#' The quantities a sample-size calculation assumes -- coverage of the
#' marker dose, respondents per cluster, its ICC and the coefficient of
#' variation of the weights -- as observed in the achieved sample.
#'
#' @param design A [vcs_design()] with coverage columns.
#' @param vaccine Marker dose (usually the first DTP-containing dose).
#' @param assumed Optional named list of the values assumed at the design
#'   stage, `list(coverage = 0.6, per_cluster = 7, icc = 0.333, cv_weights = 0.5)`.
#' @param prefix Coverage column prefix.
#' @return A tibble with `parameter`, `assumed`, `observed`.
#' @export
#' @examples
#' des <- vcs_design(derive_vaccination_status(vcs_example))
#' sample_size_parameters(des, "PENTA1", assumed = list(coverage = 0.6, per_cluster = 7,
#'                        icc = 0.33, cv_weights = 0.5))
sample_size_parameters <- function(design, vaccine, assumed = NULL, prefix = "cov_") {
  assert_design(design)
  col <- paste0(prefix, vaccine)
  est <- estimate_coverage(design, vaccines = vaccine, prefix = prefix)
  tab <- design$data
  psu <- design$spec$ids[1]
  per_cluster <- if (psu %in% names(tab)) nrow(tab) / length(unique(tab[[psu]])) else NA_real_
  w <- suppressWarnings(as.numeric(tab[[design$spec$weights[1]]]))
  cv <- if (isTRUE(design$spec$weighted) && sum(!is.na(w)) > 1) stats::sd(w, na.rm = TRUE) / mean(w, na.rm = TRUE) else NA_real_
  assumed <- assumed %||% list()
  tibble::tibble(
    parameter = c(sprintf("%s coverage", vaccine), "Average number of respondents per cluster",
                  sprintf("Intracluster correlation coefficient for %s", vaccine),
                  "Coefficient of variation of survey weights", "Design effect", "Effective sample size"),
    assumed = c(assumed$coverage %||% NA_real_, assumed$per_cluster %||% NA_real_,
                assumed$icc %||% NA_real_, assumed$cv_weights %||% NA_real_,
                assumed$deff %||% NA_real_, assumed$neff %||% NA_real_),
    observed = c(est$estimate[1], per_cluster, est$icc[1], cv, est$deff[1], est$neff[1])
  )
}

#' Compute survey weights for a compact-segment cluster design
#'
#' Implements the three steps of Annex J of the WHO 2018 reference manual as
#' applied in these surveys:
#'
#' 1. Base weight `1 / (P1 * P2 * P3)`, with `P1` the probability the PSU was
#'    selected (`n_selected * psu_population / stratum_population`), `P2` the
#'    share of the PSU's population in the canvassed segments and `P3 = 1`
#'    because every eligible child in a selected household is interviewed.
#' 2. A non-response adjustment inflating the base weight by the ratio of the
#'    estimated eligible children in the canvassed segments to the children
#'    interviewed, where households with no one at home are assigned the
#'    average number of eligible children of responding households.
#' 3. Post-stratification so the weights in each stratum sum in proportion
#'    to the stratum population, then rescaling to the sample size.
#'
#' @param frame A data frame with one row per sampled PSU: `psu`, `stratum`,
#'   `psu_population`, `stratum_population`, `n_selected` (PSUs selected in
#'   the stratum), `segment_population_canvassed`, `segment_population_total`.
#' @param households A data frame with one row per visited household: `psu`,
#'   `outcome` (`"interviewed"`, `"respondent_not_interviewed"`,
#'   `"nobody_home"`, `"ineligible"`), `n_children` (eligible children found;
#'   `NA` when nobody was home).
#' @param children A data frame with one row per interviewed child: `psu`
#'   and `child_id`.
#' @param rescale_to `"sample"` (weights sum to the number of children) or
#'   `"population"` (weights sum to the stratum populations).
#' @return `children` with `base_weight`, `nr_factor`, `adjusted_weight`,
#'   `poststrat_weight` and `weight` added, plus a `psu_table` attribute
#'   documenting every component.
#' @export
#' @examples
#' frame <- data.frame(psu = c("A", "B"), stratum = "S", psu_population = c(500, 1000),
#'                     stratum_population = 6000, n_selected = 2,
#'                     segment_population_canvassed = c(100, 250),
#'                     segment_population_total = c(500, 1000))
#' hh <- data.frame(psu = c("A", "A", "A", "B", "B"),
#'                  outcome = c("interviewed", "nobody_home", "ineligible", "interviewed", "interviewed"),
#'                  n_children = c(1, NA, 0, 2, 1))
#' kids <- data.frame(psu = c("A", "B", "B", "B"), child_id = 1:4)
#' compute_survey_weights(frame, hh, kids)
compute_survey_weights <- function(frame, households, children,
                                   rescale_to = c("sample", "population")) {
  assert_data(frame); assert_data(households); assert_data(children)
  rescale_to <- match.arg(rescale_to)
  assert_columns(frame, c("psu", "stratum", "psu_population", "stratum_population",
                          "n_selected", "segment_population_canvassed",
                          "segment_population_total"))
  assert_columns(households, c("psu", "outcome", "n_children"))
  assert_columns(children, c("psu", "child_id"))
  f <- tibble::as_tibble(frame)
  f$psu <- as.character(f$psu)
  f$p1 <- f$n_selected * f$psu_population / f$stratum_population
  f$p2 <- f$segment_population_canvassed / f$segment_population_total
  f$p3 <- 1
  f$base_weight <- 1 / (f$p1 * f$p2 * f$p3)

  hh <- tibble::as_tibble(households)
  hh$psu <- as.character(hh$psu)
  kids <- tibble::as_tibble(children)
  kids$psu <- as.character(kids$psu)
  per_psu <- lapply(split(hh, hh$psu), function(h) {
    resp <- h$outcome %in% c("interviewed", "respondent_not_interviewed", "ineligible")
    n_visited_resp <- sum(resp)
    kids_found <- sum(h$n_children[resp], na.rm = TRUE)
    avg_kids <- if (n_visited_resp > 0) kids_found / n_visited_resp else 0
    n_nobody <- sum(h$outcome == "nobody_home")
    tibble::tibble(psu = h$psu[1],
                   n_hh_visited = nrow(h),
                   n_children_found = kids_found,
                   n_nobody_home = n_nobody,
                   est_children_nobody_home = n_nobody * avg_kids,
                   total_children_est = kids_found + n_nobody * avg_kids)
  })
  per_psu <- dplyr::bind_rows(per_psu)
  interviewed <- dplyr::count(kids, .data$psu, name = "n_interviewed")
  per_psu <- dplyr::left_join(per_psu, interviewed, by = "psu")
  per_psu$n_interviewed[is.na(per_psu$n_interviewed)] <- 0L
  per_psu$nr_factor <- ifelse(per_psu$n_interviewed > 0,
                              per_psu$total_children_est / per_psu$n_interviewed, NA_real_)
  f <- dplyr::left_join(f, per_psu, by = "psu")
  f$adjusted_weight <- f$base_weight * f$nr_factor

  kids <- dplyr::left_join(kids, f[, c("psu", "stratum", "stratum_population", "base_weight",
                                       "nr_factor", "adjusted_weight")], by = "psu")
  missing <- is.na(kids$adjusted_weight)
  if (any(missing)) {
    vcs_warn(sprintf("%d child(ren) in PSUs absent from the frame have no weight.", sum(missing)),
             class = "vaxsurvR_missing_weight")
  }
  # post-stratify to stratum population
  sums <- tapply(kids$adjusted_weight, kids$stratum, sum, na.rm = TRUE)
  kids$poststrat_weight <- kids$stratum_population * kids$adjusted_weight / sums[kids$stratum]
  total <- sum(kids$poststrat_weight, na.rm = TRUE)
  kids$weight <- if (rescale_to == "sample") {
    sum(!is.na(kids$poststrat_weight)) * kids$poststrat_weight / total
  } else {
    kids$poststrat_weight
  }
  attr(kids, "psu_table") <- f
  kids
}
