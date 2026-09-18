# ---------------------------------------------------------------------------
# Coverage estimation. All variance estimation is delegated to the survey
# package; this module handles domains, tidy output and the indicator
# definitions specific to immunisation programmes.
# ---------------------------------------------------------------------------

#' Construct an estimate table
#' @noRd
new_vcs_estimate <- function(x, meta = list()) {
  x <- tibble::as_tibble(x)
  class(x) <- c("vcs_estimate", class(x))
  attr(x, "vcs_meta") <- utils::modifyList(
    list(created = vcs_timestamp(), vaxsurvR_version = vcs_version()), meta
  )
  x
}

#' Test whether an object is a vcs_estimate
#'
#' @param x An object.
#' @return A logical scalar.
#' @export
#' @examples
#' is_vcs_estimate(estimate_coverage(
#'   vcs_design(derive_vaccination_status(vcs_example)), vaccines = "BCG"
#' ))
is_vcs_estimate <- function(x) inherits(x, "vcs_estimate")

#' @export
print.vcs_estimate <- function(x, ...) {
  meta <- attr(x, "vcs_meta")
  cat(sprintf("<vcs_estimate: %d row(s)>\n", nrow(x)))
  if (!is.null(meta$evidence)) {
    cat(sprintf("  evidence : %s\n", meta$evidence))
  }
  if (!is.null(meta$weighted) && !isTRUE(meta$weighted)) {
    cat("  WARNING  : unweighted design - these are not population estimates\n")
  }
  print(tibble::as_tibble(x), ...)
  invisible(x)
}

#' Estimate one weighted proportion over a domain
#' @noRd
svy_one <- function(design, variable, by = character(0), level = 0.95,
                    na.rm = TRUE, deff = TRUE,
                    ci_method = getOption("vaxsurvR.ci_method", "normal")) {
  ci_method <- match.arg(ci_method, c("normal", "wilson"))
  fml <- stats::as.formula(paste0("~", variable))
  des <- design$design
  raw <- design$data
  z <- stats::qnorm(1 - (1 - level) / 2)
  rule <- options(survey.lonely.psu = design$spec$lonely_psu %||% "adjust")
  on.exit(options(rule), add = TRUE)

  # `deff = "replace"` compares against simple random sampling *with*
  # replacement. `deff = TRUE` compares against SRS without replacement, using
  # a population size taken from the sum of the weights -- so when weights are
  # normalised to the sample size, as they commonly are, the reference variance
  # collapses to zero and every design effect comes back as Inf.
  deff_type <- if (isTRUE(deff)) "replace" else deff
  if (!length(by)) {
    est <- try_quiet(survey::svymean(fml, des, na.rm = na.rm, deff = deff_type))
    if (is.null(est)) {
      return(empty_estimate_row(variable, "<overall>", raw[[variable]]))
    }
    out <- tibble::tibble(
      domain = "<overall>",
      estimate = as.numeric(est)[1],
      se = as.numeric(survey::SE(est))[1],
      deff = if (deff) suppressWarnings(as.numeric(survey::deff(est))[1]) else NA_real_
    )
  } else {
    bfml <- stats::as.formula(paste("~", paste(by, collapse = " + ")))
    est <- try_quiet(survey::svyby(fml, bfml, des, survey::svymean,
                                   na.rm = na.rm, deff = deff_type,
                                   vartype = "se"))
    if (is.null(est)) {
      return(empty_estimate_row(variable, NA_character_, raw[[variable]]))
    }
    est <- as.data.frame(est)
    dom <- do.call(paste, c(lapply(by, function(v) as.character(est[[v]])),
                            sep = " | "))
    se_col <- grep("^se", names(est), value = TRUE)[1]
    deff_col <- grep("^DEff|^deff", names(est), value = TRUE)[1]
    out <- tibble::tibble(
      domain = dom,
      estimate = as.numeric(est[[variable]]),
      se = as.numeric(est[[se_col]]),
      deff = if (!is.na(deff_col)) as.numeric(est[[deff_col]]) else NA_real_
    )
    for (v in by) {
      out[[v]] <- as.character(est[[v]])
    }
  }

  # Unweighted counts come from the design data, not from the survey object,
  # so that the numerator and denominator reported are the ones a reader can
  # recount by hand.
  key <- if (length(by)) {
    do.call(paste, c(lapply(by, function(v) as.character(raw[[v]])), sep = " | "))
  } else {
    rep("<overall>", nrow(raw))
  }
  vals <- suppressWarnings(as.numeric(raw[[variable]]))
  counts <- vapply(out$domain, function(g) {
    sel <- key == g & !is.na(vals)
    c(sum(vals[sel] > 0), sum(sel))
  }, numeric(2))
  out$numerator <- unname(counts[1, ])
  out$denominator <- unname(counts[2, ])
  out$n_unweighted <- unname(counts[2, ])

  # An empty denominator has no estimate, whatever the variance machinery
  # returned for it.
  empty <- out$denominator == 0
  if (any(empty)) {
    out$estimate[empty] <- NA_real_
    out$se[empty] <- NA_real_
    out$deff[empty] <- NA_real_
  }

  # Effective sample size and intracluster correlation. NEFF is the sample
  # size a simple random sample would need to reach the same variance; ICC is
  # the ANOVA estimator over the first-stage clusters, unweighted.
  out$neff <- ifelse(is.na(out$deff) | out$deff <= 0, NA_real_,
                     out$denominator / out$deff)
  psu_col <- design$spec$ids[1]
  out$icc <- vapply(out$domain, function(g) {
    sel <- key == g & !is.na(vals)
    if (is.null(psu_col) || !psu_col %in% names(raw) || sum(sel) < 2L) {
      return(NA_real_)
    }
    icc_anova(vals[sel] > 0, as.character(raw[[psu_col]])[sel])
  }, numeric(1))

  if (ci_method == "wilson") {
    # Survey-modified Wilson interval (Dean & Pagano 2015), the interval the
    # WHO 2018 reference manual recommends: a Wilson interval evaluated at the
    # design-effect-adjusted sample size.
    wil <- vapply(seq_len(nrow(out)), function(i) {
      p <- out$estimate[i]
      n_eff <- if (is.na(out$deff[i]) || !is.finite(out$deff[i]) || out$deff[i] <= 0) {
        out$denominator[i]
      } else {
        out$denominator[i] / out$deff[i]
      }
      if (is.na(p) || is.na(n_eff) || n_eff <= 0) {
        return(c(lower = NA_real_, upper = NA_real_))
      }
      wilson_ci(p * n_eff, n_eff, level)
    }, numeric(2))
    lower <- unname(wil[1, ])
    upper <- unname(wil[2, ])
  } else {
    # Normal-approximation limits, clipped to [0, 1]; a Wilson interval takes
    # over where the design gives no usable standard error.
    lower <- pmax(0, out$estimate - z * out$se)
    upper <- pmin(1, out$estimate + z * out$se)
    degenerate <- is.na(out$se) | out$se == 0
    if (any(degenerate)) {
      wil <- vapply(which(degenerate), function(i) {
        wilson_ci(out$numerator[i], out$denominator[i], level)
      }, numeric(2))
      lower[degenerate] <- wil[1, ]
      upper[degenerate] <- wil[2, ]
    }
  }
  out$conf_low <- unname(lower)
  out$conf_high <- unname(upper)
  out$ci_method <- ci_method
  out$indicator <- variable
  out
}

#' ANOVA estimator of the intracluster correlation coefficient
#'
#' `rho = (MSB - MSW) / (MSB + (m0 - 1) MSW)` with `m0` the adjusted mean
#' cluster size, on unweighted 0/1 outcomes. Negative estimates are floored
#' at zero, as VCQI does.
#' @noRd
icc_anova <- function(y, cluster) {
  y <- as.numeric(y)
  ok <- !is.na(y) & !is.na(cluster)
  y <- y[ok]
  cluster <- cluster[ok]
  k <- length(unique(cluster))
  n <- length(y)
  if (k < 2L || n <= k) {
    return(NA_real_)
  }
  m <- tapply(y, cluster, length)
  means <- tapply(y, cluster, mean)
  grand <- mean(y)
  ssb <- sum(m * (means - grand)^2)
  ssw <- sum((y - means[cluster])^2)
  msb <- ssb / (k - 1)
  msw <- ssw / (n - k)
  m0 <- (n - sum(m^2) / n) / (k - 1)
  denom <- msb + (m0 - 1) * msw
  if (!is.finite(denom) || denom <= 0) {
    return(NA_real_)
  }
  max(0, (msb - msw) / denom)
}

#' @noRd
try_quiet <- function(expr) {
  suppressWarnings(tryCatch(expr, error = function(e) NULL))
}

#' @noRd
empty_estimate_row <- function(variable, domain, values) {
  vals <- suppressWarnings(as.numeric(values))
  tibble::tibble(
    domain = domain,
    estimate = NA_real_, se = NA_real_, deff = NA_real_,
    numerator = sum(vals > 0, na.rm = TRUE),
    denominator = sum(!is.na(vals)),
    n_unweighted = sum(!is.na(vals)),
    neff = NA_real_, icc = NA_real_,
    conf_low = NA_real_, conf_high = NA_real_,
    ci_method = NA_character_,
    indicator = variable
  )
}

#' @noRd
tidy_estimate <- function(rows, by, extra = list()) {
  out <- dplyr::bind_rows(rows)
  if (!nrow(out)) {
    return(out)
  }
  front <- c("indicator", "domain", by, "numerator", "denominator", "estimate",
             "se", "conf_low", "conf_high", "n_unweighted", "deff", "neff", "icc")
  front <- intersect(unique(front), names(out))
  out <- out[, c(front, setdiff(names(out), front)), drop = FALSE]
  for (nm in names(extra)) {
    out[[nm]] <- extra[[nm]]
  }
  out
}

#' Estimate weighted coverage
#'
#' Produces design-based coverage estimates with confidence intervals for one or
#' more vaccines, optionally within survey domains.
#'
#' @param design A [vcs_design()], or a [vcs_data][new_vcs_data] object which
#'   will be passed to [vcs_design()].
#' @param vaccines Vaccine names. Defaults to every vaccine with a derived
#'   coverage column.
#' @param evidence Definition of vaccination. Ignored when the coverage columns
#'   already exist and `redirect = FALSE`.
#' @param by Domains, as a one-sided formula or character vector, e.g.
#'   `~health_zone` or `c("province", "sex")`.
#' @param prefix Prefix of the coverage columns.
#' @param level Confidence level.
#' @param deff Compute design effects. `TRUE` uses the with-replacement
#'   reference variance (Kish's design effect). Pass `"F"` for `survey`'s
#'   finite-population variant, but note it returns `Inf` whenever the weights
#'   sum to the sample size.
#' @param na.rm Exclude children whose status is unknown. `TRUE` by default:
#'   unknown status is not counted as unvaccinated.
#' @param ci_method `"normal"` (default) for symmetric limits on the design
#'   standard error, or `"wilson"` for the survey-modified Wilson interval
#'   recommended by the WHO 2018 reference manual (a Wilson interval at the
#'   effective sample size `n / deff`). The global default can be changed with
#'   `options(vaxsurvR.ci_method = "wilson")`.
#' @return A tibble of class `vcs_estimate` with columns `indicator`, `domain`,
#'   the domain variables, `numerator`, `denominator`, `estimate`, `se`,
#'   `conf_low`, `conf_high`, `n_unweighted`, `deff`, `neff` (effective sample
#'   size), `icc` (ANOVA intracluster correlation over first-stage clusters)
#'   and `ci_method`.
#' @export
#' @seealso [estimate_dropout()], [estimate_zero_dose()], [vcs_design()]
#' @examples
#' d <- derive_vaccination_status(vcs_example, evidence = "card_or_recall")
#' des <- vcs_design(d)
#'
#' estimate_coverage(des, vaccines = c("BCG", "PENTA1", "PENTA3", "MCV1"))
#' estimate_coverage(des, vaccines = "PENTA3", by = ~stratum)
estimate_coverage <- function(design,
                              vaccines = NULL,
                              evidence = "card_or_recall",
                              by = NULL,
                              prefix = "cov_",
                              level = 0.95,
                              deff = TRUE,
                              na.rm = TRUE,
                              ci_method = getOption("vaxsurvR.ci_method", "normal")) {
  assert_string(prefix)
  assert_number(level, lower = 0.5, upper = 0.999)
  assert_flag(deff)
  assert_flag(na.rm)
  ci_method <- match.arg(ci_method, c("normal", "wilson"))
  design <- coerce_design(design, vaccines, evidence, prefix)
  by <- as_column_names(by)
  if (length(by)) {
    assert_columns(design$data, by, arg = "design data")
  }

  cols <- if (is.null(vaccines)) {
    grep(paste0("^", prefix), names(design$data), value = TRUE)
  } else {
    paste0(prefix, vaccines)
  }
  missing <- setdiff(cols, names(design$data))
  if (length(missing)) {
    vcs_abort(
      sprintf(paste0("Coverage column%s %s not found. Run ",
                     "`derive_vaccination_status()` first."),
              if (length(missing) > 1L) "s" else "", collapse_quote(missing)),
      class = "vaxsurvR_value_error"
    )
  }
  if (!length(cols)) {
    vcs_abort("No coverage columns found; run `derive_vaccination_status()` first.",
              class = "vaxsurvR_value_error")
  }

  rows <- lapply(cols, function(v) {
    out <- svy_one(design, v, by, level = level, na.rm = na.rm, deff = deff,
                   ci_method = ci_method)
    out$indicator <- sub(paste0("^", prefix), "", v)
    out
  })
  new_vcs_estimate(
    tidy_estimate(rows, by),
    meta = list(evidence = evidence, by = by, level = level,
                weighted = design$spec$weighted, type = "coverage",
                ci_method = ci_method)
  )
}

#' Accept either a design or a vcs_data, deriving coverage if needed
#' @noRd
coerce_design <- function(design, vaccines, evidence, prefix) {
  if (is_vcs_design(design)) {
    return(design)
  }
  if (is_vcs_data(design)) {
    has_cols <- length(grep(paste0("^", prefix), names(design$children)))
    if (!has_cols) {
      design <- derive_vaccination_status(design, evidence = evidence,
                                          vaccines = vaccines, prefix = prefix)
    }
    return(vcs_design(design))
  }
  vcs_abort("`design` must be a `vcs_design` or a `vcs_data`.",
            class = "vaxsurvR_type_error")
}

#' @rdname estimate_coverage
#' @param vaccine A single vaccine name.
#' @param ... Passed to [estimate_coverage()].
#' @export
#' @examples
#' estimate_antigen_coverage(des, "PENTA3", by = ~stratum)
estimate_antigen_coverage <- function(design, vaccine, by = NULL, ...) {
  assert_string(vaccine)
  estimate_coverage(design, vaccines = vaccine, by = by, ...)
}

#' Estimate the proportion fully vaccinated
#'
#' @param design A [vcs_design()] or [vcs_data][new_vcs_data] object.
#' @param variable Name of the fully-vaccinated indicator, as created by
#'   [derive_fully_vaccinated()].
#' @param by Domains.
#' @param ... Passed to the underlying estimator (`level`, `deff`, `na.rm`).
#' @return A `vcs_estimate` tibble.
#' @export
#' @examples
#' d <- derive_fully_vaccinated(vcs_example)
#' estimate_full_coverage(vcs_design(d))
estimate_full_coverage <- function(design, variable = "fully_vaccinated",
                                   by = NULL, ...) {
  assert_string(variable)
  estimate_indicator(design, variable, by, type = "fully_vaccinated", ...)
}

#' Estimate the zero-dose proportion
#'
#' @param design A [vcs_design()] or [vcs_data][new_vcs_data] object.
#' @param variable Name of the zero-dose indicator, as created by
#'   [derive_zero_dose()].
#' @param by Domains.
#' @param ... Passed to the underlying estimator.
#' @return A `vcs_estimate` tibble.
#' @export
#' @examples
#' d <- derive_zero_dose(vcs_example)
#' estimate_zero_dose(vcs_design(d), by = ~stratum)
estimate_zero_dose <- function(design, variable = "zero_dose", by = NULL, ...) {
  assert_string(variable)
  estimate_indicator(design, variable, by, type = "zero_dose", ...)
}

#' Estimate card availability
#'
#' The proportion of children for whom the interviewer saw a vaccination card.
#' This is the denominator on which card-documented coverage rests, so it
#' belongs in every coverage report.
#'
#' @param design A [vcs_design()] or [vcs_data][new_vcs_data] object.
#' @param variable Name of the card-seen indicator.
#' @param by Domains.
#' @param ... Passed to the underlying estimator.
#' @return A `vcs_estimate` tibble.
#' @export
#' @examples
#' estimate_card_availability(vcs_design(vcs_example), by = ~stratum)
estimate_card_availability <- function(design, variable = "card_seen",
                                       by = NULL, ...) {
  assert_string(variable)
  estimate_indicator(design, variable, by, type = "card_availability",
                     coerce_logical = TRUE, ...)
}

#' Estimate timely coverage
#'
#' @param design A [vcs_design()] or [vcs_data][new_vcs_data] object.
#' @param vaccines Vaccine names.
#' @param by Domains.
#' @param prefix Prefix of the timeliness columns, as created by
#'   [derive_timeliness()].
#' @param ... Passed to the underlying estimator.
#' @return A `vcs_estimate` tibble.
#' @export
#' @examples
#' d <- derive_timeliness(vcs_example)
#' estimate_timely_coverage(vcs_design(d), vaccines = c("BCG", "PENTA1"))
estimate_timely_coverage <- function(design, vaccines = NULL, by = NULL,
                                     prefix = "timely_", ...) {
  assert_string(prefix)
  des <- if (is_vcs_design(design)) design else {
    assert_vcs_data(design)
    vcs_design(design)
  }
  cols <- if (is.null(vaccines)) {
    grep(paste0("^", prefix), names(des$data), value = TRUE)
  } else {
    paste0(prefix, vaccines)
  }
  missing <- setdiff(cols, names(des$data))
  if (length(missing) || !length(cols)) {
    vcs_abort(
      "Timeliness columns not found; run `derive_timeliness()` first.",
      class = "vaxsurvR_value_error"
    )
  }
  by <- as_column_names(by)
  rows <- lapply(cols, function(v) {
    out <- svy_one(des, v, by, ...)
    out$indicator <- sub(paste0("^", prefix), "", v)
    out
  })
  new_vcs_estimate(
    tidy_estimate(rows, by),
    meta = list(type = "timely_coverage", by = by,
                weighted = des$spec$weighted)
  )
}

#' Estimate a single indicator column
#' @noRd
estimate_indicator <- function(design, variable, by = NULL, type = "indicator",
                               coerce_logical = FALSE, level = 0.95,
                               deff = TRUE, na.rm = TRUE,
                               ci_method = getOption("vaxsurvR.ci_method", "normal")) {
  des <- if (is_vcs_design(design)) design else {
    assert_vcs_data(design)
    vcs_design(design)
  }
  if (!variable %in% names(des$data)) {
    vcs_abort(
      sprintf("Column \"%s\" not found. Derive it before estimating.", variable),
      class = "vaxsurvR_value_error"
    )
  }
  if (coerce_logical || is.logical(des$data[[variable]])) {
    des$data[[variable]] <- as.integer(des$data[[variable]])
    des$design$variables[[variable]] <- as.integer(des$design$variables[[variable]])
  }
  by <- as_column_names(by)
  if (length(by)) {
    assert_columns(des$data, by, arg = "design data")
  }
  rows <- list(svy_one(des, variable, by, level = level, na.rm = na.rm,
                       deff = deff, ci_method = ci_method))
  new_vcs_estimate(
    tidy_estimate(rows, by),
    meta = list(type = type, by = by, level = level,
                weighted = des$spec$weighted, ci_method = ci_method)
  )
}

#' Estimate dropout between two doses
#'
#' Dropout can be defined in two ways, and the two do not give the same answer:
#'
#' * `method = "coverage"` -- the classic programme indicator,
#'   `(coverage(first) - coverage(last)) / coverage(first)`, computed from the
#'   two survey-weighted coverage estimates. Its confidence interval is obtained
#'   by the delta method over the joint design-based covariance.
#' * `method = "individual"` -- the proportion of children who received `first`
#'   but not `last`, estimated directly from individual vaccination histories
#'   with [derive_dropout()].
#'
#' The individual method is the more defensible of the two because it uses each
#' child's own history rather than two marginal estimates, but the coverage
#' method is what most programme reports quote. Always state which was used.
#'
#' @param design A [vcs_design()] or [vcs_data][new_vcs_data] object.
#' @param first,last Vaccine names bracketing the series, e.g. `"PENTA1"` and
#'   `"PENTA3"`.
#' @param by Domains.
#' @param method `"individual"` (default) or `"coverage"`.
#' @param evidence Definition of vaccination used when the indicator must be
#'   derived.
#' @param prefix Prefix of the coverage columns.
#' @param level Confidence level.
#' @param na.rm Exclude children of unknown status.
#' @return A `vcs_estimate` tibble. The `method` column records which definition
#'   produced each row.
#' @export
#' @seealso [derive_dropout()], [estimate_coverage()]
#' @examples
#' d <- derive_vaccination_status(vcs_example)
#' des <- vcs_design(d)
#'
#' estimate_dropout(des, first = "PENTA1", last = "PENTA3")
#' estimate_dropout(des, first = "PENTA1", last = "PENTA3", by = ~stratum)
#' estimate_dropout(des, first = "BCG", last = "MCV1", method = "coverage")
estimate_dropout <- function(design, first, last, by = NULL,
                             method = c("individual", "coverage"),
                             evidence = "card_or_recall",
                             prefix = "cov_",
                             level = 0.95,
                             na.rm = TRUE) {
  assert_string(first)
  assert_string(last)
  method <- match.arg(method)
  by <- as_column_names(by)

  if (method == "individual") {
    dropped_name <- sprintf("dropout_%s_%s", first, last)
    if (is_vcs_design(design)) {
      des <- design
      if (!dropped_name %in% names(des$data)) {
        des <- add_dropout_to_design(des, first, last, prefix, dropped_name)
      }
    } else {
      assert_vcs_data(design)
      des <- vcs_design(derive_dropout(design, first, last, evidence = evidence))
    }
    out <- estimate_indicator(des, dropped_name, by, type = "dropout",
                              level = level, na.rm = na.rm)
    out$indicator <- sprintf("%s-%s dropout", first, last)
    out$method <- "individual"
    attr(out, "vcs_meta")$first <- first
    attr(out, "vcs_meta")$last <- last
    return(out)
  }

  # ---- coverage-difference method --------------------------------------
  des <- coerce_design(design, c(first, last), evidence, prefix)
  cols <- paste0(prefix, c(first, last))
  missing <- setdiff(cols, names(des$data))
  if (length(missing)) {
    vcs_abort(
      sprintf("Coverage column%s %s not found; run `derive_vaccination_status()` first.",
              if (length(missing) > 1L) "s" else "", collapse_quote(missing)),
      class = "vaxsurvR_value_error"
    )
  }
  rows <- dropout_from_coverage(des, cols, by, level, na.rm)
  out <- new_vcs_estimate(
    tidy_estimate(list(rows), by),
    meta = list(type = "dropout", method = "coverage", by = by, level = level,
                first = first, last = last, weighted = des$spec$weighted)
  )
  out$indicator <- sprintf("%s-%s dropout", first, last)
  out$method <- "coverage"
  out
}

#' Build the individual dropout indicator inside an existing design
#'
#' Uses the coverage columns already present in the design data, so that a
#' design built once can serve every dropout pair without being rebuilt.
#' @noRd
add_dropout_to_design <- function(des, first, last, prefix, name) {
  cols <- paste0(prefix, c(first, last))
  missing <- setdiff(cols, names(des$data))
  if (length(missing)) {
    vcs_abort(
      sprintf(paste0("Neither \"%s\" nor the coverage column%s %s is in the ",
                     "design. Pass a `vcs_data`, or derive the columns first."),
              name, if (length(missing) > 1L) "s" else "",
              collapse_quote(missing)),
      class = "vaxsurvR_value_error"
    )
  }
  a <- suppressWarnings(as.numeric(des$data[[cols[1]]]))
  b <- suppressWarnings(as.numeric(des$data[[cols[2]]]))
  out <- rep(NA_integer_, length(a))
  at_risk <- !is.na(a) & a == 1
  out[at_risk] <- as.integer(!is.na(b[at_risk]) & b[at_risk] == 0)
  out[at_risk & is.na(b)] <- NA_integer_
  des$data[[name]] <- out
  des$design$variables[[name]] <- out
  des
}

#' Delta-method dropout from two coverage estimates
#' @noRd
dropout_from_coverage <- function(des, cols, by, level, na.rm) {
  z <- stats::qnorm(1 - (1 - level) / 2)
  rule <- options(survey.lonely.psu = des$spec$lonely_psu %||% "adjust")
  on.exit(options(rule), add = TRUE)
  fml <- stats::as.formula(paste0("~", paste(cols, collapse = " + ")))
  ratio_row <- function(est, dom) {
    m <- as.numeric(stats::coef(est))
    V <- stats::vcov(est)
    if (length(m) < 2L || any(is.na(m)) || m[1] <= 0) {
      return(tibble::tibble(domain = dom, estimate = NA_real_, se = NA_real_,
                            deff = NA_real_, conf_low = NA_real_,
                            conf_high = NA_real_))
    }
    theta <- (m[1] - m[2]) / m[1]
    # d/dm1 = m2 / m1^2 ; d/dm2 = -1 / m1
    g <- c(m[2] / m[1]^2, -1 / m[1])
    var <- as.numeric(t(g) %*% V %*% g)
    se <- if (is.finite(var) && var >= 0) sqrt(var) else NA_real_
    tibble::tibble(
      domain = dom, estimate = theta, se = se, deff = NA_real_,
      conf_low = if (is.na(se)) NA_real_ else theta - z * se,
      conf_high = if (is.na(se)) NA_real_ else theta + z * se
    )
  }

  raw <- des$data
  vals1 <- suppressWarnings(as.numeric(raw[[cols[1]]]))
  vals2 <- suppressWarnings(as.numeric(raw[[cols[2]]]))

  if (!length(by)) {
    est <- try_quiet(survey::svymean(fml, des$design, na.rm = na.rm))
    out <- ratio_row(est, "<overall>")
    out$numerator <- sum(vals1 > 0, na.rm = TRUE) - sum(vals2 > 0, na.rm = TRUE)
    out$denominator <- sum(vals1 > 0, na.rm = TRUE)
    out$n_unweighted <- sum(!is.na(vals1))
    return(out)
  }

  key <- do.call(paste, c(lapply(by, function(v) as.character(raw[[v]])), sep = " | "))
  groups <- sort(unique(key[!is.na(key)]))
  rows <- lapply(groups, function(g) {
    sub <- try_quiet(subset(des$design, key == g))
    est <- if (is.null(sub)) NULL else try_quiet(survey::svymean(fml, sub, na.rm = na.rm))
    r <- if (is.null(est)) {
      tibble::tibble(domain = g, estimate = NA_real_, se = NA_real_,
                     deff = NA_real_, conf_low = NA_real_, conf_high = NA_real_)
    } else {
      ratio_row(est, g)
    }
    sel <- key == g
    r$numerator <- sum(vals1[sel] > 0, na.rm = TRUE) - sum(vals2[sel] > 0, na.rm = TRUE)
    r$denominator <- sum(vals1[sel] > 0, na.rm = TRUE)
    r$n_unweighted <- sum(sel & !is.na(vals1))
    for (i in seq_along(by)) {
      r[[by[i]]] <- vapply(strsplit(g, " | ", fixed = TRUE), `[`, character(1), i)
    }
    r
  })
  dplyr::bind_rows(rows)
}

#' Coverage by source of evidence: card, recall and either
#'
#' The standard presentation of a coverage survey reports each antigen three
#' ways: documented on the vaccination card, reported by the caregiver, and
#' either source ("card or recall"). This function derives all three and
#' returns them stacked, labelled by `evidence`.
#'
#' @section Denominators:
#' `denominator = "all"` (default) follows the convention of most published
#' coverage tables: every enumerated child is in the denominator of every row,
#' and a child with no evidence from a given source counts as not vaccinated
#' *by that source*. A child with no card is therefore "not documented on the
#' card" -- which is true -- and the card row reads as *documented coverage*,
#' not as coverage among card holders. This is `missing_as_unvaccinated = TRUE`
#' applied explicitly, and the rule is recorded on every derived column.
#'
#' `denominator = "determinable"` keeps the package default: a child with no
#' evidence from a source has unknown status for that source and is excluded
#' from that row. Denominators then differ between rows, and the card row
#' becomes *coverage among children whose card was seen*.
#'
#' State which convention a table uses. They answer different questions.
#'
#' @param x A [vcs_data][new_vcs_data] object with card and recall evidence
#'   mapped.
#' @param vaccines Vaccine names. Defaults to every vaccine present.
#' @param by Domains, as a one-sided formula or character vector.
#' @param evidence Evidence definitions to compare, from
#'   [vcs_evidence_definitions()].
#' @param labels Display labels for `evidence`, same length.
#' @param denominator `"all"` or `"determinable"`; see Denominators.
#' @param design_args Arguments passed to [vcs_design()].
#' @param ... Passed to [estimate_coverage()] (`level`, `deff`).
#' @return A `vcs_estimate` tibble with an `evidence` column, ordered by
#'   vaccine then evidence.
#' @export
#' @seealso [estimate_coverage()], [derive_vaccination_status()]
#' @examples
#' est <- estimate_coverage_by_evidence(
#'   vcs_example, vaccines = c("BCG", "PENTA1", "PENTA3", "MCV1")
#' )
#' est[, c("indicator", "evidence", "numerator", "denominator", "estimate")]
#'
#' estimate_coverage_by_evidence(vcs_example, vaccines = "PENTA3", by = ~stratum)
estimate_coverage_by_evidence <- function(x,
                                          vaccines = NULL,
                                          by = NULL,
                                          evidence = c("card", "recall", "card_or_recall"),
                                          labels = c("Card", "Recall", "Card or recall"),
                                          denominator = c("all", "determinable"),
                                          design_args = list(),
                                          ...) {
  assert_vcs_data(x)
  assert_character(evidence, allow_null = FALSE)
  assert_character(labels, allow_null = FALSE)
  denominator <- match.arg(denominator)
  if (length(labels) != length(evidence)) {
    vcs_abort("`labels` must have the same length as `evidence`.",
              class = "vaxsurvR_value_error")
  }
  known <- vcs_evidence_definitions()$definition
  bad <- setdiff(evidence, known)
  if (length(bad)) {
    vcs_abort(sprintf("Unknown evidence definition%s: %s.",
                      if (length(bad) > 1L) "s" else "", collapse_quote(bad)),
              class = "vaxsurvR_value_error")
  }
  if (!nrow(x$vaccinations)) {
    vcs_abort("No vaccination records to estimate from.", class = "vaxsurvR_value_error")
  }
  vaccines <- vaccines %||% unique(x$vaccinations$vaccine)

  d <- x
  prefixes <- paste0("ev_", seq_along(evidence), "_")
  for (i in seq_along(evidence)) {
    d <- derive_vaccination_status(
      d, evidence = evidence[i], vaccines = vaccines,
      missing_as_unvaccinated = identical(denominator, "all"),
      prefix = prefixes[i]
    )
  }
  des <- do.call(vcs_design, c(list(d), design_args))

  rows <- lapply(seq_along(evidence), function(i) {
    est <- estimate_coverage(des, vaccines = vaccines, by = by,
                             prefix = prefixes[i], evidence = evidence[i], ...)
    est$evidence <- labels[i]
    est
  })
  out <- dplyr::bind_rows(rows)
  out$indicator <- factor(out$indicator, levels = vaccines)
  out$evidence <- factor(out$evidence, levels = labels)
  ord <- order(out$indicator, out$domain, out$evidence)
  out <- out[ord, , drop = FALSE]
  out$indicator <- as.character(out$indicator)
  front <- c("indicator", "evidence", "domain", as_column_names(by))
  out <- out[, c(front, setdiff(names(out), front)), drop = FALSE]
  new_vcs_estimate(
    out,
    meta = list(type = "coverage_by_evidence", evidence = paste(evidence, collapse = " | "),
                denominator = denominator, by = as_column_names(by),
                weighted = des$spec$weighted)
  )
}
