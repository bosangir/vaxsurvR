# ---------------------------------------------------------------------------
# Survey design. vaxsurvR does not implement its own variance estimator: it
# constructs a survey::svydesign and hands estimation to that package.
# ---------------------------------------------------------------------------

#' Construct the complex survey design
#'
#' Builds a [survey::svydesign()] over the child-level analysis table, carrying
#' the cluster, stratification, weighting and finite population correction
#' structure that coverage estimates need.
#'
#' @param x A [vcs_data][new_vcs_data] object, or a child-level data frame.
#' @param ids Cluster identifiers, as a one-sided formula or character vector.
#'   Defaults to `~psu` and, where segments are mapped, `~psu + segment`.
#' @param strata Stratification variables. Defaults to `~stratum` when mapped.
#' @param weights Weight variable. Defaults to `~weight` when mapped. When no
#'   weight is available the design is built with equal weights and a warning,
#'   because unweighted estimates are not population estimates.
#' @param fpc Finite population correction variable.
#' @param nest Treat cluster identifiers as nested within strata. `TRUE` by
#'   default, which is correct when PSU labels restart inside each stratum.
#' @param lonely_psu How [survey::svydesign()] should handle strata with a
#'   single PSU: `"adjust"` (default), `"average"`, `"certainty"`, `"remove"`
#'   or `"fail"`. The choice is recorded in the returned object.
#' @param drop_missing_weights Drop records with missing, zero or negative
#'   weights. `TRUE` by default; the number dropped is recorded. Set to `FALSE`
#'   to let [survey::svydesign()] raise its own error instead.
#' @param data Deprecated alias for `x`.
#' @return An object of class `vcs_design`, wrapping the `survey.design`.
#' @export
#' @seealso [validate_weights()], [estimate_coverage()]
#' @examples
#' d <- derive_vaccination_status(vcs_example)
#' des <- vcs_design(d)
#' des
#'
#' # Explicit design specification.
#' vcs_design(d, ids = ~psu, strata = ~stratum, weights = ~weight)
vcs_design <- function(x,
                       ids = NULL,
                       strata = NULL,
                       weights = NULL,
                       fpc = NULL,
                       nest = TRUE,
                       lonely_psu = c("adjust", "average", "certainty",
                                      "remove", "fail"),
                       drop_missing_weights = TRUE,
                       data = NULL) {
  if (is.null(x) && !is.null(data)) {
    x <- data
  }
  lonely_psu <- match.arg(lonely_psu)
  assert_flag(nest)
  assert_flag(drop_missing_weights)

  tab <- if (is_vcs_data(x)) vcs_children(x) else {
    assert_data(x)
    tibble::as_tibble(x)
  }
  if (!nrow(tab)) {
    vcs_abort("Cannot build a survey design from zero records.",
              class = "vaxsurvR_design_error")
  }

  ids <- as_column_names(ids)
  strata <- as_column_names(strata)
  weights <- as_column_names(weights)
  fpc <- as_column_names(fpc)

  if (!length(ids)) {
    ids <- intersect(c("psu", "segment"), names(tab))
    if (!length(ids)) {
      vcs_abort(
        "No PSU variable is available; pass `ids` explicitly, or `ids = ~1` for a simple random sample.",
        class = "vaxsurvR_design_error"
      )
    }
  }
  if (!length(strata) && "stratum" %in% names(tab)) {
    strata <- "stratum"
  }
  weight_supplied <- length(weights) > 0L
  if (!weight_supplied && "weight" %in% names(tab)) {
    weights <- "weight"
    weight_supplied <- TRUE
  }

  missing_cols <- setdiff(c(ids, strata, weights, fpc), c(names(tab), "1"))
  if (length(missing_cols)) {
    vcs_abort(
      sprintf("Design variable%s not found in the data: %s.",
              if (length(missing_cols) > 1L) "s" else "",
              collapse_quote(missing_cols)),
      class = "vaxsurvR_design_error"
    )
  }

  if (!weight_supplied) {
    vcs_warn(
      paste0("No weight variable is available. Building an unweighted design: ",
             "the results are sample descriptions, not population estimates."),
      class = "vaxsurvR_no_weights"
    )
    tab[[".vcs_equal_weight"]] <- 1
    weights <- ".vcs_equal_weight"
  }

  n_before <- nrow(tab)
  n_dropped <- 0L
  if (drop_missing_weights) {
    w <- suppressWarnings(as.numeric(tab[[weights[1]]]))
    keep <- !is.na(w) & w > 0
    n_dropped <- sum(!keep)
    if (n_dropped) {
      vcs_warn(
        sprintf("Dropped %d record(s) with missing, zero or negative weights.",
                n_dropped),
        class = "vaxsurvR_dropped_weights"
      )
      tab <- tab[keep, , drop = FALSE]
    }
    if (!nrow(tab)) {
      vcs_abort("Every record has a missing, zero or negative weight.",
                class = "vaxsurvR_design_error")
    }
  }

  as_formula <- function(v) {
    if (!length(v)) {
      return(NULL)
    }
    stats::as.formula(paste("~", paste(v, collapse = " + ")))
  }

  old <- options(survey.lonely.psu = lonely_psu)
  on.exit(options(old), add = TRUE)

  des <- survey::svydesign(
    ids = as_formula(ids),
    strata = as_formula(strata),
    weights = as_formula(weights),
    fpc = as_formula(fpc),
    data = as.data.frame(tab),
    nest = nest && length(strata) > 0L
  )

  structure(
    list(
      design = des,
      data = tab,
      spec = list(ids = ids, strata = strata, weights = weights, fpc = fpc,
                  nest = nest, lonely_psu = lonely_psu,
                  weighted = weight_supplied),
      meta = list(
        n_records = nrow(tab),
        n_dropped_weights = n_dropped,
        n_input = n_before,
        created = vcs_timestamp()
      )
    ),
    class = "vcs_design"
  )
}

# Note: the `survey` package reads options(survey.lonely.psu) at *estimation*
# time, not when the design is built, so the rule chosen here is stored in
# `spec$lonely_psu` and re-applied around every estimate rather than being set
# globally.

#' Test whether an object is a vcs_design
#'
#' @param x An object.
#' @return A logical scalar.
#' @export
#' @examples
#' is_vcs_design(vcs_design(derive_vaccination_status(vcs_example)))
is_vcs_design <- function(x) inherits(x, "vcs_design")

#' @noRd
assert_design <- function(x, arg = rlang::caller_arg(x)) {
  if (!is_vcs_design(x)) {
    vcs_abort(
      sprintf("`%s` must be a `vcs_design`, not %s. See `vcs_design()`.",
              arg, obj_type(x)),
      class = "vaxsurvR_type_error"
    )
  }
  invisible(x)
}

#' @export
print.vcs_design <- function(x, ...) {
  cat("<vcs_design>\n")
  cat(sprintf("  records : %d\n", x$meta$n_records))
  cat(sprintf("  ids     : %s\n", paste(x$spec$ids, collapse = " + ")))
  cat(sprintf("  strata  : %s\n",
              if (length(x$spec$strata)) paste(x$spec$strata, collapse = " + ") else "<none>"))
  cat(sprintf("  weights : %s%s\n", x$spec$weights,
              if (x$spec$weighted) "" else "  (equal weights - NOT population estimates)"))
  if (length(x$spec$fpc)) {
    cat(sprintf("  fpc     : %s\n", paste(x$spec$fpc, collapse = " + ")))
  }
  cat(sprintf("  lonely PSU rule: %s\n", x$spec$lonely_psu))
  if (x$meta$n_dropped_weights) {
    cat(sprintf("  dropped : %d record(s) with unusable weights\n",
                x$meta$n_dropped_weights))
  }
  invisible(x)
}

#' @export
summary.vcs_design <- function(object, ...) {
  summary(object$design, ...)
}

#' Extract the underlying survey design object
#'
#' Use this to reach any function in the `survey` package that vaxsurvR does not
#' wrap.
#'
#' @param x A [vcs_design()].
#' @return A `survey.design` object.
#' @export
#' @examples
#' des <- vcs_design(derive_vaccination_status(vcs_example))
#' survey::svymean(~cov_BCG, as_survey_design(des), na.rm = TRUE)
as_survey_design <- function(x) {
  assert_design(x)
  x$design
}

#' Validate sampling weights
#'
#' Checks weights for missingness, non-positive values, extreme variability and
#' inconsistency within a PSU. Returns a
#' [vcs_validation][new_vcs_validation] object rather than modifying anything.
#'
#' @param x A [vcs_data][new_vcs_data] object, a [vcs_design()], or a data
#'   frame.
#' @param weight Name of the weight column.
#' @param id Name of the record identifier column, used in issue reporting.
#' @param max_ratio Ratio of the largest to the smallest weight above which the
#'   distribution is flagged.
#' @param max_cv Coefficient of variation of the weights above which the
#'   distribution is flagged. Weight variability inflates the design effect
#'   roughly as `1 + cv^2`.
#' @return A [vcs_validation][new_vcs_validation] object.
#' @export
#' @seealso [check_weight_distribution()], [trim_weights()]
#' @examples
#' validate_weights(vcs_example)
validate_weights <- function(x, weight = "weight", id = "child_id",
                             max_ratio = 20, max_cv = 1.5) {
  assert_string(weight)
  assert_string(id)
  assert_number(max_ratio, lower = 1)
  assert_number(max_cv, lower = 0)
  tab <- weight_table(x)
  if (!weight %in% names(tab)) {
    return(new_vcs_validation(
      vcs_issue("validate_weights", "WT_UNMAPPED", "CRITICAL", "survey",
                record_id = "<survey>", variable = weight, value = NA_character_,
                message = "No weight variable is available; population estimates are not possible."),
      "validate_weights"
    ))
  }
  w <- suppressWarnings(as.numeric(tab[[weight]]))
  rid <- if (id %in% names(tab)) as.character(tab[[id]]) else {
    sprintf("<row %d>", seq_len(nrow(tab)))
  }
  iss <- list()

  na_w <- is.na(w)
  if (any(na_w)) {
    iss[[length(iss) + 1L]] <- vcs_issue(
      "validate_weights", "WT_MISSING", "CRITICAL", "child",
      record_id = rid[na_w], variable = weight, value = NA_character_,
      message = "Weight is missing; the record cannot contribute to a population estimate."
    )
  }
  zero <- !na_w & w == 0
  if (any(zero)) {
    iss[[length(iss) + 1L]] <- vcs_issue(
      "validate_weights", "WT_ZERO", "ERROR", "child",
      record_id = rid[zero], variable = weight, value = "0",
      message = "Weight is zero; the record contributes nothing to the estimate."
    )
  }
  neg <- !na_w & w < 0
  if (any(neg)) {
    iss[[length(iss) + 1L]] <- vcs_issue(
      "validate_weights", "WT_NEGATIVE", "CRITICAL", "child",
      record_id = rid[neg], variable = weight, value = as.character(w[neg]),
      message = "Weight is negative; this is never valid."
    )
  }

  ok <- !na_w & w > 0
  if (sum(ok) > 1L) {
    ratio <- max(w[ok]) / min(w[ok])
    if (ratio > max_ratio) {
      iss[[length(iss) + 1L]] <- vcs_issue(
        "validate_weights", "WT_WIDE_RANGE", "WARNING", "survey",
        record_id = "<survey>", variable = weight,
        value = sprintf("%.1f", ratio),
        message = sprintf("Largest weight is %.1f times the smallest (threshold %g).",
                          ratio, max_ratio)
      )
    }
    cv <- stats::sd(w[ok]) / mean(w[ok])
    if (is.finite(cv) && cv > max_cv) {
      iss[[length(iss) + 1L]] <- vcs_issue(
        "validate_weights", "WT_HIGH_CV", "WARNING", "survey",
        record_id = "<survey>", variable = weight, value = sprintf("%.2f", cv),
        message = sprintf("Weight CV is %.2f; expect a design effect inflated by about %.2f.",
                          cv, 1 + cv^2)
      )
    }
  }
  if ("psu" %in% names(tab)) {
    by_psu <- split(w[ok], as.character(tab$psu)[ok])
    varying <- names(by_psu)[vapply(by_psu, function(v) {
      length(v) > 1L && stats::sd(v) / mean(v) > 0.001
    }, logical(1))]
    if (length(varying)) {
      iss[[length(iss) + 1L]] <- vcs_issue(
        "validate_weights", "WT_VARIES_IN_PSU", "INFO", "survey",
        record_id = varying, variable = weight, value = NA_character_,
        message = "Weights vary within the PSU; expected when non-response adjustment is applied at household level."
      )
    }
  }
  new_vcs_validation(dplyr::bind_rows(iss), "validate_weights")
}

#' @noRd
weight_table <- function(x) {
  if (is_vcs_data(x)) {
    vcs_children(x)
  } else if (is_vcs_design(x)) {
    x$data
  } else {
    assert_data(x)
    tibble::as_tibble(x)
  }
}

#' Summarise the weight distribution
#'
#' @param x A [vcs_data][new_vcs_data] object, a [vcs_design()], or a data
#'   frame.
#' @param weight Name of the weight column.
#' @param by Optional grouping columns, as a formula or character vector.
#' @return A tibble with `n`, `n_missing`, `min`, `q25`, `median`, `q75`, `max`,
#'   `mean`, `cv` and `sum` per group.
#' @export
#' @examples
#' check_weight_distribution(vcs_example)
#' check_weight_distribution(vcs_example, by = ~stratum)
check_weight_distribution <- function(x, weight = "weight", by = NULL) {
  assert_string(weight)
  tab <- weight_table(x)
  assert_columns(tab, weight, arg = "x")
  by <- as_column_names(by)
  if (length(by)) {
    assert_columns(tab, by, arg = "x")
  }
  w <- suppressWarnings(as.numeric(tab[[weight]]))
  grp <- if (length(by)) {
    do.call(paste, c(lapply(by, function(v) as.character(tab[[v]])), sep = " | "))
  } else {
    rep("<overall>", nrow(tab))
  }
  idx <- split(seq_along(w), grp)
  out <- lapply(names(idx), function(g) {
    v <- w[idx[[g]]]
    ok <- v[!is.na(v)]
    tibble::tibble(
      group = g,
      n = length(v),
      n_missing = sum(is.na(v)),
      min = if (length(ok)) min(ok) else NA_real_,
      q25 = if (length(ok)) unname(stats::quantile(ok, 0.25)) else NA_real_,
      median = if (length(ok)) stats::median(ok) else NA_real_,
      q75 = if (length(ok)) unname(stats::quantile(ok, 0.75)) else NA_real_,
      max = if (length(ok)) max(ok) else NA_real_,
      mean = if (length(ok)) mean(ok) else NA_real_,
      cv = if (length(ok) > 1L) stats::sd(ok) / mean(ok) else NA_real_,
      sum = if (length(ok)) sum(ok) else NA_real_
    )
  })
  dplyr::bind_rows(out)
}

#' Trim extreme weights
#'
#' Winsorises weights at given quantiles and rescales so the total stays
#' unchanged. Trimming trades variance for bias and is never applied
#' automatically: this function exists only for users who have decided to trim
#' and must document that they did. The original weights are kept.
#'
#' @param x A [vcs_data][new_vcs_data] object, or a data frame.
#' @param weight Name of the weight column.
#' @param lower,upper Quantiles at which to winsorise.
#' @param rescale Rescale trimmed weights so that their sum equals the original
#'   sum.
#' @param new_name Name of the trimmed weight column.
#' @return The input with the trimmed weight column added; the original weight
#'   column is untouched.
#' @export
#' @examples
#' d <- trim_weights(vcs_example, lower = 0.02, upper = 0.98)
#' summary(d$children$weight_trimmed)
#' sum(d$children$weight) - sum(d$children$weight_trimmed)  # rescaled to match
trim_weights <- function(x, weight = "weight", lower = 0.01, upper = 0.99,
                         rescale = TRUE, new_name = "weight_trimmed") {
  assert_string(weight)
  assert_string(new_name)
  assert_number(lower, lower = 0, upper = 1)
  assert_number(upper, lower = 0, upper = 1)
  assert_flag(rescale)
  if (lower >= upper) {
    vcs_abort("`lower` must be below `upper`.", class = "vaxsurvR_value_error")
  }
  is_data <- is_vcs_data(x)
  tab <- if (is_data) x$children else {
    assert_data(x)
    tibble::as_tibble(x)
  }
  assert_columns(tab, weight, arg = "x")
  w <- suppressWarnings(as.numeric(tab[[weight]]))
  ok <- !is.na(w)
  qs <- stats::quantile(w[ok], c(lower, upper), names = FALSE)
  trimmed <- w
  trimmed[ok] <- pmin(pmax(w[ok], qs[1]), qs[2])
  factor_ <- 1
  if (rescale && sum(trimmed[ok]) > 0) {
    factor_ <- sum(w[ok]) / sum(trimmed[ok])
    trimmed[ok] <- trimmed[ok] * factor_
  }
  tab[[new_name]] <- mark_derived(
    trimmed,
    sprintf("%s winsorised at the %g and %g quantiles (%.4g, %.4g)%s",
            weight, lower, upper, qs[1], qs[2],
            if (rescale) sprintf(" and rescaled by %.4f", factor_) else "")
  )
  if (is_data) {
    x$children <- tab
    x
  } else {
    tab
  }
}

#' Design effect for one or more indicators
#'
#' Computes Kish's design effect from the survey design, that is the ratio of
#' the variance under the complex design to the variance under simple random
#' sampling of the same size.
#'
#' @param design A [vcs_design()].
#' @param variables Character vector of numeric or 0/1 indicator columns.
#' @param na.rm Drop missing values from each estimate.
#' @param type Reference variance. `"replace"` (default) compares against
#'   simple random sampling *with* replacement, which is the usual Kish
#'   definition. `"fpc"` applies a finite population correction taken from the
#'   sum of the weights; it returns `Inf` when the weights are normalised to the
#'   sample size, which is common, so it is not the default.
#' @return A tibble with `variable`, `estimate`, `se`, `deff` and `n`.
#' @export
#' @examples
#' des <- vcs_design(derive_vaccination_status(vcs_example))
#' design_effect(des, c("cov_BCG", "cov_PENTA3"))
design_effect <- function(design, variables, na.rm = TRUE,
                          type = c("replace", "fpc")) {
  assert_design(design)
  assert_character(variables, allow_null = FALSE)
  assert_flag(na.rm)
  type <- match.arg(type)
  deff_arg <- if (type == "replace") "replace" else TRUE
  missing <- setdiff(variables, names(design$data))
  if (length(missing)) {
    vcs_abort(
      sprintf("Variable%s not in the design data: %s.",
              if (length(missing) > 1L) "s" else "", collapse_quote(missing)),
      class = "vaxsurvR_value_error"
    )
  }
  rule <- options(survey.lonely.psu = design$spec$lonely_psu %||% "adjust")
  on.exit(options(rule), add = TRUE)
  out <- lapply(variables, function(v) {
    est <- tryCatch(
      survey::svymean(stats::as.formula(paste0("~", v)), design$design,
                      na.rm = na.rm, deff = deff_arg),
      error = function(e) NULL
    )
    if (is.null(est)) {
      return(tibble::tibble(variable = v, estimate = NA_real_, se = NA_real_,
                            deff = NA_real_, n = NA_integer_))
    }
    tibble::tibble(
      variable = v,
      estimate = as.numeric(est)[1],
      se = as.numeric(survey::SE(est))[1],
      deff = as.numeric(survey::deff(est))[1],
      n = sum(!is.na(design$data[[v]]))
    )
  })
  dplyr::bind_rows(out)
}
