# ---------------------------------------------------------------------------
# Completeness summaries and report objects. Everything returns a plain tibble
# or a list of tibbles, so that any reporting stack -- Quarto, R Markdown,
# Excel, Word -- can consume it without the package taking a dependency on one.
# ---------------------------------------------------------------------------

#' Summarise missingness
#'
#' Reports the proportion missing for each variable, optionally within groups
#' such as PSU, segment, interviewer or geographic domain. Uneven missingness
#' across interviewers or clusters is one of the clearest fieldwork warning
#' signs a coverage survey produces.
#'
#' @param x A [vcs_data][new_vcs_data] object, or a data frame.
#' @param level Which table to summarise: `"child"`, `"household"` or
#'   `"vaccination"`. Ignored when `x` is a data frame.
#' @param variables Variables to summarise. Defaults to all.
#' @param by Grouping variables, as a formula or character vector.
#' @return A tibble with `variable`, the grouping columns, `n`, `n_missing` and
#'   `prop_missing`.
#' @export
#' @seealso [coverage_completeness()], [card_completeness()],
#'   [date_completeness()]
#' @examples
#' vcs_missingness(vcs_example, variables = c("sex", "child_dob", "card_seen"))
#' vcs_missingness(vcs_example, variables = "card_seen", by = ~stratum)
vcs_missingness <- function(x, level = c("child", "household", "vaccination"),
                            variables = NULL, by = NULL) {
  level <- match.arg(level)
  assert_character(variables)
  tab <- if (is_vcs_data(x)) {
    switch(level,
      child = vcs_children(x),
      household = x$households,
      vaccination = rule_context(x, "vaccination")
    )
  } else {
    assert_data(x)
    tibble::as_tibble(x)
  }
  by <- as_column_names(by)
  if (length(by)) {
    assert_columns(tab, by, arg = "x")
  }
  variables <- variables %||% setdiff(names(tab), by)
  variables <- intersect(variables, names(tab))
  if (!length(variables)) {
    return(tibble::tibble(variable = character(0), n = integer(0),
                          n_missing = integer(0), prop_missing = numeric(0)))
  }
  grp <- if (length(by)) {
    do.call(paste, c(lapply(by, function(v) as.character(tab[[v]])), sep = " | "))
  } else {
    rep("<overall>", nrow(tab))
  }
  idx <- split(seq_len(nrow(tab)), grp)
  rows <- lapply(variables, function(v) {
    col <- tab[[v]]
    miss <- if (is.character(col)) is_blank(col) else is.na(col)
    out <- tibble::tibble(
      variable = v,
      group = names(idx),
      n = unname(vapply(idx, length, integer(1))),
      n_missing = unname(vapply(idx, function(i) sum(miss[i]), integer(1)))
    )
    out$prop_missing <- ifelse(out$n > 0, out$n_missing / out$n, NA_real_)
    if (length(by)) {
      parts <- strsplit(out$group, " | ", fixed = TRUE)
      for (i in seq_along(by)) {
        out[[by[i]]] <- vapply(parts, function(p) {
          if (length(p) >= i) p[i] else NA_character_
        }, character(1))
      }
    }
    out
  })
  out <- dplyr::bind_rows(rows)
  if (!length(by)) {
    out$group <- NULL
  }
  out[order(-out$prop_missing, out$variable), , drop = FALSE]
}

#' Completeness of derived coverage indicators
#'
#' The share of children whose vaccination status could be determined at all,
#' per vaccine. A low value means the coverage estimate for that vaccine rests
#' on a thin denominator, which matters more than the estimate itself.
#'
#' @param x A [vcs_data][new_vcs_data] object with derived coverage columns.
#' @param prefix Prefix of the coverage columns.
#' @param by Grouping variables.
#' @return A tibble with `vaccine`, the grouping columns, `n`, `n_determined`
#'   and `prop_determined`.
#' @export
#' @examples
#' d <- derive_vaccination_status(vcs_example, evidence = "card")
#' coverage_completeness(d)
coverage_completeness <- function(x, prefix = "cov_", by = NULL) {
  assert_vcs_data(x)
  assert_string(prefix)
  cols <- grep(paste0("^", prefix), names(x$children), value = TRUE)
  if (!length(cols)) {
    vcs_abort(
      "No coverage columns found; run `derive_vaccination_status()` first.",
      class = "vaxsurvR_value_error"
    )
  }
  out <- vcs_missingness(x, level = "child", variables = cols, by = by)
  out$vaccine <- sub(paste0("^", prefix), "", out$variable)
  out$n_determined <- out$n - out$n_missing
  out$prop_determined <- 1 - out$prop_missing
  front <- c("vaccine", as_column_names(by), "n", "n_determined", "prop_determined")
  out[, intersect(c(front, setdiff(names(out), front)), names(out)), drop = FALSE]
}

#' Card availability and completeness
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param by Grouping variables.
#' @return A tibble with `n_children`, `n_card_seen`, `prop_card_seen`,
#'   `n_card_missing` and `prop_card_missing` per group.
#' @export
#' @examples
#' card_completeness(vcs_example)
#' card_completeness(vcs_example, by = ~stratum)
card_completeness <- function(x, by = NULL) {
  assert_vcs_data(x)
  ch <- vcs_children(x)
  if (!"card_seen" %in% names(ch)) {
    vcs_abort("No `card_seen` variable is mapped.", class = "vaxsurvR_value_error")
  }
  by <- as_column_names(by)
  if (length(by)) {
    assert_columns(ch, by, arg = "x")
  }
  grp <- if (length(by)) {
    do.call(paste, c(lapply(by, function(v) as.character(ch[[v]])), sep = " | "))
  } else {
    rep("<overall>", nrow(ch))
  }
  idx <- split(seq_len(nrow(ch)), grp)
  out <- tibble::tibble(
    group = names(idx),
    n_children = unname(vapply(idx, length, integer(1))),
    n_card_seen = unname(vapply(idx, function(i) sum(ch$card_seen[i], na.rm = TRUE),
                                integer(1))),
    n_card_missing = unname(vapply(idx, function(i) sum(is.na(ch$card_seen[i])),
                                   integer(1)))
  )
  out$prop_card_seen <- out$n_card_seen / out$n_children
  out$prop_card_missing <- out$n_card_missing / out$n_children
  if (length(by)) {
    parts <- strsplit(out$group, " | ", fixed = TRUE)
    for (i in seq_along(by)) {
      out[[by[i]]] <- vapply(parts, function(p) {
        if (length(p) >= i) p[i] else NA_character_
      }, character(1))
    }
    out$group <- NULL
  } else {
    out$group <- NULL
  }
  out
}

#' Completeness of vaccination dates
#'
#' Of the doses documented on a card, how many carry a date, and how many carry
#' a date complete enough to compute age at vaccination.
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param by Grouping variables, drawn from the child-level table.
#' @return A tibble with `n_documented`, `n_dated`, `prop_dated`, `n_partial`
#'   and `prop_partial` per group.
#' @export
#' @examples
#' date_completeness(vcs_example)
#' date_completeness(vcs_example, by = ~vaccine)
date_completeness <- function(x, by = NULL) {
  assert_vcs_data(x)
  vx <- rule_context(x, "vaccination")
  if (!nrow(vx) || !"card_date" %in% names(vx)) {
    vcs_abort("No card dates are mapped.", class = "vaxsurvR_value_error")
  }
  by <- as_column_names(by)
  if (length(by)) {
    assert_columns(vx, by, arg = "x")
  }
  doc <- if ("card_documented" %in% names(vx)) {
    !is.na(vx$card_documented) & vx$card_documented
  } else {
    rep(TRUE, nrow(vx))
  }
  prec <- vx$card_date_precision %||% rep(NA_character_, nrow(vx))
  grp <- if (length(by)) {
    do.call(paste, c(lapply(by, function(v) as.character(vx[[v]])), sep = " | "))
  } else {
    rep("<overall>", nrow(vx))
  }
  idx <- split(seq_len(nrow(vx)), grp)
  out <- tibble::tibble(
    group = names(idx),
    n_documented = unname(vapply(idx, function(i) sum(doc[i]), integer(1))),
    n_dated = unname(vapply(idx, function(i) sum(doc[i] & !is.na(vx$card_date[i])),
                            integer(1))),
    n_partial = unname(vapply(idx, function(i) {
      sum(doc[i] & prec[i] %in% c("month", "year"))
    }, integer(1)))
  )
  out$prop_dated <- ifelse(out$n_documented > 0, out$n_dated / out$n_documented,
                           NA_real_)
  out$prop_partial <- ifelse(out$n_documented > 0,
                             out$n_partial / out$n_documented, NA_real_)
  if (length(by)) {
    parts <- strsplit(out$group, " | ", fixed = TRUE)
    for (i in seq_along(by)) {
      out[[by[i]]] <- vapply(parts, function(p) {
        if (length(p) >= i) p[i] else NA_character_
      }, character(1))
    }
  }
  out$group <- NULL
  out
}

#' Summarise a survey end to end
#'
#' Assembles the tables a coverage report needs: the sample structure, card
#' availability, date completeness, the data-quality issue summary and, where
#' coverage has been derived, the coverage estimates.
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param design An optional [vcs_design()]. Built from `x` when omitted and
#'   coverage columns are present.
#' @param vaccines Vaccines to report. Defaults to all derived.
#' @param by Domains for the coverage tables.
#' @return A list of class `vcs_report` holding named tibbles.
#' @export
#' @seealso [vcs_quality_report()], [vcs_coverage_report()], [write_vcs_report()]
#' @examples
#' d <- derive_vaccination_status(vcs_example)
#' rep <- vcs_summary(d, by = ~stratum)
#' names(rep)
#' rep$coverage
vcs_summary <- function(x, design = NULL, vaccines = NULL, by = NULL) {
  assert_vcs_data(x)
  by <- as_column_names(by)
  out <- list()

  out$structure <- tibble::tibble(
    metric = c("households", "children", "psus", "segments", "strata", "vaccines"),
    value = as.numeric(summary(x)$counts)
  )
  if ("card_seen" %in% names(vcs_children(x))) {
    out$card_availability <- card_completeness(x, by = by)
  }
  if ("card_date" %in% names(x$vaccinations)) {
    out$date_completeness <- date_completeness(x, by = "vaccine")
  }
  out$quality <- summary(validate_vcs(x))

  cov_cols <- grep("^cov_", names(x$children), value = TRUE)
  if (length(cov_cols)) {
    design <- design %||% suppressWarnings(vcs_design(x))
    out$coverage <- estimate_coverage(design, vaccines = vaccines, by = by)
    out$completeness <- coverage_completeness(x, by = by)
  }
  structure(out, class = "vcs_report")
}

#' @export
print.vcs_report <- function(x, ...) {
  cat(sprintf("<vcs_report: %d table(s)>\n", length(x)))
  for (nm in names(x)) {
    cat(sprintf("  %-18s %d x %d\n", nm, nrow(x[[nm]]), ncol(x[[nm]])))
  }
  invisible(x)
}

#' A data-quality report
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param validation An optional pre-computed
#'   [vcs_validation][new_vcs_validation]; recomputed when omitted.
#' @param by Grouping for the missingness tables, e.g. `~enumerator`.
#' @return A list of class `vcs_report` with `issues`, `by_rule`, `by_severity`,
#'   `missingness` and, where available, `card_availability` and
#'   `date_completeness`.
#' @export
#' @examples
#' q <- vcs_quality_report(vcs_example)
#' q$by_severity
vcs_quality_report <- function(x, validation = NULL, by = NULL) {
  assert_vcs_data(x)
  v <- validation %||% validate_vcs(x)
  if (!is_vcs_validation(v)) {
    vcs_abort("`validation` must be a `vcs_validation`.", class = "vaxsurvR_type_error")
  }
  out <- list(
    issues = issues(v),
    by_rule = summary(v, by = "rule"),
    by_severity = summary(v, by = "severity"),
    by_check = summary(v, by = "check"),
    missingness = vcs_missingness(x, level = "child", by = by)
  )
  if ("card_seen" %in% names(vcs_children(x))) {
    out$card_availability <- card_completeness(x, by = by)
  }
  if ("card_date" %in% names(x$vaccinations)) {
    out$date_completeness <- date_completeness(x, by = "vaccine")
  }
  structure(out, class = "vcs_report")
}

#' A coverage report
#'
#' @param design A [vcs_design()] or [vcs_data][new_vcs_data] object with
#'   derived coverage columns.
#' @param vaccines Vaccines to report.
#' @param by Domains.
#' @param dropout_pairs A list of `c(first, last)` vaccine pairs for dropout
#'   indicators.
#' @return A list of class `vcs_report` with `coverage`, `coverage_by_domain`
#'   and `dropout`.
#' @export
#' @examples
#' d <- derive_vaccination_status(vcs_example)
#' r <- vcs_coverage_report(vcs_design(d), vaccines = c("BCG", "PENTA1", "PENTA3"),
#'                          by = ~stratum)
#' r$dropout
vcs_coverage_report <- function(design, vaccines = NULL, by = NULL,
                                dropout_pairs = list(c("PENTA1", "PENTA3"),
                                                     c("BCG", "MCV1"))) {
  out <- list()
  out$coverage <- estimate_coverage(design, vaccines = vaccines)
  by <- as_column_names(by)
  if (length(by)) {
    out$coverage_by_domain <- estimate_coverage(design, vaccines = vaccines, by = by)
  }
  available <- sub("^cov_", "",
                   grep("^cov_", names(
                     if (is_vcs_design(design)) design$data else design$children
                   ), value = TRUE))
  pairs <- Filter(function(p) all(p %in% available), dropout_pairs)
  if (length(pairs)) {
    out$dropout <- dplyr::bind_rows(lapply(pairs, function(p) {
      estimate_dropout(design, p[1], p[2], by = if (length(by)) by else NULL)
    }))
  }
  structure(out, class = "vcs_report")
}

#' Write a report to disk
#'
#' Writes each table of a `vcs_report` as a CSV, or all of them as sheets of one
#' Excel workbook. Deliberately format-agnostic: the tables are plain data and
#' can be dropped into any Quarto, R Markdown, Word or Excel template.
#'
#' @param x A `vcs_report`, as returned by [vcs_summary()],
#'   [vcs_quality_report()] or [vcs_coverage_report()].
#' @param path Output path. A `.xlsx` path writes one workbook; a directory
#'   path writes one CSV per table.
#' @return `path`, invisibly.
#' @export
#' @examples
#' r <- vcs_quality_report(vcs_example)
#' dir <- file.path(tempdir(), "vcs-report")
#' write_vcs_report(r, dir)
#' list.files(dir)
#' unlink(dir, recursive = TRUE)
write_vcs_report <- function(x, path) {
  if (!inherits(x, "vcs_report")) {
    vcs_abort("`x` must be a `vcs_report`.", class = "vaxsurvR_type_error")
  }
  assert_string(path)
  if (identical(tolower(tools::file_ext(path)), "xlsx")) {
    assert_installed("openxlsx", "Writing an Excel report")
    sheets <- lapply(x, function(tb) as.data.frame(tb))
    # Excel sheet names are capped at 31 characters.
    names(sheets) <- substr(names(sheets), 1, 31)
    openxlsx::write.xlsx(sheets, path, overwrite = TRUE)
    return(invisible(path))
  }
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  for (nm in names(x)) {
    utils::write.csv(as.data.frame(x[[nm]]), file.path(path, paste0(nm, ".csv")),
                     row.names = FALSE, na = "", fileEncoding = "UTF-8")
  }
  invisible(path)
}
