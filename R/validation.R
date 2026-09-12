# ---------------------------------------------------------------------------
# The validation framework. Checks never print and never modify data: they
# return a vcs_validation object holding one row per issue, which downstream
# code can filter, join, export or feed to clean_vcs().
# ---------------------------------------------------------------------------

#' Severity levels used by vaxsurvR checks
#'
#' Ordered from least to most serious. `"INFO"` records something worth
#' knowing, `"WARNING"` something a human should look at, `"ERROR"` a value
#' that cannot be correct, and `"CRITICAL"` a problem that invalidates the
#' record or the survey structure.
#'
#' @return A character vector of severity levels, in increasing order.
#' @export
#' @examples
#' vcs_severities()
vcs_severities <- function() {
  c("INFO", "WARNING", "ERROR", "CRITICAL")
}

#' Construct a validation result
#'
#' Low-level constructor used by every `check_*()` function.
#'
#' @param issues A data frame of issues, or `NULL` for a clean result. Must
#'   contain the columns produced by [vcs_issue()] when non-empty.
#' @param checks A character vector naming the checks that were run.
#' @param meta A named list of context (row counts, thresholds used, ...).
#' @return An object of class `vcs_validation`.
#' @export
#' @examples
#' new_vcs_validation(checks = "check_unique_ids")
new_vcs_validation <- function(issues = NULL, checks = character(0), meta = list()) {
  if (is.null(issues)) {
    issues <- empty_issues()
  }
  assert_data(issues)
  if (!nrow(issues)) {
    # bind_rows() over an empty list yields a 0x0 tibble; normalise it so that
    # issues(x)$rule_id works on a clean result.
    issues <- empty_issues()
  } else {
    assert_columns(issues, names(empty_issues()), arg = "issues")
    issues$severity <- factor(as.character(issues$severity), levels = vcs_severities())
    if (anyNA(issues$severity)) {
      vcs_abort(
        sprintf("`severity` must be one of %s.", collapse_quote(vcs_severities())),
        class = "vaxsurvR_value_error"
      )
    }
  }
  structure(
    list(
      issues = tibble::as_tibble(issues),
      checks = as.character(checks),
      meta = utils::modifyList(list(created = vcs_timestamp()), meta)
    ),
    class = "vcs_validation"
  )
}

#' @noRd
empty_issues <- function() {
  tibble::tibble(
    check = character(0),
    rule_id = character(0),
    severity = factor(character(0), levels = vcs_severities()),
    level = character(0),
    record_id = character(0),
    variable = character(0),
    value = character(0),
    message = character(0)
  )
}

#' Build issue rows
#'
#' Vectorised helper for writing checks. All arguments are recycled to the
#' length of `record_id`.
#'
#' @param check Name of the check function raising the issue.
#' @param rule_id Stable machine-readable rule identifier, e.g. `"ID_DUP"`.
#' @param severity One of [vcs_severities()].
#' @param level Data level the issue applies to: `"household"`, `"child"`,
#'   `"vaccination"` or `"survey"`.
#' @param record_id Identifier of the offending record.
#' @param variable Variable the issue concerns.
#' @param value Offending value, as text.
#' @param message Human-readable description.
#' @return A tibble of issue rows.
#' @export
#' @examples
#' vcs_issue("check_unique_ids", "ID_DUP", "CRITICAL", "child",
#'           record_id = c("c1", "c2"), variable = "child_id",
#'           value = c("c1", "c1"), message = "duplicated child_id")
vcs_issue <- function(check, rule_id, severity, level,
                      record_id, variable = NA_character_,
                      value = NA_character_, message = NA_character_) {
  assert_string(check)
  assert_string(rule_id)
  assert_choice(severity, vcs_severities())
  assert_choice(level, c("household", "child", "vaccination", "survey"))
  n <- length(record_id)
  if (!n) {
    return(empty_issues())
  }
  rec <- function(x) {
    x <- as.character(x)
    if (length(x) == 1L) rep(x, n) else x
  }
  tibble::tibble(
    check = rep(check, n),
    rule_id = rep(rule_id, n),
    severity = factor(rep(severity, n), levels = vcs_severities()),
    level = rep(level, n),
    record_id = rec(record_id),
    variable = rec(variable),
    value = rec(value),
    message = rec(message)
  )
}

#' Test whether an object is a vcs_validation
#'
#' @param x An object.
#' @return A logical scalar.
#' @export
#' @examples
#' is_vcs_validation(new_vcs_validation())
is_vcs_validation <- function(x) inherits(x, "vcs_validation")

#' Extract the issues from a validation result
#'
#' @param x A [vcs_validation][new_vcs_validation] object.
#' @param severity Optional severity filter. Issues at or above the given level
#'   are kept when `min_severity = TRUE`; otherwise only exact matches.
#' @param min_severity Treat `severity` as a lower bound.
#' @param check Optional character vector of check names to keep.
#' @param ... Unused, for method extensibility.
#' @return A tibble of issues.
#' @export
#' @examples
#' v <- validate_vcs(vcs_example)
#' head(issues(v))
#' head(issues(v, severity = "ERROR"))
issues <- function(x, ...) {
  UseMethod("issues")
}

#' @rdname issues
#' @export
issues.vcs_validation <- function(x, severity = NULL, min_severity = TRUE,
                                  check = NULL, ...) {
  out <- x$issues
  if (!is.null(severity)) {
    assert_choice(severity, vcs_severities())
    assert_flag(min_severity)
    lvl <- match(severity, vcs_severities())
    keep <- if (min_severity) {
      as.integer(out$severity) >= lvl
    } else {
      as.integer(out$severity) == lvl
    }
    out <- out[keep, , drop = FALSE]
  }
  if (!is.null(check)) {
    assert_character(check, allow_null = FALSE)
    out <- out[out$check %in% check, , drop = FALSE]
  }
  out
}

#' @export
print.vcs_validation <- function(x, n = 10L, ...) {
  cat(sprintf("<vcs_validation: %d check(s), %d issue(s)>\n",
              length(x$checks), nrow(x$issues)))
  if (!nrow(x$issues)) {
    cat("  no issues found\n")
    return(invisible(x))
  }
  tab <- table(factor(as.character(x$issues$severity), levels = vcs_severities()))
  cat("  ", paste(sprintf("%s: %d", names(tab), as.integer(tab)), collapse = "  "),
      "\n", sep = "")
  top <- x$issues[order(-as.integer(x$issues$severity)), , drop = FALSE]
  show <- utils::head(top, n)
  cat("\n")
  for (i in seq_len(nrow(show))) {
    cat(sprintf("  [%s] %-12s %s: %s\n",
                as.character(show$severity[i]), show$rule_id[i],
                show$record_id[i], show$message[i]))
  }
  if (nrow(top) > n) {
    cat(sprintf("  ... and %d more; use issues() for the full table\n", nrow(top) - n))
  }
  invisible(x)
}

#' Summarise a validation result
#'
#' @param object A [vcs_validation][new_vcs_validation] object.
#' @param by Grouping for the summary: `"rule"` (default), `"check"`,
#'   `"severity"` or `"level"`.
#' @param ... Unused.
#' @return A tibble with one row per group and columns `n_issues` and
#'   `n_records`.
#' @export
#' @examples
#' summary(validate_vcs(vcs_example))
#' summary(validate_vcs(vcs_example), by = "severity")
summary.vcs_validation <- function(object, by = c("rule", "check", "severity", "level"),
                                   ...) {
  by <- match.arg(by)
  iss <- object$issues
  if (!nrow(iss)) {
    return(tibble::tibble(
      group = character(0), severity = character(0),
      n_issues = integer(0), n_records = integer(0)
    ))
  }
  key <- switch(by,
    rule = iss$rule_id,
    check = iss$check,
    severity = as.character(iss$severity),
    level = iss$level
  )
  split_idx <- split(seq_len(nrow(iss)), key)
  out <- tibble::tibble(
    group = names(split_idx),
    severity = unname(vapply(split_idx, function(i) {
      as.character(iss$severity[i][which.max(as.integer(iss$severity[i]))])
    }, character(1))),
    n_issues = unname(vapply(split_idx, length, integer(1))),
    n_records = unname(vapply(split_idx, function(i) length(unique(iss$record_id[i])),
                              integer(1)))
  )
  out[order(-match(out$severity, vcs_severities()), -out$n_issues), , drop = FALSE]
}

#' Plot a validation result
#'
#' Draws a horizontal bar chart of issue counts by rule, coloured by severity,
#' using base graphics so that no plotting dependency is required.
#'
#' @param x A [vcs_validation][new_vcs_validation] object.
#' @param top Maximum number of rules to show.
#' @param ... Passed to [graphics::barplot()].
#' @return `x`, invisibly.
#' @export
#' @examples
#' v <- validate_vcs(vcs_example)
#' plot(v)
plot.vcs_validation <- function(x, top = 15L, ...) {
  assert_number(top, lower = 1)
  s <- summary(x, by = "rule")
  if (!nrow(s)) {
    graphics::plot.new()
    graphics::title(main = "No data-quality issues found")
    return(invisible(x))
  }
  s <- utils::head(s, top)
  s <- s[order(s$n_issues), , drop = FALSE]
  cols <- c(INFO = "#8CB369", WARNING = "#E0A340",
            ERROR = "#C0504D", CRITICAL = "#7B241C")
  op <- graphics::par(mar = c(4, 12, 3, 1))
  on.exit(graphics::par(op), add = TRUE)
  graphics::barplot(
    stats::setNames(s$n_issues, s$group),
    horiz = TRUE, las = 1,
    col = unname(cols[s$severity]),
    xlab = "Issues", main = "Data-quality issues by rule",
    ...
  )
  graphics::legend("bottomright", legend = names(cols), fill = unname(cols),
                   bty = "n", cex = 0.8)
  invisible(x)
}

#' Combine validation results
#'
#' @param ... [vcs_validation][new_vcs_validation] objects.
#' @return A single `vcs_validation`.
#' @export
#' @examples
#' d <- vcs_example
#' c(check_unique_ids(d), check_duplicates(d))
c.vcs_validation <- function(...) {
  parts <- list(...)
  ok <- vapply(parts, is_vcs_validation, logical(1))
  if (!all(ok)) {
    vcs_abort("All arguments must be `vcs_validation` objects.",
              class = "vaxsurvR_type_error")
  }
  new_vcs_validation(
    issues = dplyr::bind_rows(lapply(parts, `[[`, "issues")),
    checks = unlist(lapply(parts, `[[`, "checks"), use.names = FALSE),
    meta = list(combined = length(parts))
  )
}

#' Does a validation result contain blocking issues?
#'
#' @param x A [vcs_validation][new_vcs_validation] object.
#' @param severity Minimum severity that counts as blocking.
#' @return A logical scalar.
#' @export
#' @examples
#' has_issues(validate_vcs(vcs_example), "CRITICAL")
has_issues <- function(x, severity = "ERROR") {
  if (!is_vcs_validation(x)) {
    vcs_abort("`x` must be a `vcs_validation`.", class = "vaxsurvR_type_error")
  }
  nrow(issues(x, severity = severity)) > 0L
}

#' @export
as.data.frame.vcs_validation <- function(x, ...) {
  as.data.frame(x$issues, ...)
}
