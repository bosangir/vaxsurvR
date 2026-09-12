# ---------------------------------------------------------------------------
# Cleaning with an audit trail.
#
# The governing principle is that a survey dataset is evidence. A value that
# looks wrong may be a transcription error, or it may be the only record of
# something unusual that really happened. So the default action of every rule
# is to flag, and any rule that does change a value writes a log line saying
# what it changed, from what, to what, and why.
# ---------------------------------------------------------------------------

#' Define a cleaning rule
#'
#' A rule is a condition, a target variable and an action. Conditions are
#' evaluated inside the table named by `level`, so they can refer to its
#' columns directly.
#'
#' @param id Short stable identifier, e.g. `"DATE_FUTURE"`.
#' @param description What the rule does and why.
#' @param level Table the rule applies to: `"child"`, `"vaccination"` or
#'   `"household"`.
#' @param where An expression selecting the records the rule applies to,
#'   evaluated within the table. Quoted automatically.
#' @param variable Variable the rule targets. Required for actions other than
#'   `"flag"`.
#' @param action One of:
#'   \describe{
#'     \item{`"flag"`}{add a logical flag column, change nothing (default)}
#'     \item{`"set_na"`}{set `variable` to `NA` for matching records}
#'     \item{`"replace"`}{set `variable` to `value` for matching records}
#'   }
#' @param value Replacement value for `action = "replace"`.
#' @param severity One of [vcs_severities()].
#' @param flag_name Name of the flag column for `action = "flag"`. Defaults to
#'   `flag_<id>` in lower case.
#' @return An object of class `vcs_rule`.
#' @export
#' @seealso [clean_vcs()], [vcs_default_rules()]
#' @examples
#' # Flag, do not remove, doses dated after the interview.
#' vcs_rule(
#'   "DATE_FUTURE", "Card date after the interview date",
#'   level = "vaccination",
#'   where = !is.na(card_date) & card_date > interview_date
#' )
#'
#' # A rule that does change a value must say so explicitly.
#' vcs_rule(
#'   "DOB_IMPOSSIBLE", "Date of birth before 1990 cannot be a child aged 12-23 months",
#'   level = "child", where = !is.na(child_dob) & child_dob < as.Date("1990-01-01"),
#'   variable = "child_dob", action = "set_na", severity = "ERROR"
#' )
vcs_rule <- function(id, description,
                     level = c("vaccination", "child", "household"),
                     where,
                     variable = NA_character_,
                     action = c("flag", "set_na", "replace"),
                     value = NULL,
                     severity = "WARNING",
                     flag_name = NULL) {
  assert_string(id)
  assert_string(description)
  level <- match.arg(level)
  action <- match.arg(action)
  assert_choice(severity, vcs_severities())
  if (action != "flag" && (is.na(variable) || !nzchar(variable))) {
    vcs_abort(
      sprintf("Action \"%s\" needs a `variable` to act on.", action),
      class = "vaxsurvR_rule_error"
    )
  }
  if (action == "replace" && is.null(value)) {
    vcs_abort("Action \"replace\" needs a `value`.", class = "vaxsurvR_rule_error")
  }
  flag_name <- flag_name %||% paste0("flag_", tolower(id))
  assert_string(flag_name)
  structure(
    list(
      id = id,
      description = description,
      level = level,
      where = rlang::enquo(where),
      variable = variable,
      action = action,
      value = value,
      severity = severity,
      flag_name = flag_name
    ),
    class = "vcs_rule"
  )
}

#' @export
print.vcs_rule <- function(x, ...) {
  cat(sprintf("<vcs_rule %s> [%s] %s on %s\n", x$id, x$severity, x$action, x$level))
  cat(sprintf("  %s\n", x$description))
  cat(sprintf("  where: %s\n", rlang::as_label(x$where)))
  if (!is.na(x$variable)) {
    cat(sprintf("  variable: %s\n", x$variable))
  }
  invisible(x)
}

#' Bundle cleaning rules
#'
#' @param ... [vcs_rule()] objects.
#' @return An object of class `vcs_ruleset`.
#' @export
#' @examples
#' vcs_ruleset(
#'   vcs_rule("A", "flag partial dates", level = "vaccination",
#'            where = card_date_precision %in% c("month", "year"))
#' )
vcs_ruleset <- function(...) {
  rules <- rlang::list2(...)
  if (length(rules) == 1L && is.list(rules[[1]]) && !inherits(rules[[1]], "vcs_rule")) {
    rules <- rules[[1]]
  }
  ok <- vapply(rules, inherits, logical(1), "vcs_rule")
  if (!all(ok)) {
    vcs_abort("All arguments must be `vcs_rule` objects.",
              class = "vaxsurvR_type_error")
  }
  ids <- vapply(rules, `[[`, character(1), "id")
  if (anyDuplicated(ids)) {
    vcs_abort(
      sprintf("Rule id%s used more than once: %s.",
              if (sum(duplicated(ids)) > 1L) "s" else "",
              collapse_quote(unique(ids[duplicated(ids)]))),
      class = "vaxsurvR_rule_error"
    )
  }
  structure(rules, class = "vcs_ruleset")
}

#' @export
print.vcs_ruleset <- function(x, ...) {
  cat(sprintf("<vcs_ruleset: %d rule(s)>\n", length(x)))
  for (r in x) {
    cat(sprintf("  %-18s [%-8s] %-7s %s\n", r$id, r$severity, r$action,
                r$description))
  }
  invisible(x)
}

#' A conservative default ruleset
#'
#' Every rule in this set flags; none changes a value. It marks the records
#' that the vaccination checks consider impossible or ambiguous so that a human
#' can adjudicate them, which is the only defensible default for survey data.
#'
#' @return A [vcs_ruleset()].
#' @export
#' @seealso [clean_vcs()]
#' @examples
#' vcs_default_rules()
vcs_default_rules <- function() {
  vcs_ruleset(
    vcs_rule(
      "DATE_FUTURE",
      "Card date is after the interview date",
      level = "vaccination",
      where = !is.na(card_date) & !is.na(interview_date) & card_date > interview_date,
      severity = "ERROR"
    ),
    vcs_rule(
      "DATE_PREBIRTH",
      "Card date is before the child's date of birth",
      level = "vaccination",
      where = !is.na(card_date) & !is.na(child_dob) & card_date < child_dob,
      severity = "CRITICAL"
    ),
    vcs_rule(
      "DATE_PARTIAL",
      "Card date is partial; a resolution rule must be chosen explicitly",
      level = "vaccination",
      where = !is.na(card_date_precision) & card_date_precision %in% c("month", "year"),
      severity = "WARNING"
    ),
    vcs_rule(
      "DATE_UNPARSEABLE",
      "Card date could not be parsed in any known format",
      level = "vaccination",
      where = !is.na(card_date_precision) & card_date_precision == "unparseable",
      severity = "ERROR"
    ),
    vcs_rule(
      "CARD_DATE_NO_CARD",
      "A card date exists although no card was seen",
      level = "vaccination",
      where = !is.na(card_date) & !is.na(card_seen) & !card_seen,
      severity = "ERROR"
    )
  )
}

#' Apply cleaning rules and record every change
#'
#' Evaluates each rule against the relevant table, applies its action, and
#' returns the data with a [vcs_audit][vcs_log] trail attached. The raw import
#' inside `x` is never touched.
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param rules A [vcs_ruleset()]. Defaults to [vcs_default_rules()], which
#'   only flags.
#' @param dry_run Evaluate the rules and build the log, but leave the data
#'   unchanged. Useful for reviewing what a ruleset would do.
#' @return `x`, with flag columns and/or modified values, and an `audit`
#'   element of class `vcs_audit`.
#' @export
#' @seealso [audit_changes()], [export_cleaning_log()], [vcs_rule()]
#' @examples
#' cleaned <- clean_vcs(vcs_example)
#' audit_changes(cleaned)
#' summary(cleaned$audit)
#'
#' # Nothing is changed by the default ruleset: it flags only.
#' identical(cleaned$vaccinations$card_date, vcs_example$vaccinations$card_date)
clean_vcs <- function(x, rules = vcs_default_rules(), dry_run = FALSE) {
  assert_vcs_data(x)
  assert_flag(dry_run)
  if (inherits(rules, "vcs_rule")) {
    rules <- vcs_ruleset(rules)
  }
  if (!inherits(rules, "vcs_ruleset")) {
    vcs_abort("`rules` must be a `vcs_ruleset` or a single `vcs_rule`.",
              class = "vaxsurvR_type_error")
  }

  log <- list()
  for (r in rules) {
    tab <- rule_context(x, r$level)
    if (!nrow(tab)) next
    hit <- tryCatch(
      rlang::eval_tidy(r$where, data = tab),
      error = function(e) {
        vcs_warn(
          sprintf("Rule \"%s\" could not be evaluated (%s); it was skipped.",
                  r$id, conditionMessage(e)),
          class = "vaxsurvR_rule_skipped"
        )
        NULL
      }
    )
    if (is.null(hit)) next
    if (is.logical(hit) && length(hit) == 1L) {
      hit <- rep(hit, nrow(tab))
    }
    if (!is.logical(hit) || length(hit) != nrow(tab)) {
      vcs_warn(
        sprintf("Rule \"%s\" did not return a logical vector of the right length; it was skipped.",
                r$id),
        class = "vaxsurvR_rule_skipped"
      )
      next
    }
    hit[is.na(hit)] <- FALSE
    idx <- which(hit)
    rid <- record_ids(tab, r$level)

    target <- if (r$level == "household") "households" else {
      if (r$level == "child") "children" else "vaccinations"
    }

    if (r$action == "flag") {
      flag <- rep(FALSE, nrow(x[[target]]))
      flag[idx] <- TRUE
      if (!dry_run) {
        x[[target]][[r$flag_name]] <- mark_derived(
          flag, sprintf("%s: %s", r$id, r$description)
        )
      }
      if (length(idx)) {
        log[[length(log) + 1L]] <- audit_rows(
          rid[idx], r, r$flag_name,
          original = rep(NA_character_, length(idx)),
          new = rep("TRUE", length(idx))
        )
      }
      next
    }

    if (!r$variable %in% names(x[[target]])) {
      vcs_warn(
        sprintf("Rule \"%s\" targets variable \"%s\", which is not present; it was skipped.",
                r$id, r$variable),
        class = "vaxsurvR_rule_skipped"
      )
      next
    }
    if (!length(idx)) next

    col <- x[[target]][[r$variable]]
    original <- as.character(col[idx])
    new_val <- switch(
      r$action,
      set_na = col[NA_integer_][1],
      replace = r$value
    )
    if (!dry_run) {
      col[idx] <- new_val
      x[[target]][[r$variable]] <- col
    }
    log[[length(log) + 1L]] <- audit_rows(
      rid[idx], r, r$variable,
      original = original,
      new = rep(as.character(new_val), length(idx))
    )
  }

  audit <- vcs_log(dplyr::bind_rows(log), dry_run = dry_run,
                   rules = vapply(rules, `[[`, character(1), "id"))
  x$audit <- audit
  x
}

#' The table a rule sees, with the joined columns its condition may need
#' @noRd
rule_context <- function(x, level) {
  if (level == "household") {
    return(x$households)
  }
  if (level == "child") {
    return(vcs_children(x))
  }
  vx <- x$vaccinations
  if (!nrow(vx)) {
    return(vx)
  }
  ch <- vcs_children(x)
  extra <- setdiff(
    intersect(c("child_dob", "interview_date", "age_months", "sex", "card_seen",
                "psu", "segment", "stratum", "health_zone", "enumerator"),
              names(ch)),
    names(vx)
  )
  if (length(extra)) {
    m <- match(vx$child_id, ch$child_id)
    for (nm in extra) {
      vx[[nm]] <- ch[[nm]][m]
    }
  }
  vx
}

#' @noRd
record_ids <- function(tab, level) {
  if (level == "vaccination" && all(c("child_id", "vaccine") %in% names(tab))) {
    return(paste(tab$child_id, tab$vaccine, sep = "/"))
  }
  for (nm in c("child_id", "interview_id", "household_id")) {
    if (nm %in% names(tab)) {
      return(as.character(tab[[nm]]))
    }
  }
  sprintf("<row %d>", seq_len(nrow(tab)))
}

#' @noRd
audit_rows <- function(record_id, rule, variable, original, new) {
  tibble::tibble(
    record_id = as.character(record_id),
    level = rule$level,
    variable = as.character(variable),
    original_value = as.character(original),
    new_value = as.character(new),
    rule_id = rule$id,
    rule_description = rule$description,
    severity = factor(rule$severity, levels = vcs_severities()),
    action = rule$action,
    timestamp = vcs_timestamp(),
    package_version = vcs_version()
  )
}

#' Create or wrap a cleaning audit log
#'
#' @param changes A data frame of change records, or `NULL` for an empty log.
#' @param dry_run Was the log produced without applying the changes?
#' @param rules Identifiers of the rules that were evaluated.
#' @return An object of class `vcs_audit`.
#' @export
#' @seealso [clean_vcs()], [audit_changes()]
#' @examples
#' vcs_log()
vcs_log <- function(changes = NULL, dry_run = FALSE, rules = character(0)) {
  if (is.null(changes) || !nrow(changes)) {
    changes <- empty_audit()
  }
  assert_data(changes)
  structure(
    list(
      changes = tibble::as_tibble(changes),
      meta = list(
        dry_run = dry_run,
        rules = as.character(rules),
        created = vcs_timestamp(),
        vaxsurvR_version = vcs_version()
      )
    ),
    class = "vcs_audit"
  )
}

#' @noRd
empty_audit <- function() {
  tibble::tibble(
    record_id = character(0), level = character(0), variable = character(0),
    original_value = character(0), new_value = character(0),
    rule_id = character(0), rule_description = character(0),
    severity = factor(character(0), levels = vcs_severities()),
    action = character(0), timestamp = character(0),
    package_version = character(0)
  )
}

#' Test whether an object is a vcs_audit
#'
#' @param x An object.
#' @return A logical scalar.
#' @export
#' @examples
#' is_vcs_audit(vcs_log())
is_vcs_audit <- function(x) inherits(x, "vcs_audit")

#' @export
print.vcs_audit <- function(x, n = 10L, ...) {
  cat(sprintf("<vcs_audit: %d change record(s) from %d rule(s)>%s\n",
              nrow(x$changes), length(x$meta$rules),
              if (isTRUE(x$meta$dry_run)) "  [dry run: data unchanged]" else ""))
  if (!nrow(x$changes)) {
    cat("  no changes recorded\n")
    return(invisible(x))
  }
  tab <- table(x$changes$action)
  cat("  ", paste(sprintf("%s: %d", names(tab), as.integer(tab)), collapse = "  "),
      "\n", sep = "")
  print(utils::head(tibble::as_tibble(x$changes), n))
  invisible(x)
}

#' @export
summary.vcs_audit <- function(object, ...) {
  ch <- object$changes
  if (!nrow(ch)) {
    return(tibble::tibble(rule_id = character(0), action = character(0),
                          severity = character(0), n_records = integer(0),
                          n_changes = integer(0)))
  }
  idx <- split(seq_len(nrow(ch)), ch$rule_id)
  out <- tibble::tibble(
    rule_id = names(idx),
    action = unname(vapply(idx, function(i) ch$action[i][1], character(1))),
    severity = unname(vapply(idx, function(i) as.character(ch$severity[i][1]),
                             character(1))),
    n_records = unname(vapply(idx, function(i) length(unique(ch$record_id[i])),
                              integer(1))),
    n_changes = unname(vapply(idx, length, integer(1)))
  )
  out[order(-match(out$severity, vcs_severities()), -out$n_changes), , drop = FALSE]
}

#' Extract the change log
#'
#' @param x A [vcs_data][new_vcs_data] object that has been through
#'   [clean_vcs()], or a [vcs_audit][vcs_log] object.
#' @param rule_id Optional filter on rule identifier.
#' @param action Optional filter on action.
#' @return A tibble with one row per recorded change, containing the record
#'   identifier, variable, original and resulting values, rule identifier and
#'   description, severity, action, timestamp and package version.
#' @export
#' @examples
#' cleaned <- clean_vcs(vcs_example)
#' audit_changes(cleaned)
#' audit_changes(cleaned, rule_id = "DATE_PREBIRTH")
audit_changes <- function(x, rule_id = NULL, action = NULL) {
  audit <- if (is_vcs_audit(x)) {
    x
  } else if (is_vcs_data(x)) {
    x$audit %||% vcs_log()
  } else {
    vcs_abort("`x` must be a `vcs_data` or a `vcs_audit`.",
              class = "vaxsurvR_type_error")
  }
  out <- audit$changes
  if (!is.null(rule_id)) {
    assert_character(rule_id, allow_null = FALSE)
    out <- out[out$rule_id %in% rule_id, , drop = FALSE]
  }
  if (!is.null(action)) {
    assert_character(action, allow_null = FALSE)
    out <- out[out$action %in% action, , drop = FALSE]
  }
  out
}

#' Export the cleaning log
#'
#' Writes the audit trail to CSV or Excel. Direct identifiers are never written:
#' the log records the identifier of the record and the value of the variable
#' the rule touched, and [deidentify_vcs()] should be run before any log leaves
#' the data-management team.
#'
#' @param x A [vcs_data][new_vcs_data] object that has been through
#'   [clean_vcs()], or a [vcs_audit][vcs_log] object.
#' @param path Output path. The extension chooses the format: `.csv` or
#'   `.xlsx`.
#' @param exclude_variables Variables whose original values must not appear in
#'   the exported log. Their `original_value` is replaced with `"<redacted>"`.
#' @return `path`, invisibly.
#' @export
#' @examples
#' cleaned <- clean_vcs(vcs_example)
#' f <- tempfile(fileext = ".csv")
#' export_cleaning_log(cleaned, f)
#' head(read.csv(f), 3)
#' unlink(f)
export_cleaning_log <- function(x, path, exclude_variables = character(0)) {
  assert_string(path)
  assert_character(exclude_variables, allow_null = FALSE)
  out <- audit_changes(x)
  if (length(exclude_variables)) {
    hit <- out$variable %in% exclude_variables
    out$original_value[hit] <- "<redacted>"
    out$new_value[hit] <- "<redacted>"
  }
  ext <- tolower(tools::file_ext(path))
  switch(
    ext,
    csv = utils::write.csv(out, path, row.names = FALSE, na = "",
                           fileEncoding = "UTF-8"),
    xlsx = {
      assert_installed("openxlsx", "Writing an Excel cleaning log")
      openxlsx::write.xlsx(as.data.frame(out), path, overwrite = TRUE)
    },
    vcs_abort(
      sprintf("Unsupported log extension \"%s\"; use .csv or .xlsx.", ext),
      class = "vaxsurvR_io_error"
    )
  )
  invisible(path)
}
