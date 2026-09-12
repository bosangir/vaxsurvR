# ---------------------------------------------------------------------------
# Vaccination data quality. Every rule reads its parameters from the
# vcs_schedule; nothing here assumes a particular EPI calendar.
#
# These functions flag. They never delete a record and never overwrite a date:
# a card date that looks wrong is often a transcription error worth resolving
# in the field, not a value the analyst should quietly discard.
# ---------------------------------------------------------------------------

#' Prepare the vaccination table joined to child dates
#' @noRd
vx_with_dates <- function(x) {
  vx <- x$vaccinations
  if (!nrow(vx)) {
    return(vx)
  }
  ch <- vcs_children(x)
  keep <- setdiff(
    intersect(c("child_dob", "interview_date", "age_months"), names(ch)),
    names(vx)
  )
  if (length(keep)) {
    # A positional lookup rather than a join: duplicate child identifiers are a
    # data-quality problem this function must survive, not amplify.
    m <- match(vx$child_id, ch$child_id)
    for (nm in keep) {
      vx[[nm]] <- ch[[nm]][m]
    }
  }
  vx
}

#' @noRd
no_dates_result <- function(check) {
  new_vcs_validation(
    vcs_issue(check, "VX_NO_DATES", "INFO", "survey",
              record_id = "<survey>", variable = "card_date", value = NA_character_,
              message = "No card dates are mapped; this check did not run."),
    check
  )
}

#' Check vaccination dates for parseability and plausibility
#'
#' Reports doses recorded as documented on the card but with no usable date,
#' dates that could not be parsed at all, partial dates, and dates outside a
#' plausible calendar range.
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param earliest Earliest plausible vaccination date.
#' @param latest Latest plausible vaccination date. Defaults to today.
#' @return A [vcs_validation][new_vcs_validation] object.
#' @export
#' @family vaccination checks
#' @examples
#' check_vaccine_dates(vcs_example)
check_vaccine_dates <- function(x, earliest = as.Date("1980-01-01"),
                                latest = Sys.Date()) {
  assert_vcs_data(x)
  vx <- x$vaccinations
  if (!nrow(vx) || !"card_date" %in% names(vx)) {
    return(no_dates_result("check_vaccine_dates"))
  }
  earliest <- as_date_safe(earliest)
  latest <- as_date_safe(latest)
  iss <- list()
  rid <- paste(vx$child_id, vx$vaccine, sep = "/")

  if ("card_date_precision" %in% names(vx)) {
    bad <- vx$card_date_precision == "unparseable"
    if (any(bad, na.rm = TRUE)) {
      bad[is.na(bad)] <- FALSE
      iss[[length(iss) + 1L]] <- vcs_issue(
        "check_vaccine_dates", "DATE_UNPARSEABLE", "ERROR", "vaccination",
        record_id = rid[bad], variable = "card_date",
        value = vx$card_date_raw[bad],
        message = "Card date could not be parsed in any known format."
      )
    }
    partial <- vx$card_date_precision %in% c("month", "year")
    if (any(partial)) {
      iss[[length(iss) + 1L]] <- vcs_issue(
        "check_vaccine_dates", "DATE_PARTIAL", "WARNING", "vaccination",
        record_id = rid[partial], variable = "card_date",
        value = vx$card_date_raw[partial],
        message = "Card date is partial; resolve it with an explicit rule via validate_partial_date()."
      )
    }
  }
  if ("card_documented" %in% names(vx)) {
    undated <- !is.na(vx$card_documented) & vx$card_documented & is.na(vx$card_date)
    if (any(undated)) {
      iss[[length(iss) + 1L]] <- vcs_issue(
        "check_vaccine_dates", "DATE_MISSING_ON_CARD", "WARNING", "vaccination",
        record_id = rid[undated], variable = "card_date", value = NA_character_,
        message = "Dose recorded as card-documented but no usable date is present."
      )
    }
  }
  early <- !is.na(vx$card_date) & vx$card_date < earliest
  if (any(early)) {
    iss[[length(iss) + 1L]] <- vcs_issue(
      "check_vaccine_dates", "DATE_IMPLAUSIBLE", "ERROR", "vaccination",
      record_id = rid[early], variable = "card_date",
      value = as.character(vx$card_date[early]),
      message = sprintf("Vaccination date is before %s.", earliest)
    )
  }
  late <- !is.na(vx$card_date) & vx$card_date > latest
  if (any(late)) {
    iss[[length(iss) + 1L]] <- vcs_issue(
      "check_vaccine_dates", "DATE_AFTER_LIMIT", "ERROR", "vaccination",
      record_id = rid[late], variable = "card_date",
      value = as.character(vx$card_date[late]),
      message = sprintf("Vaccination date is after %s.", latest)
    )
  }
  new_vcs_validation(dplyr::bind_rows(iss), "check_vaccine_dates",
                     meta = list(earliest = earliest, latest = latest))
}

#' Check for vaccination dates in the future
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param reference Date after which a vaccination date is impossible. Defaults
#'   to each child's interview date where available, otherwise today.
#' @return A [vcs_validation][new_vcs_validation] object.
#' @export
#' @family vaccination checks
#' @examples
#' check_future_dates(vcs_example)
check_future_dates <- function(x, reference = NULL) {
  assert_vcs_data(x)
  vx <- vx_with_dates(x)
  if (!nrow(vx) || !"card_date" %in% names(vx)) {
    return(no_dates_result("check_future_dates"))
  }
  ref <- if (!is.null(reference)) {
    r <- as_date_safe(reference)
    if (length(r) == 1L) rep(r, nrow(vx)) else r
  } else if ("interview_date" %in% names(vx)) {
    r <- vx$interview_date
    r[is.na(r)] <- Sys.Date()
    r
  } else {
    rep(Sys.Date(), nrow(vx))
  }
  future <- !is.na(vx$card_date) & vx$card_date > ref
  iss <- if (any(future)) {
    vcs_issue(
      "check_future_dates", "DATE_FUTURE", "ERROR", "vaccination",
      record_id = paste(vx$child_id, vx$vaccine, sep = "/")[future],
      variable = "card_date", value = as.character(vx$card_date[future]),
      message = "Vaccination date is after the interview date."
    )
  } else {
    NULL
  }
  new_vcs_validation(iss, "check_future_dates")
}

#' Check for vaccination recorded before birth
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param tolerance_days Days before birth tolerated before flagging. Zero by
#'   default; a dose cannot precede the child.
#' @return A [vcs_validation][new_vcs_validation] object.
#' @export
#' @family vaccination checks
#' @examples
#' check_prebirth_vaccination(vcs_example)
check_prebirth_vaccination <- function(x, tolerance_days = 0) {
  assert_vcs_data(x)
  assert_number(tolerance_days, lower = 0)
  vx <- vx_with_dates(x)
  if (!nrow(vx) || !all(c("card_date", "child_dob") %in% names(vx))) {
    return(new_vcs_validation(
      vcs_issue("check_prebirth_vaccination", "VX_NO_DOB", "INFO", "survey",
                record_id = "<survey>", variable = "child_dob",
                value = NA_character_,
                message = "Card dates or dates of birth are unavailable; this check did not run."),
      "check_prebirth_vaccination"
    ))
  }
  age <- derive_age_at_vaccination(vx$child_dob, vx$card_date)
  bad <- !is.na(age) & age < -tolerance_days
  iss <- if (any(bad)) {
    vcs_issue(
      "check_prebirth_vaccination", "DATE_PREBIRTH", "CRITICAL", "vaccination",
      record_id = paste(vx$child_id, vx$vaccine, sep = "/")[bad],
      variable = "card_date", value = as.character(vx$card_date[bad]),
      message = sprintf("Vaccination recorded %d day(s) before the date of birth.",
                        abs(age[bad]))
    )
  } else {
    NULL
  }
  new_vcs_validation(iss, "check_prebirth_vaccination")
}

#' Check age at vaccination against the schedule minimum
#'
#' A dose given below `minimum_age_days` is biologically implausible or,
#' more often, a transcription error. It is flagged, never corrected.
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param schedule A [vcs_schedule()]. Defaults to the schedule stored in `x`.
#' @param tolerance_days Slack allowed below the schedule minimum before
#'   flagging.
#' @return A [vcs_validation][new_vcs_validation] object.
#' @export
#' @family vaccination checks
#' @examples
#' check_age_at_vaccination(vcs_example)
#' check_age_at_vaccination(vcs_example, tolerance_days = 7)
check_age_at_vaccination <- function(x, schedule = NULL, tolerance_days = 0) {
  assert_vcs_data(x)
  assert_number(tolerance_days, lower = 0)
  schedule <- schedule %||% x$schedule
  if (is.null(schedule)) {
    return(new_vcs_validation(
      vcs_issue("check_age_at_vaccination", "VX_NO_SCHEDULE", "INFO", "survey",
                record_id = "<survey>", variable = NA_character_,
                value = NA_character_,
                message = "No schedule available; this check did not run."),
      "check_age_at_vaccination"
    ))
  }
  assert_schedule(schedule)
  vx <- vx_with_dates(x)
  if (!nrow(vx) || !all(c("card_date", "child_dob") %in% names(vx))) {
    return(new_vcs_validation(
      vcs_issue("check_age_at_vaccination", "VX_NO_DOB", "INFO", "survey",
                record_id = "<survey>", variable = "child_dob",
                value = NA_character_,
                message = "Card dates or dates of birth are unavailable; this check did not run."),
      "check_age_at_vaccination"
    ))
  }
  age <- derive_age_at_vaccination(vx$child_dob, vx$card_date)
  lo <- schedule$minimum_age_days[match(vx$vaccine, schedule$vaccine)]
  bad <- !is.na(age) & !is.na(lo) & age >= 0 & age < (lo - tolerance_days)
  iss <- if (any(bad)) {
    vcs_issue(
      "check_age_at_vaccination", "AGE_BELOW_MINIMUM", "ERROR", "vaccination",
      record_id = paste(vx$child_id, vx$vaccine, sep = "/")[bad],
      variable = "card_date", value = as.character(age[bad]),
      message = sprintf("Given at %d days, below the schedule minimum of %g days.",
                        age[bad], lo[bad])
    )
  } else {
    NULL
  }
  new_vcs_validation(iss, "check_age_at_vaccination",
                     meta = list(tolerance_days = tolerance_days))
}

#' Check that doses follow the schedule sequence
#'
#' Flags a dose recorded on or before its predecessor, and a dose recorded
#' without its predecessor being recorded at all.
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param schedule A [vcs_schedule()]. Defaults to the schedule stored in `x`.
#' @param missing_previous_severity Severity for a dose whose predecessor is
#'   absent. Defaults to `"WARNING"`: a missing earlier dose can be a genuine
#'   out-of-schedule vaccination rather than a data error.
#' @return A [vcs_validation][new_vcs_validation] object.
#' @export
#' @family vaccination checks
#' @examples
#' check_vaccine_sequence(vcs_example)
check_vaccine_sequence <- function(x, schedule = NULL,
                                   missing_previous_severity = "WARNING") {
  assert_vcs_data(x)
  assert_choice(missing_previous_severity, vcs_severities())
  schedule <- schedule %||% x$schedule
  if (is.null(schedule) || !nrow(x$vaccinations)) {
    return(new_vcs_validation(checks = "check_vaccine_sequence"))
  }
  assert_schedule(schedule)
  vx <- x$vaccinations
  if (!"card_date" %in% names(vx)) {
    return(no_dates_result("check_vaccine_sequence"))
  }
  has_prev <- which(!is.na(schedule$previous_dose))
  iss <- list()
  for (i in has_prev) {
    this <- schedule$vaccine[i]
    prev <- schedule$previous_dose[i]
    a <- vx[vx$vaccine == this, , drop = FALSE]
    b <- vx[vx$vaccine == prev, , drop = FALSE]
    if (!nrow(a) || !nrow(b)) next
    m <- match(a$child_id, b$child_id)
    prev_date <- b$card_date[m]
    prev_doc <- if ("card_documented" %in% names(b)) b$card_documented[m] else NA
    this_doc <- if ("card_documented" %in% names(a)) a$card_documented else NA

    out_of_order <- !is.na(a$card_date) & !is.na(prev_date) & a$card_date < prev_date
    if (any(out_of_order)) {
      iss[[length(iss) + 1L]] <- vcs_issue(
        "check_vaccine_sequence", "SEQ_OUT_OF_ORDER", "ERROR", "vaccination",
        record_id = paste(a$child_id, this, sep = "/")[out_of_order],
        variable = "card_date", value = as.character(a$card_date[out_of_order]),
        message = sprintf("%s recorded before %s (%s).", this, prev,
                          as.character(prev_date[out_of_order]))
      )
    }
    orphan <- !is.na(this_doc) & this_doc & !is.na(prev_doc) & !prev_doc
    if (any(orphan)) {
      iss[[length(iss) + 1L]] <- vcs_issue(
        "check_vaccine_sequence", "SEQ_MISSING_PREVIOUS",
        missing_previous_severity, "vaccination",
        record_id = paste(a$child_id, this, sep = "/")[orphan],
        variable = "card_status", value = NA_character_,
        message = sprintf("%s is documented but %s is not.", this, prev)
      )
    }
  }
  new_vcs_validation(dplyr::bind_rows(iss), "check_vaccine_sequence")
}

#' Check minimum intervals between consecutive doses
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param schedule A [vcs_schedule()]. Defaults to the schedule stored in `x`.
#' @param tolerance_days Slack allowed below the schedule minimum interval.
#' @return A [vcs_validation][new_vcs_validation] object.
#' @export
#' @family vaccination checks
#' @examples
#' check_dose_intervals(vcs_example)
check_dose_intervals <- function(x, schedule = NULL, tolerance_days = 0) {
  assert_vcs_data(x)
  assert_number(tolerance_days, lower = 0)
  schedule <- schedule %||% x$schedule
  if (is.null(schedule) || !nrow(x$vaccinations)) {
    return(new_vcs_validation(checks = "check_dose_intervals"))
  }
  assert_schedule(schedule)
  vx <- x$vaccinations
  if (!"card_date" %in% names(vx)) {
    return(no_dates_result("check_dose_intervals"))
  }
  target <- which(!is.na(schedule$previous_dose) &
                    !is.na(schedule$minimum_interval_days))
  iss <- list()
  for (i in target) {
    this <- schedule$vaccine[i]
    prev <- schedule$previous_dose[i]
    gap_min <- schedule$minimum_interval_days[i]
    a <- vx[vx$vaccine == this, , drop = FALSE]
    b <- vx[vx$vaccine == prev, , drop = FALSE]
    if (!nrow(a) || !nrow(b)) next
    prev_date <- b$card_date[match(a$child_id, b$child_id)]
    gap <- as.numeric(a$card_date - prev_date)
    bad <- !is.na(gap) & gap >= 0 & gap < (gap_min - tolerance_days)
    if (any(bad)) {
      iss[[length(iss) + 1L]] <- vcs_issue(
        "check_dose_intervals", "INTERVAL_TOO_SHORT", "ERROR", "vaccination",
        record_id = paste(a$child_id, this, sep = "/")[bad],
        variable = "card_date", value = as.character(gap[bad]),
        message = sprintf("Only %g day(s) after %s; the schedule requires %g.",
                          gap[bad], prev, gap_min)
      )
    }
  }
  new_vcs_validation(dplyr::bind_rows(iss), "check_dose_intervals",
                     meta = list(tolerance_days = tolerance_days))
}

#' Check for duplicated doses on the same day
#'
#' Two doses of the *same antigen* recorded on the same date are almost always
#' a transcription error: the same line of the card read twice.
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @return A [vcs_validation][new_vcs_validation] object.
#' @export
#' @family vaccination checks
#' @examples
#' check_duplicate_doses(vcs_example)
check_duplicate_doses <- function(x) {
  assert_vcs_data(x)
  vx <- x$vaccinations
  if (!nrow(vx) || !"card_date" %in% names(vx) || !"antigen" %in% names(vx)) {
    return(no_dates_result("check_duplicate_doses"))
  }
  dated <- vx[!is.na(vx$card_date), , drop = FALSE]
  if (!nrow(dated)) {
    return(new_vcs_validation(checks = "check_duplicate_doses"))
  }
  key <- paste(dated$child_id, dated$antigen, dated$card_date, sep = "\r")
  dup <- key %in% key[duplicated(key)]
  iss <- if (any(dup)) {
    vcs_issue(
      "check_duplicate_doses", "DOSE_DUPLICATE_DATE", "ERROR", "vaccination",
      record_id = paste(dated$child_id, dated$vaccine, sep = "/")[dup],
      variable = "card_date", value = as.character(dated$card_date[dup]),
      message = "Two doses of the same antigen carry the same card date."
    )
  } else {
    NULL
  }
  new_vcs_validation(iss, "check_duplicate_doses")
}

#' Check card and recall evidence against each other
#'
#' Compares the card-documented and caregiver-recalled status of each dose.
#' Disagreement is expected in coverage surveys and is *not* an error: it is
#' reported at `INFO` by default so that the rate can be monitored, because a
#' PSU or interviewer with an unusual disagreement rate is worth a look.
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param severity Severity to report disagreements at.
#' @return A [vcs_validation][new_vcs_validation] object.
#' @export
#' @family vaccination checks
#' @examples
#' check_card_transcription(vcs_example)
#' summary(check_card_transcription(vcs_example, severity = "WARNING"))
check_card_transcription <- function(x, severity = "INFO") {
  assert_vcs_data(x)
  assert_choice(severity, vcs_severities())
  vx <- x$vaccinations
  if (!nrow(vx) || !all(c("card_documented", "recall_reported") %in% names(vx))) {
    return(new_vcs_validation(
      vcs_issue("check_card_transcription", "VX_NO_RECALL", "INFO", "survey",
                record_id = "<survey>", variable = "recall_status",
                value = NA_character_,
                message = "Card or recall evidence is unavailable; this check did not run."),
      "check_card_transcription"
    ))
  }
  iss <- list()
  rid <- paste(vx$child_id, vx$vaccine, sep = "/")
  both <- !is.na(vx$card_documented) & !is.na(vx$recall_reported)

  card_only <- both & vx$card_documented & !vx$recall_reported
  if (any(card_only)) {
    iss[[length(iss) + 1L]] <- vcs_issue(
      "check_card_transcription", "EVID_CARD_NOT_RECALL", severity, "vaccination",
      record_id = rid[card_only], variable = "recall_status", value = "no",
      message = "Dose is documented on the card but the caregiver reported no."
    )
  }
  recall_only <- both & !vx$card_documented & vx$recall_reported
  if (any(recall_only)) {
    iss[[length(iss) + 1L]] <- vcs_issue(
      "check_card_transcription", "EVID_RECALL_NOT_CARD", severity, "vaccination",
      record_id = rid[recall_only], variable = "card_status", value = "no",
      message = "Caregiver reported the dose but the card does not document it."
    )
  }
  # A card date with no card sighting is a genuine inconsistency.
  if ("card_seen" %in% names(vcs_children(x))) {
    ch <- vcs_children(x)
    seen <- ch$card_seen[match(vx$child_id, ch$child_id)]
    ghost <- !is.na(seen) & !seen & !is.na(vx$card_date)
    if (any(ghost)) {
      iss[[length(iss) + 1L]] <- vcs_issue(
        "check_card_transcription", "EVID_DATE_WITHOUT_CARD", "ERROR", "vaccination",
        record_id = rid[ghost], variable = "card_date",
        value = as.character(vx$card_date[ghost]),
        message = "A card date is recorded although no card was seen."
      )
    }
  }
  new_vcs_validation(dplyr::bind_rows(iss), "check_card_transcription")
}

#' Flag all vaccination inconsistencies at the record level
#'
#' Runs the vaccination checks and collapses the result into one row per
#' child-vaccine record, so that flags can be joined back onto the analysis
#' table. Records are flagged, never dropped.
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param checks Vaccination checks to run.
#' @param ... Passed to the individual checks.
#' @return A tibble with one row per flagged child-vaccine record, and columns
#'   `child_id`, `vaccine`, `n_flags`, `max_severity` and `rules`.
#' @export
#' @family vaccination checks
#' @examples
#' head(flag_vaccine_inconsistencies(vcs_example))
flag_vaccine_inconsistencies <- function(x, checks = NULL, ...) {
  assert_vcs_data(x)
  if (is.null(checks)) {
    checks <- c("check_vaccine_dates", "check_future_dates",
                "check_prebirth_vaccination", "check_age_at_vaccination",
                "check_vaccine_sequence", "check_dose_intervals",
                "check_duplicate_doses", "check_card_transcription")
  }
  v <- validate_vcs(x, checks = checks, ...)
  iss <- issues(v)
  iss <- iss[iss$level == "vaccination", , drop = FALSE]
  if (!nrow(iss)) {
    return(tibble::tibble(
      child_id = character(0), vaccine = character(0), n_flags = integer(0),
      max_severity = character(0), rules = character(0)
    ))
  }
  parts <- strsplit(iss$record_id, "/", fixed = TRUE)
  iss$child_id <- vapply(parts, function(p) p[1], character(1))
  iss$vaccine <- vapply(parts, function(p) {
    if (length(p) > 1L) paste(p[-1], collapse = "/") else NA_character_
  }, character(1))
  grp <- split(seq_len(nrow(iss)), paste(iss$child_id, iss$vaccine, sep = "\r"))
  out <- tibble::tibble(
    child_id = unname(vapply(grp, function(i) iss$child_id[i][1], character(1))),
    vaccine = unname(vapply(grp, function(i) iss$vaccine[i][1], character(1))),
    n_flags = unname(vapply(grp, length, integer(1))),
    max_severity = unname(vapply(grp, function(i) {
      as.character(iss$severity[i][which.max(as.integer(iss$severity[i]))])
    }, character(1))),
    rules = unname(vapply(grp, function(i) {
      paste(sort(unique(iss$rule_id[i])), collapse = ";")
    }, character(1)))
  )
  out[order(-match(out$max_severity, vcs_severities()), out$child_id), , drop = FALSE]
}
