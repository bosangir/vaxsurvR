# ---------------------------------------------------------------------------
# Structural validation: identifiers, household composition, sampling
# structure. These checks answer "is this dataset shaped like a household
# cluster survey?" before any vaccination logic is applied.
# ---------------------------------------------------------------------------

#' Check that required concepts were mapped and are populated
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param required Concepts that must be present. Defaults to the concepts
#'   marked required in [vcs_concepts()].
#' @return A [vcs_validation][new_vcs_validation] object.
#' @export
#' @family structural checks
#' @examples
#' check_required_variables(vcs_example)
check_required_variables <- function(x, required = NULL) {
  assert_vcs_data(x)
  assert_character(required)
  if (is.null(required)) {
    cc <- vcs_concepts()
    required <- cc$concept[cc$required]
  }
  tables <- list(child = x$children, household = x$households)
  iss <- list()
  cc <- vcs_concepts()
  for (nm in required) {
    lvl <- cc$level[cc$concept == nm]
    if (!length(lvl)) lvl <- "child"
    tb <- tables[[if (lvl == "household") "household" else "child"]]
    if (!nm %in% names(tb)) {
      iss[[length(iss) + 1L]] <- vcs_issue(
        "check_required_variables", "REQ_MISSING", "CRITICAL", "survey",
        record_id = "<survey>", variable = nm, value = NA_character_,
        message = sprintf("Required concept \"%s\" is not mapped.", nm)
      )
      next
    }
    n_missing <- sum(is_blank(tb[[nm]]))
    if (n_missing) {
      iss[[length(iss) + 1L]] <- vcs_issue(
        "check_required_variables", "REQ_INCOMPLETE", "ERROR", "survey",
        record_id = "<survey>", variable = nm, value = as.character(n_missing),
        message = sprintf("Required concept \"%s\" is missing for %d of %d records.",
                          nm, n_missing, nrow(tb))
      )
    }
  }
  new_vcs_validation(
    issues = dplyr::bind_rows(iss),
    checks = "check_required_variables",
    meta = list(required = required)
  )
}

#' Check that identifiers are unique
#'
#' Duplicate child or household identifiers break every downstream join, so
#' they are reported as `CRITICAL`.
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param id_vars Identifier columns to check. Defaults to `child_id` at the
#'   child level and `interview_id` at the household level.
#' @return A [vcs_validation][new_vcs_validation] object.
#' @export
#' @family structural checks
#' @examples
#' check_unique_ids(vcs_example)
check_unique_ids <- function(x, id_vars = NULL) {
  assert_vcs_data(x)
  assert_character(id_vars)
  targets <- if (is.null(id_vars)) {
    list(child = "child_id", household = "interview_id")
  } else {
    stats::setNames(
      as.list(id_vars),
      vapply(id_vars, function(v) {
        if (v %in% names(x$children)) "child" else "household"
      }, character(1))
    )
  }
  iss <- list()
  for (i in seq_along(targets)) {
    lvl <- names(targets)[i]
    nm <- targets[[i]]
    tb <- if (lvl == "child") x$children else x$households
    if (!nm %in% names(tb)) next
    v <- as.character(tb[[nm]])
    dup <- v %in% v[duplicated(v)] & !is_blank(v)
    if (any(dup)) {
      iss[[length(iss) + 1L]] <- vcs_issue(
        "check_unique_ids", "ID_DUPLICATE", "CRITICAL", lvl,
        record_id = v[dup], variable = nm, value = v[dup],
        message = sprintf("Identifier \"%s\" is not unique.", nm)
      )
    }
    blank <- is_blank(v)
    if (any(blank)) {
      iss[[length(iss) + 1L]] <- vcs_issue(
        "check_unique_ids", "ID_MISSING", "CRITICAL", lvl,
        record_id = sprintf("<row %d>", which(blank)), variable = nm,
        value = NA_character_,
        message = sprintf("Identifier \"%s\" is missing.", nm)
      )
    }
  }
  new_vcs_validation(dplyr::bind_rows(iss), "check_unique_ids")
}

#' Check for duplicated records
#'
#' Flags records that repeat across a combination of variables -- the signature
#' of an interview submitted twice, or of a household visited by two teams.
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param by Character vector of child-level columns defining a duplicate.
#'   Defaults to household, caregiver and child identity plus date of birth.
#' @param severity Severity to report. Duplicates are ambiguous rather than
#'   impossible, so the default is `"WARNING"`.
#' @return A [vcs_validation][new_vcs_validation] object.
#' @export
#' @family structural checks
#' @examples
#' check_duplicates(vcs_example)
check_duplicates <- function(x, by = NULL, severity = "WARNING") {
  assert_vcs_data(x)
  assert_character(by)
  assert_choice(severity, vcs_severities())
  ch <- vcs_children(x)
  if (is.null(by)) {
    by <- intersect(c("household_id", "child_dob", "sex"), names(ch))
  }
  by <- intersect(by, names(ch))
  if (!length(by) || !nrow(ch)) {
    return(new_vcs_validation(checks = "check_duplicates"))
  }
  key <- do.call(paste, c(lapply(by, function(v) as.character(ch[[v]])), sep = "\r"))
  usable <- !vapply(seq_len(nrow(ch)), function(i) {
    all(vapply(by, function(v) is_blank(ch[[v]][i]), logical(1)))
  }, logical(1))
  dup <- usable & key %in% key[duplicated(key) & usable]
  iss <- if (any(dup)) {
    vcs_issue(
      "check_duplicates", "REC_DUPLICATE", severity, "child",
      record_id = ch$child_id[dup], variable = paste(by, collapse = "+"),
      value = key[dup],
      message = sprintf("Record duplicated on %s.", paste(by, collapse = " + "))
    )
  } else {
    NULL
  }
  new_vcs_validation(iss, "check_duplicates", meta = list(by = by))
}

#' Check household structure
#'
#' Verifies that every child belongs to a household present in the household
#' table, that households report a plausible number of eligible children, and
#' that the reported count matches the number of child modules completed.
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param max_children Number of children above which a household is flagged for
#'   review.
#' @return A [vcs_validation][new_vcs_validation] object.
#' @export
#' @family structural checks
#' @examples
#' check_household_structure(vcs_example)
check_household_structure <- function(x, max_children = 6L) {
  assert_vcs_data(x)
  assert_number(max_children, lower = 1)
  ch <- x$children
  hh <- x$households
  iss <- list()

  if (nrow(ch) && "interview_id" %in% names(ch) && "interview_id" %in% names(hh)) {
    orphan <- !ch$interview_id %in% hh$interview_id
    if (any(orphan)) {
      iss[[length(iss) + 1L]] <- vcs_issue(
        "check_household_structure", "HH_ORPHAN_CHILD", "CRITICAL", "child",
        record_id = ch$child_id[orphan], variable = "interview_id",
        value = ch$interview_id[orphan],
        message = "Child does not belong to any household in the household table."
      )
    }
    counts <- table(ch$interview_id)
    big <- names(counts)[counts > max_children]
    if (length(big)) {
      iss[[length(iss) + 1L]] <- vcs_issue(
        "check_household_structure", "HH_MANY_CHILDREN", "WARNING", "household",
        record_id = big, variable = "n_children",
        value = as.character(as.integer(counts[big])),
        message = sprintf("More than %d eligible children in one household.",
                          max_children)
      )
    }
    if ("n_eligible" %in% names(hh)) {
      obs <- as.integer(counts[match(hh$interview_id, names(counts))])
      obs[is.na(obs)] <- 0L
      rep_n <- suppressWarnings(as.numeric(hh$n_eligible))
      mism <- !is.na(rep_n) & rep_n != obs
      if (any(mism)) {
        iss[[length(iss) + 1L]] <- vcs_issue(
          "check_household_structure", "HH_COUNT_MISMATCH", "WARNING", "household",
          record_id = hh$interview_id[mism], variable = "n_eligible",
          value = sprintf("reported %s, observed %d", rep_n[mism], obs[mism]),
          message = "Reported number of eligible children differs from the number of child modules."
        )
      }
    }
  }

  if (nrow(hh) && all(c("eligible", "interview_id") %in% names(hh))) {
    with_child <- hh$interview_id %in% ch$interview_id
    bad <- !is.na(hh$eligible) & !hh$eligible & with_child
    if (any(bad)) {
      iss[[length(iss) + 1L]] <- vcs_issue(
        "check_household_structure", "HH_INELIGIBLE_WITH_CHILD", "ERROR", "household",
        record_id = hh$interview_id[bad], variable = "eligible", value = "FALSE",
        message = "Household screened as ineligible but has a completed child module."
      )
    }
  }
  new_vcs_validation(dplyr::bind_rows(iss), "check_household_structure")
}

#' Check child eligibility against the survey age window
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param min_age_months,max_age_months Inclusive age window, in completed
#'   months, that defines an eligible child. The default 12--23 months is the
#'   usual window for coverage surveys of the first year of life; set it to the
#'   window of the survey being analysed.
#' @param reference_date Date at which age is evaluated when age is derived from
#'   `child_dob`. Defaults to each child's interview date, falling back to the
#'   maximum interview date in the data.
#' @return A [vcs_validation][new_vcs_validation] object.
#' @export
#' @family structural checks
#' @examples
#' check_child_eligibility(vcs_example)
#' check_child_eligibility(vcs_example, min_age_months = 9, max_age_months = 23)
check_child_eligibility <- function(x,
                                    min_age_months = 12,
                                    max_age_months = 23,
                                    reference_date = NULL) {
  assert_vcs_data(x)
  assert_number(min_age_months, lower = 0)
  assert_number(max_age_months, lower = 0)
  if (max_age_months < min_age_months) {
    vcs_abort("`max_age_months` must not be below `min_age_months`.",
              class = "vaxsurvR_value_error")
  }
  ch <- vcs_children(x)
  if (!nrow(ch)) {
    return(new_vcs_validation(checks = "check_child_eligibility"))
  }
  age <- child_age_months(ch, reference_date)
  iss <- list()
  known <- !is.na(age)
  out_of_range <- known & (age < min_age_months | age > max_age_months)
  if (any(out_of_range)) {
    iss[[length(iss) + 1L]] <- vcs_issue(
      "check_child_eligibility", "AGE_OUT_OF_WINDOW", "ERROR", "child",
      record_id = ch$child_id[out_of_range], variable = "age_months",
      value = as.character(round(age[out_of_range], 1)),
      message = sprintf("Age outside the %g-%g month eligibility window.",
                        min_age_months, max_age_months)
    )
  }
  if (any(!known)) {
    iss[[length(iss) + 1L]] <- vcs_issue(
      "check_child_eligibility", "AGE_UNKNOWN", "WARNING", "child",
      record_id = ch$child_id[!known], variable = "age_months",
      value = NA_character_,
      message = "Child age could not be determined from age_months or child_dob."
    )
  }
  new_vcs_validation(
    dplyr::bind_rows(iss), "check_child_eligibility",
    meta = list(min_age_months = min_age_months, max_age_months = max_age_months)
  )
}

#' Child age in months, from the age variable or from date of birth
#' @noRd
child_age_months <- function(ch, reference_date = NULL) {
  if ("age_months" %in% names(ch) && any(!is.na(ch$age_months))) {
    age <- suppressWarnings(as.numeric(ch$age_months))
  } else {
    age <- rep(NA_real_, nrow(ch))
  }
  if ("child_dob" %in% names(ch) && any(is.na(age))) {
    ref <- if (!is.null(reference_date)) {
      as_date_safe(reference_date)
    } else if ("interview_date" %in% names(ch)) {
      ch$interview_date
    } else {
      as.Date(NA)
    }
    if (length(ref) == 1L) ref <- rep(ref, nrow(ch))
    if (all(is.na(ref)) && "interview_date" %in% names(ch)) {
      ref <- rep(suppressWarnings(max(ch$interview_date, na.rm = TRUE)), nrow(ch))
    }
    from_dob <- as.numeric(ref - ch$child_dob) / 30.4375
    age[is.na(age)] <- from_dob[is.na(age)]
  }
  age
}

#' Check the primary sampling unit structure
#'
#' Reports PSUs with very few or very many interviews, strata containing a
#' single PSU (which leaves the variance of that stratum unestimable), and
#' PSUs that straddle more than one stratum.
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param min_interviews,max_interviews Expected range of interviews per PSU.
#' @return A [vcs_validation][new_vcs_validation] object.
#' @export
#' @family structural checks
#' @examples
#' check_psu_structure(vcs_example)
check_psu_structure <- function(x, min_interviews = 5L, max_interviews = 50L) {
  assert_vcs_data(x)
  assert_number(min_interviews, lower = 0)
  assert_number(max_interviews, lower = 1)
  hh <- x$households
  iss <- list()
  if (!"psu" %in% names(hh) || !nrow(hh)) {
    return(new_vcs_validation(
      vcs_issue("check_psu_structure", "PSU_UNMAPPED", "CRITICAL", "survey",
                record_id = "<survey>", variable = "psu", value = NA_character_,
                message = "No PSU variable is mapped; cluster structure cannot be checked."),
      "check_psu_structure"
    ))
  }
  psu <- as.character(hh$psu)
  blank <- is_blank(psu)
  if (any(blank)) {
    iss[[length(iss) + 1L]] <- vcs_issue(
      "check_psu_structure", "PSU_MISSING", "CRITICAL", "household",
      record_id = hh$interview_id[blank], variable = "psu", value = NA_character_,
      message = "Interview has no PSU assigned."
    )
  }
  counts <- table(psu[!blank])
  small <- names(counts)[counts < min_interviews]
  if (length(small)) {
    iss[[length(iss) + 1L]] <- vcs_issue(
      "check_psu_structure", "PSU_FEW_INTERVIEWS", "WARNING", "survey",
      record_id = small, variable = "psu",
      value = as.character(as.integer(counts[small])),
      message = sprintf("Fewer than %d interviews in the PSU.", min_interviews)
    )
  }
  large <- names(counts)[counts > max_interviews]
  if (length(large)) {
    iss[[length(iss) + 1L]] <- vcs_issue(
      "check_psu_structure", "PSU_MANY_INTERVIEWS", "WARNING", "survey",
      record_id = large, variable = "psu",
      value = as.character(as.integer(counts[large])),
      message = sprintf("More than %d interviews in the PSU.", max_interviews)
    )
  }
  if ("stratum" %in% names(hh)) {
    tab <- unique(data.frame(psu = psu, stratum = as.character(hh$stratum),
                             stringsAsFactors = FALSE))
    tab <- tab[!is_blank(tab$psu) & !is_blank(tab$stratum), , drop = FALSE]
    if (nrow(tab)) {
      per_stratum <- table(tab$stratum)
      lonely <- names(per_stratum)[per_stratum == 1L]
      if (length(lonely)) {
        iss[[length(iss) + 1L]] <- vcs_issue(
          "check_psu_structure", "PSU_LONELY_STRATUM", "ERROR", "survey",
          record_id = lonely, variable = "stratum", value = "1",
          message = "Stratum contains a single PSU; its variance contribution is not estimable."
        )
      }
      per_psu <- table(tab$psu)
      crossing <- names(per_psu)[per_psu > 1L]
      if (length(crossing)) {
        iss[[length(iss) + 1L]] <- vcs_issue(
          "check_psu_structure", "PSU_CROSSES_STRATA", "ERROR", "survey",
          record_id = crossing, variable = "stratum",
          value = as.character(as.integer(per_psu[crossing])),
          message = "PSU appears in more than one stratum."
        )
      }
    }
  }
  new_vcs_validation(dplyr::bind_rows(iss), "check_psu_structure")
}

#' Check the segment structure within PSUs
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param max_segments Number of segments per PSU above which the PSU is
#'   flagged.
#' @return A [vcs_validation][new_vcs_validation] object.
#' @export
#' @family structural checks
#' @examples
#' check_segment_structure(vcs_example)
check_segment_structure <- function(x, max_segments = 10L) {
  assert_vcs_data(x)
  assert_number(max_segments, lower = 1)
  hh <- x$households
  if (!"segment" %in% names(hh) || !nrow(hh)) {
    return(new_vcs_validation(
      vcs_issue("check_segment_structure", "SEG_UNMAPPED", "INFO", "survey",
                record_id = "<survey>", variable = "segment", value = NA_character_,
                message = "No segment variable is mapped; segmentation was not checked."),
      "check_segment_structure"
    ))
  }
  iss <- list()
  blank <- is_blank(hh$segment)
  if (any(blank)) {
    iss[[length(iss) + 1L]] <- vcs_issue(
      "check_segment_structure", "SEG_MISSING", "WARNING", "household",
      record_id = hh$interview_id[blank], variable = "segment",
      value = NA_character_,
      message = "Interview has no segment assigned."
    )
  }
  if ("psu" %in% names(hh)) {
    tab <- unique(data.frame(
      psu = as.character(hh$psu), segment = as.character(hh$segment),
      stringsAsFactors = FALSE
    ))
    tab <- tab[!is_blank(tab$psu) & !is_blank(tab$segment), , drop = FALSE]
    if (nrow(tab)) {
      per_psu <- table(tab$psu)
      many <- names(per_psu)[per_psu > max_segments]
      if (length(many)) {
        iss[[length(iss) + 1L]] <- vcs_issue(
          "check_segment_structure", "SEG_MANY_PER_PSU", "WARNING", "survey",
          record_id = many, variable = "segment",
          value = as.character(as.integer(per_psu[many])),
          message = sprintf("More than %d segments in the PSU.", max_segments)
        )
      }
      # A segment label reused across PSUs is only a problem if segment ids are
      # meant to be globally unique, so this is informational.
      per_seg <- table(tab$segment)
      shared <- names(per_seg)[per_seg > 1L]
      if (length(shared)) {
        iss[[length(iss) + 1L]] <- vcs_issue(
          "check_segment_structure", "SEG_LABEL_REUSED", "INFO", "survey",
          record_id = shared, variable = "segment",
          value = as.character(as.integer(per_seg[shared])),
          message = "Segment label is used in more than one PSU; nest it within PSU when analysing."
        )
      }
    }
  }
  new_vcs_validation(dplyr::bind_rows(iss), "check_segment_structure")
}

#' Run the structural validation suite
#'
#' Runs the structural checks and, when a schedule is available, the
#' vaccination data-quality checks, returning one combined
#' [vcs_validation][new_vcs_validation] object.
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param checks Names of check functions to run. Defaults to the full suite
#'   appropriate to the data.
#' @param ... Passed to the individual checks.
#' @return A [vcs_validation][new_vcs_validation] object.
#' @export
#' @seealso [issues()], [summary.vcs_validation()], [clean_vcs()]
#' @examples
#' v <- validate_vcs(vcs_example)
#' v
#' summary(v, by = "severity")
validate_vcs <- function(x, checks = NULL, ...) {
  assert_vcs_data(x)
  assert_character(checks)
  structural <- c("check_required_variables", "check_unique_ids",
                  "check_duplicates", "check_household_structure",
                  "check_child_eligibility", "check_psu_structure",
                  "check_segment_structure")
  vaccination <- c("check_vaccine_dates", "check_future_dates",
                   "check_prebirth_vaccination", "check_age_at_vaccination",
                   "check_vaccine_sequence", "check_dose_intervals",
                   "check_duplicate_doses", "check_card_transcription")
  if (is.null(checks)) {
    checks <- structural
    if (!is.null(x$schedule) && nrow(x$vaccinations)) {
      checks <- c(checks, vaccination)
    }
  }
  unknown <- checks[!vapply(checks, function(f) {
    exists(f, envir = asNamespace("vaxsurvR"), mode = "function")
  }, logical(1))]
  if (length(unknown)) {
    vcs_abort(
      sprintf("Unknown check%s: %s.",
              if (length(unknown) > 1L) "s" else "", collapse_quote(unknown)),
      class = "vaxsurvR_value_error"
    )
  }
  dots <- list(...)
  results <- lapply(checks, function(f) {
    fn <- get(f, envir = asNamespace("vaxsurvR"))
    args <- dots[intersect(names(dots), names(formals(fn)))]
    do.call(fn, c(list(x), args))
  })
  out <- do.call(c, results)
  out$meta$n_children <- nrow(x$children)
  out$meta$n_households <- nrow(x$households)
  out
}
