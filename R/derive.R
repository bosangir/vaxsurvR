# ---------------------------------------------------------------------------
# Deriving vaccination status.
#
# Two rules govern this module:
#   * absence of evidence is not evidence of absence. A dose with no card and
#     no recall is NA, not zero, unless the user explicitly asks otherwise.
#   * every definition is recorded. The rule that produced a column is stored
#     on the column and readable through derivation_rules().
# ---------------------------------------------------------------------------

#' Evidence hierarchies recognised by vaxsurvR
#'
#' Coverage surveys distinguish what is written on a vaccination card from what
#' a caregiver remembers. The strength of evidence for a dose is graded:
#'
#' 1. `"card_date"` -- documented on the card with a usable date
#' 2. `"card_mark"` -- documented on the card by a tick with no date
#' 3. `"recall"` -- reported by the caregiver, no documentation
#' 4. `"none"` -- no evidence of the dose from any source
#'
#' `"unknown"` is used when neither card nor recall information exists, which
#' is different from evidence that the dose was not given.
#'
#' @return A character vector of evidence grades, strongest first.
#' @export
#' @examples
#' vcs_evidence_levels()
vcs_evidence_levels <- function() {
  c("card_date", "card_mark", "recall", "none", "unknown")
}

#' Definitions of vaccination that vaxsurvR can apply
#'
#' @return A [tibble][tibble::tibble] describing each definition.
#' @export
#' @examples
#' vcs_evidence_definitions()
vcs_evidence_definitions <- function() {
  tibble::tribble(
    ~definition,        ~description,
    "card",             "Documented on the card (with or without a date). Children with no card are unknown.",
    "card_date",        "Documented on the card with a usable date. Card marks without a date are unknown.",
    "recall",           "Reported by the caregiver, ignoring card documentation.",
    "card_or_recall",   "Documented on the card OR reported by the caregiver.",
    "card_then_recall", "Card where a card exists; caregiver recall only for children with no card."
  )
}

#' Grade the evidence for each dose
#'
#' Adds an `evidence` column to the long vaccination table, following the
#' hierarchy in [vcs_evidence_levels()].
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @return `x`, with `evidence` added to `x$vaccinations`.
#' @export
#' @seealso [derive_vaccination_status()]
#' @examples
#' d <- derive_evidence(vcs_example)
#' table(d$vaccinations$evidence)
derive_evidence <- function(x) {
  assert_vcs_data(x)
  vx <- x$vaccinations
  if (!nrow(vx)) {
    vx$evidence <- character(0)
    x$vaccinations <- vx
    return(x)
  }
  n <- nrow(vx)
  card_doc <- if ("card_documented" %in% names(vx)) vx$card_documented else rep(NA, n)
  card_date <- if ("card_date" %in% names(vx)) vx$card_date else rep(as.Date(NA), n)
  recall <- if ("recall_reported" %in% names(vx)) vx$recall_reported else rep(NA, n)

  yes_card <- !is.na(card_doc) & card_doc
  no_card <- !is.na(card_doc) & !card_doc
  yes_recall <- !is.na(recall) & recall
  no_recall <- !is.na(recall) & !recall

  ev <- rep("unknown", n)
  # Any source positively saying no is evidence of absence. A card marked "not
  # given" counts on its own; it does not need recall to corroborate it.
  ev[no_card | no_recall] <- "none"
  # A yes outranks a no, and card outranks recall.
  ev[yes_recall] <- "recall"
  ev[yes_card] <- "card_mark"
  ev[yes_card & !is.na(card_date)] <- "card_date"

  vx$evidence <- mark_derived(
    factor(ev, levels = vcs_evidence_levels()),
    "evidence graded card_date > card_mark > recall > none > unknown"
  )
  x$vaccinations <- vx
  x
}

#' Is a dose vaccinated under a given definition?
#'
#' Works from the underlying card and recall flags rather than the collapsed
#' `evidence` grade. The grade is a single ordered label and cannot express, for
#' example, "the card records no dose but the caregiver says yes" -- which is
#' `FALSE` under a card definition and `TRUE` under a recall one.
#' @noRd
evidence_to_status <- function(card_doc, has_date, recall, definition) {
  n <- length(card_doc)
  yes_card <- !is.na(card_doc) & card_doc
  no_card <- !is.na(card_doc) & !card_doc
  yes_recall <- !is.na(recall) & recall
  no_recall <- !is.na(recall) & !recall
  out <- rep(NA, n)
  switch(
    definition,
    card = {
      out[yes_card] <- TRUE
      out[no_card] <- FALSE
    },
    card_date = {
      # A tick with no date cannot support a dated definition, so it is
      # unknown rather than false.
      out[yes_card & has_date] <- TRUE
      out[no_card] <- FALSE
    },
    recall = {
      out[yes_recall] <- TRUE
      out[no_recall] <- FALSE
    },
    card_or_recall = {
      out[no_card | no_recall] <- FALSE
      out[yes_card | yes_recall] <- TRUE
    },
    card_then_recall = {
      # Recall fills in only where the card is silent.
      out[yes_recall] <- TRUE
      out[no_recall] <- FALSE
      out[yes_card] <- TRUE
      out[no_card] <- FALSE
    }
  )
  out
}

#' Derive per-vaccine coverage indicators
#'
#' Adds one `cov_<vaccine>` column per vaccine to the child-level table, coded
#' `1` (vaccinated), `0` (not vaccinated) or `NA` (no evidence either way).
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param evidence Definition of vaccination; see [vcs_evidence_definitions()].
#' @param vaccines Vaccines to derive. Defaults to every vaccine present.
#' @param missing_as_unvaccinated Treat absence of evidence as evidence of
#'   non-vaccination. `FALSE` by default, and the choice is recorded in the
#'   derivation rule when set to `TRUE`.
#' @param prefix Prefix for the derived columns.
#' @return `x`, with coverage columns added to `x$children` and `evidence` and
#'   `vaccinated` added to `x$vaccinations`.
#' @export
#' @seealso [derive_card_coverage()], [derive_combined_coverage()],
#'   [estimate_coverage()]
#' @examples
#' d <- derive_vaccination_status(vcs_example, evidence = "card_or_recall")
#' mean(d$children$cov_PENTA1, na.rm = TRUE)
#' derivation_rules(d)[1:3, ]
derive_vaccination_status <- function(x,
                                      evidence = c("card_or_recall", "card",
                                                   "card_date", "recall",
                                                   "card_then_recall"),
                                      vaccines = NULL,
                                      missing_as_unvaccinated = FALSE,
                                      prefix = "cov_") {
  assert_vcs_data(x)
  evidence <- match.arg(evidence)
  assert_character(vaccines)
  assert_flag(missing_as_unvaccinated)
  assert_string(prefix)

  x <- derive_evidence(x)
  vx <- x$vaccinations
  if (!nrow(vx)) {
    return(x)
  }
  if (is.null(vaccines)) {
    vaccines <- unique(vx$vaccine)
  }
  unknown <- setdiff(vaccines, unique(vx$vaccine))
  if (length(unknown)) {
    vcs_abort(
      sprintf("Vaccine%s not present in the data: %s.",
              if (length(unknown) > 1L) "s" else "", collapse_quote(unknown)),
      class = "vaxsurvR_unknown_vaccine"
    )
  }

  status <- evidence_to_status(
    card_doc = if ("card_documented" %in% names(vx)) vx$card_documented else rep(NA, nrow(vx)),
    has_date = if ("card_date" %in% names(vx)) !is.na(vx$card_date) else rep(FALSE, nrow(vx)),
    recall = if ("recall_reported" %in% names(vx)) vx$recall_reported else rep(NA, nrow(vx)),
    definition = evidence
  )
  if (missing_as_unvaccinated) {
    status[is.na(status)] <- FALSE
  }
  rule <- sprintf(
    "vaccinated = %s (%s); missing evidence treated as %s",
    evidence,
    vcs_evidence_definitions()$description[
      vcs_evidence_definitions()$definition == evidence
    ],
    if (missing_as_unvaccinated) "not vaccinated" else "unknown"
  )
  vx$vaccinated <- mark_derived(status, rule)
  x$vaccinations <- vx

  ch <- x$children
  for (v in vaccines) {
    sel <- vx$vaccine == v
    idx <- match(ch$child_id, vx$child_id[sel])
    ch[[paste0(prefix, v)]] <- mark_derived(as.integer(vx$vaccinated[sel][idx]), rule)
  }
  x$children <- ch
  x$meta$evidence_definition <- evidence
  x
}

#' @rdname derive_vaccination_status
#' @export
#' @examples
#' derive_card_coverage(vcs_example)
derive_card_coverage <- function(x, vaccines = NULL, prefix = "card_") {
  derive_vaccination_status(x, evidence = "card", vaccines = vaccines,
                            prefix = prefix)
}

#' @rdname derive_vaccination_status
#' @export
#' @examples
#' derive_recall_coverage(vcs_example)
derive_recall_coverage <- function(x, vaccines = NULL, prefix = "recall_") {
  derive_vaccination_status(x, evidence = "recall", vaccines = vaccines,
                            prefix = prefix)
}

#' @rdname derive_vaccination_status
#' @export
#' @examples
#' derive_combined_coverage(vcs_example)
derive_combined_coverage <- function(x, vaccines = NULL, prefix = "cov_") {
  derive_vaccination_status(x, evidence = "card_or_recall", vaccines = vaccines,
                            prefix = prefix)
}

#' Derive a fully-vaccinated indicator
#'
#' There is no universal definition of a fully vaccinated child, so the set of
#' doses is an argument. The default is the last dose of every antigen series in
#' the schedule, which [schedule_final_doses()] computes; state the definition
#' explicitly in any published output.
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param vaccines Doses that must all be received. Defaults to
#'   [schedule_final_doses()] of the schedule stored in `x`.
#' @param evidence Definition of vaccination, passed to
#'   [derive_vaccination_status()].
#' @param name Name of the derived column.
#' @param require_all When `TRUE` (default) every dose in `vaccines` must be
#'   received. When `FALSE` the indicator is the proportion of doses received.
#' @return `x`, with the indicator added to `x$children`.
#' @export
#' @examples
#' d <- derive_fully_vaccinated(vcs_example)
#' mean(d$children$fully_vaccinated, na.rm = TRUE)
#'
#' # A different, explicitly stated definition.
#' d2 <- derive_fully_vaccinated(
#'   vcs_example,
#'   vaccines = c("BCG", "PENTA3", "MCV1"),
#'   name = "basic_fully_vaccinated"
#' )
#' mean(d2$children$basic_fully_vaccinated, na.rm = TRUE)
derive_fully_vaccinated <- function(x,
                                    vaccines = NULL,
                                    evidence = "card_or_recall",
                                    name = "fully_vaccinated",
                                    require_all = TRUE) {
  assert_vcs_data(x)
  assert_string(name)
  assert_flag(require_all)
  if (is.null(vaccines)) {
    if (is.null(x$schedule)) {
      vcs_abort("Supply `vaccines`: `x` carries no schedule to default from.",
                class = "vaxsurvR_value_error")
    }
    vaccines <- schedule_final_doses(x$schedule)
  }
  assert_character(vaccines, allow_null = FALSE)
  x <- derive_vaccination_status(x, evidence = evidence, vaccines = vaccines)
  ch <- x$children
  cols <- paste0("cov_", vaccines)
  mat <- as.matrix(ch[, cols, drop = FALSE])
  rule <- sprintf(
    "%s = %s of {%s} under evidence \"%s\"",
    name,
    if (require_all) "all" else "proportion",
    paste(vaccines, collapse = ", "), evidence
  )
  out <- if (require_all) {
    ok <- apply(mat, 1, function(r) {
      if (any(!is.na(r) & r == 0)) 0L else if (anyNA(r)) NA_integer_ else 1L
    })
    as.integer(ok)
  } else {
    apply(mat, 1, function(r) mean(r, na.rm = TRUE))
  }
  ch[[name]] <- mark_derived(out, rule)
  x$children <- ch
  x
}

#' Derive a zero-dose indicator
#'
#' A zero-dose child is conventionally one who has received no dose of a
#' DTP-containing vaccine, but the marker antigen differs between programmes.
#' It is therefore an argument, not a constant.
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param marker Vaccine whose absence defines zero-dose status. Defaults to the
#'   first dose of the first DTP-containing series found in the schedule.
#' @param evidence Definition of vaccination.
#' @param name Name of the derived column.
#' @return `x`, with the indicator added to `x$children`.
#' @export
#' @examples
#' d <- derive_zero_dose(vcs_example)
#' mean(d$children$zero_dose, na.rm = TRUE)
#' derive_zero_dose(vcs_example, marker = "BCG", name = "no_bcg")
derive_zero_dose <- function(x, marker = NULL, evidence = "card_or_recall",
                             name = "zero_dose") {
  assert_vcs_data(x)
  assert_string(name)
  if (is.null(marker)) {
    marker <- default_dtp_marker(x)
  }
  assert_string(marker)
  x <- derive_vaccination_status(x, evidence = evidence, vaccines = marker)
  ch <- x$children
  ch[[name]] <- mark_derived(
    as.integer(1L - ch[[paste0("cov_", marker)]]),
    sprintf("%s = no dose of %s under evidence \"%s\"", name, marker, evidence)
  )
  x$children <- ch
  x
}

#' First DTP-containing dose in a schedule
#' @noRd
default_dtp_marker <- function(x) {
  vaccines <- unique(x$vaccinations$vaccine)
  if (!length(vaccines)) {
    vcs_abort("No vaccines are present in the data; supply `marker`.",
              class = "vaxsurvR_value_error")
  }
  candidates <- c("PENTA1", "DTP1", "DPT1", "PENTA_1", "DTP_1")
  hit <- intersect(candidates, vaccines)
  if (length(hit)) {
    return(hit[1])
  }
  vcs_abort(
    sprintf(paste0("No DTP-containing first dose found among %s. ",
                   "Supply `marker` explicitly."), collapse_quote(vaccines)),
    class = "vaxsurvR_value_error"
  )
}

#' Derive an individual-level dropout indicator
#'
#' A child has dropped out when they received `first` but not `last`. Children
#' who never received `first` are not at risk of dropout and are `NA`, which is
#' the denominator convention that makes the individual-level dropout rate
#' comparable to the coverage-difference form.
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param first,last Vaccine names bracketing the series.
#' @param evidence Definition of vaccination.
#' @param name Name of the derived column. Defaults to
#'   `dropout_<first>_<last>`.
#' @return `x`, with the indicator added to `x$children`.
#' @export
#' @seealso [estimate_dropout()]
#' @examples
#' d <- derive_dropout(vcs_example, "PENTA1", "PENTA3")
#' mean(d$children$dropout_PENTA1_PENTA3, na.rm = TRUE)
derive_dropout <- function(x, first, last, evidence = "card_or_recall",
                           name = NULL) {
  assert_vcs_data(x)
  assert_string(first)
  assert_string(last)
  name <- name %||% sprintf("dropout_%s_%s", first, last)
  assert_string(name)
  x <- derive_vaccination_status(x, evidence = evidence,
                                 vaccines = unique(c(first, last)))
  ch <- x$children
  a <- ch[[paste0("cov_", first)]]
  b <- ch[[paste0("cov_", last)]]
  out <- rep(NA_integer_, nrow(ch))
  at_risk <- !is.na(a) & a == 1L
  out[at_risk] <- as.integer(!is.na(b[at_risk]) & b[at_risk] == 0L)
  out[at_risk & is.na(b)] <- NA_integer_
  ch[[name]] <- mark_derived(
    out,
    sprintf(paste0("%s = received %s but not %s, among children who received ",
                   "%s (evidence \"%s\")"), name, first, last, first, evidence)
  )
  x$children <- ch
  x
}

#' Derive dose timeliness
#'
#' Classifies each dated, card-documented dose as early, timely or late against
#' the schedule window, and adds a per-vaccine timeliness indicator to the
#' child-level table.
#'
#' Only doses with both a date of birth and a usable card date can be
#' classified; recall-only doses are `NA`, because recall carries no date.
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param schedule A [vcs_schedule()]. Defaults to the schedule stored in `x`.
#' @param vaccines Vaccines to summarise into child-level columns. Defaults to
#'   every vaccine with a `maximum_age_days` in the schedule.
#' @param prefix Prefix for the derived child-level columns.
#' @return `x`, with `age_at_vaccination_days` and `timeliness` added to
#'   `x$vaccinations` and `<prefix><vaccine>` indicators added to `x$children`.
#' @export
#' @seealso [classify_vaccination_timeliness()], [estimate_timely_coverage()]
#' @examples
#' d <- derive_timeliness(vcs_example)
#' table(d$vaccinations$timeliness, useNA = "ifany")
#' mean(d$children$timely_PENTA1, na.rm = TRUE)
derive_timeliness <- function(x, schedule = NULL, vaccines = NULL,
                              prefix = "timely_") {
  assert_vcs_data(x)
  assert_string(prefix)
  schedule <- schedule %||% x$schedule
  if (is.null(schedule)) {
    vcs_abort("Supply `schedule`: `x` carries no schedule.",
              class = "vaxsurvR_value_error")
  }
  assert_schedule(schedule)
  vx <- vx_with_dates(x)
  if (!nrow(vx) || !all(c("card_date", "child_dob") %in% names(vx))) {
    vcs_abort(
      "Timeliness needs both card dates and dates of birth to be mapped.",
      class = "vaxsurvR_value_error"
    )
  }
  age <- derive_age_at_vaccination(vx$child_dob, vx$card_date)
  cls <- classify_vaccination_timeliness(age, vx$vaccine, schedule)
  # A dose given before birth is a data error, not an early vaccination.
  cls[!is.na(age) & age < 0] <- NA_character_

  out_vx <- x$vaccinations
  out_vx$age_at_vaccination_days <- mark_derived(
    age, "days between child_dob and card_date"
  )
  out_vx$timeliness <- mark_derived(
    factor(cls, levels = c("early", "timely", "late")),
    "age at vaccination compared with the schedule minimum/maximum age window"
  )
  x$vaccinations <- out_vx

  if (is.null(vaccines)) {
    vaccines <- schedule$vaccine[!is.na(schedule$maximum_age_days)]
    vaccines <- intersect(vaccines, unique(out_vx$vaccine))
  }
  assert_character(vaccines, allow_null = FALSE)
  ch <- x$children
  for (v in vaccines) {
    sel <- out_vx$vaccine == v
    idx <- match(ch$child_id, out_vx$child_id[sel])
    tl <- out_vx$timeliness[sel][idx]
    ch[[paste0(prefix, v)]] <- mark_derived(
      as.integer(!is.na(tl) & tl == "timely"),
      sprintf("%s received within the schedule age window, among dated card doses", v)
    )
    ch[[paste0(prefix, v)]][is.na(tl)] <- NA_integer_
  }
  x$children <- ch
  x
}
