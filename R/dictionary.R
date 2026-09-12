# ---------------------------------------------------------------------------
# The variable dictionary: the bridge between an arbitrary survey export and
# the standardised concepts vaxsurvR works with. Nothing else in the package
# refers to a source variable name directly.
# ---------------------------------------------------------------------------

#' Standard concepts recognised by a vaxsurvR dictionary
#'
#' Returns the vocabulary of standardised concept names that
#' [vcs_dictionary()] accepts, together with the level each concept applies to
#' and whether it is required for the core pipeline.
#'
#' Concepts at level `"vaccine"` are *templates* evaluated once per vaccine in a
#' [vcs_schedule()]; see [vcs_dictionary()] for the placeholder syntax.
#'
#' @return A [tibble][tibble::tibble] with columns `concept`, `level`,
#'   `required` and `description`.
#' @export
#' @examples
#' vcs_concepts()
#' subset(vcs_concepts(), required)
vcs_concepts <- function() {
  tibble::tribble(
    ~concept,          ~level,       ~required, ~description,
    "interview_id",    "household",  TRUE,      "Unique identifier of the interview/submission",
    "household_id",    "household",  TRUE,      "Household identifier",
    "child_id",        "child",      TRUE,      "Child identifier (may be constructed)",
    "caregiver_id",    "child",      FALSE,     "Caregiver/respondent identifier",
    "psu",             "household",  TRUE,      "Primary sampling unit / cluster",
    "segment",         "household",  FALSE,     "Segment within the PSU",
    "stratum",         "household",  FALSE,     "Sampling stratum",
    "weight",          "child",      FALSE,     "Final sampling weight",
    "fpc",             "household",  FALSE,     "Finite population correction",
    "province",        "household",  FALSE,     "Administrative level 1",
    "district",        "household",  FALSE,     "Administrative level 2",
    "health_zone",     "household",  FALSE,     "Health zone (administrative level 3)",
    "health_area",     "household",  FALSE,     "Health area (administrative level 4)",
    "residence",       "household",  FALSE,     "Urban/rural residence",
    "interview_date",  "household",  FALSE,     "Date of the interview",
    "interview_start", "household",  FALSE,     "Interview start time",
    "interview_end",   "household",  FALSE,     "Interview end time",
    "duration_min",    "household",  FALSE,     "Interview duration in minutes",
    "enumerator",      "household",  FALSE,     "Interviewer identifier",
    "team",            "household",  FALSE,     "Team or team leader identifier",
    "supervisor",      "household",  FALSE,     "Field supervisor identifier",
    "gps_lat",         "household",  FALSE,     "Latitude at the dwelling",
    "gps_lon",         "household",  FALSE,     "Longitude at the dwelling",
    "gps_accuracy",    "household",  FALSE,     "GPS accuracy in metres",
    "eligible",        "household",  FALSE,     "Household eligibility indicator",
    "consent",         "household",  FALSE,     "Interview consent indicator",
    "n_eligible",      "household",  FALSE,     "Number of eligible children reported",
    "child_dob",       "child",      FALSE,     "Child date of birth",
    "age_months",      "child",      FALSE,     "Child age in completed months",
    "sex",             "child",      FALSE,     "Child sex",
    "card_seen",       "child",      FALSE,     "Vaccination card seen by the interviewer",
    "child_name",      "child",      FALSE,     "Child name (direct identifier)",
    "caregiver_name",  "child",      FALSE,     "Caregiver name (direct identifier)",
    "telephone",       "household",  FALSE,     "Contact telephone number (direct identifier)",
    "address",         "household",  FALSE,     "Street address (direct identifier)",
    "card_status",     "vaccine",    FALSE,     "Card-documented dose status, per vaccine",
    "card_date",       "vaccine",    FALSE,     "Card-documented dose date, per vaccine",
    "recall_status",   "vaccine",    FALSE,     "Caregiver-recalled dose status, per vaccine",
    "recall_ever",     "vaccine",    FALSE,     "Caregiver recall: any dose of the antigen ever received",
    "recall_count",    "vaccine",    FALSE,     "Caregiver recall: number of doses of the antigen received"
  )
}

#' Create a variable dictionary
#'
#' A dictionary maps source variable names in a survey export to the
#' standardised concepts used throughout vaxsurvR. No source variable name is
#' ever hard-coded in the package: everything flows through this object.
#'
#' @section Repeat-group templates:
#' Household surveys nest children inside caregivers inside households, and wide
#' exports encode that nesting in the column name. Concepts at the `"child"` and
#' `"vaccine"` levels therefore accept templates with `{}` placeholders:
#'
#' * `{c}` -- caregiver (or first-level repeat) index, 1..`n_caregivers`
#' * `{k}` -- child (or second-level repeat) index, 1..`n_children`
#' * `{v}` -- vaccine index from the schedule, unpadded (`1`, `2`, ...)
#' * `{vv}` -- vaccine index zero-padded to two digits (`01`, `02`, ...)
#' * `{vaccine}` -- vaccine name from the schedule (`"BCG"`, `"PENTA1"`, ...)
#' * `{antigen}` -- antigen series from the schedule (`"PENTA"` for `PENTA1`,
#'   `PENTA2` and `PENTA3`), for concepts collected once per series such as
#'   `recall_ever` and `recall_count`
#'
#' For example `card_date = "CVH{vv}_date_{c}_{k}"` resolves to
#' `CVH04_date_1_2` for the fourth vaccine of the second child of the first
#' caregiver. A concept given without placeholders is treated as a single
#' column, which is what long-format (one row per child) exports need.
#'
#' @param ... Named concept-to-source mappings, e.g. `child_id = "childid"`.
#'   Names must appear in [vcs_concepts()].
#' @param n_caregivers,n_children Maximum number of caregiver and child repeats
#'   to expand when the mapping uses `{c}` / `{k}` placeholders.
#' @param yes_values,no_values Values in the source data that mean yes and no
#'   for indicator concepts such as `card_seen`, `eligible` and `consent`.
#'   Anything else (including don't-know codes) becomes `NA`.
#' @param card_yes_values Values of `card_status` that count as a
#'   card-documented dose. Defaults to `c("1", "2")`, matching the common
#'   "yes with date" / "yes, date incomplete" coding.
#' @param card_no_values Values of `card_status` that positively mean the dose
#'   is *not* recorded on the card. Values in neither list -- a don't-know code
#'   such as 98, or a blank -- become `NA`, because "the interviewer could not
#'   tell" is not the same as "the card says no".
#' @param count_dk_values Values of a `recall_count` variable that mean "don't
#'   know" rather than a number of doses.
#' @param date_formats Candidate date formats passed to [parse_vaccine_date()].
#'
#' @return An object of class `vcs_dictionary`.
#' @export
#' @seealso [map_vcs_variables()], [vcs_concepts()], [read_vcs_dictionary()]
#' @examples
#' # A long-format export: one row per child, one column per concept.
#' vcs_dictionary(
#'   child_id = "childid",
#'   household_id = "hh_id",
#'   interview_id = "hh_id",
#'   psu = "cluster",
#'   age_months = "child_age_month",
#'   sex = "child_sex"
#' )
#'
#' # A wide export with repeat groups and per-vaccine columns.
#' vcs_dictionary(
#'   interview_id = "KEY",
#'   household_id = "KEY",
#'   psu = "submission_psu",
#'   child_id = "EC03_{c}_{k}",
#'   card_seen = "CVH_card_shown_{c}_{k}",
#'   card_status = "CVH{vv}_{c}_{k}",
#'   card_date = "CVH{vv}_date_{c}_{k}",
#'   recall_status = "VR{vv}_{c}_{k}",
#'   n_caregivers = 3,
#'   n_children = 2
#' )
vcs_dictionary <- function(...,
                           n_caregivers = 1L,
                           n_children = 1L,
                           yes_values = c("1", "yes", "oui", "y", "true"),
                           no_values = c("0", "2", "no", "non", "n", "false"),
                           card_yes_values = c("1", "2"),
                           card_no_values = c("0", "3", "no", "non"),
                           count_dk_values = c("98", "99", "999"),
                           date_formats = c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y",
                                            "%d-%m-%Y", "%b %d, %Y", "%d %b %Y")) {
  map <- rlang::list2(...)
  assert_number(n_caregivers, lower = 1)
  assert_number(n_children, lower = 1)
  assert_character(yes_values, allow_null = FALSE)
  assert_character(no_values, allow_null = FALSE)
  assert_character(card_yes_values, allow_null = FALSE)
  assert_character(card_no_values, allow_null = FALSE)
  assert_character(count_dk_values, allow_null = FALSE)
  assert_character(date_formats, allow_null = FALSE)
  overlap <- intersect(card_yes_values, card_no_values)
  if (length(overlap)) {
    vcs_abort(
      sprintf("Value%s in both `card_yes_values` and `card_no_values`: %s.",
              if (length(overlap) > 1L) "s" else "", collapse_quote(overlap)),
      class = "vaxsurvR_dictionary_error"
    )
  }

  if (length(map) && (is.null(names(map)) || any(!nzchar(names(map))))) {
    vcs_abort(
      "All mappings passed to `vcs_dictionary()` must be named.",
      class = "vaxsurvR_dictionary_error"
    )
  }
  if (anyDuplicated(names(map))) {
    dup <- unique(names(map)[duplicated(names(map))])
    vcs_abort(
      sprintf("Concept%s mapped more than once: %s.",
              if (length(dup) > 1L) "s" else "", collapse_quote(dup)),
      class = "vaxsurvR_dictionary_error"
    )
  }

  known <- vcs_concepts()
  unknown <- setdiff(names(map), known$concept)
  if (length(unknown)) {
    vcs_abort(
      sprintf(
        "Unknown concept%s %s. See `vcs_concepts()` for the vocabulary.",
        if (length(unknown) > 1L) "s" else "", collapse_quote(unknown)
      ),
      class = "vaxsurvR_dictionary_error"
    )
  }

  bad <- names(map)[!vapply(map, function(x) is.character(x) && length(x) == 1L && !is.na(x),
                            logical(1))]
  if (length(bad)) {
    vcs_abort(
      sprintf("Concept%s %s must map to a single source variable name.",
              if (length(bad) > 1L) "s" else "", collapse_quote(bad)),
      class = "vaxsurvR_dictionary_error"
    )
  }

  # Reject placeholders that make no sense at the concept's level, e.g. a
  # {vv} in a child-level mapping.
  allowed <- list(
    household = character(0),
    child = c("c", "k"),
    vaccine = c("c", "k", "v", "vv", "vaccine", "antigen")
  )
  for (nm in names(map)) {
    lvl <- known$level[known$concept == nm]
    used <- template_vars(map[[nm]])
    illegal <- setdiff(used, allowed[[lvl]])
    if (length(illegal)) {
      vcs_abort(
        sprintf(
          "Concept \"%s\" is at the %s level and cannot use placeholder%s %s.",
          nm, lvl, if (length(illegal) > 1L) "s" else "", collapse_quote(illegal)
        ),
        class = "vaxsurvR_dictionary_error"
      )
    }
  }

  structure(
    list(
      map = map,
      n_caregivers = as.integer(n_caregivers),
      n_children = as.integer(n_children),
      yes_values = yes_values,
      no_values = no_values,
      card_yes_values = card_yes_values,
      card_no_values = card_no_values,
      count_dk_values = count_dk_values,
      date_formats = date_formats
    ),
    class = "vcs_dictionary"
  )
}

#' @export
print.vcs_dictionary <- function(x, ...) {
  cat("<vcs_dictionary>\n")
  cat(sprintf("  repeats: %d caregiver(s) x %d child(ren)\n",
              x$n_caregivers, x$n_children))
  if (!length(x$map)) {
    cat("  <no concepts mapped>\n")
    return(invisible(x))
  }
  known <- vcs_concepts()
  for (lvl in c("household", "child", "vaccine")) {
    concepts <- intersect(known$concept[known$level == lvl], names(x$map))
    if (!length(concepts)) next
    cat(sprintf("  %s level:\n", lvl))
    width <- max(nchar(concepts))
    for (nm in concepts) {
      cat(sprintf("    %-*s -> %s\n", width, nm, x$map[[nm]]))
    }
  }
  invisible(x)
}

#' @export
format.vcs_dictionary <- function(x, ...) {
  sprintf("<vcs_dictionary: %d concept(s)>", length(x$map))
}

#' Test whether an object is a vcs_dictionary
#'
#' @param x An object.
#' @return A logical scalar.
#' @export
#' @examples
#' is_vcs_dictionary(vcs_dictionary(child_id = "id"))
#' is_vcs_dictionary(list())
is_vcs_dictionary <- function(x) inherits(x, "vcs_dictionary")

#' Convert a dictionary to a data frame
#'
#' @param x A [vcs_dictionary()].
#' @param ... Unused.
#' @return A tibble with columns `concept`, `source`, `level` and `required`.
#' @export
#' @examples
#' as.data.frame(vcs_dictionary(child_id = "cid", psu = "clust"))
as.data.frame.vcs_dictionary <- function(x, ...) {
  known <- vcs_concepts()
  out <- tibble::tibble(
    concept = as.character(names(x$map) %||% character(0)),
    source = as.character(unlist(x$map, use.names = FALSE) %||% character(0))
  )
  out$level <- known$level[match(out$concept, known$concept)]
  out$required <- known$required[match(out$concept, known$concept)]
  out
}

#' Resolve a dictionary concept to concrete source column names
#'
#' Expands `{c}`, `{k}`, `{v}`, `{vv}` and `{vaccine}` placeholders into the
#' full set of source columns implied by the dictionary and schedule.
#'
#' @param dictionary A [vcs_dictionary()].
#' @param concept Concept name.
#' @param schedule An optional [vcs_schedule()], required for vaccine-level
#'   concepts.
#' @return A tibble with one row per resolved column: `concept`, `column`,
#'   `caregiver`, `child`, `vaccine`. Returns zero rows when the concept is not
#'   mapped.
#' @export
#' @examples
#' dict <- vcs_dictionary(card_date = "CVH{vv}_date_{c}_{k}",
#'                        n_caregivers = 1, n_children = 2)
#' sched <- vcs_schedule_who()
#' head(resolve_concept(dict, "card_date", sched))
resolve_concept <- function(dictionary, concept, schedule = NULL) {
  stopifnot(is_vcs_dictionary(dictionary))
  assert_string(concept)
  empty <- tibble::tibble(
    concept = character(0), column = character(0),
    caregiver = integer(0), child = integer(0), vaccine = character(0)
  )
  if (!concept %in% names(dictionary$map)) {
    return(empty)
  }
  template <- dictionary$map[[concept]]
  used <- template_vars(template)

  cg <- if ("c" %in% used) seq_len(dictionary$n_caregivers) else NA_integer_
  ch <- if ("k" %in% used) seq_len(dictionary$n_children) else NA_integer_
  needs_vaccine <- any(c("v", "vv", "vaccine", "antigen") %in% used)
  if (needs_vaccine && is.null(schedule)) {
    vcs_abort(
      sprintf("Concept \"%s\" uses a vaccine placeholder; supply `schedule`.", concept),
      class = "vaxsurvR_template_error"
    )
  }
  vx <- if (needs_vaccine) schedule$vaccine else NA_character_

  grid <- expand.grid(
    caregiver = cg, child = ch, vaccine = vx,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  grid$column <- vapply(seq_len(nrow(grid)), function(i) {
    vidx <- if (needs_vaccine) match(grid$vaccine[i], schedule$vaccine) else NA_integer_
    fill_template(template, list(
      c = grid$caregiver[i],
      k = grid$child[i],
      v = vidx,
      vv = if (is.na(vidx)) NA_character_ else sprintf("%02d", vidx),
      vaccine = grid$vaccine[i],
      antigen = if (is.na(vidx)) NA_character_ else schedule$antigen[vidx]
    ))
  }, character(1))

  tibble::tibble(
    concept = concept,
    column = grid$column,
    caregiver = as.integer(grid$caregiver),
    child = as.integer(grid$child),
    vaccine = as.character(grid$vaccine)
  )
}

#' Read a variable dictionary from a file
#'
#' Reads a two-column mapping (`concept`, `source`) from CSV or Excel and builds
#' a [vcs_dictionary()]. Useful when survey teams maintain the mapping in a
#' spreadsheet alongside the questionnaire.
#'
#' @param path Path to a `.csv`, `.xlsx` or `.xls` file.
#' @param concept_col,source_col Column names holding concepts and source
#'   variables.
#' @param sheet Sheet name or index for Excel files.
#' @param ... Further arguments passed to [vcs_dictionary()], such as
#'   `n_caregivers` and `n_children`.
#' @return A [vcs_dictionary()].
#' @export
#' @examples
#' tmp <- tempfile(fileext = ".csv")
#' write.csv(
#'   data.frame(concept = c("child_id", "psu"), source = c("cid", "clust")),
#'   tmp, row.names = FALSE
#' )
#' read_vcs_dictionary(tmp)
#' unlink(tmp)
read_vcs_dictionary <- function(path,
                                concept_col = "concept",
                                source_col = "source",
                                sheet = 1,
                                ...) {
  assert_string(path)
  assert_string(concept_col)
  assert_string(source_col)
  if (!file.exists(path)) {
    vcs_abort(sprintf("File does not exist: %s", path), class = "vaxsurvR_io_error")
  }
  ext <- tolower(tools::file_ext(path))
  raw <- switch(
    ext,
    csv = utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    xlsx = ,
    xls = {
      assert_installed("readxl", "Reading a dictionary from Excel")
      as.data.frame(readxl::read_excel(path, sheet = sheet))
    },
    vcs_abort(
      sprintf("Unsupported dictionary file extension: \"%s\".", ext),
      class = "vaxsurvR_io_error"
    )
  )
  assert_columns(raw, c(concept_col, source_col), arg = "dictionary file")
  keep <- !is_blank(raw[[concept_col]]) & !is_blank(raw[[source_col]])
  args <- as.list(stats::setNames(
    as.character(raw[[source_col]][keep]),
    as.character(raw[[concept_col]][keep])
  ))
  do.call(vcs_dictionary, c(args, list(...)))
}
