# ---------------------------------------------------------------------------
# Caregiver recall crosswalks.
#
# Recall questions rarely mirror the card block one-for-one. A form typically
# asks "ever received?" once per antigen series and "how many times?" as a
# follow-up, with a plain yes/no only for single-dose antigens. The helpers
# here turn that questionnaire structure into the standardised columns the
# dictionary can map with {vaccine} and {antigen} placeholders.
# ---------------------------------------------------------------------------

#' Describe how recall questions map onto the schedule
#'
#' Each entry names either a single dose (a vaccine in the schedule) mapped to
#' one yes/no variable, or an antigen series mapped to an "ever received"
#' variable and, optionally, a dose-count variable. Variable names are
#' templates and may use `{c}` and `{k}` placeholders.
#'
#' @param ... Named entries. A character string maps a **vaccine** to a per-dose
#'   yes/no variable. A list with elements `ever` and optionally `count` maps
#'   an **antigen** series.
#' @return An object of class `vcs_recall_map`.
#' @export
#' @seealso [apply_recall_map()]
#' @examples
#' vcs_recall_map(
#'   BCG   = "VR01_{c}_{k}",
#'   OPV0  = "VR03_{c}_{k}",
#'   OPV   = list(ever = "VR04_{c}_{k}", count = "VR05_{c}_{k}"),
#'   PENTA = list(ever = "VR08_{c}_{k}", count = "VR09_{c}_{k}"),
#'   MCV   = list(ever = "VR12_{c}_{k}", count = "VR13_{c}_{k}")
#' )
vcs_recall_map <- function(...) {
  entries <- rlang::list2(...)
  if (!length(entries) || is.null(names(entries)) || any(!nzchar(names(entries)))) {
    vcs_abort("All entries of a recall map must be named.",
              class = "vaxsurvR_recall_error")
  }
  if (anyDuplicated(names(entries))) {
    vcs_abort("Recall map names must be unique.", class = "vaxsurvR_recall_error")
  }
  for (nm in names(entries)) {
    e <- entries[[nm]]
    ok <- (is.character(e) && length(e) == 1L && !is.na(e)) ||
      (is.list(e) && "ever" %in% names(e) &&
         is.character(e$ever) && length(e$ever) == 1L &&
         all(names(e) %in% c("ever", "count")))
    if (!ok) {
      vcs_abort(
        sprintf(paste0("Entry \"%s\" must be a single variable name (per-dose) ",
                       "or a list(ever = , count = ) (per series)."), nm),
        class = "vaxsurvR_recall_error"
      )
    }
  }
  structure(entries, class = "vcs_recall_map")
}

#' @export
print.vcs_recall_map <- function(x, ...) {
  cat(sprintf("<vcs_recall_map: %d entr%s>\n", length(x),
              if (length(x) == 1L) "y" else "ies"))
  for (nm in names(x)) {
    e <- x[[nm]]
    if (is.character(e)) {
      cat(sprintf("  %-8s dose   <- %s\n", nm, e))
    } else {
      cat(sprintf("  %-8s series <- ever: %s%s\n", nm, e$ever,
                  if (!is.null(e$count)) sprintf(", count: %s", e$count) else ""))
    }
  }
  invisible(x)
}

#' Build standardised recall columns from a crosswalk
#'
#' Creates, for every repeat slot, the columns
#' `recall_status_<vaccine>_<c>_<k>`, `recall_ever_<antigen>_<c>_<k>` and
#' `recall_count_<antigen>_<c>_<k>` implied by a [vcs_recall_map()], so that the
#' dictionary can map them with
#'
#' ```
#' recall_status = "recall_status_{vaccine}_{c}_{k}",
#' recall_ever   = "recall_ever_{antigen}_{c}_{k}",
#' recall_count  = "recall_count_{antigen}_{c}_{k}"
#' ```
#'
#' Columns are created for *every* vaccine and antigen in the schedule -- filled
#' with `NA` where the map says nothing -- so that no mapped column is absent
#' from the data.
#'
#' @param data A data frame (the raw export).
#' @param map A [vcs_recall_map()].
#' @param schedule A [vcs_schedule()].
#' @param n_caregivers,n_children Repeat extents, as in [vcs_dictionary()].
#'   Use `1` for long-format data with no repeats.
#' @param repeats Do the map's templates use `{c}` / `{k}`? Set to `FALSE` for
#'   long-format data; the created columns then carry no suffix.
#' @return `data` with the recall columns added, and an attribute
#'   `recall_map_report` describing which schedule entries were covered.
#' @export
#' @examples
#' raw <- data.frame(
#'   KEY = c("h1", "h2"),
#'   VR01_1_1 = c("1", "2"),                 # BCG yes / no
#'   VR08_1_1 = c("1", "1"),                 # PENTA ever
#'   VR09_1_1 = c("3", "1"),                 # PENTA count
#'   stringsAsFactors = FALSE
#' )
#' sched <- vcs_schedule_who(c("BCG", "PENTA1", "PENTA2", "PENTA3"))
#' rm <- vcs_recall_map(
#'   BCG = "VR01_{c}_{k}",
#'   PENTA = list(ever = "VR08_{c}_{k}", count = "VR09_{c}_{k}")
#' )
#' out <- apply_recall_map(raw, rm, sched, n_caregivers = 1, n_children = 1)
#' out[, grep("^recall_", names(out))]
apply_recall_map <- function(data, map, schedule, n_caregivers = 1L,
                             n_children = 1L, repeats = TRUE) {
  assert_data(data)
  if (!inherits(map, "vcs_recall_map")) {
    vcs_abort("`map` must be a `vcs_recall_map`.", class = "vaxsurvR_type_error")
  }
  assert_schedule(schedule)
  assert_number(n_caregivers, lower = 1)
  assert_number(n_children, lower = 1)
  assert_flag(repeats)

  known_vaccines <- schedule$vaccine
  known_antigens <- unique(schedule$antigen)
  unknown <- setdiff(names(map), c(known_vaccines, known_antigens))
  if (length(unknown)) {
    vcs_abort(
      sprintf("Recall map name%s not in the schedule as a vaccine or antigen: %s.",
              if (length(unknown) > 1L) "s" else "", collapse_quote(unknown)),
      class = "vaxsurvR_recall_error"
    )
  }
  per_dose <- names(map)[vapply(map, is.character, logical(1))]
  per_series <- setdiff(names(map), per_dose)
  bad_dose <- setdiff(per_dose, known_vaccines)
  bad_series <- setdiff(per_series, known_antigens)
  if (length(bad_dose)) {
    vcs_abort(
      sprintf("Per-dose entr%s %s must name a vaccine, not an antigen.",
              if (length(bad_dose) > 1L) "ies" else "y", collapse_quote(bad_dose)),
      class = "vaxsurvR_recall_error"
    )
  }
  if (length(bad_series)) {
    vcs_abort(
      sprintf("Series entr%s %s must name an antigen, not a vaccine.",
              if (length(bad_series) > 1L) "ies" else "y", collapse_quote(bad_series)),
      class = "vaxsurvR_recall_error"
    )
  }

  slots <- if (repeats) {
    expand.grid(c = seq_len(n_caregivers), k = seq_len(n_children))
  } else {
    data.frame(c = NA_integer_, k = NA_integer_)
  }
  suffix <- function(c_i, k) if (repeats) sprintf("_%d_%d", c_i, k) else ""
  n <- nrow(data)
  missing_src <- character(0)

  source_col <- function(template, c_i, k) {
    col <- if (repeats) fill_template(template, list(c = c_i, k = k)) else template
    if (!col %in% names(data)) {
      missing_src <<- c(missing_src, col)
      return(rep(NA_character_, n))
    }
    as.character(data[[col]])
  }

  for (i in seq_len(nrow(slots))) {
    c_i <- slots$c[i]; k <- slots$k[i]; sfx <- suffix(c_i, k)
    for (v in known_vaccines) {
      data[[paste0("recall_status_", v, sfx)]] <-
        if (v %in% per_dose) source_col(map[[v]], c_i, k) else rep(NA_character_, n)
    }
    for (a in known_antigens) {
      e <- if (a %in% per_series) map[[a]] else NULL
      data[[paste0("recall_ever_", a, sfx)]] <-
        if (!is.null(e)) source_col(e$ever, c_i, k) else rep(NA_character_, n)
      data[[paste0("recall_count_", a, sfx)]] <-
        if (!is.null(e) && !is.null(e$count)) source_col(e$count, c_i, k) else rep(NA_character_, n)
    }
  }

  covered <- vapply(known_vaccines, function(v) {
    v %in% per_dose || schedule$antigen[schedule$vaccine == v] %in% per_series
  }, logical(1))
  attr(data, "recall_map_report") <- list(
    covered = known_vaccines[covered],
    uncovered = known_vaccines[!covered],
    missing_source_columns = unique(missing_src)
  )
  if (length(missing_src)) {
    vcs_warn(
      sprintf("%d recall source column%s absent from the data: %s.",
              length(unique(missing_src)),
              if (length(unique(missing_src)) > 1L) "s" else "",
              collapse_quote(unique(missing_src))),
      class = "vaxsurvR_missing_column"
    )
  }
  data
}

#' Dictionary entries that pick up the columns [apply_recall_map()] creates
#'
#' @param repeats Whether the columns carry a `_{c}_{k}` suffix.
#' @return A named list to splice into [vcs_dictionary()].
#' @export
#' @examples
#' recall_dictionary_entries()
#' # do.call(vcs_dictionary, c(list(psu = "cluster"), recall_dictionary_entries()))
recall_dictionary_entries <- function(repeats = TRUE) {
  assert_flag(repeats)
  sfx <- if (repeats) "_{c}_{k}" else ""
  list(
    recall_status = paste0("recall_status_{vaccine}", sfx),
    recall_ever = paste0("recall_ever_{antigen}", sfx),
    recall_count = paste0("recall_count_{antigen}", sfx)
  )
}
