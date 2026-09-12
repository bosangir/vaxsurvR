# ---------------------------------------------------------------------------
# The vaccination schedule object. Antigens, minimum ages, dose sequences and
# minimum intervals are data, never constants baked into check functions.
# ---------------------------------------------------------------------------

#' Define a vaccination schedule
#'
#' A schedule describes the antigens a survey measures and the chronological
#' rules that make a recorded dose plausible. Every data-quality and derivation
#' function in vaxsurvR takes a schedule rather than assuming a country's EPI
#' calendar.
#'
#' @param vaccine Character vector of vaccine/dose names, in the order they
#'   appear on the vaccination card. Must be unique and non-missing.
#' @param minimum_age_days Numeric vector: the earliest age, in days since
#'   birth, at which the dose may validly be given. `NA` means unconstrained.
#' @param maximum_age_days Numeric vector: the latest age in days at which the
#'   dose is still considered timely. `NA` means unconstrained. Used by
#'   [classify_vaccination_timeliness()], never to invalidate a dose.
#' @param previous_dose Character vector naming the dose that must precede each
#'   entry, or `NA` for the first dose of a series. Values must themselves be
#'   entries in `vaccine`.
#' @param minimum_interval_days Numeric vector: minimum number of days between
#'   `previous_dose` and this dose. `NA` means unconstrained.
#' @param antigen Optional character vector grouping doses into antigen series
#'   (e.g. `PENTA1`, `PENTA2`, `PENTA3` all have antigen `PENTA`). Defaults to
#'   the vaccine name with trailing digits stripped.
#' @param dose Optional integer dose number within the antigen series. Defaults
#'   to the trailing digits of the vaccine name, or 1.
#' @param label Optional display labels.
#'
#' @return An object of class `vcs_schedule`, which is also a tibble.
#' @export
#' @seealso [vcs_schedule_who()], [check_vaccine_sequence()],
#'   [check_age_at_vaccination()]
#' @examples
#' vcs_schedule(
#'   vaccine = c("BCG", "PENTA1", "PENTA2", "PENTA3", "MCV1"),
#'   minimum_age_days = c(0, 42, 70, 98, 270),
#'   previous_dose = c(NA, NA, "PENTA1", "PENTA2", NA),
#'   minimum_interval_days = c(NA, NA, 28, 28, NA)
#' )
vcs_schedule <- function(vaccine,
                         minimum_age_days = NA_real_,
                         maximum_age_days = NA_real_,
                         previous_dose = NA_character_,
                         minimum_interval_days = NA_real_,
                         antigen = NULL,
                         dose = NULL,
                         label = NULL) {
  assert_character(vaccine, allow_null = FALSE)
  if (!length(vaccine)) {
    vcs_abort("`vaccine` must name at least one vaccine.",
              class = "vaxsurvR_schedule_error")
  }
  if (anyNA(vaccine) || any(!nzchar(trimws(vaccine)))) {
    vcs_abort("`vaccine` must not contain missing or empty names.",
              class = "vaxsurvR_schedule_error")
  }
  if (anyDuplicated(vaccine)) {
    dup <- unique(vaccine[duplicated(vaccine)])
    vcs_abort(sprintf("`vaccine` names must be unique; duplicated: %s.",
                      collapse_quote(dup)),
              class = "vaxsurvR_schedule_error")
  }

  n <- length(vaccine)
  recycle <- function(x, nm, type = "numeric") {
    if (length(x) == 1L) x <- rep(x, n)
    if (length(x) != n) {
      vcs_abort(
        sprintf("`%s` must have length 1 or %d, not %d.", nm, n, length(x)),
        class = "vaxsurvR_schedule_error"
      )
    }
    if (identical(type, "numeric")) as.numeric(x) else as.character(x)
  }

  minimum_age_days <- recycle(minimum_age_days, "minimum_age_days")
  maximum_age_days <- recycle(maximum_age_days, "maximum_age_days")
  minimum_interval_days <- recycle(minimum_interval_days, "minimum_interval_days")
  previous_dose <- recycle(previous_dose, "previous_dose", "character")

  bad_prev <- setdiff(stats::na.omit(previous_dose), vaccine)
  if (length(bad_prev)) {
    vcs_abort(
      sprintf("`previous_dose` refers to unknown vaccine%s: %s.",
              if (length(bad_prev) > 1L) "s" else "", collapse_quote(bad_prev)),
      class = "vaxsurvR_schedule_error"
    )
  }
  self_ref <- which(!is.na(previous_dose) & previous_dose == vaccine)
  if (length(self_ref)) {
    vcs_abort(
      sprintf("A dose cannot be its own `previous_dose`: %s.",
              collapse_quote(vaccine[self_ref])),
      class = "vaxsurvR_schedule_error"
    )
  }
  if (any(minimum_age_days < 0, na.rm = TRUE)) {
    vcs_abort("`minimum_age_days` must not be negative.",
              class = "vaxsurvR_schedule_error")
  }
  if (any(minimum_interval_days < 0, na.rm = TRUE)) {
    vcs_abort("`minimum_interval_days` must not be negative.",
              class = "vaxsurvR_schedule_error")
  }
  bad_window <- which(!is.na(minimum_age_days) & !is.na(maximum_age_days) &
                        maximum_age_days < minimum_age_days)
  if (length(bad_window)) {
    vcs_abort(
      sprintf("`maximum_age_days` is below `minimum_age_days` for %s.",
              collapse_quote(vaccine[bad_window])),
      class = "vaxsurvR_schedule_error"
    )
  }
  detect_cycle(vaccine, previous_dose)

  if (is.null(antigen)) {
    antigen <- toupper(sub("[0-9]+$", "", vaccine))
    antigen[!nzchar(antigen)] <- toupper(vaccine[!nzchar(antigen)])
  }
  antigen <- recycle(antigen, "antigen", "character")

  if (is.null(dose)) {
    trailing <- regmatches(vaccine, regexpr("[0-9]+$", vaccine))
    dose <- rep(1L, n)
    has <- grepl("[0-9]+$", vaccine)
    dose[has] <- as.integer(trailing)
  }
  dose <- as.integer(recycle(dose, "dose"))

  label <- if (is.null(label)) vaccine else recycle(label, "label", "character")

  out <- tibble::tibble(
    vaccine = vaccine,
    antigen = antigen,
    dose = dose,
    label = label,
    minimum_age_days = minimum_age_days,
    maximum_age_days = maximum_age_days,
    previous_dose = previous_dose,
    minimum_interval_days = minimum_interval_days
  )
  class(out) <- c("vcs_schedule", class(out))
  out
}

#' Detect circular `previous_dose` chains
#' @noRd
detect_cycle <- function(vaccine, previous_dose) {
  prev <- stats::setNames(previous_dose, vaccine)
  for (start in vaccine) {
    seen <- character(0)
    node <- start
    while (!is.na(node)) {
      if (node %in% seen) {
        vcs_abort(
          sprintf("`previous_dose` forms a circular chain involving %s.",
                  collapse_quote(unique(c(seen, node)))),
          class = "vaxsurvR_schedule_error"
        )
      }
      seen <- c(seen, node)
      node <- unname(prev[node])
      if (length(node) == 0L) break
    }
  }
  invisible(TRUE)
}

#' A WHO-style default infant immunisation schedule
#'
#' A convenience starting point covering the antigens most household coverage
#' surveys measure. It is a *template*, not a standard: adapt it to the national
#' EPI calendar of the country being surveyed before using it in production.
#'
#' @param vaccines Optional subset of vaccine names to keep, in the order given.
#' @return A [vcs_schedule()].
#' @export
#' @examples
#' vcs_schedule_who()
#' vcs_schedule_who(c("BCG", "PENTA1", "PENTA3", "MCV1"))
vcs_schedule_who <- function(vaccines = NULL) {
  s <- vcs_schedule(
    vaccine = c("BCG", "OPV0", "OPV1", "PENTA1", "PCV1", "ROTA1",
                "OPV2", "PENTA2", "PCV2", "ROTA2",
                "OPV3", "IPV1", "PENTA3", "PCV3", "ROTA3",
                "MCV1", "YF", "MCV2", "IPV2"),
    minimum_age_days = c(0, 0, 42, 42, 42, 42,
                         70, 70, 70, 70,
                         98, 98, 98, 98, 98,
                         270, 270, 450, 270),
    maximum_age_days = c(28, 14, 76, 76, 76, 76,
                         104, 104, 104, 104,
                         132, 132, 132, 132, 132,
                         330, 330, 540, 330),
    previous_dose = c(NA, NA, "OPV0", NA, NA, NA,
                      "OPV1", "PENTA1", "PCV1", "ROTA1",
                      "OPV2", NA, "PENTA2", "PCV2", "ROTA2",
                      NA, NA, "MCV1", "IPV1"),
    minimum_interval_days = c(NA, NA, 28, NA, NA, NA,
                              28, 28, 28, 28,
                              28, NA, 28, 28, 28,
                              NA, NA, 28, 28),
    antigen = c("BCG", "OPV", "OPV", "PENTA", "PCV", "ROTA",
                "OPV", "PENTA", "PCV", "ROTA",
                "OPV", "IPV", "PENTA", "PCV", "ROTA",
                "MCV", "YF", "MCV", "IPV"),
    dose = c(1L, 0L, 1L, 1L, 1L, 1L,
             2L, 2L, 2L, 2L,
             3L, 1L, 3L, 3L, 3L,
             1L, 1L, 2L, 2L)
  )
  if (is.null(vaccines)) {
    return(s)
  }
  assert_character(vaccines, allow_null = FALSE)
  unknown <- setdiff(vaccines, s$vaccine)
  if (length(unknown)) {
    vcs_abort(
      sprintf("Unknown vaccine%s: %s.",
              if (length(unknown) > 1L) "s" else "", collapse_quote(unknown)),
      class = "vaxsurvR_schedule_error"
    )
  }
  out <- s[match(vaccines, s$vaccine), , drop = FALSE]
  # A retained dose whose predecessor was dropped loses its interval rule
  # rather than silently pointing at a vaccine that is no longer present.
  drop_prev <- !is.na(out$previous_dose) & !out$previous_dose %in% vaccines
  out$previous_dose[drop_prev] <- NA_character_
  out$minimum_interval_days[drop_prev] <- NA_real_
  class(out) <- c("vcs_schedule", class(tibble::tibble()))
  out
}

#' Test whether an object is a vcs_schedule
#'
#' @param x An object.
#' @return A logical scalar.
#' @export
#' @examples
#' is_vcs_schedule(vcs_schedule_who())
is_vcs_schedule <- function(x) inherits(x, "vcs_schedule")

#' @export
print.vcs_schedule <- function(x, ...) {
  cat(sprintf("<vcs_schedule: %d dose(s), %d antigen(s)>\n",
              nrow(x), length(unique(x$antigen))))
  print(tibble::as_tibble(x), ...)
  invisible(x)
}

#' Doses making up the default "fully vaccinated" definition of a schedule
#'
#' Returns the last dose of each antigen series, which is the usual basis for a
#' fully-vaccinated definition. It is a suggestion the user can override, not a
#' definition the package imposes; see [derive_fully_vaccinated()].
#'
#' @param schedule A [vcs_schedule()].
#' @return A character vector of vaccine names.
#' @export
#' @examples
#' schedule_final_doses(vcs_schedule_who())
schedule_final_doses <- function(schedule) {
  assert_schedule(schedule)
  idx <- unlist(lapply(split(seq_len(nrow(schedule)), schedule$antigen), function(i) {
    i[which.max(schedule$dose[i])]
  }), use.names = FALSE)
  schedule$vaccine[sort(idx)]
}

#' @noRd
assert_schedule <- function(x, arg = rlang::caller_arg(x)) {
  if (!is_vcs_schedule(x)) {
    vcs_abort(
      sprintf("`%s` must be a `vcs_schedule`, not %s. See `vcs_schedule()`.",
              arg, obj_type(x)),
      class = "vaxsurvR_type_error"
    )
  }
  invisible(x)
}
