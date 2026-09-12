# ---------------------------------------------------------------------------
# Date handling. Vaccination-card dates are frequently partial: a caregiver
# remembers the month but not the day, or the card records only a year. The
# package never invents the missing component unless the user names a rule.
# ---------------------------------------------------------------------------

#' Parse vaccination dates, tolerating partial and malformed values
#'
#' Tries a sequence of candidate formats and records, for every value, whether
#' the result is a complete date, a partial date, missing, or unparseable. No
#' imputation happens here: a partial date parses to `NA` unless
#' [validate_partial_date()] is used with an explicit rule.
#'
#' @param x A character, Date or POSIXct vector of dates.
#' @param formats Candidate [strptime()] formats, tried in order.
#' @param partial_formats Candidate formats for year-month and year-only values.
#' @param na_values Strings treated as missing (in addition to `NA`).
#' @return A [tibble][tibble::tibble] with one row per input value:
#'   \describe{
#'     \item{input}{the original value as a string}
#'     \item{date}{parsed [Date], `NA` when the value is not a complete date}
#'     \item{year, month, day}{integer components recovered, `NA` when absent}
#'     \item{precision}{one of `"day"`, `"month"`, `"year"`, `"missing"`,
#'       `"unparseable"`}
#'   }
#' @export
#' @seealso [validate_partial_date()], [check_vaccine_dates()]
#' @examples
#' parse_vaccine_date(c("2025-03-14", "03/2025", "2025", "", "not a date"))
parse_vaccine_date <- function(x,
                               formats = c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y",
                                           "%d-%m-%Y", "%Y/%m/%d", "%b %d, %Y",
                                           "%d %b %Y", "%d.%m.%Y"),
                               partial_formats = c("%Y-%m", "%m/%Y", "%b %Y", "%Y"),
                               na_values = c("", "NA", "N/A", ".", "98", "99", "9999")) {
  assert_character(formats, allow_null = FALSE)
  assert_character(partial_formats, allow_null = FALSE)

  if (inherits(x, "Date")) {
    return(tibble::tibble(
      input = format(x),
      date = x,
      year = as.integer(format(x, "%Y")),
      month = as.integer(format(x, "%m")),
      day = as.integer(format(x, "%d")),
      precision = ifelse(is.na(x), "missing", "day")
    ))
  }
  if (inherits(x, "POSIXt")) {
    return(parse_vaccine_date(as.Date(x), formats, partial_formats, na_values))
  }

  chr <- trimws(as.character(x))
  n <- length(chr)
  out <- tibble::tibble(
    input = chr,
    date = as.Date(rep(NA_real_, n), origin = "1970-01-01"),
    year = rep(NA_integer_, n),
    month = rep(NA_integer_, n),
    day = rep(NA_integer_, n),
    precision = rep("unparseable", n)
  )
  if (!n) {
    return(out)
  }

  miss <- is.na(chr) | chr %in% na_values
  out$precision[miss] <- "missing"
  todo <- which(!miss)

  # Full dates first. strptime() is lenient about trailing text, so the value
  # is re-formatted and compared back: that rejects things which merely start
  # like a date. The comparison ignores zero padding, because exports write
  # both "5/6/2025" and "05/06/2025" and neither is wrong.
  for (fmt in formats) {
    if (!length(todo)) break
    parsed <- as.Date(strptime(chr[todo], format = fmt, tz = "UTC"))
    round_trip <- suppressWarnings(format(parsed, fmt))
    ok <- !is.na(parsed) & !is.na(round_trip) &
      unpad(trimws(round_trip)) == unpad(chr[todo])
    if (any(ok)) {
      idx <- todo[ok]
      out$date[idx] <- parsed[ok]
      out$precision[idx] <- "day"
      todo <- todo[!ok]
    }
  }

  # Partial dates: year-month, then year alone.
  for (fmt in partial_formats) {
    if (!length(todo)) break
    cand <- chr[todo]
    parsed <- as.Date(strptime(paste0(cand, "-01"), format = paste0(fmt, "-%d"), tz = "UTC"))
    ok <- !is.na(parsed) & nchar(cand) <= 10L
    if (any(ok)) {
      idx <- todo[ok]
      has_month <- grepl("m|b|B", fmt)
      out$year[idx] <- as.integer(format(parsed[ok], "%Y"))
      if (has_month) {
        out$month[idx] <- as.integer(format(parsed[ok], "%m"))
        out$precision[idx] <- "month"
      } else {
        out$precision[idx] <- "year"
      }
      todo <- todo[!ok]
    }
  }

  complete <- out$precision == "day" & !is.na(out$date)
  out$year[complete] <- as.integer(format(out$date[complete], "%Y"))
  out$month[complete] <- as.integer(format(out$date[complete], "%m"))
  out$day[complete] <- as.integer(format(out$date[complete], "%d"))
  out
}

#' Resolve partial dates under an explicit rule
#'
#' Turns the output of [parse_vaccine_date()] into a `Date` vector, applying a
#' user-chosen rule to values whose day (and possibly month) is unknown. The
#' rule is returned as an attribute so it can be recorded in the audit trail.
#'
#' There is no default that silently fills in dates: `rule = "none"` is the
#' default and leaves partial dates as `NA`.
#'
#' @param x Output of [parse_vaccine_date()], or a vector accepted by it.
#' @param rule How to resolve partial dates:
#'   \describe{
#'     \item{`"none"`}{leave partial dates missing (default)}
#'     \item{`"midpoint"`}{15th of a known month; 1 July of a known year}
#'     \item{`"first"`}{first day of the known month or year}
#'     \item{`"last"`}{last day of the known month or year}
#'   }
#' @param min_precision Minimum precision to resolve. `"month"` (default) leaves
#'   year-only values missing even when a rule is given; `"year"` resolves both.
#' @return A `Date` vector with attributes `rule`, `resolved` (logical vector
#'   marking imputed values) and `precision`.
#' @export
#' @seealso [parse_vaccine_date()]
#' @examples
#' p <- parse_vaccine_date(c("2025-03-14", "2025-03", "2025"))
#' validate_partial_date(p)
#' validate_partial_date(p, rule = "midpoint")
#' validate_partial_date(p, rule = "midpoint", min_precision = "year")
validate_partial_date <- function(x, rule = c("none", "midpoint", "first", "last"),
                                  min_precision = c("month", "year")) {
  rule <- match.arg(rule)
  min_precision <- match.arg(min_precision)
  if (!is.data.frame(x)) {
    x <- parse_vaccine_date(x)
  }
  assert_columns(x, c("date", "year", "month", "precision"), arg = "x")

  out <- x$date
  resolved <- rep(FALSE, nrow(x))

  if (rule != "none") {
    levels_ok <- if (min_precision == "month") "month" else c("month", "year")
    target <- which(x$precision %in% levels_ok & is.na(out))
    for (i in target) {
      yr <- x$year[i]
      mo <- x$month[i]
      if (is.na(yr)) next
      if (is.na(mo)) {
        # Year-only: rules operate on the calendar year.
        d <- switch(rule,
          midpoint = as.Date(sprintf("%04d-07-01", yr)),
          first = as.Date(sprintf("%04d-01-01", yr)),
          last = as.Date(sprintf("%04d-12-31", yr))
        )
      } else {
        first <- as.Date(sprintf("%04d-%02d-01", yr, mo))
        last <- seq(first, by = "month", length.out = 2)[2] - 1
        d <- switch(rule,
          midpoint = first + 14,
          first = first,
          last = last
        )
      }
      out[i] <- d
      resolved[i] <- TRUE
    }
  }

  attr(out, "rule") <- rule
  attr(out, "resolved") <- resolved
  attr(out, "precision") <- x$precision
  out
}

#' Age at vaccination, in days
#'
#' @param date_of_birth Date (or coercible) vector of birth dates.
#' @param date_of_vaccination Date (or coercible) vector of vaccination dates.
#' @return An integer vector of days. Negative values mean the dose is recorded
#'   before birth and are returned as-is so that
#'   [check_prebirth_vaccination()] can flag them; nothing is clipped.
#' @export
#' @examples
#' derive_age_at_vaccination(as.Date("2024-01-01"), as.Date("2024-03-01"))
#' derive_age_at_vaccination(as.Date("2024-01-01"), as.Date("2023-12-01"))
derive_age_at_vaccination <- function(date_of_birth, date_of_vaccination) {
  dob <- as_date_safe(date_of_birth)
  dov <- as_date_safe(date_of_vaccination)
  n <- max(length(dob), length(dov))
  if (length(dob) == 1L) dob <- rep(dob, n)
  if (length(dov) == 1L) dov <- rep(dov, n)
  if (length(dob) != length(dov)) {
    vcs_abort(
      "`date_of_birth` and `date_of_vaccination` must have the same length.",
      class = "vaxsurvR_value_error"
    )
  }
  as.integer(as.numeric(dov - dob))
}

#' Classify a dose as early, timely or late
#'
#' Compares age at vaccination against the schedule window. A dose given before
#' `minimum_age_days` is `"early"`; a dose given after `maximum_age_days` is
#' `"late"`; anything in between is `"timely"`. Where the schedule leaves a
#' bound unset the corresponding verdict cannot be reached and the dose is
#' `"timely"` if the other bound is satisfied, or `NA` if neither is set.
#'
#' @param age_days Integer vector of ages at vaccination in days.
#' @param vaccine Character vector of vaccine names, same length as `age_days`
#'   or length 1.
#' @param schedule A [vcs_schedule()].
#' @return A character vector of `"early"`, `"timely"`, `"late"` or `NA`.
#' @export
#' @examples
#' s <- vcs_schedule_who(c("BCG", "PENTA1", "MCV1"))
#' classify_vaccination_timeliness(c(1, 20, 400), c("BCG", "PENTA1", "MCV1"), s)
classify_vaccination_timeliness <- function(age_days, vaccine, schedule) {
  assert_schedule(schedule)
  age_days <- as.numeric(age_days)
  vaccine <- as.character(vaccine)
  n <- max(length(age_days), length(vaccine))
  if (length(age_days) == 1L) age_days <- rep(age_days, n)
  if (length(vaccine) == 1L) vaccine <- rep(vaccine, n)
  if (length(age_days) != length(vaccine)) {
    vcs_abort("`age_days` and `vaccine` must have the same length.",
              class = "vaxsurvR_value_error")
  }
  idx <- match(vaccine, schedule$vaccine)
  lo <- schedule$minimum_age_days[idx]
  hi <- schedule$maximum_age_days[idx]

  out <- rep(NA_character_, n)
  known <- !is.na(age_days) & (!is.na(lo) | !is.na(hi))
  out[known] <- "timely"
  out[known & !is.na(lo) & age_days < lo] <- "early"
  out[known & !is.na(hi) & age_days > hi] <- "late"
  out
}

#' Strip leading zeros from each number in a date string
#'
#' So that "05/06/2025" and "5/6/2025" compare equal on the round-trip check.
#' @noRd
unpad <- function(x) {
  gsub("(^|[^0-9])0+([0-9])", "\\1\\2", x)
}

#' Coerce to Date without warning noise
#' @noRd
as_date_safe <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1970-01-01"))
  }
  parse_vaccine_date(x)$date
}
