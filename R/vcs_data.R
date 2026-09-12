# ---------------------------------------------------------------------------
# The vcs_data container. Holds the untouched import alongside the standardised
# child-, household- and vaccination-level views. Nothing in the package ever
# writes back to `raw`.
# ---------------------------------------------------------------------------

#' Construct a vcs_data object
#'
#' Low-level constructor. Most users reach a `vcs_data` through
#' [map_vcs_variables()] rather than calling this directly.
#'
#' @param children Tibble with one row per eligible child.
#' @param vaccinations Tibble with one row per child-vaccine combination.
#' @param households Tibble with one row per interview/household.
#' @param dictionary The [vcs_dictionary()] used for mapping.
#' @param schedule The [vcs_schedule()] used for mapping.
#' @param raw The untouched imported data frame.
#' @param meta A named list of provenance metadata.
#' @return An object of class `vcs_data`.
#' @export
#' @examples
#' new_vcs_data(
#'   children = tibble::tibble(child_id = "c1"),
#'   vaccinations = tibble::tibble(child_id = "c1", vaccine = "BCG"),
#'   households = tibble::tibble(interview_id = "h1")
#' )
new_vcs_data <- function(children,
                         vaccinations,
                         households,
                         dictionary = NULL,
                         schedule = NULL,
                         raw = NULL,
                         meta = list()) {
  assert_data(children)
  assert_data(vaccinations)
  assert_data(households)
  structure(
    list(
      children = tibble::as_tibble(children),
      vaccinations = tibble::as_tibble(vaccinations),
      households = tibble::as_tibble(households),
      dictionary = dictionary,
      schedule = schedule,
      raw = raw,
      meta = utils::modifyList(
        list(created = vcs_timestamp(), vaxsurvR_version = vcs_version()),
        meta
      )
    ),
    class = "vcs_data"
  )
}

#' Test whether an object is a vcs_data
#'
#' @param x An object.
#' @return A logical scalar.
#' @export
#' @examples
#' is_vcs_data(vcs_example)
is_vcs_data <- function(x) inherits(x, "vcs_data")

#' @noRd
assert_vcs_data <- function(x, arg = rlang::caller_arg(x)) {
  if (!is_vcs_data(x)) {
    vcs_abort(
      sprintf("`%s` must be a `vcs_data`, not %s. See `map_vcs_variables()`.",
              arg, obj_type(x)),
      class = "vaxsurvR_type_error"
    )
  }
  invisible(x)
}

#' @export
print.vcs_data <- function(x, ...) {
  cat("<vcs_data>\n")
  cat(sprintf("  households  : %d\n", nrow(x$households)))
  cat(sprintf("  children    : %d\n", nrow(x$children)))
  cat(sprintf("  vaccinations: %d rows (%d vaccine[s])\n",
              nrow(x$vaccinations),
              length(unique(x$vaccinations$vaccine))))
  if (!is.null(x$raw)) {
    cat(sprintf("  raw import  : %d x %d (preserved)\n", nrow(x$raw), ncol(x$raw)))
  }
  derived <- names(x$children)[vapply(x$children, function(col) {
    isTRUE(attr(col, "vcs_derived"))
  }, logical(1))]
  if (length(derived)) {
    cat(sprintf("  derived     : %s\n", collapse_quote(derived)))
  }
  if (!is.null(x$meta$source)) {
    cat(sprintf("  source      : %s\n", x$meta$source))
  }
  invisible(x)
}

#' Summarise a vcs_data object
#'
#' @param object A [vcs_data][new_vcs_data] object.
#' @param ... Unused.
#' @return A list of class `vcs_data_summary`, printed for humans and
#'   convertible with [as.data.frame()].
#' @export
#' @examples
#' summary(vcs_example)
summary.vcs_data <- function(object, ...) {
  ch <- vcs_children(object)
  vx <- object$vaccinations
  counts <- c(
    households = nrow(object$households),
    children = nrow(ch),
    psus = n_distinct_safe(ch[["psu"]]),
    segments = n_distinct_safe(ch[["segment"]]),
    strata = n_distinct_safe(ch[["stratum"]]),
    vaccines = n_distinct_safe(vx[["vaccine"]])
  )
  card <- if ("card_seen" %in% names(ch)) {
    mean(ch$card_seen, na.rm = TRUE)
  } else {
    NA_real_
  }
  out <- list(
    counts = counts,
    card_seen_prop = card,
    card_documented_prop = if ("card_documented" %in% names(vx)) {
      mean(vx$card_documented, na.rm = TRUE)
    } else {
      NA_real_
    },
    date_complete_prop = if ("card_date" %in% names(vx)) {
      mean(!is.na(vx$card_date[!is.na(vx$card_documented) & vx$card_documented]))
    } else {
      NA_real_
    },
    meta = object$meta
  )
  class(out) <- "vcs_data_summary"
  out
}

#' @export
print.vcs_data_summary <- function(x, ...) {
  cat("<vcs_data summary>\n")
  for (nm in names(x$counts)) {
    cat(sprintf("  %-12s %s\n", nm, format(x$counts[[nm]], big.mark = ",")))
  }
  pct <- function(p) if (is.na(p) || is.nan(p)) "not available" else sprintf("%.1f%%", 100 * p)
  cat(sprintf("  %-12s %s\n", "card seen", pct(x$card_seen_prop)))
  cat(sprintf("  %-12s %s\n", "card doses", pct(x$card_documented_prop)))
  cat(sprintf("  %-12s %s\n", "dated doses", pct(x$date_complete_prop)))
  invisible(x)
}

#' @export
as.data.frame.vcs_data_summary <- function(x, ...) {
  tibble::tibble(
    metric = c(names(x$counts), "card_seen_prop", "card_documented_prop",
               "date_complete_prop"),
    value = c(as.numeric(x$counts), x$card_seen_prop, x$card_documented_prop,
              x$date_complete_prop)
  )
}

#' Child-level analysis table
#'
#' Extracts the child-level table, optionally joining the household-level
#' variables that survey design and domain estimation need. This is the table
#' [vcs_design()] consumes.
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param include_household Join household-level columns not already present at
#'   the child level.
#' @return A tibble with one row per child.
#' @export
#' @examples
#' head(vcs_children(vcs_example))
vcs_children <- function(x, include_household = TRUE) {
  assert_vcs_data(x)
  assert_flag(include_household)
  out <- x$children
  if (include_household && nrow(x$households) &&
      "interview_id" %in% names(out) && "interview_id" %in% names(x$households)) {
    extra <- setdiff(names(x$households), names(out))
    if (length(extra)) {
      out <- dplyr::left_join(
        out,
        x$households[, c("interview_id", extra), drop = FALSE],
        by = "interview_id"
      )
    }
  }
  out
}

#' Long vaccination table
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param vaccines Optional subset of vaccine names.
#' @return A tibble with one row per child-vaccine combination.
#' @export
#' @examples
#' head(vcs_vaccinations(vcs_example, vaccines = "BCG"))
vcs_vaccinations <- function(x, vaccines = NULL) {
  assert_vcs_data(x)
  assert_character(vaccines)
  out <- x$vaccinations
  if (!is.null(vaccines)) {
    unknown <- setdiff(vaccines, unique(out$vaccine))
    if (length(unknown)) {
      vcs_warn(
        sprintf("Vaccine%s not present in the data: %s.",
                if (length(unknown) > 1L) "s" else "", collapse_quote(unknown)),
        class = "vaxsurvR_unknown_vaccine"
      )
    }
    out <- out[out$vaccine %in% vaccines, , drop = FALSE]
  }
  out
}

#' Mark a column as derived rather than observed
#'
#' vaxsurvR keeps observed and derived variables distinguishable. Derived
#' columns carry a `vcs_derived` attribute recording the rule that produced
#' them; [derivation_rules()] reads it back.
#'
#' @param x A vector.
#' @param rule A short human-readable description of the derivation rule.
#' @return `x`, with attributes set.
#' @export
#' @examples
#' z <- mark_derived(c(TRUE, FALSE), "card date present")
#' attr(z, "vcs_rule")
mark_derived <- function(x, rule) {
  assert_string(rule)
  attr(x, "vcs_derived") <- TRUE
  attr(x, "vcs_rule") <- rule
  x
}

#' List the derivation rules recorded in a table
#'
#' @param x A data frame, or a [vcs_data][new_vcs_data] object.
#' @return A tibble with columns `variable` and `rule`.
#' @export
#' @examples
#' derivation_rules(vcs_example)
derivation_rules <- function(x) {
  if (is_vcs_data(x)) {
    tables <- list(children = x$children, vaccinations = x$vaccinations,
                   households = x$households)
  } else {
    assert_data(x)
    tables <- list(data = x)
  }
  out <- lapply(names(tables), function(tn) {
    tb <- tables[[tn]]
    keep <- vapply(tb, function(col) isTRUE(attr(col, "vcs_derived")), logical(1))
    if (!any(keep)) {
      return(NULL)
    }
    tibble::tibble(
      table = tn,
      variable = names(tb)[keep],
      rule = vapply(tb[keep], function(col) attr(col, "vcs_rule") %||% NA_character_,
                    character(1))
    )
  })
  out <- out[!vapply(out, is.null, logical(1))]
  if (!length(out)) {
    return(tibble::tibble(table = character(0), variable = character(0),
                          rule = character(0)))
  }
  dplyr::bind_rows(out)
}

#' @noRd
n_distinct_safe <- function(x) {
  if (is.null(x)) {
    return(0L)
  }
  length(unique(x[!is.na(x)]))
}
