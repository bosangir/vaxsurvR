# ---------------------------------------------------------------------------
# Internal utilities: argument validation and small helpers.
#
# vaxsurvR deliberately keeps its Imports conservative. Argument checking is
# implemented with rlang conditions rather than pulling in an extra validation
# dependency; the helpers below give checkmate-style guarantees with informative
# messages and stable condition classes that tests can assert on.
# ---------------------------------------------------------------------------

#' Abort with a vaxsurvR condition class
#' @noRd
vcs_abort <- function(message, class = NULL, ...) {
  rlang::abort(message, class = c(class, "vaxsurvR_error"), ...)
}

#' Warn with a vaxsurvR condition class
#' @noRd
vcs_warn <- function(message, class = NULL, ...) {
  rlang::warn(message, class = c(class, "vaxsurvR_warning"), ...)
}

#' Assert that `x` is a data frame
#' @noRd
assert_data <- function(x, arg = rlang::caller_arg(x)) {
  if (!is.data.frame(x)) {
    vcs_abort(
      sprintf("`%s` must be a data frame, not %s.", arg, obj_type(x)),
      class = "vaxsurvR_type_error"
    )
  }
  invisible(x)
}

#' Assert that `x` is a single non-missing string
#' @noRd
assert_string <- function(x, arg = rlang::caller_arg(x), allow_null = FALSE) {
  if (is.null(x) && allow_null) {
    return(invisible(x))
  }
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    vcs_abort(
      sprintf("`%s` must be a single non-missing string.", arg),
      class = "vaxsurvR_type_error"
    )
  }
  invisible(x)
}

#' Assert that `x` is a character vector
#' @noRd
assert_character <- function(x, arg = rlang::caller_arg(x), allow_null = TRUE) {
  if (is.null(x) && allow_null) {
    return(invisible(x))
  }
  if (!is.character(x)) {
    vcs_abort(
      sprintf("`%s` must be a character vector, not %s.", arg, obj_type(x)),
      class = "vaxsurvR_type_error"
    )
  }
  invisible(x)
}

#' Assert that `x` is `TRUE` or `FALSE`
#' @noRd
assert_flag <- function(x, arg = rlang::caller_arg(x)) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    vcs_abort(
      sprintf("`%s` must be `TRUE` or `FALSE`.", arg),
      class = "vaxsurvR_type_error"
    )
  }
  invisible(x)
}

#' Assert that `x` is a single number, optionally bounded
#' @noRd
assert_number <- function(x, arg = rlang::caller_arg(x),
                          lower = -Inf, upper = Inf, allow_null = FALSE) {
  if (is.null(x) && allow_null) {
    return(invisible(x))
  }
  if (!is.numeric(x) || length(x) != 1L || is.na(x)) {
    vcs_abort(
      sprintf("`%s` must be a single non-missing number.", arg),
      class = "vaxsurvR_type_error"
    )
  }
  if (x < lower || x > upper) {
    vcs_abort(
      sprintf("`%s` must be between %s and %s, not %s.", arg, lower, upper, x),
      class = "vaxsurvR_value_error"
    )
  }
  invisible(x)
}

#' Assert that named columns exist in `data`
#' @noRd
assert_columns <- function(data, cols, arg = rlang::caller_arg(data)) {
  missing <- setdiff(cols, names(data))
  if (length(missing)) {
    vcs_abort(
      sprintf(
        "`%s` is missing required column%s: %s.",
        arg, if (length(missing) > 1L) "s" else "", collapse_quote(missing)
      ),
      class = "vaxsurvR_missing_column"
    )
  }
  invisible(data)
}

#' Match an argument against allowed values
#' @noRd
assert_choice <- function(x, choices, arg = rlang::caller_arg(x)) {
  assert_string(x, arg = arg)
  if (!x %in% choices) {
    vcs_abort(
      sprintf("`%s` must be one of %s, not \"%s\".", arg, collapse_quote(choices), x),
      class = "vaxsurvR_value_error"
    )
  }
  invisible(x)
}

#' Assert that a suggested package is installed
#' @noRd
assert_installed <- function(pkg, why) {
  missing <- pkg[!vapply(pkg, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    vcs_abort(
      sprintf(
        "%s requires the %s package%s.",
        why, collapse_quote(missing), if (length(missing) > 1L) "s" else ""
      ),
      class = "vaxsurvR_missing_package"
    )
  }
  invisible(TRUE)
}

#' @noRd
obj_type <- function(x) {
  if (is.null(x)) "NULL" else paste0("a ", class(x)[1])
}

#' @noRd
collapse_quote <- function(x, max = 10L) {
  x <- as.character(x)
  extra <- length(x) - max
  if (extra > 0L) {
    x <- c(x[seq_len(max)], sprintf("... and %d more", extra))
  }
  paste0("\"", x, "\"", collapse = ", ")
}

#' Coerce a one-sided formula, character vector or NULL to column names
#'
#' Accepts `~a + b`, `c("a", "b")` or `NULL`.
#' @noRd
as_column_names <- function(x, arg = rlang::caller_arg(x)) {
  if (is.null(x)) {
    return(character(0))
  }
  if (is.character(x)) {
    return(x)
  }
  if (rlang::is_formula(x)) {
    if (length(x) != 2L) {
      vcs_abort(
        sprintf("`%s` must be a one-sided formula such as `~province`.", arg),
        class = "vaxsurvR_type_error"
      )
    }
    return(all.vars(x))
  }
  vcs_abort(
    sprintf("`%s` must be a one-sided formula, a character vector or `NULL`.", arg),
    class = "vaxsurvR_type_error"
  )
}

#' Is a value missing or whitespace-only?
#' @noRd
is_blank <- function(x) {
  if (is.null(x)) {
    return(logical(0))
  }
  if (!is.character(x)) {
    x <- as.character(x)
  }
  is.na(x) | !nzchar(trimws(x))
}

#' Fill a template such as `"CVH{vv}_date_{c}_{k}"`
#'
#' A minimal, dependency-free substitution restricted to simple `{name}`
#' placeholders. Unknown placeholders are an error, which keeps dictionary typos
#' from silently producing columns full of `NA`.
#' @noRd
fill_template <- function(template, values) {
  assert_string(template)
  keys <- regmatches(
    template,
    gregexpr("\\{[A-Za-z_][A-Za-z0-9_]*\\}", template)
  )[[1]]
  if (!length(keys)) {
    return(template)
  }
  bare <- gsub("[{}]", "", keys)
  unknown <- setdiff(bare, names(values))
  if (length(unknown)) {
    vcs_abort(
      sprintf(
        "Template \"%s\" uses unknown placeholder%s %s. Available: %s.",
        template, if (length(unknown) > 1L) "s" else "",
        collapse_quote(unknown), collapse_quote(names(values))
      ),
      class = "vaxsurvR_template_error"
    )
  }
  out <- template
  for (i in seq_along(keys)) {
    out <- sub(keys[i], as.character(values[[bare[i]]]), out, fixed = TRUE)
  }
  out
}

#' Which placeholders does a template use?
#' @noRd
template_vars <- function(template) {
  keys <- regmatches(
    template,
    gregexpr("\\{[A-Za-z_][A-Za-z0-9_]*\\}", template)
  )[[1]]
  gsub("[{}]", "", keys)
}

#' Extract a column, returning `NA` of the right length when absent
#' @noRd
col_or_na <- function(data, name, n = nrow(data)) {
  if (!is.null(name) && length(name) == 1L && !is.na(name) && name %in% names(data)) {
    data[[name]]
  } else {
    rep(NA_character_, n)
  }
}

#' Timestamp used throughout audit logs
#' @noRd
vcs_timestamp <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
}

#' Package version as a string
#' @noRd
vcs_version <- function() {
  out <- tryCatch(
    as.character(utils::packageVersion("vaxsurvR")),
    error = function(e) NA_character_
  )
  out %||% NA_character_
}

#' Standardise a yes/no coded vector to logical
#'
#' Survey exports code yes/no in many ways ("1"/"0", "1"/"2", "Oui"/"Non").
#' Values outside `yes` and `no` -- including don't-know codes such as 98 --
#' become `NA` rather than being read as "no".
#' @noRd
recode_yesno <- function(x,
                         yes = c("1", "yes", "oui", "y", "o", "true"),
                         no = c("0", "2", "no", "non", "n", "false")) {
  chr <- tolower(trimws(as.character(x)))
  out <- rep(NA, length(chr))
  out[chr %in% tolower(yes)] <- TRUE
  out[chr %in% tolower(no)] <- FALSE
  out
}

#' Wilson score interval, used when a domain has no usable variance estimate
#' @noRd
wilson_ci <- function(x, n, level = 0.95) {
  if (is.na(x) || is.na(n) || n <= 0) {
    return(c(lower = NA_real_, upper = NA_real_))
  }
  z <- stats::qnorm(1 - (1 - level) / 2)
  p <- x / n
  denom <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / denom
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / denom
  c(lower = max(0, centre - half), upper = min(1, centre + half))
}
