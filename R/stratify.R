# ---------------------------------------------------------------------------
# Stratified tables. Coverage-survey reports present the same indicator over
# a fixed ladder of domains -- the whole study area, then each administrative
# level, then residence, sex, caregiver education -- as one table with group
# headers and indented rows. This module describes that ladder once and
# applies any estimator to it.
# ---------------------------------------------------------------------------

#' Describe the stratifiers a report tabulates by
#'
#' Each stratifier names a column of the child-level analysis table and,
#' optionally, a parent column for nested administrative levels (a province
#' with its health zones under it). The order given is the order rows appear
#' in every stratified table and figure.
#'
#' @param ... Named stratifier specifications. Each is either a column name
#'   (`residence = "residence"`) or a list with elements `var` (column name),
#'   `label` (group header shown in tables; defaults to the name), `parent`
#'   (column name of the enclosing level, optional), `levels` (display order,
#'   optional) and `parent_levels` (display order of the parent, optional).
#' @param overall_label Label of the whole-sample row.
#' @return An object of class `vcs_strata`.
#' @export
#' @examples
#' vcs_strata(
#'   zone = list(var = "health_zone", label = "Health zone", parent = "province"),
#'   residence = list(var = "residence", label = "Area"),
#'   sex = list(var = "sex", label = "Child's sex")
#' )
vcs_strata <- function(..., overall_label = "Entire study area") {
  specs <- rlang::list2(...)
  assert_string(overall_label)
  if (length(specs) && (is.null(names(specs)) || any(!nzchar(names(specs))))) {
    vcs_abort("Every stratifier must be named.", class = "vaxsurvR_value_error")
  }
  specs <- lapply(names(specs), function(nm) {
    s <- specs[[nm]]
    if (is.character(s)) s <- list(var = s)
    if (!is.list(s) || is.null(s$var)) {
      vcs_abort(sprintf("Stratifier \"%s\" must give a `var`.", nm),
                class = "vaxsurvR_value_error")
    }
    s$name <- nm
    s$label <- s$label %||% nm
    s$parent <- s$parent %||% NA_character_
    s$levels <- s$levels %||% NULL
    s$parent_levels <- s$parent_levels %||% NULL
    s
  })
  names(specs) <- vapply(specs, `[[`, character(1), "name")
  structure(list(specs = specs, overall_label = overall_label),
            class = "vcs_strata")
}

#' @export
print.vcs_strata <- function(x, ...) {
  cat("<vcs_strata>\n")
  cat(sprintf("  overall: %s\n", x$overall_label))
  for (s in x$specs) {
    cat(sprintf("  %-12s -> %s%s\n", s$label, s$var,
                if (!is.na(s$parent)) sprintf(" (nested in %s)", s$parent) else ""))
  }
  invisible(x)
}

#' @noRd
assert_strata <- function(x, arg = rlang::caller_arg(x)) {
  if (!inherits(x, "vcs_strata")) {
    vcs_abort(sprintf("`%s` must be a `vcs_strata`; see `vcs_strata()`.", arg),
              class = "vaxsurvR_type_error")
  }
  invisible(x)
}

#' Apply an estimator over a ladder of stratifiers
#'
#' Runs `fun(x, by = <stratifier>, ...)` for the whole sample and for every
#' stratifier in `strata`, and stacks the results into one table with the row
#' structure a stratified report table needs: an overall row, then for each
#' stratifier a header row followed by one row per level, with nested levels
#' indented under their parent.
#'
#' Any estimator works as long as it accepts `by` as a one-sided formula or
#' character vector and returns a data frame whose domain columns are named
#' after the `by` variables: [estimate_coverage()], [estimate_dropout()],
#' [estimate_zero_dose()], [table_dose_intervals()], [estimate_mosv_visits()]
#' and so on.
#'
#' @param x The object `fun` takes as its first argument -- usually a
#'   [vcs_design()].
#' @param fun The estimator.
#' @param strata A [vcs_strata()].
#' @param ... Passed to `fun`.
#' @return A tibble with the columns `fun` returns plus `stratifier` (the
#'   header label), `stratum` (the row label), `row_type` (`"overall"`,
#'   `"header"`, `"parent"` or `"level"`) and `indent` (0, 1 or 2). Header
#'   rows carry `NA` in every estimate column.
#' @export
#' @examples
#' d <- derive_vaccination_status(vcs_example, evidence = "card_or_recall")
#' des <- vcs_design(d)
#' st <- vcs_strata(stratum = list(var = "stratum", label = "Stratum"),
#'                  sex = list(var = "sex", label = "Child's sex"))
#' estimate_stratified(des, estimate_coverage, st, vaccines = "PENTA3")
estimate_stratified <- function(x, fun, strata, ...) {
  assert_strata(strata)
  if (!is.function(fun)) {
    vcs_abort("`fun` must be a function.", class = "vaxsurvR_type_error")
  }
  tab <- strata_table(x)

  overall <- tibble::as_tibble(fun(x, by = NULL, ...))
  overall$stratifier <- strata$overall_label
  overall$stratum <- strata$overall_label
  overall$row_type <- "overall"
  overall$indent <- 0L
  overall$stratum_var <- NA_character_
  overall$stratum_value <- NA_character_
  pieces <- list(overall)

  header_row <- function(label) {
    tibble::tibble(stratifier = label, stratum = label, row_type = "header",
                   indent = 0L, stratum_var = NA_character_,
                   stratum_value = NA_character_)
  }
  level_order <- function(values, wanted) {
    values <- unique(values[!is.na(values)])
    if (is.null(wanted)) {
      return(sort(values))
    }
    c(intersect(wanted, values), setdiff(values, wanted))
  }

  for (s in strata$specs) {
    if (is.null(tab)) {
      # No child table to read levels from: take them from the estimates.
      probe <- tibble::as_tibble(fun(x, by = c(if (!is.na(s$parent)) s$parent, s$var), ...))
      tab <- probe[, intersect(c(s$parent, s$var), names(probe)), drop = FALSE]
      tab_is_probe <- TRUE
    } else {
      tab_is_probe <- FALSE
    }
    if (is.data.frame(tab) && !s$var %in% names(tab)) {
      vcs_warn(sprintf("Stratifier \"%s\" (column \"%s\") is not in the data; skipped.",
                       s$label, s$var), class = "vaxsurvR_missing_column")
      next
    }
    pieces[[length(pieces) + 1L]] <- header_row(s$label)

    if (!is.na(s$parent) && s$parent %in% names(tab)) {
      par_est <- tibble::as_tibble(fun(x, by = s$parent, ...))
      chi_est <- tibble::as_tibble(fun(x, by = c(s$parent, s$var), ...))
      parents <- level_order(as.character(tab[[s$parent]]), s$parent_levels)
      for (p in parents) {
        pr <- par_est[as.character(par_est[[s$parent]]) == p, , drop = FALSE]
        pr$stratifier <- s$label
        pr$stratum <- p
        pr$row_type <- "parent"
        pr$indent <- 0L
        pr$stratum_var <- s$parent
        pr$stratum_value <- p
        pieces[[length(pieces) + 1L]] <- pr
        kids <- chi_est[as.character(chi_est[[s$parent]]) == p, , drop = FALSE]
        kid_levels <- level_order(as.character(tab[[s$var]][as.character(tab[[s$parent]]) == p]),
                                  s$levels)
        for (lv in kid_levels) {
          kr <- kids[as.character(kids[[s$var]]) == lv, , drop = FALSE]
          if (!nrow(kr)) next
          kr$stratifier <- s$label
          kr$stratum <- lv
          kr$row_type <- "level"
          kr$indent <- 1L
          kr$stratum_var <- s$var
          kr$stratum_value <- lv
          pieces[[length(pieces) + 1L]] <- kr
        }
      }
    } else {
      est <- tibble::as_tibble(fun(x, by = s$var, ...))
      levels <- level_order(as.character(tab[[s$var]]), s$levels)
      for (lv in levels) {
        r <- est[as.character(est[[s$var]]) == lv, , drop = FALSE]
        if (!nrow(r)) next
        r$stratifier <- s$label
        r$stratum <- lv
        r$row_type <- "level"
        r$indent <- 0L
        r$stratum_var <- s$var
        r$stratum_value <- lv
        pieces[[length(pieces) + 1L]] <- r
      }
    }
    if (tab_is_probe) tab <- NULL
  }
  out <- dplyr::bind_rows(pieces)
  # Domain helper columns from the individual calls are not meaningful once
  # stacked; keep the stratum columns instead.
  drop <- intersect(c("domain"), names(out))
  drop <- c(drop, unique(unlist(lapply(strata$specs, function(s) c(s$var, s$parent)))))
  drop <- setdiff(drop, c("stratifier", "stratum"))
  out <- out[, setdiff(names(out), stats::na.omit(drop)), drop = FALSE]
  front <- c("stratifier", "stratum", "row_type", "indent")
  out <- out[, c(front, setdiff(names(out), front)), drop = FALSE]
  out$row_id <- seq_len(nrow(out))
  out
}

#' The child-level table an object can be stratified over
#' @noRd
strata_table <- function(x) {
  if (is_vcs_design(x)) return(x$data)
  if (is_vcs_data(x)) return(vcs_children(x))
  if (inherits(x, "vcs_mosv")) return(if (nrow(x$child)) x$child else NULL)
  if (is.data.frame(x)) return(x)
  NULL
}

#' Recode coded values to display labels
#'
#' A small convenience for turning questionnaire codes (`"1"`, `"2"`) into the
#' labels a table shows (`"Urban"`, `"Rural"`), keeping unknown codes as `NA`.
#'
#' @param x A vector of codes.
#' @param labels A named character vector, names are codes.
#' @param ordered Return a factor with levels in the order of `labels`.
#' @return A character vector or factor.
#' @export
#' @examples
#' vcs_recode(c("1", "2", "98"), c("1" = "Urban", "2" = "Rural"))
vcs_recode <- function(x, labels, ordered = TRUE) {
  assert_character(labels, allow_null = FALSE)
  if (is.null(names(labels))) {
    vcs_abort("`labels` must be named by code.", class = "vaxsurvR_value_error")
  }
  out <- unname(labels[trimws(as.character(x))])
  if (ordered) factor(out, levels = unique(labels)) else out
}
