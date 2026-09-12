#' @keywords internal
#' @aliases vaxsurvR-package
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data abort warn inform %||%
#' @importFrom stats setNames median sd quantile weighted.mean
#' @importFrom utils packageVersion write.csv head
## usethis namespace: end
NULL

# `vcs_default_rules()` builds rules whose conditions are quoted expressions
# evaluated later inside a survey table, so these names are column names rather
# than objects in scope here.
utils::globalVariables(c(
  ".", "card_date", "card_date_precision", "card_seen", "child_dob",
  "interview_date"
))
