test_that("vcs_rule() validates its arguments", {
  r <- vcs_rule("R1", "test", level = "vaccination", where = !is.na(card_date))
  expect_s3_class(r, "vcs_rule")
  expect_equal(r$action, "flag")
  expect_equal(r$flag_name, "flag_r1")
  expect_output(print(r), "vcs_rule R1")

  expect_error(vcs_rule("R", "d", where = TRUE, action = "set_na"),
               class = "vaxsurvR_rule_error")
  expect_error(vcs_rule("R", "d", where = TRUE, action = "replace",
                        variable = "v"),
               class = "vaxsurvR_rule_error")
  expect_error(vcs_rule("R", "d", where = TRUE, severity = "LOUD"),
               class = "vaxsurvR_value_error")
})

test_that("vcs_ruleset() rejects duplicates and non-rules", {
  a <- vcs_rule("A", "a", where = TRUE)
  expect_s3_class(vcs_ruleset(a), "vcs_ruleset")
  expect_error(vcs_ruleset(a, vcs_rule("A", "b", where = TRUE)),
               class = "vaxsurvR_rule_error")
  expect_error(vcs_ruleset(a, "not a rule"), class = "vaxsurvR_type_error")
  expect_output(print(vcs_default_rules()), "vcs_ruleset")
})

test_that("the default ruleset flags without changing any value", {
  cleaned <- clean_vcs(vcs_example)
  expect_s3_class(cleaned$audit, "vcs_audit")
  expect_true(is_vcs_audit(cleaned$audit))
  # Values are untouched.
  expect_identical(cleaned$vaccinations$card_date, vcs_example$vaccinations$card_date)
  expect_identical(cleaned$children$child_dob, vcs_example$children$child_dob)
  # Flags were added.
  expect_true("flag_date_prebirth" %in% names(cleaned$vaccinations))
  expect_true(any(cleaned$vaccinations$flag_date_prebirth))
  expect_equal(unique(audit_changes(cleaned)$action), "flag")
  # No record is dropped by cleaning.
  expect_equal(nrow(cleaned$children), nrow(vcs_example$children))
  expect_equal(nrow(cleaned$vaccinations), nrow(vcs_example$vaccinations))
})

test_that("the audit log carries every documented field", {
  cleaned <- clean_vcs(vcs_example)
  log <- audit_changes(cleaned)
  expect_true(all(c("record_id", "variable", "original_value", "new_value",
                    "rule_id", "rule_description", "severity", "action",
                    "timestamp", "package_version") %in% names(log)))
  expect_gt(nrow(log), 0L)
  expect_true(all(log$rule_id %in% vapply(vcs_default_rules(), `[[`,
                                          character(1), "id")))
  expect_true(all(!is.na(log$timestamp)))
  expect_equal(nrow(audit_changes(cleaned, rule_id = "DATE_PREBIRTH")),
               sum(log$rule_id == "DATE_PREBIRTH"))
  expect_equal(nrow(audit_changes(cleaned, action = "set_na")), 0L)
})

test_that("set_na and replace rules change values and record both sides", {
  r <- vcs_ruleset(
    vcs_rule("KILL_FUTURE", "blank card dates after the interview",
             level = "vaccination",
             where = !is.na(card_date) & !is.na(interview_date) &
               card_date > interview_date,
             variable = "card_date", action = "set_na", severity = "ERROR")
  )
  cleaned <- clean_vcs(vcs_example, rules = r)
  log <- audit_changes(cleaned)
  expect_gt(nrow(log), 0L)
  expect_equal(unique(log$action), "set_na")
  expect_true(all(!is.na(log$original_value)))
  expect_true(all(is.na(log$new_value)))
  # The targeted values really are gone, and nothing else changed.
  expect_lt(sum(!is.na(cleaned$vaccinations$card_date)),
            sum(!is.na(vcs_example$vaccinations$card_date)))
  expect_equal(nrow(cleaned$vaccinations), nrow(vcs_example$vaccinations))

  r2 <- vcs_ruleset(
    vcs_rule("FIX_SEX", "recode an out-of-range sex code", level = "child",
             where = !sex %in% c("1", "2"), variable = "sex",
             action = "replace", value = NA_character_)
  )
  expect_s3_class(clean_vcs(vcs_example, rules = r2)$audit, "vcs_audit")
})

test_that("dry_run reports what would change without changing it", {
  r <- vcs_ruleset(
    vcs_rule("KILL", "blank every card date", level = "vaccination",
             where = !is.na(card_date), variable = "card_date",
             action = "set_na")
  )
  dry <- clean_vcs(vcs_example, rules = r, dry_run = TRUE)
  expect_true(dry$audit$meta$dry_run)
  expect_gt(nrow(audit_changes(dry)), 0L)
  expect_identical(dry$vaccinations$card_date, vcs_example$vaccinations$card_date)

  wet <- clean_vcs(vcs_example, rules = r)
  expect_true(all(is.na(wet$vaccinations$card_date)))
})

test_that("clean_vcs() skips rules it cannot evaluate rather than failing", {
  r <- vcs_ruleset(
    vcs_rule("BAD", "refers to a column that is not there", level = "child",
             where = no_such_column > 1)
  )
  expect_warning(out <- clean_vcs(vcs_example, rules = r),
                 class = "vaxsurvR_rule_skipped")
  expect_equal(nrow(audit_changes(out)), 0L)

  r2 <- vcs_ruleset(
    vcs_rule("MISSINGVAR", "targets an absent variable", level = "child",
             where = TRUE, variable = "not_here", action = "set_na")
  )
  expect_warning(clean_vcs(vcs_example, rules = r2),
                 class = "vaxsurvR_rule_skipped")

  r3 <- vcs_ruleset(
    vcs_rule("NOTLOGICAL", "condition is not logical", level = "child",
             where = child_id)
  )
  expect_warning(clean_vcs(vcs_example, rules = r3),
                 class = "vaxsurvR_rule_skipped")
})

test_that("clean_vcs() validates its input", {
  expect_error(clean_vcs(data.frame()), class = "vaxsurvR_type_error")
  expect_error(clean_vcs(vcs_example, rules = "nope"), class = "vaxsurvR_type_error")
  # A single rule is accepted without wrapping.
  out <- clean_vcs(vcs_example,
                   rules = vcs_rule("A", "a", level = "child", where = TRUE))
  expect_s3_class(out$audit, "vcs_audit")
})

test_that("clean_vcs() copes with empty tables", {
  empty <- suppressWarnings(
    map_vcs_variables(fixture_wide()[0, ], fixture_dictionary(), fixture_schedule())
  )
  out <- clean_vcs(empty)
  expect_equal(nrow(audit_changes(out)), 0L)
})

test_that("vcs_log() and summary() describe the trail", {
  expect_s3_class(vcs_log(), "vcs_audit")
  expect_output(print(vcs_log()), "no changes recorded")
  expect_equal(nrow(summary(vcs_log())), 0L)

  cleaned <- clean_vcs(vcs_example)
  s <- summary(cleaned$audit)
  expect_s3_class(s, "tbl_df")
  expect_true(all(c("rule_id", "action", "severity", "n_records", "n_changes")
                  %in% names(s)))
  expect_equal(s$severity[1], "CRITICAL")   # ordered by severity
  expect_output(print(cleaned$audit), "vcs_audit")
})

test_that("export_cleaning_log() writes CSV and can redact values", {
  cleaned <- clean_vcs(vcs_example)
  f <- withr::local_tempfile(fileext = ".csv")
  export_cleaning_log(cleaned, f)
  back <- utils::read.csv(f, stringsAsFactors = FALSE)
  expect_equal(nrow(back), nrow(audit_changes(cleaned)))
  expect_true("rule_id" %in% names(back))

  f2 <- withr::local_tempfile(fileext = ".csv")
  export_cleaning_log(cleaned, f2, exclude_variables = "card_date")
  back2 <- utils::read.csv(f2, stringsAsFactors = FALSE)
  redacted <- back2$original_value[back2$variable == "card_date"]
  expect_true(all(redacted == "<redacted>" | is.na(redacted)))

  bad <- withr::local_tempfile(fileext = ".txt")
  expect_error(export_cleaning_log(cleaned, bad), class = "vaxsurvR_io_error")
  expect_error(audit_changes(1), class = "vaxsurvR_type_error")
})

test_that("audit_changes() works on data that was never cleaned", {
  expect_equal(nrow(audit_changes(vcs_example)), 0L)
})
