test_that("vcs_issue() builds well-formed issue rows", {
  i <- vcs_issue("check_unique_ids", "ID_DUP", "CRITICAL", "child",
                 record_id = c("c1", "c2"), variable = "child_id",
                 value = c("x", "x"), message = "dup")
  expect_equal(nrow(i), 2L)
  expect_s3_class(i$severity, "factor")
  expect_equal(levels(i$severity), vcs_severities())
  expect_equal(nrow(vcs_issue("a", "b", "INFO", "child", character(0))), 0L)
  expect_error(vcs_issue("a", "b", "NOPE", "child", "c1"),
               class = "vaxsurvR_value_error")
  expect_error(vcs_issue("a", "b", "INFO", "galaxy", "c1"),
               class = "vaxsurvR_value_error")
})

test_that("new_vcs_validation() validates its input", {
  v <- new_vcs_validation()
  expect_s3_class(v, "vcs_validation")
  expect_true(is_vcs_validation(v))
  expect_equal(nrow(issues(v)), 0L)
  expect_output(print(v), "no issues found")

  bad <- data.frame(check = "a", rule_id = "b", severity = "NOPE", level = "child",
                    record_id = "c", variable = NA, value = NA, message = "m",
                    stringsAsFactors = FALSE)
  expect_error(new_vcs_validation(bad), class = "vaxsurvR_value_error")
  expect_error(new_vcs_validation(data.frame(x = 1)), class = "vaxsurvR_missing_column")
})

test_that("issues() filters by severity and check", {
  v <- new_vcs_validation(dplyr::bind_rows(
    vcs_issue("c1", "R1", "INFO", "child", "a"),
    vcs_issue("c1", "R2", "ERROR", "child", "b"),
    vcs_issue("c2", "R3", "CRITICAL", "child", "c")
  ), checks = c("c1", "c2"))

  expect_equal(nrow(issues(v)), 3L)
  expect_equal(nrow(issues(v, severity = "ERROR")), 2L)
  expect_equal(nrow(issues(v, severity = "ERROR", min_severity = FALSE)), 1L)
  expect_equal(nrow(issues(v, check = "c1")), 2L)
  expect_true(has_issues(v, "CRITICAL"))
  expect_false(has_issues(new_vcs_validation(), "INFO"))
  expect_error(has_issues(list()), class = "vaxsurvR_type_error")
})

test_that("summary() groups issues and orders by severity", {
  v <- new_vcs_validation(dplyr::bind_rows(
    vcs_issue("c1", "R1", "INFO", "child", c("a", "b")),
    vcs_issue("c1", "R2", "CRITICAL", "child", "b")
  ), checks = "c1")
  s <- summary(v)
  expect_s3_class(s, "tbl_df")
  expect_equal(s$group[1], "R2")           # CRITICAL sorts first
  expect_equal(s$n_issues[s$group == "R1"], 2L)
  expect_equal(s$n_records[s$group == "R1"], 2L)
  expect_equal(nrow(summary(v, by = "severity")), 2L)
  expect_equal(nrow(summary(new_vcs_validation())), 0L)
})

test_that("validation objects combine and print", {
  a <- check_unique_ids(vcs_example)
  b <- check_duplicates(vcs_example)
  ab <- c(a, b)
  expect_s3_class(ab, "vcs_validation")
  expect_equal(nrow(issues(ab)), nrow(issues(a)) + nrow(issues(b)))
  expect_setequal(ab$checks, c("check_unique_ids", "check_duplicates"))
  expect_error(c(a, 1), class = "vaxsurvR_type_error")
  expect_output(print(a), "vcs_validation")
  expect_s3_class(as.data.frame(a), "data.frame")
})

test_that("plot() works with and without issues", {
  skip_if_not_installed("grDevices")
  f <- withr::local_tempfile(fileext = ".png")
  grDevices::png(f)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_invisible(plot(check_unique_ids(vcs_example)))
  expect_invisible(plot(new_vcs_validation()))
})

# ---- structural checks ----------------------------------------------------

test_that("check_unique_ids() finds the planted duplicate child id", {
  v <- check_unique_ids(vcs_example)
  iss <- issues(v)
  expect_true("ID_DUPLICATE" %in% iss$rule_id)
  expect_true(all(iss$severity[iss$rule_id == "ID_DUPLICATE"] == "CRITICAL"))
})

test_that("check_unique_ids() reports blank identifiers", {
  d <- fixture_vcs()
  d$children$child_id[1] <- NA
  v <- check_unique_ids(d)
  expect_true("ID_MISSING" %in% issues(v)$rule_id)
})

test_that("check_required_variables() reports unmapped and incomplete concepts", {
  d <- fixture_vcs()
  d$households$psu <- NULL
  v <- check_required_variables(d)
  expect_true("REQ_MISSING" %in% issues(v)$rule_id)

  d2 <- fixture_vcs()
  d2$households$psu[1] <- NA
  expect_true("REQ_INCOMPLETE" %in% issues(check_required_variables(d2))$rule_id)

  expect_equal(nrow(issues(check_required_variables(fixture_vcs()))), 0L)
})

test_that("check_duplicates() finds records repeated on the key", {
  d <- fixture_vcs()
  d$children$child_dob[2] <- d$children$child_dob[1]
  d$children$household_id[2] <- d$children$household_id[1]
  d$children$sex[2] <- d$children$sex[1]
  v <- check_duplicates(d)
  expect_true("REC_DUPLICATE" %in% issues(v)$rule_id)
  expect_equal(unique(as.character(issues(v)$severity)), "WARNING")

  # No usable key columns: nothing to check, no error.
  d2 <- fixture_vcs()
  expect_equal(nrow(issues(check_duplicates(d2, by = "not_a_column"))), 0L)
})

test_that("check_household_structure() finds orphans and count mismatches", {
  d <- fixture_vcs()
  d$children$interview_id[1] <- "ghost"
  expect_true("HH_ORPHAN_CHILD" %in% issues(check_household_structure(d))$rule_id)

  d2 <- fixture_vcs()
  d2$households$n_eligible <- c(5, 1, 1)
  expect_true("HH_COUNT_MISMATCH" %in% issues(check_household_structure(d2))$rule_id)

  d3 <- fixture_vcs()
  d3$households$eligible <- c(FALSE, TRUE, TRUE)
  expect_true("HH_INELIGIBLE_WITH_CHILD" %in%
                issues(check_household_structure(d3))$rule_id)

  d4 <- fixture_vcs()
  expect_true("HH_MANY_CHILDREN" %in%
                issues(check_household_structure(d4, max_children = 1))$rule_id)
})

test_that("check_child_eligibility() applies a configurable age window", {
  v <- check_child_eligibility(vcs_example)
  expect_true("AGE_OUT_OF_WINDOW" %in% issues(v)$rule_id)

  # Widening the window removes the flags for the planted out-of-window children.
  wide <- check_child_eligibility(vcs_example, min_age_months = 0,
                                  max_age_months = 60)
  expect_lt(
    sum(issues(wide)$rule_id == "AGE_OUT_OF_WINDOW"),
    sum(issues(v)$rule_id == "AGE_OUT_OF_WINDOW")
  )
  expect_error(
    check_child_eligibility(vcs_example, min_age_months = 24, max_age_months = 12),
    class = "vaxsurvR_value_error"
  )
})

test_that("check_child_eligibility() reports children of unknown age", {
  d <- fixture_vcs()
  d$children$child_dob <- as.Date(NA)
  v <- check_child_eligibility(d)
  expect_true("AGE_UNKNOWN" %in% issues(v)$rule_id)
})

test_that("check_psu_structure() detects lonely strata and crossing PSUs", {
  d <- fixture_vcs()
  # h1/h2 are P1/urban, h3 is P2/rural -> rural has a single PSU.
  v <- check_psu_structure(d, min_interviews = 1)
  expect_true("PSU_LONELY_STRATUM" %in% issues(v)$rule_id)

  d2 <- fixture_vcs()
  d2$households$stratum <- c("urban", "rural", "urban")
  expect_true("PSU_CROSSES_STRATA" %in% issues(check_psu_structure(d2))$rule_id)

  d3 <- fixture_vcs()
  d3$households$psu[1] <- NA
  expect_true("PSU_MISSING" %in% issues(check_psu_structure(d3))$rule_id)

  d4 <- fixture_vcs()
  d4$households$psu <- NULL
  expect_true("PSU_UNMAPPED" %in% issues(check_psu_structure(d4))$rule_id)

  expect_true("PSU_FEW_INTERVIEWS" %in%
                issues(check_psu_structure(fixture_vcs()))$rule_id)
  expect_true("PSU_MANY_INTERVIEWS" %in%
                issues(check_psu_structure(vcs_example, max_interviews = 5))$rule_id)
})

test_that("check_segment_structure() handles mapped and unmapped segments", {
  v <- check_segment_structure(vcs_example)
  expect_s3_class(v, "vcs_validation")
  expect_true("SEG_LABEL_REUSED" %in% issues(v)$rule_id)

  d <- fixture_vcs()
  d$households$segment <- NULL
  expect_true("SEG_UNMAPPED" %in% issues(check_segment_structure(d))$rule_id)

  d2 <- fixture_vcs()
  d2$households$segment[1] <- NA
  expect_true("SEG_MISSING" %in% issues(check_segment_structure(d2))$rule_id)

  expect_true("SEG_MANY_PER_PSU" %in%
                issues(check_segment_structure(vcs_example, max_segments = 1))$rule_id)
})

test_that("validate_vcs() runs the suite and passes arguments through", {
  v <- validate_vcs(vcs_example)
  expect_s3_class(v, "vcs_validation")
  expect_gt(nrow(issues(v)), 0L)
  expect_true(all(c("check_unique_ids", "check_vaccine_dates") %in% v$checks))
  expect_equal(v$meta$n_children, nrow(vcs_example$children))

  narrow <- validate_vcs(vcs_example, checks = "check_child_eligibility",
                         min_age_months = 0, max_age_months = 60)
  expect_equal(narrow$checks, "check_child_eligibility")
  expect_error(validate_vcs(vcs_example, checks = "no_such_check"),
               class = "vaxsurvR_value_error")
  expect_error(validate_vcs(data.frame()), class = "vaxsurvR_type_error")
})

test_that("validate_vcs() copes with an empty dataset", {
  empty <- suppressWarnings(
    map_vcs_variables(fixture_wide()[0, ], fixture_dictionary(), fixture_schedule())
  )
  v <- validate_vcs(empty)
  expect_s3_class(v, "vcs_validation")
  expect_equal(v$meta$n_children, 0L)
})
