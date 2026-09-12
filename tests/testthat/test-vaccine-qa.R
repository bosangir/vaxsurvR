test_that("check_vaccine_dates() finds partial, unparseable and out-of-range dates", {
  v <- check_vaccine_dates(vcs_example)
  rules <- issues(v)$rule_id
  expect_true("DATE_PARTIAL" %in% rules)
  expect_true("DATE_UNPARSEABLE" %in% rules)
  expect_true("DATE_MISSING_ON_CARD" %in% rules)

  d <- fixture_vcs()
  d$vaccinations$card_date[1] <- as.Date("1900-01-01")
  expect_true("DATE_IMPLAUSIBLE" %in% issues(check_vaccine_dates(d))$rule_id)
  expect_true("DATE_AFTER_LIMIT" %in%
                issues(check_vaccine_dates(d, latest = as.Date("1800-01-01")))$rule_id)
})

test_that("checks degrade gracefully when dates are unmapped", {
  raw <- data.frame(cid = "c1", hid = "h1", clust = "P1", stringsAsFactors = FALSE)
  dict <- vcs_dictionary(child_id = "cid", household_id = "hid",
                         interview_id = "hid", psu = "clust")
  d <- map_vcs_variables(raw, dict)
  for (f in list(check_vaccine_dates, check_future_dates, check_duplicate_doses)) {
    v <- f(d)
    expect_s3_class(v, "vcs_validation")
    expect_true(all(as.character(issues(v)$severity) %in% c("INFO")))
  }
  expect_true("VX_NO_DOB" %in% issues(check_prebirth_vaccination(d))$rule_id)
  expect_true("VX_NO_RECALL" %in% issues(check_card_transcription(d))$rule_id)
})

test_that("check_future_dates() flags doses dated after the interview", {
  v <- check_future_dates(vcs_example)
  expect_true("DATE_FUTURE" %in% issues(v)$rule_id)
  expect_equal(unique(as.character(issues(v)$severity)), "ERROR")

  # A reference date far in the future makes the flags disappear.
  clean <- check_future_dates(vcs_example, reference = as.Date("2099-01-01"))
  expect_equal(nrow(issues(clean)), 0L)
})

test_that("check_prebirth_vaccination() flags doses before birth", {
  v <- check_prebirth_vaccination(vcs_example)
  iss <- issues(v)
  expect_true("DATE_PREBIRTH" %in% iss$rule_id)
  expect_equal(unique(as.character(iss$severity)), "CRITICAL")
  # Tolerance suppresses small negatives but not large ones.
  expect_lte(
    nrow(issues(check_prebirth_vaccination(vcs_example, tolerance_days = 10))),
    nrow(iss)
  )
})

test_that("check_age_at_vaccination() uses the schedule minimum", {
  v <- check_age_at_vaccination(vcs_example)
  expect_true("AGE_BELOW_MINIMUM" %in% issues(v)$rule_id)
  loose <- check_age_at_vaccination(vcs_example, tolerance_days = 400)
  expect_equal(nrow(issues(loose)), 0L)

  d <- fixture_vcs()
  d$schedule <- NULL
  expect_true("VX_NO_SCHEDULE" %in% issues(check_age_at_vaccination(d))$rule_id)
})

test_that("check_vaccine_sequence() finds out-of-order doses", {
  v <- check_vaccine_sequence(vcs_example)
  rules <- issues(v)$rule_id
  expect_true("SEQ_OUT_OF_ORDER" %in% rules)
  expect_true("SEQ_MISSING_PREVIOUS" %in% rules)

  strict <- check_vaccine_sequence(vcs_example,
                                   missing_previous_severity = "ERROR")
  sev <- issues(strict)$severity[issues(strict)$rule_id == "SEQ_MISSING_PREVIOUS"]
  expect_true(all(as.character(sev) == "ERROR"))
})

test_that("check_dose_intervals() finds intervals below the schedule minimum", {
  v <- check_dose_intervals(vcs_example)
  expect_true("INTERVAL_TOO_SHORT" %in% issues(v)$rule_id)
  expect_equal(nrow(issues(check_dose_intervals(vcs_example, tolerance_days = 60))), 0L)

  # A schedule without intervals has nothing to check.
  d <- fixture_vcs()
  expect_equal(nrow(issues(check_dose_intervals(d))), 0L)
})

test_that("check_duplicate_doses() finds repeated antigen dates", {
  v <- check_duplicate_doses(vcs_example)
  expect_true("DOSE_DUPLICATE_DATE" %in% issues(v)$rule_id)
})

test_that("check_card_transcription() compares card and recall evidence", {
  v <- check_card_transcription(vcs_example)
  rules <- issues(v)$rule_id
  expect_true("EVID_RECALL_NOT_CARD" %in% rules)
  expect_true("EVID_DATE_WITHOUT_CARD" %in% rules)
  # Disagreement is informational by default, not an error.
  disagree <- issues(v)[issues(v)$rule_id == "EVID_RECALL_NOT_CARD", ]
  expect_equal(unique(as.character(disagree$severity)), "INFO")

  loud <- check_card_transcription(vcs_example, severity = "WARNING")
  disagree2 <- issues(loud)[issues(loud)$rule_id == "EVID_RECALL_NOT_CARD", ]
  expect_equal(unique(as.character(disagree2$severity)), "WARNING")
})

test_that("flag_vaccine_inconsistencies() collapses to one row per record", {
  f <- flag_vaccine_inconsistencies(vcs_example)
  expect_s3_class(f, "tbl_df")
  expect_named(f, c("child_id", "vaccine", "n_flags", "max_severity", "rules"))
  expect_false(anyDuplicated(paste(f$child_id, f$vaccine)) > 0)
  expect_true(all(f$n_flags >= 1))
  expect_true(all(f$max_severity %in% vcs_severities()))
  expect_true(all(f$child_id %in% vcs_example$children$child_id))
  # Nothing is dropped from the data by flagging.
  expect_equal(nrow(vcs_example$children), 256L)
})

test_that("flag_vaccine_inconsistencies() returns an empty table when clean", {
  d <- fixture_vcs()
  d$vaccinations$card_date <- as.Date(NA)
  d$vaccinations$card_date_precision <- "missing"
  d$vaccinations$card_documented <- NA
  d$vaccinations$recall_reported <- NA
  f <- flag_vaccine_inconsistencies(d)
  expect_equal(nrow(f), 0L)
  expect_named(f, c("child_id", "vaccine", "n_flags", "max_severity", "rules"))
})

test_that("all-missing vaccination variables do not break the checks", {
  d <- fixture_vcs()
  d$vaccinations$card_status <- NA_character_
  d$vaccinations$card_date <- as.Date(NA)
  d$vaccinations$recall_status <- NA_character_
  d$vaccinations$card_documented <- NA
  d$vaccinations$recall_reported <- NA
  expect_s3_class(validate_vcs(d), "vcs_validation")
})
