test_that("vcs_missingness() summarises overall and by group", {
  m <- vcs_missingness(vcs_example, variables = c("sex", "child_dob", "card_seen"))
  expect_s3_class(m, "tbl_df")
  expect_setequal(m$variable, c("sex", "child_dob", "card_seen"))
  expect_true(all(m$prop_missing >= 0 & m$prop_missing <= 1))
  expect_true(all(m$n == nrow(vcs_example$children)))

  by_stratum <- vcs_missingness(vcs_example, variables = "card_seen", by = ~stratum)
  expect_equal(nrow(by_stratum), 2L)
  expect_true("stratum" %in% names(by_stratum))

  expect_equal(nrow(vcs_missingness(vcs_example, variables = "not_a_column")), 0L)
  expect_s3_class(vcs_missingness(vcs_example, level = "household"), "tbl_df")
  expect_s3_class(vcs_missingness(vcs_example, level = "vaccination"), "tbl_df")
  expect_s3_class(vcs_missingness(data.frame(a = c(1, NA))), "tbl_df")
})

test_that("coverage_completeness() reports determinable status per vaccine", {
  d <- derive_vaccination_status(vcs_example, evidence = "card")
  cc <- coverage_completeness(d)
  expect_true(all(c("vaccine", "n", "n_determined", "prop_determined") %in% names(cc)))
  expect_true(all(cc$prop_determined >= 0 & cc$prop_determined <= 1))
  expect_true(all(cc$n_determined <= cc$n))
  # Card-only evidence leaves the children with no card undetermined.
  expect_lt(min(cc$prop_determined), 1)

  expect_error(coverage_completeness(vcs_example), class = "vaxsurvR_value_error")
})

test_that("card_completeness() adds up", {
  cc <- card_completeness(vcs_example)
  expect_equal(nrow(cc), 1L)
  expect_equal(cc$n_children, nrow(vcs_example$children))
  expect_true(cc$prop_card_seen >= 0 && cc$prop_card_seen <= 1)

  by_stratum <- card_completeness(vcs_example, by = ~stratum)
  expect_equal(nrow(by_stratum), 2L)
  expect_equal(sum(by_stratum$n_children), nrow(vcs_example$children))

  d <- fixture_vcs()
  d$children$card_seen <- NULL
  expect_error(card_completeness(d), class = "vaxsurvR_value_error")
})

test_that("date_completeness() counts dated and partial card doses", {
  dc <- date_completeness(vcs_example)
  expect_equal(nrow(dc), 1L)
  expect_true(dc$n_dated <= dc$n_documented)
  expect_true(dc$prop_partial >= 0 && dc$prop_partial <= 1)

  by_vaccine <- date_completeness(vcs_example, by = ~vaccine)
  expect_equal(nrow(by_vaccine), length(unique(vcs_example$vaccinations$vaccine)))

  d <- fixture_vcs()
  d$vaccinations$card_date <- NULL
  expect_error(date_completeness(d), class = "vaxsurvR_value_error")
})

test_that("vcs_summary() assembles the report tables", {
  d <- derive_vaccination_status(vcs_example)
  r <- vcs_summary(d, by = ~stratum)
  expect_s3_class(r, "vcs_report")
  expect_true(all(c("structure", "card_availability", "date_completeness",
                    "quality", "coverage", "completeness") %in% names(r)))
  expect_s3_class(r$coverage, "vcs_estimate")
  expect_output(print(r), "vcs_report")

  # Without derived coverage the coverage tables are simply absent.
  bare <- vcs_summary(vcs_example)
  expect_false("coverage" %in% names(bare))
})

test_that("vcs_quality_report() bundles issues and missingness", {
  q <- vcs_quality_report(vcs_example, by = ~enumerator)
  expect_s3_class(q, "vcs_report")
  expect_true(all(c("issues", "by_rule", "by_severity", "by_check",
                    "missingness") %in% names(q)))
  expect_gt(nrow(q$issues), 0L)
  expect_true("enumerator" %in% names(q$missingness))

  v <- validate_vcs(vcs_example, checks = "check_unique_ids")
  q2 <- vcs_quality_report(vcs_example, validation = v)
  expect_equal(unique(q2$issues$check), "check_unique_ids")
  expect_error(vcs_quality_report(vcs_example, validation = "nope"),
               class = "vaxsurvR_type_error")
})

test_that("vcs_coverage_report() includes coverage, domains and dropout", {
  d <- derive_vaccination_status(vcs_example)
  r <- vcs_coverage_report(vcs_design(d),
                           vaccines = c("BCG", "PENTA1", "PENTA3", "MCV1"),
                           by = ~stratum)
  expect_true(all(c("coverage", "coverage_by_domain", "dropout") %in% names(r)))
  expect_equal(nrow(r$coverage), 4L)
  expect_equal(nrow(r$coverage_by_domain), 8L)
  expect_gt(nrow(r$dropout), 0L)

  # Pairs whose vaccines are not derived are skipped rather than erroring.
  narrow <- derive_vaccination_status(vcs_example, vaccines = "BCG")
  r2 <- vcs_coverage_report(vcs_design(narrow), vaccines = "BCG")
  expect_null(r2$dropout)
})

test_that("write_vcs_report() writes one CSV per table", {
  r <- vcs_quality_report(vcs_example)
  dir <- withr::local_tempdir()
  write_vcs_report(r, dir)
  files <- list.files(dir)
  expect_true(all(paste0(names(r), ".csv") %in% files))
  back <- utils::read.csv(file.path(dir, "by_severity.csv"), stringsAsFactors = FALSE)
  expect_equal(nrow(back), nrow(r$by_severity))
  expect_error(write_vcs_report(list(), dir), class = "vaxsurvR_type_error")
})

test_that("write_vcs_report() writes an Excel workbook when openxlsx is available", {
  skip_if_not_installed("openxlsx")
  r <- vcs_quality_report(vcs_example)
  f <- withr::local_tempfile(fileext = ".xlsx")
  write_vcs_report(r, f)
  expect_true(file.exists(f))
  expect_gt(file.size(f), 0)
})
