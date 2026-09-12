design_fixture <- function() {
  vcs_design(derive_vaccination_status(vcs_example, evidence = "card_or_recall"))
}

test_that("estimate_coverage() returns the documented tidy columns", {
  des <- design_fixture()
  est <- estimate_coverage(des, vaccines = c("BCG", "PENTA1", "PENTA3", "MCV1"))
  expect_s3_class(est, "vcs_estimate")
  expect_true(is_vcs_estimate(est))
  expect_true(all(c("indicator", "domain", "numerator", "denominator",
                    "estimate", "se", "conf_low", "conf_high", "n_unweighted",
                    "deff") %in% names(est)))
  expect_equal(nrow(est), 4L)
  expect_setequal(est$indicator, c("BCG", "PENTA1", "PENTA3", "MCV1"))
  expect_true(all(est$estimate >= 0 & est$estimate <= 1))
  expect_true(all(est$conf_low <= est$estimate + 1e-9))
  expect_true(all(est$conf_high >= est$estimate - 1e-9))
  expect_true(all(est$conf_low >= 0 & est$conf_high <= 1))
  expect_true(all(est$numerator <= est$denominator))
  expect_output(print(est), "vcs_estimate")
})

test_that("estimate_coverage() estimates within domains", {
  des <- design_fixture()
  est <- estimate_coverage(des, vaccines = "PENTA3", by = ~stratum)
  expect_equal(nrow(est), 2L)
  expect_true("stratum" %in% names(est))
  expect_setequal(est$stratum, c("rural", "urban"))
  # Rural coverage is simulated lower than urban.
  expect_lt(est$estimate[est$stratum == "rural"],
            est$estimate[est$stratum == "urban"])

  multi <- estimate_coverage(des, vaccines = "BCG", by = c("stratum", "sex"))
  expect_true(all(c("stratum", "sex") %in% names(multi)))
  expect_gte(nrow(multi), 4L)
})

test_that("estimate_coverage() reflects the evidence definition", {
  card <- estimate_coverage(
    vcs_design(derive_vaccination_status(vcs_example, evidence = "card")),
    vaccines = "PENTA1"
  )
  both <- estimate_coverage(design_fixture(), vaccines = "PENTA1")
  expect_false(isTRUE(all.equal(card$estimate, both$estimate)))
  expect_equal(attr(both, "vcs_meta")$evidence, "card_or_recall")
})

test_that("unknown status is excluded, not counted as unvaccinated", {
  d <- derive_vaccination_status(vcs_example, evidence = "card")
  des <- vcs_design(d)
  keep <- estimate_coverage(des, vaccines = "PENTA1", na.rm = TRUE)
  expect_lt(keep$denominator, nrow(d$children))
  expect_equal(keep$denominator, sum(!is.na(d$children$cov_PENTA1)))
})

test_that("estimate_coverage() accepts a vcs_data directly", {
  est <- estimate_coverage(vcs_example, vaccines = "BCG")
  expect_s3_class(est, "vcs_estimate")
  expect_equal(nrow(est), 1L)
})

test_that("estimate_coverage() errors on missing coverage columns", {
  des <- vcs_design(vcs_example)
  expect_error(estimate_coverage(des, vaccines = "BCG"),
               class = "vaxsurvR_value_error")
  expect_error(estimate_coverage(1), class = "vaxsurvR_type_error")
  expect_error(estimate_coverage(design_fixture(), vaccines = "BCG",
                                 by = ~not_a_column),
               class = "vaxsurvR_missing_column")
})

test_that("estimate_antigen_coverage() is a single-vaccine wrapper", {
  est <- estimate_antigen_coverage(design_fixture(), "PENTA3", by = ~stratum)
  expect_equal(unique(est$indicator), "PENTA3")
  expect_equal(nrow(est), 2L)
})

test_that("estimate_full_coverage() and estimate_zero_dose() work", {
  d <- derive_zero_dose(derive_fully_vaccinated(vcs_example))
  des <- vcs_design(d)

  fc <- estimate_full_coverage(des)
  expect_equal(nrow(fc), 1L)
  expect_true(fc$estimate >= 0 && fc$estimate <= 1)

  zd <- estimate_zero_dose(des, by = ~stratum)
  expect_equal(nrow(zd), 2L)
  # Zero-dose is the complement of PENTA1 coverage.
  p1 <- estimate_coverage(vcs_design(derive_vaccination_status(vcs_example)),
                          vaccines = "PENTA1", by = ~stratum)
  expect_equal(zd$estimate[order(zd$stratum)],
               1 - p1$estimate[order(p1$stratum)], tolerance = 1e-8)

  expect_error(estimate_zero_dose(vcs_design(vcs_example)),
               class = "vaxsurvR_value_error")
})

test_that("estimate_card_availability() coerces the logical indicator", {
  est <- estimate_card_availability(vcs_design(vcs_example), by = ~stratum)
  expect_equal(nrow(est), 2L)
  expect_true(all(est$estimate >= 0 & est$estimate <= 1))
})

test_that("estimate_timely_coverage() needs derived timeliness", {
  d <- derive_timeliness(vcs_example)
  est <- estimate_timely_coverage(vcs_design(d), vaccines = c("BCG", "PENTA1"))
  expect_equal(nrow(est), 2L)
  expect_setequal(est$indicator, c("BCG", "PENTA1"))
  expect_error(estimate_timely_coverage(vcs_design(vcs_example)),
               class = "vaxsurvR_value_error")
})

test_that("estimate_dropout() supports both definitions", {
  d <- derive_vaccination_status(vcs_example)
  des <- vcs_design(d)

  ind <- estimate_dropout(des, "PENTA1", "PENTA3")
  expect_equal(unique(ind$method), "individual")
  expect_equal(unique(ind$indicator), "PENTA1-PENTA3 dropout")
  expect_true(ind$estimate >= 0 && ind$estimate <= 1)

  cov <- estimate_dropout(des, "PENTA1", "PENTA3", method = "coverage")
  expect_equal(unique(cov$method), "coverage")
  expect_true(is.finite(cov$estimate))
  expect_true(is.finite(cov$se))
  # The two definitions should agree closely when histories are consistent.
  expect_equal(ind$estimate, cov$estimate, tolerance = 0.15)
})

test_that("estimate_dropout() works by domain and derives what it needs", {
  ind <- estimate_dropout(vcs_example, "PENTA1", "PENTA3", by = ~stratum)
  expect_equal(nrow(ind), 2L)
  expect_true("stratum" %in% names(ind))

  cov <- estimate_dropout(vcs_example, "BCG", "MCV1", by = ~stratum,
                          method = "coverage")
  expect_equal(nrow(cov), 2L)
  expect_true(all(c("conf_low", "conf_high") %in% names(cov)))
})

test_that("estimate_dropout() validates arguments", {
  des <- vcs_design(derive_vaccination_status(vcs_example))
  expect_error(estimate_dropout(des, "PENTA1", "PENTA3", method = "nope"))
  expect_error(
    estimate_dropout(vcs_design(vcs_example), "PENTA1", "PENTA3",
                     method = "coverage"),
    class = "vaxsurvR_value_error"
  )
})

test_that("estimates survive a domain with a single observation", {
  d <- derive_vaccination_status(vcs_example)
  d$children$tiny <- "big"
  d$children$tiny[1] <- "alone"
  des <- vcs_design(d)
  est <- estimate_coverage(des, vaccines = "BCG", by = ~tiny)
  expect_equal(nrow(est), 2L)
  # The singleton domain still reports a numerator and a Wilson interval.
  solo <- est[est$tiny == "alone", ]
  expect_equal(solo$denominator, 1L)
  expect_false(is.na(solo$conf_low))
})

test_that("estimates survive an all-missing indicator", {
  d <- derive_vaccination_status(vcs_example)
  d$children$cov_BCG <- NA_integer_
  des <- vcs_design(d)
  est <- estimate_coverage(des, vaccines = "BCG")
  expect_equal(est$denominator, 0L)
  expect_true(is.na(est$estimate))
})
