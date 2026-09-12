test_that("vcs_design() picks up mapped design variables", {
  d <- derive_vaccination_status(vcs_example)
  des <- vcs_design(d)
  expect_s3_class(des, "vcs_design")
  expect_true(is_vcs_design(des))
  expect_equal(des$spec$ids, c("psu", "segment"))
  expect_equal(des$spec$strata, "stratum")
  expect_equal(des$spec$weights, "weight")
  expect_true(des$spec$weighted)
  expect_s3_class(as_survey_design(des), "survey.design")
  expect_output(print(des), "vcs_design")
})

test_that("vcs_design() honours explicit specifications", {
  d <- derive_vaccination_status(vcs_example)
  des <- vcs_design(d, ids = ~psu, strata = ~stratum, weights = ~weight)
  expect_equal(des$spec$ids, "psu")
  des2 <- vcs_design(d, ids = c("psu"), strata = NULL)
  expect_length(des2$spec$strata, 1L)   # falls back to the mapped stratum
  expect_error(vcs_design(d, ids = ~nope), class = "vaxsurvR_design_error")
})

test_that("vcs_design() warns rather than pretending an unweighted design is weighted", {
  d <- derive_vaccination_status(vcs_example)
  d$children$weight <- NULL
  d$households$weight <- NULL
  expect_warning(des <- vcs_design(d), class = "vaxsurvR_no_weights")
  expect_false(des$spec$weighted)
  expect_output(print(des), "NOT population estimates")
})

test_that("vcs_design() handles missing, zero and negative weights", {
  d <- derive_vaccination_status(vcs_example)
  d$children$weight[1:3] <- NA
  d$children$weight[4] <- 0
  d$children$weight[5] <- -1
  expect_warning(des <- vcs_design(d), class = "vaxsurvR_dropped_weights")
  expect_equal(des$meta$n_dropped_weights, 5L)
  expect_equal(des$meta$n_records, nrow(d$children) - 5L)

  d2 <- derive_vaccination_status(vcs_example)
  d2$children$weight <- 0
  expect_error(suppressWarnings(vcs_design(d2)), class = "vaxsurvR_design_error")
})

test_that("vcs_design() refuses an empty dataset and bad input", {
  d <- fixture_vcs()
  d$children <- d$children[0, ]
  expect_error(vcs_design(d), class = "vaxsurvR_design_error")
  expect_error(vcs_design(1), class = "vaxsurvR_type_error")
})

test_that("vcs_design() copes with a single-PSU stratum", {
  d <- derive_vaccination_status(vcs_example)
  # Isolate one PSU into its own stratum (PSU and stratum are household-level).
  lone <- d$households$psu[1]
  d$households$stratum[d$households$psu == lone] <- "lonely"
  expect_silent(des <- vcs_design(d, lonely_psu = "adjust"))
  expect_equal(des$spec$lonely_psu, "adjust")
  est <- estimate_coverage(des, vaccines = "BCG")
  expect_true(is.finite(est$estimate[1]))
})

test_that("validate_weights() flags missing, zero and negative weights", {
  d <- vcs_example
  d$children$weight[1] <- NA
  d$children$weight[2] <- 0
  d$children$weight[3] <- -5
  v <- validate_weights(d)
  rules <- issues(v)$rule_id
  expect_true(all(c("WT_MISSING", "WT_ZERO", "WT_NEGATIVE") %in% rules))
  expect_equal(
    as.character(issues(v)$severity[issues(v)$rule_id == "WT_NEGATIVE"]),
    "CRITICAL"
  )
})

test_that("validate_weights() flags wide ranges and reports unmapped weights", {
  d <- vcs_example
  d$children$weight[1] <- 10000
  expect_true("WT_WIDE_RANGE" %in% issues(validate_weights(d))$rule_id)

  d2 <- vcs_example
  d2$children$weight <- NULL
  d2$households$weight <- NULL
  expect_true("WT_UNMAPPED" %in% issues(validate_weights(d2))$rule_id)
  expect_equal(nrow(issues(validate_weights(vcs_example), severity = "ERROR")), 0L)
})

test_that("check_weight_distribution() summarises overall and by domain", {
  s <- check_weight_distribution(vcs_example)
  expect_equal(nrow(s), 1L)
  expect_true(all(c("n", "median", "cv", "sum") %in% names(s)))

  by_stratum <- check_weight_distribution(vcs_example, by = ~stratum)
  expect_equal(nrow(by_stratum), 2L)
  expect_error(check_weight_distribution(vcs_example, weight = "nope"),
               class = "vaxsurvR_missing_column")
})

test_that("trim_weights() winsorises, rescales and keeps the original", {
  d <- trim_weights(vcs_example, lower = 0.05, upper = 0.95)
  w <- d$children$weight
  tw <- d$children$weight_trimmed
  expect_true("weight" %in% names(d$children))
  expect_equal(sum(w), sum(tw), tolerance = 1e-8)
  expect_lte(max(tw), max(w) + 1e-8)
  expect_match(derivation_rules(d)$rule[derivation_rules(d)$variable == "weight_trimmed"],
               "winsorised")

  raw <- trim_weights(vcs_example$children, lower = 0.1, upper = 0.9,
                      rescale = FALSE)
  expect_s3_class(raw, "tbl_df")
  expect_error(trim_weights(vcs_example, lower = 0.9, upper = 0.1),
               class = "vaxsurvR_value_error")
})

test_that("design_effect() returns a deff per variable", {
  des <- vcs_design(derive_vaccination_status(vcs_example))
  de <- design_effect(des, c("cov_BCG", "cov_PENTA3"))
  expect_equal(nrow(de), 2L)
  expect_true(all(c("estimate", "se", "deff", "n") %in% names(de)))
  expect_true(all(de$deff > 0, na.rm = TRUE))
  expect_error(design_effect(des, "nope"), class = "vaxsurvR_value_error")
  expect_error(design_effect(list(), "cov_BCG"), class = "vaxsurvR_type_error")
})

test_that("design effects are finite when weights sum to the sample size", {
  # Regression: survey's deff = TRUE takes the population size from the sum of
  # the weights, so an equal-weight (or sample-size-normalised) design gives a
  # zero SRS reference variance and every deff comes back Inf.
  d <- derive_vaccination_status(vcs_example)
  d$children$weight <- NULL
  d$households$weight <- NULL
  des <- suppressWarnings(vcs_design(d))

  de <- design_effect(des, "cov_BCG")
  expect_true(is.finite(de$deff))
  expect_gt(de$deff, 0)

  est <- estimate_coverage(des, vaccines = "BCG")
  expect_true(is.finite(est$deff[1]))

  # Normalising real weights to the sample size must not break it either.
  d2 <- derive_vaccination_status(vcs_example)
  d2$children$weight <- d2$children$weight / mean(d2$children$weight)
  des2 <- vcs_design(d2)
  expect_true(all(is.finite(estimate_coverage(des2, vaccines = "BCG")$deff)))

  # The finite-population variant is still reachable for those who want it.
  expect_true(is.na(design_effect(des, "cov_BCG", type = "fpc")$deff) ||
                !is.finite(design_effect(des, "cov_BCG", type = "fpc")$deff))
})
