test_that("derive_evidence() grades card and recall evidence", {
  d <- derive_evidence(vcs_example)
  ev <- d$vaccinations$evidence
  expect_s3_class(ev, "factor")
  expect_setequal(levels(ev), vcs_evidence_levels())
  expect_true(all(c("card_date", "card_mark", "recall", "none") %in%
                    as.character(unique(ev))))
  # A dose with a usable card date must be graded card_date.
  vx <- d$vaccinations
  dated <- !is.na(vx$card_date) & !is.na(vx$card_documented) & vx$card_documented
  expect_true(all(as.character(vx$evidence[dated]) == "card_date"))
})

test_that("derive_evidence() handles a table with no vaccination columns", {
  d <- fixture_vcs()
  d$vaccinations <- d$vaccinations[0, ]
  out <- derive_evidence(d)
  expect_equal(nrow(out$vaccinations), 0L)
  expect_true("evidence" %in% names(out$vaccinations))
})

test_that("derive_vaccination_status() never treats missing as unvaccinated by default", {
  d <- derive_vaccination_status(vcs_example, evidence = "card")
  cov <- d$children$cov_PENTA1
  expect_true(anyNA(cov))                       # children with no card stay NA
  expect_setequal(stats::na.omit(unique(cov)), c(0L, 1L))

  forced <- derive_vaccination_status(vcs_example, evidence = "card",
                                      missing_as_unvaccinated = TRUE)
  expect_false(anyNA(forced$children$cov_PENTA1))
  expect_lt(mean(forced$children$cov_PENTA1), mean(cov, na.rm = TRUE))
  expect_match(
    derivation_rules(forced)$rule[derivation_rules(forced)$variable == "cov_PENTA1"],
    "not vaccinated"
  )
})

test_that("card-only evidence is never equated with recall", {
  d_card <- derive_vaccination_status(vcs_example, evidence = "card")
  d_recall <- derive_vaccination_status(vcs_example, evidence = "recall")
  d_both <- derive_vaccination_status(vcs_example, evidence = "card_or_recall")
  # The combined definition is at least as inclusive as either alone.
  expect_gte(sum(d_both$children$cov_BCG, na.rm = TRUE),
             sum(d_card$children$cov_BCG, na.rm = TRUE))
  expect_gte(sum(d_both$children$cov_BCG, na.rm = TRUE),
             sum(d_recall$children$cov_BCG, na.rm = TRUE))
  # The three definitions genuinely differ.
  expect_false(identical(d_card$children$cov_BCG, d_recall$children$cov_BCG))
})

test_that("card_date is stricter than card", {
  a <- derive_vaccination_status(vcs_example, evidence = "card")
  b <- derive_vaccination_status(vcs_example, evidence = "card_date")
  expect_lte(sum(b$children$cov_BCG, na.rm = TRUE),
             sum(a$children$cov_BCG, na.rm = TRUE))
})

test_that("derive_vaccination_status() records its rule and validates input", {
  d <- derive_vaccination_status(vcs_example, evidence = "card_or_recall")
  r <- derivation_rules(d)
  expect_true("cov_BCG" %in% r$variable)
  expect_match(r$rule[r$variable == "cov_BCG"], "card_or_recall")
  expect_equal(d$meta$evidence_definition, "card_or_recall")

  expect_error(derive_vaccination_status(vcs_example, vaccines = "NOPE"),
               class = "vaxsurvR_unknown_vaccine")
  expect_error(derive_vaccination_status(vcs_example, evidence = "telepathy"))
  expect_error(derive_vaccination_status(data.frame()), class = "vaxsurvR_type_error")
})

test_that("the coverage wrappers use distinct prefixes", {
  expect_true("card_BCG" %in% names(derive_card_coverage(vcs_example)$children))
  expect_true("recall_BCG" %in% names(derive_recall_coverage(vcs_example)$children))
  expect_true("cov_BCG" %in% names(derive_combined_coverage(vcs_example)$children))
})

test_that("derive_fully_vaccinated() uses a configurable dose set", {
  d <- derive_fully_vaccinated(vcs_example)
  fv <- d$children$fully_vaccinated
  expect_setequal(stats::na.omit(unique(fv)), c(0L, 1L))

  # A narrower definition can only be easier to satisfy.
  d2 <- derive_fully_vaccinated(vcs_example, vaccines = c("BCG", "PENTA3"),
                                name = "fv2")
  expect_gte(mean(d2$children$fv2, na.rm = TRUE), mean(fv, na.rm = TRUE))
  expect_match(
    derivation_rules(d2)$rule[derivation_rules(d2)$variable == "fv2"],
    "BCG, PENTA3"
  )

  prop <- derive_fully_vaccinated(vcs_example, vaccines = c("BCG", "PENTA3"),
                                  name = "fv_prop", require_all = FALSE)
  expect_true(all(prop$children$fv_prop >= 0 & prop$children$fv_prop <= 1,
                  na.rm = TRUE))

  d3 <- fixture_vcs()
  d3$schedule <- NULL
  expect_error(derive_fully_vaccinated(d3), class = "vaxsurvR_value_error")
})

test_that("fully vaccinated is FALSE on a known missed dose, NA on unknown", {
  d <- fixture_vcs()
  d <- derive_fully_vaccinated(d, vaccines = c("BCG", "PENTA1"), name = "fv")
  ch <- d$children
  # Child 3 has no card and recall "0" for both doses -> definitely not full.
  expect_equal(ch$fv[ch$child_id == "h3_1_1"], 0L)
})

test_that("derive_zero_dose() defaults to the DTP-containing first dose", {
  d <- derive_zero_dose(vcs_example)
  expect_true("zero_dose" %in% names(d$children))
  expect_match(
    derivation_rules(d)$rule[derivation_rules(d)$variable == "zero_dose"],
    "PENTA1"
  )
  expect_equal(
    d$children$zero_dose,
    1L - d$children$cov_PENTA1,
    ignore_attr = TRUE
  )

  named <- derive_zero_dose(vcs_example, marker = "BCG", name = "no_bcg")
  expect_true("no_bcg" %in% names(named$children))

  # With no DTP-containing dose in the data, the marker cannot be guessed.
  d2 <- fixture_vcs()
  d2$vaccinations <- d2$vaccinations[d2$vaccinations$vaccine == "BCG", ]
  expect_error(derive_zero_dose(d2), class = "vaxsurvR_value_error")

  d3 <- fixture_vcs()
  d3$vaccinations <- d3$vaccinations[0, ]
  expect_error(derive_zero_dose(d3), class = "vaxsurvR_value_error")
})

test_that("derive_dropout() restricts the denominator to children at risk", {
  d <- derive_dropout(vcs_example, "PENTA1", "PENTA3")
  ch <- d$children
  v <- ch$dropout_PENTA1_PENTA3
  # Children who did not receive PENTA1 are not at risk of dropout.
  expect_true(all(is.na(v[!is.na(ch$cov_PENTA1) & ch$cov_PENTA1 == 0L])))
  # Children with PENTA1 but not PENTA3 have dropped out.
  hit <- !is.na(ch$cov_PENTA1) & ch$cov_PENTA1 == 1L &
    !is.na(ch$cov_PENTA3) & ch$cov_PENTA3 == 0L
  expect_true(all(v[hit] == 1L))
  expect_true(all(v[!is.na(ch$cov_PENTA3) & ch$cov_PENTA3 == 1L &
                      !is.na(ch$cov_PENTA1) & ch$cov_PENTA1 == 1L] == 0L))

  named <- derive_dropout(vcs_example, "BCG", "MCV1", name = "bcg_mcv_dropout")
  expect_true("bcg_mcv_dropout" %in% names(named$children))
})

test_that("derive_timeliness() classifies only dated card doses", {
  d <- derive_timeliness(derive_evidence(vcs_example))
  vx <- d$vaccinations
  expect_true("timeliness" %in% names(vx))
  expect_setequal(levels(vx$timeliness), c("early", "timely", "late"))
  # Recall-only doses carry no date and cannot be classified.
  recall_only <- as.character(d$vaccinations$evidence) == "recall"
  expect_true(all(is.na(vx$timeliness[recall_only])))
  # Pre-birth dates are errors, not early doses.
  expect_true(all(is.na(vx$timeliness[!is.na(vx$age_at_vaccination_days) &
                                        vx$age_at_vaccination_days < 0])))
  expect_true("timely_PENTA1" %in% names(d$children))
  expect_setequal(stats::na.omit(unique(d$children$timely_PENTA1)), c(0L, 1L))
})

test_that("derive_timeliness() errors when dates are unavailable", {
  raw <- data.frame(cid = "c1", hid = "h1", clust = "P1", stringsAsFactors = FALSE)
  dict <- vcs_dictionary(child_id = "cid", household_id = "hid",
                         interview_id = "hid", psu = "clust")
  d <- map_vcs_variables(raw, dict, fixture_schedule())
  expect_error(derive_timeliness(d), class = "vaxsurvR_value_error")

  d2 <- fixture_vcs()
  d2$schedule <- NULL
  expect_error(derive_timeliness(d2), class = "vaxsurvR_value_error")
})

test_that("evidence definitions are documented", {
  defs <- vcs_evidence_definitions()
  expect_s3_class(defs, "tbl_df")
  expect_setequal(
    defs$definition,
    c("card", "card_date", "recall", "card_or_recall", "card_then_recall")
  )
})

test_that("a card marked 'not given' is evidence of absence on its own", {
  # Regression: with no recall variable mapped at all, an explicit "not on the
  # card" code used to stay "unknown", so every determinable child looked
  # vaccinated and coverage came out at exactly 1.
  raw <- data.frame(
    KEY = c("h1", "h2", "h3"),
    psu = "P1",
    card_1_1 = c("1", "1", "1"),
    CVH01_1_1 = c("1", "3", "98"),      # yes / no / don't know
    CVH01_date_1_1 = c("2024-01-05", NA, NA),
    stringsAsFactors = FALSE
  )
  dict <- vcs_dictionary(
    interview_id = "KEY", household_id = "KEY", psu = "psu",
    card_seen = "card_{c}_{k}",
    card_status = "CVH{vv}_{c}_{k}", card_date = "CVH{vv}_date_{c}_{k}"
  )
  d <- map_vcs_variables(raw, dict, vcs_schedule("BCG"))

  # "98" is neither yes nor no.
  expect_equal(d$vaccinations$card_documented, c(TRUE, FALSE, NA),
               ignore_attr = TRUE)

  ev <- derive_evidence(d)$vaccinations$evidence
  expect_equal(as.character(ev), c("card_date", "none", "unknown"))

  cov <- derive_vaccination_status(d, evidence = "card")$children$cov_BCG
  expect_equal(cov, c(1L, 0L, NA), ignore_attr = TRUE)
})

test_that("evidence definitions resolve card/recall disagreement correctly", {
  raw <- data.frame(
    KEY = c("h1", "h2", "h3", "h4"),
    psu = "P1",
    CVH01_1_1 = c("3", "1", NA, "3"),      # card: no  / yes / silent / no
    CVH01_date_1_1 = c(NA, NA, NA, NA),    # no dates anywhere
    VR01_1_1 = c("1", "2", "1", NA),       # recall: yes / no / yes / silent
    stringsAsFactors = FALSE
  )
  dict <- vcs_dictionary(
    interview_id = "KEY", household_id = "KEY", psu = "psu",
    card_status = "CVH{vv}_{c}_{k}", card_date = "CVH{vv}_date_{c}_{k}",
    recall_status = "VR{vv}_{c}_{k}"
  )
  d <- map_vcs_variables(raw, dict, vcs_schedule("BCG"))

  # The card definition follows the card, even when recall disagrees.
  expect_equal(derive_vaccination_status(d, evidence = "card")$children$cov_BCG,
               c(0L, 1L, NA, 0L), ignore_attr = TRUE)
  # The recall definition follows recall, even when the card disagrees.
  expect_equal(derive_vaccination_status(d, evidence = "recall")$children$cov_BCG,
               c(1L, 0L, 1L, NA), ignore_attr = TRUE)
  # Either source saying yes is enough for the combined definition.
  expect_equal(derive_vaccination_status(d, evidence = "card_or_recall")$children$cov_BCG,
               c(1L, 1L, 1L, 0L), ignore_attr = TRUE)
  # A card tick with no date cannot support a dated definition.
  expect_equal(derive_vaccination_status(d, evidence = "card_date")$children$cov_BCG,
               c(0L, NA, NA, 0L), ignore_attr = TRUE)
})
