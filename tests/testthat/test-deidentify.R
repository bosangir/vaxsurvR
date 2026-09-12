SALT <- "unit-test-salt-not-for-production"

test_that("hash_identifier() is deterministic, salted and irreversible-looking", {
  a <- hash_identifier(c("CH1", "CH2"), salt = SALT)
  expect_type(a, "character")
  expect_equal(a, hash_identifier(c("CH1", "CH2"), salt = SALT))
  expect_false(any(a == c("CH1", "CH2")))
  # A different salt gives different codes.
  expect_false(identical(a, hash_identifier(c("CH1", "CH2"), salt = "other")))
  # A namespace separates uses of the same identifier.
  expect_false(identical(hash_identifier("CH1", salt = SALT),
                         hash_identifier("CH1", salt = SALT, namespace = "hh")))
  expect_equal(nchar(hash_identifier("CH1", salt = SALT, length = 8)), 8L)
  expect_true(startsWith(hash_identifier("CH1", salt = SALT, prefix = "C_"), "C_"))
})

test_that("hash_identifier() preserves missingness and refuses a missing salt", {
  out <- hash_identifier(c("CH1", NA, ""), salt = SALT)
  expect_true(is.na(out[2]))
  expect_true(is.na(out[3]))
  expect_false(is.na(out[1]))

  expect_error(hash_identifier("CH1"), class = "vaxsurvR_salt_error")
  expect_error(hash_identifier("CH1", salt = ""), class = "vaxsurvR_salt_error")
  expect_equal(length(hash_identifier(character(0), salt = SALT)), 0L)
})

test_that("identify_pii() finds names, phones, addresses and coordinates", {
  pii <- identify_pii(vcs_example_raw, vcs_example_dictionary)
  expect_s3_class(pii, "tbl_df")
  expect_true(all(c("variable", "category", "basis", "confidence") %in% names(pii)))
  expect_true(any(pii$category == "name"))
  expect_true(any(pii$category == "telephone"))
  expect_true(any(pii$category == "gps"))
  expect_true(any(pii$confidence == "certain"))
  expect_true("respondent_phone" %in% pii$variable)
  expect_true(all(pii$n_nonmissing >= 0))
})

test_that("identify_pii() works on a plain data frame and finds nothing in clean data", {
  clean <- data.frame(a = 1:3, b = letters[1:3], stringsAsFactors = FALSE)
  expect_equal(nrow(identify_pii(clean)), 0L)

  extra <- identify_pii(clean, extra_patterns = c(custom = "^b$"))
  expect_equal(extra$variable, "b")
})

test_that("remove_direct_identifiers() drops the mapped identifiers", {
  expect_true("child_name" %in% names(vcs_example$children))
  d <- remove_direct_identifiers(vcs_example)
  expect_false("child_name" %in% names(d$children))
  expect_false("caregiver_name" %in% names(d$children))
  expect_false("telephone" %in% names(d$households))
  expect_false("address" %in% names(d$households))
  # Analytical variables survive.
  expect_true("psu" %in% names(d$households))
  expect_equal(nrow(d$children), nrow(vcs_example$children))
})

test_that("generalize_age() bands ages and drops the exact values", {
  expect_equal(
    as.character(generalize_age(c(11, 13, 19, 30), breaks = c(0, 12, 18, 24, Inf))),
    c("[0,12)", "[12,18)", "[18,24)", "[24,Inf]")
  )
  d <- generalize_age(vcs_example)
  expect_true("age_band" %in% names(d$children))
  expect_false("child_dob" %in% names(d$children))

  keep <- generalize_age(vcs_example, drop_exact = FALSE)
  expect_true("child_dob" %in% names(keep$children))
  expect_error(generalize_age(vcs_example, breaks = 1), class = "vaxsurvR_value_error")
})

test_that("generalize_geography() drops, rounds and suppresses small cells", {
  d <- generalize_geography(vcs_example)
  expect_false("gps_lat" %in% names(d$households))

  rounded <- generalize_geography(vcs_example, round_gps = 2)
  expect_true("gps_lat" %in% names(rounded$households))
  expect_equal(rounded$households$gps_lat,
               round(vcs_example$households$gps_lat, 2))

  coarse <- generalize_geography(vcs_example, lowest_level = "district")
  expect_true("district" %in% names(coarse$households))
  expect_false("health_zone" %in% names(coarse$households))
  expect_false("health_area" %in% names(coarse$households))

  small <- generalize_geography(vcs_example, min_cell = 1000)
  expect_true(all(is.na(small$households$province)))
  expect_error(generalize_geography(vcs_example, lowest_level = "planet"),
               class = "vaxsurvR_value_error")
})

test_that("shift_dates() preserves intervals and needs a salt", {
  d <- shift_dates(vcs_example, salt = SALT)
  a <- vcs_example$vaccinations
  b <- d$vaccinations
  cid <- a$child_id[!is.na(a$card_date)][1]
  expect_equal(
    diff(a$card_date[a$child_id == cid]),
    diff(b$card_date[b$child_id == cid])
  )
  # Calendar dates really moved for at least some records.
  expect_false(identical(a$card_date, b$card_date))
  # Deterministic given the salt.
  expect_identical(b$card_date, shift_dates(vcs_example, salt = SALT)$vaccinations$card_date)
  expect_false(identical(b$card_date,
                         shift_dates(vcs_example, salt = "other")$vaccinations$card_date))
  expect_true(all(abs(as.numeric(b$card_date - a$card_date)) <= 30, na.rm = TRUE))

  expect_error(shift_dates(vcs_example), class = "vaxsurvR_salt_error")
  expect_error(shift_dates(vcs_example, salt = ""), class = "vaxsurvR_salt_error")
})

test_that("deidentify_vcs() runs the whole pipeline and never keeps the salt", {
  anon <- deidentify_vcs(
    vcs_example,
    hash = c("child_id", "household_id"),
    salt = SALT,
    age_bands = c(0, 12, 18, 24, Inf),
    shift_dates = TRUE
  )
  expect_s3_class(anon, "vcs_data")
  expect_false("child_name" %in% names(anon$children))
  expect_false(any(anon$children$child_id %in% vcs_example$children$child_id))
  expect_false("gps_lat" %in% names(anon$households))
  expect_true("age_band" %in% names(anon$children))
  expect_null(anon$raw)

  # The salt appears nowhere in the object.
  serialised <- paste(utils::capture.output(utils::str(anon, max.level = 4)),
                      collapse = " ")
  expect_false(grepl(SALT, serialised, fixed = TRUE))
  expect_false(any(vapply(anon$deidentification, function(v) {
    is.character(v) && any(grepl(SALT, v, fixed = TRUE))
  }, logical(1))))
  expect_null(anon$deidentification$salt)
  expect_true(anon$deidentification$salt_used)
  expect_true(any(grepl("hashed identifiers", anon$deidentification$actions)))
})

test_that("deidentify_vcs() refuses to hash without a salt", {
  expect_error(deidentify_vcs(vcs_example, hash = "child_id"),
               class = "vaxsurvR_salt_error")
  expect_error(deidentify_vcs(vcs_example, hash = "child_id", salt = ""),
               class = "vaxsurvR_salt_error")
  expect_error(deidentify_vcs(vcs_example, shift_dates = TRUE),
               class = "vaxsurvR_salt_error")
  # No salt is needed when nothing is hashed or shifted.
  expect_s3_class(deidentify_vcs(vcs_example), "vcs_data")
})

test_that("hashing keeps the child-vaccination link intact", {
  anon <- deidentify_vcs(vcs_example, hash = "child_id", salt = SALT)
  expect_true(all(anon$vaccinations$child_id %in% anon$children$child_id))
  expect_equal(nrow(anon$vaccinations), nrow(vcs_example$vaccinations))
  # Analysis still runs on the de-identified data.
  est <- estimate_coverage(derive_vaccination_status(anon), vaccines = "BCG")
  expect_true(is.finite(est$estimate[1]))
})

test_that("assess_reidentification_risk() computes k-anonymity", {
  r <- assess_reidentification_risk(vcs_example)
  expect_s3_class(r, "vcs_risk")
  expect_true(r$min_k >= 1L)
  expect_true(r$prop_below_k >= 0 && r$prop_below_k <= 1)
  expect_s3_class(r$k_distribution, "tbl_df")
  expect_output(print(r), "k-anonymity")

  # Coarsening geography cannot lower k.
  coarse <- generalize_geography(vcs_example, lowest_level = "province")
  r2 <- assess_reidentification_risk(coarse)
  expect_gte(r2$min_k, r$min_k)
  expect_lte(r2$prop_below_k, r$prop_below_k)

  expect_error(assess_reidentification_risk(vcs_example,
                                            quasi_identifiers = "nope"),
               class = "vaxsurvR_value_error")
  empty <- fixture_vcs()
  empty$children <- empty$children[0, ]
  expect_error(assess_reidentification_risk(empty), class = "vaxsurvR_value_error")
})

test_that("de-identification leaves no direct identifier in the cleaning log", {
  cleaned <- clean_vcs(vcs_example)
  log <- audit_changes(cleaned)
  identifiers <- c(vcs_example$children$child_name,
                   vcs_example$households$telephone)
  identifiers <- identifiers[!is_blank(identifiers)]
  expect_false(any(log$original_value %in% identifiers))
  expect_false(any(log$new_value %in% identifiers))
})
