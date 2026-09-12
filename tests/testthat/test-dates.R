test_that("parse_vaccine_date() recognises complete dates", {
  p <- parse_vaccine_date(c("2025-03-14", "14/03/2025", "2025/03/14"))
  expect_equal(p$precision, rep("day", 3))
  expect_equal(p$date, rep(as.Date("2025-03-14"), 3))
  expect_equal(p$year, rep(2025L, 3))
  expect_equal(p$day, rep(14L, 3))
})

test_that("parse_vaccine_date() classifies partial, missing and malformed values", {
  p <- parse_vaccine_date(c("2025-03", "2025", "", NA, "not a date", "03/2025"))
  expect_equal(
    p$precision,
    c("month", "year", "missing", "missing", "unparseable", "month")
  )
  # A partial date must not be silently completed.
  expect_true(all(is.na(p$date[p$precision != "day"])))
  expect_equal(p$year[1], 2025L)
  expect_equal(p$month[1], 3L)
  expect_true(is.na(p$month[2]))
})

test_that("parse_vaccine_date() handles empty input and Date input", {
  p <- parse_vaccine_date(character(0))
  expect_equal(nrow(p), 0L)
  expect_s3_class(p$date, "Date")

  d <- parse_vaccine_date(as.Date(c("2025-01-02", NA)))
  expect_equal(d$precision, c("day", "missing"))
  expect_equal(d$day[1], 2L)
})

test_that("parse_vaccine_date() treats configured sentinels as missing", {
  p <- parse_vaccine_date(c("98", "99", "9999"))
  expect_equal(p$precision, rep("missing", 3))
})

test_that("validate_partial_date() never imputes by default", {
  p <- parse_vaccine_date(c("2025-03-14", "2025-03", "2025"))
  d <- validate_partial_date(p)
  expect_equal(attr(d, "rule"), "none")
  expect_equal(d, as.Date(c("2025-03-14", NA, NA)), ignore_attr = TRUE)
  expect_false(any(attr(d, "resolved")))
})

test_that("validate_partial_date() applies explicit rules", {
  p <- parse_vaccine_date(c("2025-03-14", "2025-02", "2025"))

  mid <- validate_partial_date(p, rule = "midpoint")
  expect_equal(as.character(mid[2]), "2025-02-15")
  expect_true(is.na(mid[3]))          # year-only needs min_precision = "year"
  expect_equal(attr(mid, "resolved"), c(FALSE, TRUE, FALSE))

  first <- validate_partial_date(p, rule = "first")
  expect_equal(as.character(first[2]), "2025-02-01")

  last <- validate_partial_date(p, rule = "last")
  expect_equal(as.character(last[2]), "2025-02-28")   # non-leap February

  yr <- validate_partial_date(p, rule = "midpoint", min_precision = "year")
  expect_equal(as.character(yr[3]), "2025-07-01")
  expect_equal(attr(yr, "resolved"), c(FALSE, TRUE, TRUE))
})

test_that("validate_partial_date() accepts raw input and rejects bad rules", {
  d <- validate_partial_date(c("2025-03-14"), rule = "first")
  expect_equal(as.character(d), "2025-03-14")
  expect_error(validate_partial_date(c("2025"), rule = "invent"))
})

test_that("derive_age_at_vaccination() returns signed day differences", {
  expect_equal(
    derive_age_at_vaccination(as.Date("2024-01-01"), as.Date("2024-03-01")),
    60L
  )
  # Pre-birth doses stay negative so downstream checks can flag them.
  expect_equal(
    derive_age_at_vaccination(as.Date("2024-01-01"), as.Date("2023-12-01")),
    -31L
  )
  expect_true(is.na(derive_age_at_vaccination(as.Date(NA), as.Date("2024-01-01"))))
  expect_length(
    derive_age_at_vaccination(as.Date("2024-01-01"), as.Date(c("2024-01-02", "2024-01-03"))),
    2L
  )
  expect_error(
    derive_age_at_vaccination(as.Date(c("2024-01-01", "2024-01-02")),
                              as.Date(c("2024-01-02", "2024-01-03", "2024-01-04"))),
    class = "vaxsurvR_value_error"
  )
})

test_that("classify_vaccination_timeliness() uses the schedule window", {
  s <- vcs_schedule(
    vaccine = c("BCG", "PENTA1", "MCV1"),
    minimum_age_days = c(0, 42, 270),
    maximum_age_days = c(28, 76, 330)
  )
  expect_equal(
    classify_vaccination_timeliness(c(1, 20, 400), c("BCG", "PENTA1", "MCV1"), s),
    c("timely", "early", "late")
  )
  expect_true(is.na(classify_vaccination_timeliness(NA, "BCG", s)))
  expect_true(is.na(classify_vaccination_timeliness(10, "UNKNOWN", s)))

  # A schedule with no windows cannot classify anything.
  s2 <- vcs_schedule("BCG")
  expect_true(is.na(classify_vaccination_timeliness(10, "BCG", s2)))
  expect_error(classify_vaccination_timeliness(10, "BCG", data.frame()),
               class = "vaxsurvR_type_error")
})

test_that("unpadded dates parse (regression: m/d/Y exports)", {
  # SurveyCTO writes "5/6/2025", not "05/06/2025"; a strict round-trip check
  # used to reject every such value as unparseable.
  p <- parse_vaccine_date(c("5/28/2025", "7/7/2025", "11/11/2024", "8/1/2024"),
                          formats = "%m/%d/%Y")
  expect_equal(p$precision, rep("day", 4))
  expect_equal(p$date[1], as.Date("2025-05-28"))
  expect_equal(p$date[3], as.Date("2024-11-11"))
  expect_equal(p$date[4], as.Date("2024-08-01"))

  # Zero-padded values still parse under the same format.
  expect_equal(
    parse_vaccine_date("05/28/2025", formats = "%m/%d/%Y")$date,
    as.Date("2025-05-28")
  )
  # The first matching format wins, so an ambiguous value follows the order given.
  expect_equal(parse_vaccine_date("5/6/2025", formats = "%m/%d/%Y")$date,
               as.Date("2025-05-06"))
  expect_equal(parse_vaccine_date("5/6/2025", formats = "%d/%m/%Y")$date,
               as.Date("2025-06-05"))
  # Things that merely start like a date are still rejected.
  expect_equal(parse_vaccine_date("5/28/2025 vu", formats = "%m/%d/%Y")$precision,
               "unparseable")
})
