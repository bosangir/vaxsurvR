test_that("read_vcs() reads CSV as character by default", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(id = c("01", "02"), psu = c("A", "A"), stringsAsFactors = FALSE),
    tmp, row.names = FALSE
  )
  raw <- read_vcs(tmp)
  expect_s3_class(raw, "tbl_df")
  expect_type(raw$id, "character")
  expect_equal(raw$id, c("01", "02"))     # leading zero survives
  expect_equal(attr(raw, "vcs_source")$format, "csv")
  expect_equal(attr(raw, "vcs_source")$n_rows, 2L)
})

test_that("read_vcs() errors clearly on bad paths and formats", {
  expect_error(read_vcs("definitely_missing.csv"), class = "vaxsurvR_io_error")
  bad <- withr::local_tempfile(fileext = ".sav")
  writeLines("x", bad)
  expect_error(read_vcs(bad), class = "vaxsurvR_io_error")

  notdf <- withr::local_tempfile(fileext = ".rds")
  saveRDS(1:3, notdf)
  expect_error(read_vcs(notdf), class = "vaxsurvR_io_error")
})

test_that("read_vcs() round-trips rds", {
  tmp <- withr::local_tempfile(fileext = ".rds")
  saveRDS(data.frame(a = 1:2), tmp)
  expect_equal(nrow(read_vcs(tmp)), 2L)
})

test_that("detect_delim() finds tabs, commas and semicolons", {
  f <- function(lines) {
    p <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
    writeLines(lines, p)
    p
  }
  expect_equal(detect_delim(f(c("a\tb\tc", "1\t2\t3"))), "\t")
  expect_equal(detect_delim(f(c("a,b,c", "1,2,3"))), ",")
  expect_equal(detect_delim(f(c("a;b;c", "1;2;3"))), ";")
  expect_equal(detect_delim(f("noseparators")), ",")
})

test_that("read_surveycto() warns when the delimiter looks mis-detected", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("KEY\tPVT06", "uuid:1\tPSU01"), tmp)
  expect_warning(read_surveycto(tmp), class = "vaxsurvR_delimiter_warning")
  expect_silent(read_surveycto(tmp, min_cols = 2))
})

test_that("map_vcs_variables() expands repeat groups into child rows", {
  d <- fixture_vcs()
  expect_s3_class(d, "vcs_data")
  expect_true(is_vcs_data(d))
  expect_equal(nrow(d$households), 3L)
  # h1 has two children, h2 and h3 one each.
  expect_equal(nrow(d$children), 4L)
  expect_equal(sort(table(d$children$interview_id)), c(h2 = 1L, h3 = 1L, h1 = 2L),
               ignore_attr = TRUE)
  expect_equal(nrow(d$vaccinations), 4L * 2L)
  expect_setequal(unique(d$vaccinations$vaccine), c("BCG", "PENTA1"))
})

test_that("map_vcs_variables() standardises types and preserves the raw import", {
  d <- fixture_vcs()
  expect_type(d$households$eligible, "logical")
  expect_true(all(d$households$eligible))
  expect_s3_class(d$children$child_dob, "Date")
  expect_type(d$children$card_seen, "logical")
  expect_identical(d$raw, fixture_wide())
  expect_equal(d$meta$n_raw_rows, 3L)
})

test_that("map_vcs_variables() reads card evidence without inventing negatives", {
  d <- fixture_vcs()
  vx <- d$vaccinations
  bcg <- vx[vx$vaccine == "BCG", ]
  # "1" and "2" are card-documented; "3" (no) is FALSE; blank stays NA.
  expect_true(all(bcg$card_documented[bcg$card_status %in% c("1", "2")]))
  expect_false(any(bcg$card_documented[bcg$card_status == "3"], na.rm = TRUE))
  expect_true(all(is.na(vx$card_documented[is.na(vx$card_status)])))

  # A partial card date ("2024-04") parses to NA, not to an invented day.
  partial <- vx[!is.na(vx$card_date_precision) & vx$card_date_precision == "month", ]
  expect_gt(nrow(partial), 0L)
  expect_true(all(is.na(partial$card_date)))
})

test_that("map_vcs_variables() constructs ids when they are not mapped", {
  d <- fixture_vcs()
  expect_false(anyDuplicated(d$children$child_id) > 0)
  expect_true(all(grepl("^h[0-9]_1_[12]$", d$children$child_id)))
  rules <- derivation_rules(d)
  expect_true(any(grepl("child_id", rules$variable)))
})

test_that("map_vcs_variables() handles long-format data with no repeats", {
  raw <- data.frame(
    cid = c("c1", "c2"), hid = c("h1", "h2"), clust = c("P1", "P1"),
    age = c("14", "20"), stringsAsFactors = FALSE
  )
  dict <- vcs_dictionary(child_id = "cid", household_id = "hid",
                         interview_id = "hid", psu = "clust", age_months = "age")
  d <- map_vcs_variables(raw, dict)
  expect_equal(nrow(d$children), 2L)
  expect_equal(d$children$child_id, c("c1", "c2"))
  expect_equal(d$children$age_months, c(14, 20))
  expect_equal(nrow(d$vaccinations), 0L)
  expect_true(all(is.na(d$children$caregiver_index)))
})

test_that("map_vcs_variables() handles an empty dataset", {
  raw <- fixture_wide()[0, ]
  d <- suppressWarnings(map_vcs_variables(raw, fixture_dictionary(), fixture_schedule()))
  expect_equal(nrow(d$children), 0L)
  expect_equal(nrow(d$vaccinations), 0L)
  expect_equal(nrow(d$households), 0L)
  expect_s3_class(d, "vcs_data")
})

test_that("map_vcs_variables() reports missing source columns", {
  raw <- fixture_wide()
  raw$CVH02_date_1_1 <- NULL
  expect_warning(
    map_vcs_variables(raw, fixture_dictionary(), fixture_schedule()),
    class = "vaxsurvR_missing_column"
  )
  expect_error(
    map_vcs_variables(raw, fixture_dictionary(), fixture_schedule(), strict = TRUE),
    class = "vaxsurvR_missing_column"
  )
})

test_that("map_vcs_variables() requires a schedule for vaccine concepts", {
  expect_error(
    map_vcs_variables(fixture_wide(), fixture_dictionary()),
    class = "vaxsurvR_type_error"
  )
  expect_error(
    map_vcs_variables(list(a = 1), fixture_dictionary(), fixture_schedule()),
    class = "vaxsurvR_type_error"
  )
  expect_error(
    map_vcs_variables(fixture_wide(), list(), fixture_schedule()),
    class = "vaxsurvR_type_error"
  )
})

test_that("keep_raw = FALSE drops the raw import", {
  d <- suppressWarnings(
    map_vcs_variables(fixture_wide(), fixture_dictionary(), fixture_schedule(),
                      keep_raw = FALSE)
  )
  expect_null(d$raw)
})

test_that("vcs_data accessors and print methods work", {
  d <- fixture_vcs()
  expect_output(print(d), "vcs_data")
  s <- summary(d)
  expect_s3_class(s, "vcs_data_summary")
  expect_output(print(s), "children")
  expect_equal(s$counts[["children"]], 4L)
  expect_s3_class(as.data.frame(s), "tbl_df")

  ch <- vcs_children(d)
  expect_true("psu" %in% names(ch))
  expect_equal(nrow(ch), 4L)

  expect_equal(nrow(vcs_vaccinations(d, "BCG")), 4L)
  expect_warning(vcs_vaccinations(d, "NOPE"), class = "vaxsurvR_unknown_vaccine")
  expect_error(vcs_children(list()), class = "vaxsurvR_type_error")
})

test_that("derivation rules are recorded and readable", {
  d <- fixture_vcs()
  r <- derivation_rules(d)
  expect_s3_class(r, "tbl_df")
  expect_true(all(c("table", "variable", "rule") %in% names(r)))
  expect_true("card_documented" %in% r$variable)
  expect_equal(nrow(derivation_rules(data.frame(a = 1))), 0L)
})

test_that("presence detection ignores columns constant within a repeat slot", {
  # Regression: a concept keyed only by {c} (a caregiver name, say) is constant
  # across that caregiver's child slots, and must not conjure a child into
  # every empty k slot.
  raw <- data.frame(
    KEY = c("h1", "h2"),
    psu = c("P1", "P1"),
    CB02_1 = c("caregiver A", "caregiver B"),   # present for both households
    CB02_2 = c(NA, NA),
    child_1_1 = c("c1", "c2"),                  # only one child each
    child_1_2 = c(NA, NA),
    child_2_1 = c(NA, NA),
    child_2_2 = c(NA, NA),
    stringsAsFactors = FALSE
  )
  dict <- vcs_dictionary(
    interview_id = "KEY", household_id = "KEY", psu = "psu",
    caregiver_name = "CB02_{c}", child_id = "child_{c}_{k}",
    n_caregivers = 2, n_children = 2
  )
  d <- map_vcs_variables(raw, dict)
  expect_equal(nrow(d$children), 2L)
  expect_setequal(d$children$child_id, c("c1", "c2"))
})

test_that("presence falls back to partially varying columns with a warning", {
  raw <- data.frame(
    KEY = c("h1", "h2"),
    psu = c("P1", "P1"),
    CB02_1 = c("A", "B"),
    CB02_2 = c(NA, NA),
    stringsAsFactors = FALSE
  )
  dict <- vcs_dictionary(
    interview_id = "KEY", household_id = "KEY", psu = "psu",
    caregiver_name = "CB02_{c}",
    n_caregivers = 2, n_children = 2
  )
  expect_warning(d <- map_vcs_variables(raw, dict),
                 class = "vaxsurvR_weak_presence")
  expect_gt(nrow(d$children), 0L)
})
