test_that("vcs_concepts() describes the vocabulary", {
  cc <- vcs_concepts()
  expect_s3_class(cc, "tbl_df")
  expect_named(cc, c("concept", "level", "required", "description"))
  expect_false(anyDuplicated(cc$concept) > 0)
  expect_setequal(unique(cc$level), c("household", "child", "vaccine"))
})

test_that("vcs_dictionary() accepts valid mappings", {
  d <- vcs_dictionary(
    child_id = "childid", household_id = "hh_id", psu = "cluster",
    age_months = "child_age_month", sex = "child_sex"
  )
  expect_s3_class(d, "vcs_dictionary")
  expect_true(is_vcs_dictionary(d))
  expect_equal(d$map$child_id, "childid")
  expect_equal(d$n_caregivers, 1L)

  df <- as.data.frame(d)
  expect_true(all(c("concept", "source", "level", "required") %in% names(df)))
  expect_equal(nrow(df), 5L)
})

test_that("vcs_dictionary() rejects invalid mappings", {
  expect_error(vcs_dictionary(nonsense = "x"), class = "vaxsurvR_dictionary_error")
  expect_error(vcs_dictionary("unnamed"), class = "vaxsurvR_dictionary_error")
  expect_error(vcs_dictionary(child_id = c("a", "b")), class = "vaxsurvR_dictionary_error")
  expect_error(vcs_dictionary(child_id = 1), class = "vaxsurvR_dictionary_error")
  # A household-level concept may not use a repeat placeholder.
  expect_error(vcs_dictionary(psu = "psu_{c}"), class = "vaxsurvR_dictionary_error")
  # A child-level concept may not use a vaccine placeholder.
  expect_error(vcs_dictionary(child_id = "id_{vv}"), class = "vaxsurvR_dictionary_error")
  expect_error(vcs_dictionary(child_id = "a", n_caregivers = 0), class = "vaxsurvR_value_error")
})

test_that("empty dictionaries are allowed and print", {
  d <- vcs_dictionary()
  expect_s3_class(d, "vcs_dictionary")
  expect_length(d$map, 0L)
  expect_output(print(d), "no concepts mapped")
  expect_equal(nrow(as.data.frame(d)), 0L)
})

test_that("resolve_concept() expands repeat and vaccine placeholders", {
  d <- vcs_dictionary(
    card_date = "CVH{vv}_date_{c}_{k}",
    card_seen = "CVH_card_shown_{c}_{k}",
    psu = "submission_psu",
    n_caregivers = 2, n_children = 2
  )
  s <- vcs_schedule_who(c("BCG", "PENTA1", "PENTA3"))

  r <- resolve_concept(d, "card_date", s)
  expect_equal(nrow(r), 2 * 2 * 3)
  expect_true("CVH01_date_1_1" %in% r$column)
  expect_true("CVH03_date_2_2" %in% r$column)

  rk <- resolve_concept(d, "card_seen")
  expect_equal(nrow(rk), 4L)
  expect_true(all(is.na(rk$vaccine)))

  rp <- resolve_concept(d, "psu")
  expect_equal(rp$column, "submission_psu")
  expect_true(is.na(rp$caregiver))

  expect_equal(nrow(resolve_concept(d, "sex")), 0L)
  expect_error(resolve_concept(d, "card_date"), class = "vaxsurvR_template_error")
})

test_that("read_vcs_dictionary() round-trips a CSV", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(
      concept = c("child_id", "psu", ""),
      source = c("cid", "clust", "ignored"),
      stringsAsFactors = FALSE
    ),
    tmp, row.names = FALSE
  )
  d <- read_vcs_dictionary(tmp, n_children = 3)
  expect_s3_class(d, "vcs_dictionary")
  expect_equal(d$map$child_id, "cid")
  expect_equal(d$n_children, 3L)
  expect_length(d$map, 2L)

  expect_error(read_vcs_dictionary("no_such_file.csv"), class = "vaxsurvR_io_error")

  bad <- withr::local_tempfile(fileext = ".txt")
  writeLines("x", bad)
  expect_error(read_vcs_dictionary(bad), class = "vaxsurvR_io_error")
})

test_that("fill_template() errors on unknown placeholders", {
  expect_equal(fill_template("a_{c}", list(c = 2)), "a_2")
  expect_equal(fill_template("plain", list()), "plain")
  expect_error(fill_template("a_{zzz}", list(c = 1)), class = "vaxsurvR_template_error")
})

test_that("card_no_values separates 'no' from 'don't know'", {
  d <- vcs_dictionary(card_status = "CVH{vv}")
  expect_equal(d$card_yes_values, c("1", "2"))
  expect_true("3" %in% d$card_no_values)
  expect_false("98" %in% d$card_no_values)
  expect_error(
    vcs_dictionary(card_yes_values = c("1", "3"), card_no_values = c("3")),
    class = "vaxsurvR_dictionary_error"
  )
})
