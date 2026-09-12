test_that("vcs_schedule() builds a well-formed schedule", {
  s <- vcs_schedule(
    vaccine = c("BCG", "PENTA1", "PENTA2", "PENTA3", "MCV1"),
    minimum_age_days = c(0, 42, 70, 98, 270),
    previous_dose = c(NA, NA, "PENTA1", "PENTA2", NA),
    minimum_interval_days = c(NA, NA, 28, 28, NA)
  )
  expect_s3_class(s, "vcs_schedule")
  expect_true(is_vcs_schedule(s))
  expect_equal(nrow(s), 5L)
  expect_equal(s$antigen, c("BCG", "PENTA", "PENTA", "PENTA", "MCV"))
  expect_equal(s$dose, c(1L, 1L, 2L, 3L, 1L))
  expect_true(all(is.na(s$maximum_age_days)))
})

test_that("vcs_schedule() rejects malformed input", {
  expect_error(vcs_schedule(character(0)), class = "vaxsurvR_schedule_error")
  expect_error(vcs_schedule(c("BCG", "BCG")), class = "vaxsurvR_schedule_error")
  expect_error(vcs_schedule(c("BCG", NA)), class = "vaxsurvR_schedule_error")
  expect_error(
    vcs_schedule(c("BCG", "PENTA1"), minimum_age_days = c(1, 2, 3)),
    class = "vaxsurvR_schedule_error"
  )
  expect_error(
    vcs_schedule(c("BCG", "PENTA1"), previous_dose = c(NA, "NOPE")),
    class = "vaxsurvR_schedule_error"
  )
  expect_error(
    vcs_schedule("BCG", previous_dose = "BCG"),
    class = "vaxsurvR_schedule_error"
  )
  expect_error(
    vcs_schedule(c("A", "B"), previous_dose = c("B", "A")),
    class = "vaxsurvR_schedule_error"
  )
  expect_error(
    vcs_schedule("BCG", minimum_age_days = -1),
    class = "vaxsurvR_schedule_error"
  )
  expect_error(
    vcs_schedule("BCG", minimum_age_days = 100, maximum_age_days = 10),
    class = "vaxsurvR_schedule_error"
  )
})

test_that("schedules with different numbers of doses are supported", {
  one <- vcs_schedule("MCV1", minimum_age_days = 270)
  expect_equal(nrow(one), 1L)

  many <- vcs_schedule(
    vaccine = paste0("OPV", 0:4),
    minimum_age_days = c(0, 42, 70, 98, 126),
    previous_dose = c(NA, "OPV0", "OPV1", "OPV2", "OPV3"),
    minimum_interval_days = c(NA, 28, 28, 28, 28)
  )
  expect_equal(nrow(many), 5L)
  expect_equal(unique(many$antigen), "OPV")
  expect_equal(many$dose, 0:4)
})

test_that("vcs_schedule_who() is a valid schedule and can be subset", {
  s <- vcs_schedule_who()
  expect_s3_class(s, "vcs_schedule")
  expect_true(all(c("BCG", "PENTA1", "PENTA3", "MCV1") %in% s$vaccine))
  expect_true(all(s$minimum_age_days >= 0, na.rm = TRUE))

  sub <- vcs_schedule_who(c("BCG", "PENTA1", "PENTA3", "MCV1"))
  expect_equal(sub$vaccine, c("BCG", "PENTA1", "PENTA3", "MCV1"))
  # PENTA2 was dropped, so PENTA3 must not keep a dangling predecessor.
  expect_true(is.na(sub$previous_dose[sub$vaccine == "PENTA3"]))
  expect_true(is.na(sub$minimum_interval_days[sub$vaccine == "PENTA3"]))
  expect_error(vcs_schedule_who("NOTAVACCINE"), class = "vaxsurvR_schedule_error")
})

test_that("schedule_final_doses() picks the last dose per antigen", {
  s <- vcs_schedule_who(c("BCG", "PENTA1", "PENTA2", "PENTA3", "MCV1"))
  expect_setequal(schedule_final_doses(s), c("BCG", "PENTA3", "MCV1"))
  expect_error(schedule_final_doses(data.frame()), class = "vaxsurvR_type_error")
})

test_that("print method returns its input invisibly", {
  s <- vcs_schedule_who(c("BCG"))
  expect_output(print(s), "vcs_schedule")
  expect_identical(withVisible(utils::capture.output(print(s)))$visible, TRUE)
})
