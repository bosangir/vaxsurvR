# Shared fixtures. Everything here is synthetic; no real survey data is used
# anywhere in the package's tests, examples or vignettes.

#' A small wide-format export in the shape SurveyCTO produces
#' @noRd
fixture_wide <- function() {
  data.frame(
    KEY = c("h1", "h2", "h3"),
    psu = c("P1", "P1", "P2"),
    seg = c("S1", "S2", "S1"),
    zone = c("ZA", "ZA", "ZB"),
    stratum = c("urban", "urban", "rural"),
    wt = c("1.2", "1.2", "0.8"),
    elig = c("1", "1", "1"),
    consent = c("1", "1", "1"),
    dob_1_1 = c("2024-01-10", "2024-02-15", "2024-03-20"),
    dob_1_2 = c("2024-01-11", NA, NA),
    sex_1_1 = c("1", "2", "1"),
    sex_1_2 = c("2", NA, NA),
    card_1_1 = c("1", "1", "0"),
    card_1_2 = c("1", NA, NA),
    # BCG
    CVH01_1_1 = c("1", "1", "3"),
    CVH01_date_1_1 = c("2024-01-11", "2024-02-16", NA),
    VR01_1_1 = c("1", "1", "0"),
    CVH01_1_2 = c("1", NA, NA),
    CVH01_date_1_2 = c("2024-01-12", NA, NA),
    VR01_1_2 = c("1", NA, NA),
    # PENTA1
    CVH02_1_1 = c("1", "2", "3"),
    CVH02_date_1_1 = c("2024-03-01", "2024-04", NA),
    VR02_1_1 = c("1", "1", "0"),
    CVH02_1_2 = c("3", NA, NA),
    CVH02_date_1_2 = c(NA, NA, NA),
    VR02_1_2 = c("0", NA, NA),
    stringsAsFactors = FALSE
  )
}

#' Dictionary matching fixture_wide()
#' @noRd
fixture_dictionary <- function() {
  vcs_dictionary(
    interview_id = "KEY", household_id = "KEY",
    psu = "psu", segment = "seg", health_zone = "zone", stratum = "stratum",
    weight = "wt", eligible = "elig", consent = "consent",
    child_dob = "dob_{c}_{k}", sex = "sex_{c}_{k}", card_seen = "card_{c}_{k}",
    card_status = "CVH{vv}_{c}_{k}", card_date = "CVH{vv}_date_{c}_{k}",
    recall_status = "VR{vv}_{c}_{k}",
    n_caregivers = 1, n_children = 2
  )
}

#' Two-dose schedule matching fixture_wide()
#' @noRd
fixture_schedule <- function() {
  vcs_schedule(
    vaccine = c("BCG", "PENTA1"),
    minimum_age_days = c(0, 42),
    maximum_age_days = c(28, 76),
    previous_dose = c(NA, NA),
    minimum_interval_days = c(NA, NA)
  )
}

#' fixture_wide() already mapped
#' @noRd
fixture_vcs <- function() {
  suppressWarnings(
    map_vcs_variables(fixture_wide(), fixture_dictionary(), fixture_schedule())
  )
}
