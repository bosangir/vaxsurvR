# ---------------------------------------------------------------------------
# Generates the synthetic example data shipped with vaxsurvR.
#
# NOTHING HERE COMES FROM A REAL SURVEY. Every value is simulated. The
# generator plants a known set of data-quality problems so that the checks in
# the package have something to find in examples, vignettes and tests.
#
# Run with:  Rscript data-raw/make_vcs_example.R
# ---------------------------------------------------------------------------

set.seed(20260818)

pkgload::load_all(".", quiet = TRUE)

N_PSU <- 30L
HH_PER_PSU <- 20L
CG_MAX <- 2L
CH_MAX <- 2L

VACCINES <- c("BCG", "OPV0", "OPV1", "PENTA1", "PCV1", "OPV2", "PENTA2",
              "PCV2", "OPV3", "PENTA3", "PCV3", "IPV1", "MCV1", "YF")

sched <- vcs_schedule(
  vaccine = VACCINES,
  minimum_age_days = c(0, 0, 42, 42, 42, 70, 70, 70, 98, 98, 98, 98, 270, 270),
  maximum_age_days = c(28, 14, 76, 76, 76, 104, 104, 104, 132, 132, 132, 132, 330, 330),
  previous_dose = c(NA, NA, "OPV0", NA, NA, "OPV1", "PENTA1", "PCV1",
                    "OPV2", "PENTA2", "PCV2", NA, NA, NA),
  minimum_interval_days = c(NA, NA, 28, NA, NA, 28, 28, 28, 28, 28, 28, NA, NA, NA),
  antigen = c("BCG", "OPV", "OPV", "PENTA", "PCV", "OPV", "PENTA", "PCV",
              "OPV", "PENTA", "PCV", "IPV", "MCV", "YF"),
  dose = c(1L, 0L, 1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 1L, 1L, 1L)
)

# Nominal age at each dose, used to simulate plausible card dates.
nominal_age <- c(BCG = 1, OPV0 = 1, OPV1 = 45, PENTA1 = 45, PCV1 = 45,
                 OPV2 = 75, PENTA2 = 75, PCV2 = 75, OPV3 = 105, PENTA3 = 105,
                 PCV3 = 105, IPV1 = 105, MCV1 = 275, YF = 275)

# ---- sampling frame -------------------------------------------------------

psu <- sprintf("PSU%03d", seq_len(N_PSU))
psu_frame <- data.frame(
  psu = psu,
  stratum = rep(c("urban", "rural"), length.out = N_PSU),
  province = rep(c("Province A", "Province B"), each = N_PSU / 2),
  district = sprintf("District %s",
                     rep(LETTERS[1:4], each = ceiling(N_PSU / 4))[seq_len(N_PSU)]),
  health_zone = sprintf("Zone %02d", rep(seq_len(N_PSU / 2), each = 2)),
  base_lat = round(runif(N_PSU, -5.9, -4.2), 4),
  base_lon = round(runif(N_PSU, 12.4, 15.9), 4),
  stringsAsFactors = FALSE
)
psu_frame$health_area <- sprintf("%s / Area %d", psu_frame$health_zone,
                                 rep(1:2, length.out = N_PSU))
# Population-proportional selection probability, inverted into a design weight.
psu_frame$weight <- round(ifelse(psu_frame$stratum == "urban", 1, 1) *
                            runif(N_PSU, 0.7, 1.6), 3)

enumerators <- sprintf("ENU%02d", 1:12)
teams <- sprintf("TEAM%d", 1:4)
supervisors <- sprintf("SUP%d", 1:2)

first_names <- c("Amina", "Joseph", "Marie", "Patrick", "Grace", "Daniel",
                 "Esther", "Emmanuel", "Sarah", "Michel", "Julie", "Pierre")
last_names <- c("Kabemba", "Ilunga", "Mbayo", "Nsimba", "Tshibangu", "Lukusa",
                "Mpoyi", "Kalala", "Mutombo", "Ndaya")
fake_name <- function(n) {
  paste(sample(first_names, n, TRUE), sample(last_names, n, TRUE))
}

survey_start <- as.Date("2026-05-04")

# ---- households -----------------------------------------------------------

n_hh <- N_PSU * HH_PER_PSU
hh <- data.frame(
  KEY = sprintf("uuid:HH%04d", seq_len(n_hh)),
  psu_id = rep(psu, each = HH_PER_PSU),
  stringsAsFactors = FALSE
)
fr <- psu_frame[match(hh$psu_id, psu_frame$psu), ]
hh$segment_id <- sprintf("SEG%d", sample(1:2, n_hh, TRUE))
hh$stratum <- fr$stratum
hh$province <- fr$province
hh$district <- fr$district
hh$health_zone <- fr$health_zone
hh$health_area <- fr$health_area
hh$residence <- ifelse(fr$stratum == "urban", "1", "2")
hh$final_weight <- round(fr$weight * runif(n_hh, 0.95, 1.05), 4)
hh$interview_date <- format(
  survey_start + rep(seq_len(N_PSU) - 1L, each = HH_PER_PSU) + sample(0:2, n_hh, TRUE),
  "%Y-%m-%d"
)
hh$enumerator <- sample(enumerators, n_hh, TRUE)
hh$team <- teams[(match(hh$enumerator, enumerators) - 1L) %/% 3L + 1L]
hh$supervisor <- supervisors[(match(hh$team, teams) - 1L) %/% 2L + 1L]
hh$duration_min <- round(rgamma(n_hh, shape = 9, scale = 5) + 12, 1)
hh$gps_lat <- round(fr$base_lat + rnorm(n_hh, 0, 0.004), 5)
hh$gps_lon <- round(fr$base_lon + rnorm(n_hh, 0, 0.004), 5)
hh$gps_accuracy <- round(pmax(2, rgamma(n_hh, shape = 2, scale = 3)), 1)
hh$respondent_name <- fake_name(n_hh)
hh$respondent_phone <- sprintf("+24381%06d", sample(0:999999, n_hh))
hh$address <- sprintf("Av. %d, no %d", sample(1:40, n_hh, TRUE), sample(1:200, n_hh, TRUE))

# Eligibility and consent funnel.
hh$eligible <- ifelse(runif(n_hh) < 0.42, "1", "0")
hh$consent <- ifelse(hh$eligible == "1" & runif(n_hh) < 0.96, "1", "0")
hh$n_eligible <- ifelse(hh$eligible == "1", sample(1:2, n_hh, TRUE, c(0.85, 0.15)), 0)
hh$n_eligible <- as.character(hh$n_eligible)

interviewed <- hh$eligible == "1" & hh$consent == "1"

# ---- children and vaccination cards --------------------------------------

blank_col <- function() rep(NA_character_, n_hh)
for (c_i in seq_len(CG_MAX)) {
  for (k in seq_len(CH_MAX)) {
    for (nm in c("EC_ID", "EC_NAME", "EC_DOB", "EC_SEX", "CG_NAME", "CARD")) {
      hh[[sprintf("%s_%d_%d", nm, c_i, k)]] <- blank_col()
    }
    for (v in seq_along(VACCINES)) {
      hh[[sprintf("CVH%02d_%d_%d", v, c_i, k)]] <- blank_col()
      hh[[sprintf("CVH%02d_date_%d_%d", v, c_i, k)]] <- blank_col()
      hh[[sprintf("VR%02d_%d_%d", v, c_i, k)]] <- blank_col()
    }
  }
}

# True coverage declines along the schedule, with a rural penalty.
base_cov <- c(BCG = 0.93, OPV0 = 0.78, OPV1 = 0.90, PENTA1 = 0.90, PCV1 = 0.89,
              OPV2 = 0.84, PENTA2 = 0.84, PCV2 = 0.83, OPV3 = 0.74,
              PENTA3 = 0.74, PCV3 = 0.73, IPV1 = 0.62, MCV1 = 0.66, YF = 0.63)

child_rows <- list()
for (i in which(interviewed)) {
  n_child <- as.integer(hh$n_eligible[i])
  if (is.na(n_child) || n_child < 1L) next
  for (k in seq_len(min(n_child, CH_MAX))) {
    c_i <- 1L
    age_m <- sample(12:23, 1)
    dob <- as.Date(hh$interview_date[i]) - round(age_m * 30.4375) - sample(0:29, 1)
    cid <- sprintf("CH%04d%d", i, k)

    hh[[sprintf("EC_ID_%d_%d", c_i, k)]][i] <- cid
    hh[[sprintf("EC_NAME_%d_%d", c_i, k)]][i] <- fake_name(1)
    hh[[sprintf("CG_NAME_%d_%d", c_i, k)]][i] <- hh$respondent_name[i]
    hh[[sprintf("EC_DOB_%d_%d", c_i, k)]][i] <- format(dob, "%Y-%m-%d")
    hh[[sprintf("EC_SEX_%d_%d", c_i, k)]][i] <- sample(c("1", "2"), 1)

    has_card <- runif(1) < 0.63
    hh[[sprintf("CARD_%d_%d", c_i, k)]][i] <- if (has_card) "1" else "0"

    penalty <- if (hh$stratum[i] == "rural") 0.08 else 0
    got <- runif(length(VACCINES)) < (base_cov - penalty)
    names(got) <- VACCINES
    # Doses are given in order: once a child drops out of a series they stay out.
    for (ag in unique(sched$antigen)) {
      ds <- sched$vaccine[sched$antigen == ag]
      ds <- ds[order(sched$dose[match(ds, sched$vaccine)])]
      dropped <- FALSE
      for (d in ds) {
        if (dropped) got[d] <- FALSE
        if (!got[d]) dropped <- TRUE
      }
    }

    for (v in seq_along(VACCINES)) {
      vac <- VACCINES[v]
      cvh <- sprintf("CVH%02d_%d_%d", v, c_i, k)
      cvhd <- sprintf("CVH%02d_date_%d_%d", v, c_i, k)
      vr <- sprintf("VR%02d_%d_%d", v, c_i, k)

      if (!got[[vac]]) {
        hh[[cvh]][i] <- if (has_card) "3" else "98"
        hh[[vr]][i] <- if (runif(1) < 0.05) "1" else "0"   # a little recall over-report
        next
      }
      given <- dob + round(nominal_age[[vac]] + rnorm(1, 4, 9))
      if (given > as.Date(hh$interview_date[i])) {
        given <- as.Date(hh$interview_date[i]) - sample(1:10, 1)
      }
      hh[[vr]][i] <- if (runif(1) < 0.93) "1" else if (runif(1) < 0.5) "0" else "98"
      if (!has_card) {
        hh[[cvh]][i] <- "98"
        next
      }
      # On-card, but the date line is sometimes blank or only partly legible.
      r <- runif(1)
      if (r < 0.86) {
        hh[[cvh]][i] <- "1"
        hh[[cvhd]][i] <- format(given, "%Y-%m-%d")
      } else if (r < 0.94) {
        hh[[cvh]][i] <- "2"
        hh[[cvhd]][i] <- format(given, "%Y-%m")     # partial date
      } else {
        hh[[cvh]][i] <- "2"                          # tick, no date at all
      }
    }
    child_rows[[length(child_rows) + 1L]] <- c(row = i, caregiver = c_i, child = k)
  }
}

idx <- do.call(rbind, child_rows)
message(sprintf("Simulated %d households, %d children.", n_hh, nrow(idx)))

# ---------------------------------------------------------------------------
# Planted data-quality problems. Each one is documented in ?vcs_example.
# ---------------------------------------------------------------------------

pick <- function(n, exclude = integer(0)) {
  pool <- setdiff(idx[, "row"], exclude)
  sample(pool, n)
}
planted <- list()

# 1. Duplicate child identifier across two different households.
dd <- pick(2)
hh$EC_ID_1_1[dd[2]] <- hh$EC_ID_1_1[dd[1]]
planted$duplicate_child_id <- hh$KEY[dd]

# 2. Vaccination recorded before the date of birth.
r <- pick(3, exclude = dd)
for (i in r) {
  dob <- as.Date(hh$EC_DOB_1_1[i])
  hh$CVH01_1_1[i] <- "1"
  hh$CVH01_date_1_1[i] <- format(dob - sample(3:40, 1), "%Y-%m-%d")
}
planted$prebirth <- hh$KEY[r]

# 3. Impossible dose sequence: PENTA2 (v = 7) before PENTA1 (v = 4).
r <- pick(3, exclude = dd)
for (i in r) {
  dob <- as.Date(hh$EC_DOB_1_1[i])
  hh$CVH04_1_1[i] <- "1"
  hh$CVH04_date_1_1[i] <- format(dob + 90, "%Y-%m-%d")
  hh$CVH07_1_1[i] <- "1"
  hh$CVH07_date_1_1[i] <- format(dob + 60, "%Y-%m-%d")
}
planted$out_of_sequence <- hh$KEY[r]

# 4. Dose interval far below the schedule minimum.
r <- pick(2, exclude = dd)
for (i in r) {
  dob <- as.Date(hh$EC_DOB_1_1[i])
  hh$CVH04_1_1[i] <- "1"
  hh$CVH04_date_1_1[i] <- format(dob + 45, "%Y-%m-%d")
  hh$CVH07_1_1[i] <- "1"
  hh$CVH07_date_1_1[i] <- format(dob + 48, "%Y-%m-%d")
}
planted$short_interval <- hh$KEY[r]

# 5. Vaccination date after the interview date.
r <- pick(3, exclude = dd)
for (i in r) {
  hh$CVH13_1_1[i] <- "1"
  hh$CVH13_date_1_1[i] <- format(as.Date(hh$interview_date[i]) + sample(5:60, 1), "%Y-%m-%d")
}
planted$future_date <- hh$KEY[r]

# 6. Two doses of the same antigen sharing a date.
r <- pick(2, exclude = dd)
for (i in r) {
  dob <- as.Date(hh$EC_DOB_1_1[i])
  d <- format(dob + 70, "%Y-%m-%d")
  hh$CVH04_1_1[i] <- "1"; hh$CVH04_date_1_1[i] <- d
  hh$CVH07_1_1[i] <- "1"; hh$CVH07_date_1_1[i] <- d
}
planted$duplicate_dose <- hh$KEY[r]

# 7. Card dates recorded although the interviewer saw no card.
r <- pick(3, exclude = dd)
for (i in r) {
  hh$CARD_1_1[i] <- "0"
  hh$CVH01_1_1[i] <- "1"
  hh$CVH01_date_1_1[i] <- format(as.Date(hh$EC_DOB_1_1[i]) + 2, "%Y-%m-%d")
}
planted$date_without_card <- hh$KEY[r]

# 8. Duplicated household GPS: a team that recorded one point for a whole street.
r <- pick(4, exclude = dd)
hh$gps_lat[r] <- hh$gps_lat[r[1]]
hh$gps_lon[r] <- hh$gps_lon[r[1]]
planted$duplicate_gps <- hh$KEY[r]

# 9. Implausibly short interviews.
r <- pick(4, exclude = dd)
hh$duration_min[r] <- round(runif(length(r), 2, 6), 1)
planted$short_interview <- hh$KEY[r]

# 10. A malformed and an out-of-range date that no format can parse.
r <- pick(2, exclude = dd)
hh$CVH01_date_1_1[r[1]] <- "31/02/2025"
hh$CVH01_date_1_1[r[2]] <- "vu sur carte"
planted$unparseable_date <- hh$KEY[r]

# 11. A child aged outside the 12-23 month eligibility window.
r <- pick(2, exclude = dd)
for (i in r) {
  hh$EC_DOB_1_1[i] <- format(as.Date(hh$interview_date[i]) - 1000, "%Y-%m-%d")
}
planted$age_out_of_window <- hh$KEY[r]

# 12. A household screened ineligible that nevertheless has a child module.
r <- pick(2, exclude = dd)
hh$eligible[r] <- "0"
planted$ineligible_with_child <- hh$KEY[r]

# 13. PENTA3 documented while PENTA2 is not: a dose with no predecessor.
r <- pick(3, exclude = dd)
for (i in r) {
  dob <- as.Date(hh$EC_DOB_1_1[i])
  hh$CARD_1_1[i] <- "1"
  hh$CVH07_1_1[i] <- "3"                                     # PENTA2 not given
  hh$CVH07_date_1_1[i] <- NA
  hh$CVH10_1_1[i] <- "1"                                     # PENTA3 documented
  hh$CVH10_date_1_1[i] <- format(dob + 110, "%Y-%m-%d")
}
planted$missing_previous_dose <- hh$KEY[r]

# ---- assemble the shipped objects ----------------------------------------

vcs_example_raw <- tibble::as_tibble(hh)
attr(vcs_example_raw, "planted_issues") <- planted

vcs_example_schedule <- sched

vcs_example_dictionary <- vcs_dictionary(
  interview_id = "KEY", household_id = "KEY",
  psu = "psu_id", segment = "segment_id", stratum = "stratum",
  province = "province", district = "district",
  health_zone = "health_zone", health_area = "health_area",
  residence = "residence", weight = "final_weight",
  interview_date = "interview_date", duration_min = "duration_min",
  enumerator = "enumerator", team = "team", supervisor = "supervisor",
  gps_lat = "gps_lat", gps_lon = "gps_lon", gps_accuracy = "gps_accuracy",
  eligible = "eligible", consent = "consent", n_eligible = "n_eligible",
  telephone = "respondent_phone", address = "address",
  child_id = "EC_ID_{c}_{k}", child_name = "EC_NAME_{c}_{k}",
  caregiver_name = "CG_NAME_{c}_{k}",
  child_dob = "EC_DOB_{c}_{k}", sex = "EC_SEX_{c}_{k}",
  card_seen = "CARD_{c}_{k}",
  card_status = "CVH{vv}_{c}_{k}", card_date = "CVH{vv}_date_{c}_{k}",
  recall_status = "VR{vv}_{c}_{k}",
  n_caregivers = CG_MAX, n_children = CH_MAX
)

vcs_example <- map_vcs_variables(
  vcs_example_raw, vcs_example_dictionary, vcs_example_schedule,
  keep_raw = FALSE
)
vcs_example$meta$source <- "synthetic (data-raw/make_vcs_example.R)"
vcs_example$meta$planted_issues <- planted

save(vcs_example_raw, file = "data/vcs_example_raw.rda", compress = "xz")
save(vcs_example, file = "data/vcs_example.rda", compress = "xz")
save(vcs_example_dictionary, file = "data/vcs_example_dictionary.rda", compress = "xz")
save(vcs_example_schedule, file = "data/vcs_example_schedule.rda", compress = "xz")

message("Wrote data/vcs_example*.rda")
print(vcs_example)
