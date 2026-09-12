series_fixture <- function() {
  # Three children, PENTA asked as ever + count, BCG as a plain yes/no.
  raw <- data.frame(
    KEY = c("h1", "h2", "h3", "h4"),
    psu = "P1",
    child_1_1 = c("c1", "c2", "c3", "c4"),
    VR01_1_1 = c("1", "2", "98", NA),          # BCG: yes / no / dk / silent
    VR08_1_1 = c("1", "1", "2", "1"),          # PENTA ever: yes / yes / no / yes
    VR09_1_1 = c("3", "1", NA, "98"),          # PENTA count: 3 / 1 / - / dk
    stringsAsFactors = FALSE
  )
  sched <- vcs_schedule_who(c("BCG", "PENTA1", "PENTA2", "PENTA3"))
  rm <- vcs_recall_map(
    BCG = "VR01_{c}_{k}",
    PENTA = list(ever = "VR08_{c}_{k}", count = "VR09_{c}_{k}")
  )
  list(raw = raw, sched = sched, map = rm)
}

test_that("vcs_recall_map() validates its entries", {
  expect_s3_class(vcs_recall_map(BCG = "VR01"), "vcs_recall_map")
  expect_error(vcs_recall_map("unnamed"), class = "vaxsurvR_recall_error")
  expect_error(vcs_recall_map(BCG = "a", BCG = "b"), class = "vaxsurvR_recall_error")
  expect_error(vcs_recall_map(PENTA = list(count = "x")), class = "vaxsurvR_recall_error")
  expect_error(vcs_recall_map(PENTA = list(ever = "x", other = "y")),
               class = "vaxsurvR_recall_error")
  expect_output(print(vcs_recall_map(BCG = "a", PENTA = list(ever = "b", count = "c"))),
                "series")
})

test_that("apply_recall_map() builds one column per vaccine and antigen", {
  fx <- series_fixture()
  out <- apply_recall_map(fx$raw, fx$map, fx$sched, n_caregivers = 1, n_children = 1)
  expect_true(all(c("recall_status_BCG_1_1", "recall_status_PENTA1_1_1",
                    "recall_ever_PENTA_1_1", "recall_count_PENTA_1_1",
                    "recall_ever_BCG_1_1") %in% names(out)))
  expect_equal(out$recall_status_BCG_1_1, fx$raw$VR01_1_1)
  expect_equal(out$recall_count_PENTA_1_1, fx$raw$VR09_1_1)
  # Nothing maps to PENTA1 per dose, or to BCG as a series.
  expect_true(all(is.na(out$recall_status_PENTA1_1_1)))
  expect_true(all(is.na(out$recall_ever_BCG_1_1)))
  rep <- attr(out, "recall_map_report")
  expect_setequal(rep$covered, c("BCG", "PENTA1", "PENTA2", "PENTA3"))
  expect_length(rep$uncovered, 0L)
})

test_that("apply_recall_map() rejects names not in the schedule and wrong kinds", {
  fx <- series_fixture()
  expect_error(apply_recall_map(fx$raw, vcs_recall_map(NOPE = "x"), fx$sched),
               class = "vaxsurvR_recall_error")
  # A vaccine mapped as a series, and an antigen mapped per dose, are both wrong.
  expect_error(apply_recall_map(fx$raw, vcs_recall_map(PENTA1 = list(ever = "x")), fx$sched),
               class = "vaxsurvR_recall_error")
  expect_error(apply_recall_map(fx$raw, vcs_recall_map(PENTA = "x"), fx$sched),
               class = "vaxsurvR_recall_error")
  expect_warning(apply_recall_map(fx$raw, vcs_recall_map(BCG = "MISSING_{c}_{k}"),
                                  fx$sched),
                 class = "vaxsurvR_missing_column")
  expect_error(apply_recall_map(fx$raw, "not a map", fx$sched), class = "vaxsurvR_type_error")
})

test_that("series recall resolves per dose from ever + count", {
  fx <- series_fixture()
  raw <- apply_recall_map(fx$raw, fx$map, fx$sched, n_caregivers = 1, n_children = 1)
  dict <- do.call(vcs_dictionary, c(
    list(interview_id = "KEY", household_id = "KEY", psu = "psu",
         child_id = "child_{c}_{k}"),
    recall_dictionary_entries()
  ))
  d <- map_vcs_variables(raw, dict, fx$sched)
  vx <- d$vaccinations
  get <- function(cid, v) vx$recall_reported[vx$child_id == cid & vx$vaccine == v]

  # c1: ever yes, count 3 -> all three doses.
  expect_equal(c(get("c1", "PENTA1"), get("c1", "PENTA2"), get("c1", "PENTA3")),
               c(TRUE, TRUE, TRUE))
  # c2: ever yes, count 1 -> first only.
  expect_equal(c(get("c2", "PENTA1"), get("c2", "PENTA2"), get("c2", "PENTA3")),
               c(TRUE, FALSE, FALSE))
  # c3: never -> all FALSE, no count needed.
  expect_equal(c(get("c3", "PENTA1"), get("c3", "PENTA3")), c(FALSE, FALSE))
  # c4: ever yes, count "don't know" -> every dose unknown, because the count
  # is the evidence for routine doses once it is asked.
  expect_true(is.na(get("c4", "PENTA1")))
  expect_true(is.na(get("c4", "PENTA2")))
  expect_true(is.na(get("c4", "PENTA3")))
  # BCG per-dose: yes / no / dk / silent.
  expect_equal(c(get("c1", "BCG"), get("c2", "BCG")), c(TRUE, FALSE))
  expect_true(is.na(get("c3", "BCG")))
  expect_true(is.na(get("c4", "BCG")))

  rule <- derivation_rules(d)
  expect_match(rule$rule[rule$variable == "recall_reported"], "recall_count")
})

test_that("per-dose recall overrides the series answer where both exist", {
  raw <- data.frame(
    KEY = "h1", psu = "P1", child_1_1 = "c1",
    recall_status_PENTA2_1_1 = "2",          # explicit no for dose 2
    recall_ever_PENTA_1_1 = "1",
    recall_count_PENTA_1_1 = "3",            # series says 3 doses
    stringsAsFactors = FALSE
  )
  sched <- vcs_schedule_who(c("PENTA1", "PENTA2", "PENTA3"))
  raw$recall_status_PENTA1_1_1 <- NA_character_
  raw$recall_status_PENTA3_1_1 <- NA_character_
  dict <- do.call(vcs_dictionary, c(
    list(interview_id = "KEY", household_id = "KEY", psu = "psu",
         child_id = "child_{c}_{k}"),
    recall_dictionary_entries()
  ))
  d <- map_vcs_variables(raw, dict, sched)
  vx <- d$vaccinations
  expect_equal(vx$recall_reported[vx$vaccine == "PENTA1"], TRUE)
  expect_equal(vx$recall_reported[vx$vaccine == "PENTA2"], FALSE)
  expect_equal(vx$recall_reported[vx$vaccine == "PENTA3"], TRUE)
})

test_that("the {antigen} placeholder resolves per series", {
  dict <- vcs_dictionary(recall_ever = "ever_{antigen}_{c}_{k}")
  s <- vcs_schedule_who(c("PENTA1", "PENTA2", "MCV1"))
  r <- resolve_concept(dict, "recall_ever", s)
  expect_equal(r$column, c("ever_PENTA_1_1", "ever_PENTA_1_1", "ever_MCV_1_1"))
  expect_error(vcs_dictionary(child_id = "x_{antigen}"), class = "vaxsurvR_dictionary_error")
})

test_that("recall without repeats works on long-format data", {
  raw <- data.frame(
    cid = c("c1", "c2"), hid = c("h1", "h2"), clust = "P1",
    VR08 = c("1", "2"), VR09 = c("2", NA), stringsAsFactors = FALSE
  )
  sched <- vcs_schedule_who(c("PENTA1", "PENTA2", "PENTA3"))
  raw <- apply_recall_map(raw, vcs_recall_map(PENTA = list(ever = "VR08", count = "VR09")),
                          sched, repeats = FALSE)
  expect_true("recall_ever_PENTA" %in% names(raw))
  dict <- do.call(vcs_dictionary, c(
    list(child_id = "cid", household_id = "hid", interview_id = "hid", psu = "clust"),
    recall_dictionary_entries(repeats = FALSE)
  ))
  d <- map_vcs_variables(raw, dict, sched)
  vx <- d$vaccinations
  expect_equal(vx$recall_reported[vx$child_id == "c1" & vx$vaccine == "PENTA2"], TRUE)
  expect_equal(vx$recall_reported[vx$child_id == "c1" & vx$vaccine == "PENTA3"], FALSE)
  expect_equal(vx$recall_reported[vx$child_id == "c2" & vx$vaccine == "PENTA1"], FALSE)
})

# ---- three-way estimation --------------------------------------------------

test_that("estimate_coverage_by_evidence() stacks card, recall and either", {
  est <- estimate_coverage_by_evidence(vcs_example, vaccines = c("BCG", "PENTA3"))
  expect_s3_class(est, "vcs_estimate")
  expect_equal(nrow(est), 6L)
  expect_equal(levels(est$evidence), c("Card", "Recall", "Card or recall"))
  expect_equal(est$indicator[1:3], rep("BCG", 3))
  # Under the "all" convention every row has the same denominator.
  expect_equal(length(unique(est$denominator)), 1L)
  expect_equal(unique(est$denominator), nrow(vcs_example$children))
  # "Either" is at least as high as each source alone.
  for (v in c("BCG", "PENTA3")) {
    e <- est[est$indicator == v, ]
    expect_gte(e$estimate[e$evidence == "Card or recall"],
               max(e$estimate[e$evidence != "Card or recall"]))
  }
  expect_equal(attr(est, "vcs_meta")$denominator, "all")
})

test_that("denominator = 'determinable' excludes unknown status per source", {
  est <- estimate_coverage_by_evidence(vcs_example, vaccines = "PENTA1",
                                       denominator = "determinable")
  card <- est[est$evidence == "Card", ]
  # Card-only among card holders has a smaller denominator than all children.
  expect_lt(card$denominator, nrow(vcs_example$children))
  all_ <- estimate_coverage_by_evidence(vcs_example, vaccines = "PENTA1")
  # Documented coverage over all children cannot exceed coverage among holders.
  expect_lte(all_$estimate[all_$evidence == "Card"], card$estimate)
})

test_that("estimate_coverage_by_evidence() works by domain and validates", {
  est <- estimate_coverage_by_evidence(vcs_example, vaccines = "MCV1", by = ~stratum)
  expect_equal(nrow(est), 6L)
  expect_true("stratum" %in% names(est))

  custom <- estimate_coverage_by_evidence(vcs_example, vaccines = "BCG",
                                          evidence = c("card_date", "card"),
                                          labels = c("Dated", "Any card"))
  expect_equal(levels(custom$evidence), c("Dated", "Any card"))

  expect_error(estimate_coverage_by_evidence(vcs_example, evidence = "nope"),
               class = "vaxsurvR_value_error")
  expect_error(estimate_coverage_by_evidence(vcs_example, labels = "one"),
               class = "vaxsurvR_value_error")
  expect_error(estimate_coverage_by_evidence(data.frame()), class = "vaxsurvR_type_error")
})

test_that("a series with an 'ever' question but no count settles dose 1 only", {
  raw <- data.frame(
    KEY = c("h1", "h2"), psu = "P1", child_1_1 = c("c1", "c2"),
    VR08_1_1 = c("1", "2"), stringsAsFactors = FALSE
  )
  sched <- vcs_schedule_who(c("PENTA1", "PENTA2"))
  raw <- apply_recall_map(raw, vcs_recall_map(PENTA = list(ever = "VR08_{c}_{k}")),
                          sched, n_caregivers = 1, n_children = 1)
  dict <- do.call(vcs_dictionary, c(
    list(interview_id = "KEY", household_id = "KEY", psu = "psu",
         child_id = "child_{c}_{k}"),
    recall_dictionary_entries()
  ))
  vx <- map_vcs_variables(raw, dict, sched)$vaccinations
  expect_equal(vx$recall_reported[vx$child_id == "c1" & vx$vaccine == "PENTA1"], TRUE)
  expect_true(is.na(vx$recall_reported[vx$child_id == "c1" & vx$vaccine == "PENTA2"]))
  expect_equal(vx$recall_reported[vx$child_id == "c2" & vx$vaccine == "PENTA2"], FALSE)
})

test_that("a count of zero after 'ever yes' means no routine dose", {
  # e.g. measles received only in a campaign: ever = yes, facility count = 0
  raw <- data.frame(KEY = "h1", psu = "P1", child_1_1 = "c1",
                    VR12_1_1 = "1", VR13_1_1 = "0", stringsAsFactors = FALSE)
  sched <- vcs_schedule_who(c("MCV1", "MCV2"))
  raw <- apply_recall_map(raw, vcs_recall_map(MCV = list(ever = "VR12_{c}_{k}",
                                                          count = "VR13_{c}_{k}")),
                          sched, n_caregivers = 1, n_children = 1)
  dict <- do.call(vcs_dictionary, c(
    list(interview_id = "KEY", household_id = "KEY", psu = "psu",
         child_id = "child_{c}_{k}"),
    recall_dictionary_entries()
  ))
  vx <- map_vcs_variables(raw, dict, sched)$vaccinations
  expect_equal(vx$recall_reported[vx$vaccine == "MCV1"], FALSE)
  expect_equal(vx$recall_reported[vx$vaccine == "MCV2"], FALSE)
})
