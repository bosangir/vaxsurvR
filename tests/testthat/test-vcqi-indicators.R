# Tests for the report-oriented indicators added in 0.2.0: VCTC timing,
# stratified tables, intervals, curves, MOSV, BeSD, date quality, weights.

d0 <- derive_vaccination_status(vcs_example, evidence = "card_or_recall")
des0 <- suppressWarnings(vcs_design(d0))

test_that("estimate_coverage adds neff, icc and supports Wilson intervals", {
  est <- estimate_coverage(des0, vaccines = "PENTA1")
  expect_true(all(c("neff", "icc", "ci_method") %in% names(est)))
  expect_equal(est$ci_method, "normal")
  w <- estimate_coverage(des0, vaccines = "PENTA1", ci_method = "wilson")
  expect_equal(w$ci_method, "wilson")
  expect_true(w$conf_low >= 0 && w$conf_high <= 1 && w$conf_low < w$estimate && w$estimate < w$conf_high)
  expect_true(is.na(est$icc) || (est$icc >= 0 && est$icc <= 1))
  expect_equal(est$neff, est$denominator / est$deff, tolerance = 1e-8)
})

test_that("ICC estimator behaves on constructed clusters", {
  y <- c(rep(1, 10), rep(0, 10))
  cl <- rep(c("a", "b"), each = 10)
  expect_equal(vaxsurvR:::icc_anova(y, cl), 1)
  y2 <- rep(c(1, 0), 10)
  expect_equal(vaxsurvR:::icc_anova(y2, cl), 0)
})

test_that("timing categories follow the rules", {
  d <- derive_dose_timing(vcs_example)
  tc <- d$vaccinations$timing_category
  expect_s3_class(tc, "factor")
  expect_true(all(levels(tc) == vcs_timing_levels()))
  vx <- d$vaccinations
  dated <- !is.na(vx$age_at_vaccination_days) & !is.na(tc) & tc != "Timing unknown"
  sched <- vcs_example$schedule
  m <- sched$minimum_age_days[match(vx$vaccine, sched$vaccine)]
  early <- dated & vx$age_at_vaccination_days < m
  expect_true(all(tc[early] == "Too early"))
  # no timing category for doses not received
  expect_true(all(is.na(tc[!is.na(vx$vaccinated) & !vx$vaccinated])))
})

test_that("estimate_vctc returns consistent coverage and timing shares", {
  v <- estimate_vctc(vcs_example, vaccines = c("BCG", "PENTA1", "PENTA3", "MCV1"))
  expect_s3_class(v, "vcs_vctc")
  expect_equal(nrow(v$coverage), 4)
  # timing shares (excluding not vaccinated) sum to coverage
  for (vac in v$coverage$vaccine) {
    sh <- sum(v$timing$share[v$timing$vaccine == vac])
    expect_equal(sh, v$coverage$estimate[v$coverage$vaccine == vac], tolerance = 1e-6)
  }
  expect_true(v$card_seen[["estimate"]] >= 0 && v$card_seen[["estimate"]] <= 1)
  expect_s3_class(as.data.frame(v), "data.frame")
  p <- plot_vctc(v)
  expect_s3_class(p, "ggplot")
  # domain subset
  s <- estimate_vctc(vcs_example, vaccines = "PENTA1", subset = stratum == "urban")
  expect_true(s$n_children < v$n_children)
})

test_that("estimate_stratified stacks overall, headers and levels", {
  st <- vcs_strata(stratum = list(var = "stratum", label = "Stratum"),
                   sex = list(var = "sex", label = "Sex"))
  tab <- estimate_stratified(des0, estimate_coverage, st, vaccines = "PENTA1")
  expect_equal(tab$row_type[1], "overall")
  expect_equal(sum(tab$row_type == "header"), 2)
  expect_true(all(c("stratifier", "stratum", "row_type", "indent") %in% names(tab)))
  expect_true(all(is.na(tab$estimate[tab$row_type == "header"])))
  # a missing stratifier is skipped with a warning
  st2 <- vcs_strata(nope = list(var = "no_such_column"))
  expect_warning(estimate_stratified(des0, estimate_coverage, st2, vaccines = "PENTA1"),
                 class = "vaxsurvR_missing_column")
})

test_that("bar tables render as ggplot and flextable", {
  st <- vcs_strata(stratum = list(var = "stratum", label = "Stratum"))
  tab <- estimate_stratified(des0, function(x, by = NULL) estimate_dropout(x, "PENTA1", "PENTA3", by = by), st)
  m <- list(bar_measure("estimate", "denominator", label = "Dropout (%)", n_label = "N"))
  p <- plot_bar_table(tab, m)
  expect_s3_class(p, "ggplot")
  expect_length(bar_table_size(p), 2)
  skip_if_not_installed("flextable")
  ft <- ft_bar_table(tab, m)
  expect_s3_class(ft, "flextable")
  # suppression
  tab$denominator[tab$row_type == "overall"] <- 3
  txt <- vaxsurvR:::bar_cell_text(tab$estimate, tab$denominator, m[[1]], 25)
  expect_equal(txt[1], "(*)")
})

test_that("intervals, curves and organ pipes work on the example data", {
  iv <- derive_dose_intervals(vcs_example)
  expect_true(all(c("child_id", "antigen", "from", "to", "interval_days") %in% names(iv)))
  t <- table_dose_intervals(vcs_example, antigen = "PENTA")
  expect_equal(t$n_intervals, t$n_short + sum(t$n_intervals - t$n_short))
  expect_true(t$prop_short >= 0 && t$prop_long <= 1)
  tb <- table_dose_intervals(vcs_example, antigen = "PENTA", by = ~stratum)
  expect_true("stratum" %in% names(tb))
  cc <- compute_ccc(vcs_example, vaccines = c("BCG", "PENTA1"))
  expect_s3_class(cc, "vcs_ccc")
  expect_true(all(diff(cc$cum_pct[cc$vaccine == "BCG"]) >= 0))
  expect_s3_class(plot_ccc(cc), "ggplot")
  ci <- compute_cic(vcs_example)
  expect_s3_class(ci, "vcs_cic")
  if (nrow(ci)) expect_s3_class(plot_cic(ci, ci$to[1]), "ggplot")
  expect_s3_class(plot_organ_pipe(d0, "PENTA1"), "ggplot")
})

test_that("MOSV logic identifies eligibility, correction and summaries", {
  m <- derive_mosv(vcs_example)
  expect_s3_class(m, "vcs_mosv")
  vd <- m$visit_dose
  # a dose received at the visit is never a MOSV
  expect_false(any(vd$mosv & vd$received_here))
  # a dose already received before the visit is not eligible
  expect_false(any(vd$eligible & vd$received_before))
  # a MOSV requires eligibility
  expect_true(all(vd$eligible[vd$mosv]))
  cd <- m$child_dose
  expect_true(all(cd$status %in% c("first_opportunity", "corrected", "uncorrected")))
  expect_true(all(cd$n_mosv[cd$status == "first_opportunity"] == 0))
  expect_true(all(is.na(cd$days_to_correction) | cd$days_to_correction > 0))
  expect_true(all(m$child$mosv_summary %in% c("NM", "AC", "SC", "NC")))
  mv <- estimate_mosv_visits(m)
  expect_true(all(c("mosv_any", "n_visits", "mosv_per_visit") %in% names(mv)))
  expect_true(mv$mosv_any >= 0 && mv$mosv_any <= 1)
  mc <- estimate_mosv_children(m)
  s <- mc$summary
  expect_equal(s$NM + s$AC + s$SC + s$NC, 1, tolerance = 1e-8)
  bd <- mc$by_dose
  ok <- !is.na(bd$first_opportunity)
  expect_equal(bd$first_opportunity[ok] + bd$corrected[ok] + bd$uncorrected[ok], rep(1, sum(ok)), tolerance = 1e-8)
  expect_s3_class(plot_mosv_children(m, by = "stratum"), "ggplot")
  expect_s3_class(plot_mosv_correction(m), "ggplot")
})

test_that("MOSV on a constructed history", {
  raw <- data.frame(
    KEY = "h1", psu = "P1", stratum = "s",
    dob_1_1 = "2024-01-01", card_1_1 = "1",
    CVH01_1_1 = "1", CVH01_date_1_1 = "2024-01-02",   # BCG at birth
    CVH02_1_1 = "1", CVH02_date_1_1 = "2024-03-01",   # PENTA1 at 60 days
    CVH03_1_1 = "1", CVH03_date_1_1 = "2024-05-01",   # PENTA2 at 121 days
    CVH04_1_1 = "3", CVH04_date_1_1 = NA,             # PENTA3 never
    stringsAsFactors = FALSE
  )
  dict <- vcs_dictionary(interview_id = "KEY", household_id = "KEY", psu = "psu",
                         child_dob = "dob_{c}_{k}", card_seen = "card_{c}_{k}",
                         card_status = "CVH{vv}_{c}_{k}", card_date = "CVH{vv}_date_{c}_{k}")
  sched <- vcs_schedule(c("BCG", "PENTA1", "PENTA2", "PENTA3"),
                        minimum_age_days = c(0, 42, 70, 98), maximum_age_days = c(28, 76, 104, 132),
                        previous_dose = c(NA, NA, "PENTA1", "PENTA2"),
                        minimum_interval_days = c(NA, NA, 28, 28))
  x <- suppressWarnings(map_vcs_variables(raw, dict, sched))
  m <- derive_mosv(x)
  expect_equal(nrow(m$visits), 3)
  cd <- m$child_dose
  # PENTA1 was eligible at the 60-day visit and given there: first opportunity
  expect_equal(cd$status[cd$vaccine == "PENTA1"], "first_opportunity")
  # PENTA2: at the 60-day visit not eligible (PENTA1 given same day, not earlier);
  # at 121 days eligible and received -> first opportunity
  expect_equal(cd$status[cd$vaccine == "PENTA2"], "first_opportunity")
  # PENTA3: never eligible at a documented visit (PENTA2 given at the last visit)
  expect_false("PENTA3" %in% cd$vaccine)
  # BCG: given at first visit
  expect_equal(cd$status[cd$vaccine == "BCG"], "first_opportunity")
  expect_equal(m$child$mosv_summary, "NM")
})

test_that("BeSD derivation counts concerning codes and ignores don't-know", {
  x <- vcs_example
  x$children$Q1 <- rep(c("1", "2", "3", "98", NA), length.out = nrow(x$children))
  map <- vcs_besd_map(Q1 = list(var = "Q1", concerning = c("1", "2"), group = "intention"))
  x <- derive_besd(x, map)
  v <- x$children$besd_Q1
  expect_equal(v[1:5], c(1L, 1L, 0L, NA_integer_, NA_integer_))
  des <- suppressWarnings(vcs_design(x))
  est <- estimate_besd(des, map)
  expect_equal(nrow(est), 1)
  expect_equal(est$estimate, 2 / 3, tolerance = 0.05)
  bz <- besd_by_stratum(des, map, by = "stratum")
  expect_equal(dim(bz$table), c(1, 2))
  expect_s3_class(plot_besd_by_stratum(bz), "ggplot")
})

test_that("multi-select and yes/no tabulations", {
  x <- vcs_example
  x$children$MS <- rep(c("1 2", "2", "", NA, "1"), length.out = nrow(x$children))
  x$children$YN <- rep(c("1", "2", "98"), length.out = nrow(x$children))
  des <- suppressWarnings(vcs_design(x))
  t <- tabulate_multiselect(des, "MS", c("1" = "A", "2" = "B"))
  expect_equal(nrow(t), 2)
  expect_true(all(t$n == t$n[1]))
  yn <- estimate_yes_no(des, "YN")
  expect_equal(yn$estimate, 0.5, tolerance = 0.05)
  rs <- reasons_by_stratum(des, "MS", c("1" = "A", "2" = "B"), c("1" = "Access", "2" = "Beliefs"), by = "stratum")
  expect_equal(colnames(rs$table)[1], "All combined")
  expect_s3_class(plot_reasons_table(rs), "ggplot")
})

test_that("date quality summary and erasure", {
  s <- date_quality_summary(vcs_example)
  expect_equal(nrow(s), 6)
  expect_true(s$n_dates[6] >= max(s$n_dates[1:5]))
  e <- erase_illogical_dates(vcs_example)
  after <- date_quality_summary(e)
  expect_true(after$n_dates[6] <= s$n_dates[6])
  expect_true("n_dates_after" %in% names(e$meta$date_quality))
  expect_true(nrow(e$meta$date_erasures) >= s$n_dates[6])
})

test_that("sample size parameters and weights", {
  sp <- sample_size_parameters(des0, "PENTA1", assumed = list(coverage = 0.6, per_cluster = 7))
  expect_equal(nrow(sp), 6)
  expect_equal(sp$assumed[1], 0.6)
  frame <- data.frame(psu = c("A", "B"), stratum = "S", psu_population = c(500, 1000),
                      stratum_population = 6000, n_selected = 2,
                      segment_population_canvassed = c(100, 250),
                      segment_population_total = c(500, 1000))
  hh <- data.frame(psu = c("A", "A", "A", "B", "B"),
                   outcome = c("interviewed", "nobody_home", "ineligible", "interviewed", "interviewed"),
                   n_children = c(1, NA, 0, 2, 1))
  kids <- data.frame(psu = c("A", "B", "B", "B"), child_id = 1:4)
  w <- compute_survey_weights(frame, hh, kids)
  expect_equal(sum(w$weight), 4, tolerance = 1e-8)
  pt <- attr(w, "psu_table")
  expect_equal(pt$p1, c(2 * 500 / 6000, 2 * 1000 / 6000))
  expect_equal(pt$p2, c(0.2, 0.25))
  # PSU A: 1 child found in 2 responding households, 1 nobody home -> 1.5 est children / 1 interviewed
  expect_equal(pt$nr_factor[pt$psu == "A"], 1.5)
})

test_that("theme helpers", {
  expect_type(vcs_palette()$primary, "character")
  expect_equal(vcs_palette(primary = "red")$primary, "red")
  expect_error(vcs_palette("red"), class = "vaxsurvR_value_error")
  expect_type(vcs_font(), "character")
  expect_s3_class(theme_vcs(), "theme")
  skip_if_not_installed("flextable")
  expect_s3_class(ft_vcs(data.frame(a = 1:2, b = c("x", "y"))), "flextable")
})

test_that("KC v9 configuration objects are consistent", {
  s <- kc9_schedule()
  expect_equal(nrow(s), 26)
  expect_equal(s$vaccine[1:4], c("BCG", "OPV0", "OPV1", "PENTA1"))
  expect_equal(length(kc9_epi_doses()), 23)
  expect_false(any(grepl("VITA|DEWORM", kc9_epi_doses())))
  rm <- kc9_recall_map()
  expect_s3_class(rm, "vcs_recall_map")
  d <- kc9_dictionary()
  expect_s3_class(d, "vcs_dictionary")
  expect_equal(d$map$card_status, "CVH{vv}_{c}_{k}")
  expect_s3_class(kc9_strata(), "vcs_strata")
  expect_true(all(c("residence", "cvh35", "cvh35_category") %in% names(kc9_labels())))
  expect_s3_class(kc9_besd_map(), "vcs_besd_map")
})

test_that("kc9_link_roster joins children to caregivers", {
  raw <- data.frame(
    KEY = "h1",
    HL08_caregiver_id_1 = "2", HL05_child_dob_1 = "2025-01-01", HL03_child_gender_1 = "1", HL04_child_months_1 = "14",
    HL08_caregiver_id_2 = "2", HL05_child_dob_2 = "2025-03-01", HL03_child_gender_2 = "2", HL04_child_months_2 = "12",
    HL07_school_2 = "1", HL06_marital_2 = "1", HL03_adult_gender_2 = "2",
    EC01_1_1 = "10:00", EC04_1_1 = "2", EC01_1_2 = "10:30", EC04_1_2 = "2",
    stringsAsFactors = FALSE
  )
  out <- kc9_link_roster(raw, n_caregivers = 1, n_children = 2, n_roster = 2)
  expect_equal(out$child_dob_1_1, "2025-01-01")
  expect_equal(out$child_dob_1_2, "2025-03-01")
  expect_equal(out$child_sex_1_2, "2")
  expect_equal(out$caregiver_education_1_1, "1")
  expect_equal(unname(attr(out, "roster_match")), c(2, 2))
})

test_that("report template is installed", {
  tp <- coverage_report_template()
  expect_true(all(nzchar(tp)))
  expect_true(file.exists(tp[["rmd"]]))
  expect_true(file.exists(tp[["reference_docx"]]))
})
