example_frame <- function() {
  raw <- vcs_example_frame_raw
  read_psu_frame(primary = raw[raw$psu_type == "Primary", ],
                 backup  = raw[raw$psu_type == "Backup", ])
}

pm_fixture <- function() {
  frame <- example_frame()
  psu_map_data(vcs_example_raw, frame, psu_var = "psu_id",
               zone_var = "health_zone", child_marker = "EC_ID",
               n_caregivers = 2, n_children = 2)
}

# ---- frame reading ---------------------------------------------------------

test_that("read_psu_frame() carries the administrative hierarchy", {
  f <- example_frame()
  expect_true(all(c("psu", "psu_type", "zone", "lon", "lat", "est_pop",
                    "adm0", "adm1", "adm2", "adm3") %in% names(f)))
  expect_equal(nrow(f), 45L)
  expect_setequal(unique(f$psu_type), c("Primary", "Backup"))
  # zone stays the adm3 level the PSU maps already use.
  expect_equal(f$zone, f$adm3)
  expect_equal(length(unique(f$adm1)), 2L)
  expect_equal(length(unique(f$adm2)), 5L)
})

test_that("read_psu_frame() skips admin levels the file does not carry", {
  raw <- vcs_example_frame_raw[, c("project_psu_id", "adm3",
                                   "centroid_longitude", "centroid_latitude")]
  f <- read_psu_frame(raw)
  expect_true("adm3" %in% names(f))
  expect_false("adm1" %in% names(f))
  expect_equal(nrow(f), 45L)

  # An explicit subset of levels is honoured.
  f2 <- read_psu_frame(vcs_example_frame_raw, adm_cols = c(province = "adm1"))
  expect_true("province" %in% names(f2))
  expect_false("adm2" %in% names(f2))
})

test_that("read_psu_frame() still errors on a frame without the core columns", {
  expect_error(read_psu_frame(data.frame(a = 1)),
               class = "vaxsurvR_missing_variable")
})

# ---- aggregation -----------------------------------------------------------

test_that("admin_map_data() aggregates against the frame, keeping empty units", {
  pm <- pm_fixture()
  z <- admin_map_data(pm, level = "zone")
  expect_s3_class(z, "vcs_admin_map")
  expect_true(all(c("level", "name", "psus_frame", "psus_opened",
                    "pct_psus_opened", "psus_with_children",
                    "psus_zero_children", "visits", "children") %in% names(z)))
  expect_true(all(z$psus_opened <= z$psus_frame))
  expect_true(all(z$pct_psus_opened >= 0 & z$pct_psus_opened <= 1))
  # Units that were never opened survive aggregation.
  # Zones in the frame where nothing was collected survive aggregation.
  expect_true(any(z$psus_opened == 0L))
  expect_true(all(z$children[z$psus_opened == 0L] == 0L))
  expect_equal(nrow(z), length(unique(example_frame()$zone)))
  # Children reconcile with the survey.
  expect_equal(sum(z$children), nrow(vcs_example$children))
})

test_that("admin_map_data() works at every level of the hierarchy", {
  pm <- pm_fixture()
  z <- admin_map_data(pm, "zone")
  p <- admin_map_data(pm, "adm1")
  d <- admin_map_data(pm, "adm2")
  o <- admin_map_data(pm, "overall")

  expect_equal(nrow(o), 1L)
  expect_lt(nrow(p), nrow(d))
  expect_lt(nrow(d), nrow(z))
  # The frame total is the same however it is sliced.
  expect_equal(sum(p$psus_frame), sum(z$psus_frame))
  expect_equal(sum(o$children), sum(z$children))
  expect_equal(o$name, "Entire study area")
})

test_that("admin_map_data() counts replacements only when asked", {
  pm <- pm_fixture()
  prim <- admin_map_data(pm, "zone")
  both <- admin_map_data(pm, "zone", primary_only = FALSE)
  expect_lte(sum(prim$psus_frame), sum(both$psus_frame))
  expect_equal(sum(both$psus_frame), nrow(pm))
})

test_that("admin_map_data() carries the coarser levels for facetting", {
  pm <- pm_fixture()
  z <- admin_map_data(pm, "zone")
  expect_true(all(c("adm0", "adm1", "adm2", "adm3") %in% names(z)))
  expect_equal(length(unique(z$adm1)), 2L)
})

test_that("admin_map_data() joins an indicator table", {
  pm <- pm_fixture()
  d <- derive_zero_dose(vcs_example)
  est <- estimate_zero_dose(vcs_design(d), by = ~health_zone)

  z <- admin_map_data(pm, "zone", values = est, values_by = "health_zone")
  expect_true("estimate" %in% names(z))
  expect_true(any(!is.na(z$estimate)))
  expect_equal(nrow(z), nrow(admin_map_data(pm, "zone")))

  auto <- admin_map_data(pm, "zone", values = est)
  expect_equal(attr(auto, "values_by"), "health_zone")
  expect_error(admin_map_data(pm, "zone", values = data.frame(x = 1)),
               class = "vaxsurvR_missing_variable")
})

test_that("admin_map_data() errors clearly on a missing level", {
  pm <- pm_fixture()
  expect_error(admin_map_data(pm, "adm9"), class = "vaxsurvR_missing_variable")
  expect_error(admin_map_data(data.frame(a = 1)), class = "vaxsurvR_missing_variable")

  pm$adm1 <- NA_character_
  expect_error(admin_map_data(pm, "adm1"), class = "vaxsurvR_missing_variable")
})

# ---- boundaries ------------------------------------------------------------

square_boundaries <- function(names_, lon, lat, r = 0.05) {
  skip_if_not_installed("sf")
  g <- sf::st_sfc(lapply(seq_along(names_), function(i) {
    sf::st_polygon(list(cbind(
      c(lon[i] - r, lon[i] + r, lon[i] + r, lon[i] - r, lon[i] - r),
      c(lat[i] - r, lat[i] - r, lat[i] + r, lat[i] + r, lat[i] - r))))
  }), crs = 4326)
  sf::st_sf(ZONE_NAME = names_, geometry = g)
}

zone_boundaries <- function() {
  f <- example_frame()
  agg <- stats::aggregate(cbind(lon, lat) ~ zone, f, mean)
  square_boundaries(agg$zone, agg$lon, agg$lat)
}

test_that("read_admin_boundaries() standardises the name column", {
  b <- zone_boundaries()
  out <- read_admin_boundaries(b, name_col = "ZONE_NAME")
  expect_s3_class(out, "sf")
  expect_equal(names(out)[1], "name")
  expect_setequal(out$name, b$ZONE_NAME)

  # The column can be guessed from the names we expect to match.
  guessed <- read_admin_boundaries(b, names_wanted = b$ZONE_NAME)
  expect_setequal(guessed$name, b$ZONE_NAME)
  expect_error(read_admin_boundaries(b), class = "vaxsurvR_missing_variable")
  expect_error(read_admin_boundaries("no_such_file.geojson"),
               class = "vaxsurvR_io_error")
})

test_that("dissolve_frame_boundaries() derives polygons and warns that it did", {
  skip_if_not_installed("sf")
  b <- zone_boundaries()
  b$adm3 <- b$ZONE_NAME
  expect_warning(out <- dissolve_frame_boundaries(b, level = "adm3"),
                 class = "vaxsurvR_derived_boundaries")
  expect_s3_class(out, "sf")
  expect_setequal(out$name, b$adm3)
  expect_equal(attr(out, "derived"), "union")
  expect_error(dissolve_frame_boundaries(b, level = "nope"),
               class = "vaxsurvR_missing_variable")

  # Hulls give one contiguous shape per unit, which is the readable fallback.
  # Two separated patches in one unit: the union keeps the gap, the hull spans it.
  split_zone <- square_boundaries(c("Z", "Z"), c(15.0, 15.4), c(-5.0, -5.4))
  split_zone$adm3 <- split_zone$ZONE_NAME
  expect_warning(u <- dissolve_frame_boundaries(split_zone, "adm3", method = "union"),
                 class = "vaxsurvR_derived_boundaries")
  expect_warning(h <- dissolve_frame_boundaries(split_zone, "adm3", method = "hull"),
                 class = "vaxsurvR_derived_boundaries")
  expect_equal(attr(h, "derived"), "hull")
  expect_equal(nrow(h), 1L)
  expect_gt(as.numeric(sf::st_area(h)), as.numeric(sf::st_area(u)))
})

# ---- maps ------------------------------------------------------------------

test_that("plot_admin_map() draws a choropleth and keeps units with no data", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("sf")
  pm <- pm_fixture()
  z <- admin_map_data(pm, "zone")
  b <- read_admin_boundaries(zone_boundaries(), name_col = "ZONE_NAME")

  g <- plot_admin_map(z, b, metric = "pct_psus_opened")
  expect_s3_class(g, "ggplot")
  expect_silent(invisible(ggplot2::ggplot_build(g)))

  # A boundary unit absent from the data must still be drawn, as "no data".
  z_short <- z[-1, , drop = FALSE]
  g2 <- plot_admin_map(z_short, b, metric = "children")
  expect_silent(invisible(ggplot2::ggplot_build(g2)))
  drawn <- attr(g2, "vcs_map_data")
  expect_equal(nrow(drawn), nrow(b))
  expect_equal(sum(!drawn$has_data), 1L)
})

test_that("plot_admin_map() overlays sampling units for hotspot maps", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("sf")
  pm <- pm_fixture()
  z <- admin_map_data(pm, "zone")
  b <- read_admin_boundaries(zone_boundaries(), name_col = "ZONE_NAME")

  g <- plot_admin_map(z, b, metric = "children", points = pm)
  expect_s3_class(g, "ggplot")
  expect_silent(invisible(ggplot2::ggplot_build(g)))

  expect_error(plot_admin_map(z, b, points = data.frame(a = 1)),
               class = "vaxsurvR_missing_variable")
})

test_that("plot_admin_map() validates its arguments", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("sf")
  pm <- pm_fixture()
  z <- admin_map_data(pm, "zone")
  b <- read_admin_boundaries(zone_boundaries(), name_col = "ZONE_NAME")

  expect_error(plot_admin_map(z, b, metric = "nope"),
               class = "vaxsurvR_missing_variable")
  expect_error(plot_admin_map(z, data.frame(a = 1)),
               class = "vaxsurvR_missing_variable")
  expect_error(plot_admin_map(z, b[, 0]), class = "vaxsurvR_missing_variable")
})

test_that("plot_admin_map() formats percentages and counts differently", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("sf")
  pm <- pm_fixture()
  z <- admin_map_data(pm, "zone")
  b <- read_admin_boundaries(zone_boundaries(), name_col = "ZONE_NAME")

  pct <- plot_admin_map(z, b, metric = "pct_psus_opened")
  cnt <- plot_admin_map(z, b, metric = "children")
  pct_d <- attr(pct, "vcs_map_data")
  cnt_d <- attr(cnt, "vcs_map_data")
  expect_true(all(pct_d$value >= 0 & pct_d$value <= 1, na.rm = TRUE))
  # A count metric must not be rescaled as a proportion.
  expect_gt(max(cnt_d$value, na.rm = TRUE), 1)
  expect_match(pct_d$label_text[pct_d$has_data][1], "%")
  # A zone in the frame where nothing was collected is a real zero, labelled
  # with its count, not dropped and not shown as missing.
  zero <- cnt_d[cnt_d$has_data & cnt_d$value == 0, , drop = FALSE]
  expect_gt(nrow(zero), 0L)
  expect_match(zero$label_text[1], "0 (n=0)", fixed = TRUE)
})

test_that("plot_admin_progress_bars() needs no spatial data", {
  skip_if_not_installed("ggplot2")
  pm <- pm_fixture()
  z <- admin_map_data(pm, "zone")
  g <- plot_admin_progress_bars(z, target = 0.8)
  expect_s3_class(g, "ggplot")
  expect_silent(invisible(ggplot2::ggplot_build(g)))
  expect_s3_class(plot_admin_progress_bars(z, metric = "children",
                                           fill_by = "adm1"), "ggplot")
  expect_error(plot_admin_progress_bars(z, metric = "nope"),
               class = "vaxsurvR_missing_variable")
})

test_that("vcs_font() falls back when the device cannot render the family", {
  skip_if_not_installed("systemfonts")
  # On a postscript device -- the one `R CMD check` renders examples on -- a
  # family that is installed on the system is still unusable unless it is in
  # the device's own font database.
  f <- withr::local_tempfile(fileext = ".ps")
  grDevices::postscript(f)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_false(vcs_font("NoSuchFontFamily") == "NoSuchFontFamily")
  # "sans" is not in the postscript font database either, so the fallback has
  # to be the device default rather than another generic name.
  expect_identical(vcs_font("NoSuchFontFamily"), "")
  # Whatever comes back must render without warning or error.
  graphics::plot.new()
  expect_silent(graphics::text(0.5, 0.5, "x", family = vcs_font()))
  expect_silent(graphics::text(0.5, 0.4, "y", family = vcs_font("Arial")))
})
