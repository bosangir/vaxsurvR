# ---------------------------------------------------------------------------
# Administrative-level mapping of data collection.
#
# [plot_psu_map()] answers "which sampling units did the teams reach?". These
# functions answer "how much of each province / district / health zone has been
# covered, and where are the gaps?" -- the same question one level up, and the
# one a supervisor or a donor asks.
#
# Two rules shape the module:
#
#   * The frame is the denominator. An administrative unit where nothing has
#     been collected is in the output with zero, and is drawn as "no data yet"
#     rather than omitted, because an omitted zone looks like a zone that does
#     not exist.
#   * Nothing is country-specific. Units are whatever levels the frame carries,
#     named adm0..adm3 by convention; `zone` is accepted as a synonym for the
#     level the PSU maps already use.
# ---------------------------------------------------------------------------

#' Aggregate sampling-unit progress to an administrative level
#'
#' Collapses the output of [psu_map_data()] to one row per administrative unit,
#' keeping units in which no data has yet been collected.
#'
#' @param x Output of [psu_map_data()].
#' @param level Column of `x` holding the administrative unit: `"zone"` (the
#'   default, the level the PSU maps use), or any level carried through by
#'   `adm_cols` in [read_psu_frame()] such as `"adm1"` or `"adm2"`. Use
#'   `"overall"` for a single national row.
#' @param values Optional data frame of indicator values to join, such as an
#'   estimate table from [estimate_coverage()] with `by = ~zone`.
#' @param values_by Column of `values` holding the unit name. Guessed from the
#'   unit names when `NULL`.
#' @param primary_only Count only primary sampling units in the denominator.
#'   Backup units are replacements, so including them inflates the frame.
#' @return A tibble of class `vcs_admin_map` with one row per unit:
#'   `level`, `name`, `psus_frame`, `psus_opened`, `pct_psus_opened`,
#'   `psus_with_children`, `psus_zero_children`, `visits`, `children`,
#'   `children_per_opened_psu`, plus any joined values.
#' @export
#' @family PSU maps
#' @seealso [plot_admin_map()], [psu_map_data()]
#' @examples
#' raw <- vcs_example_frame_raw
#' frame <- read_psu_frame(primary = raw[raw$psu_type == "Primary", ],
#'                         backup  = raw[raw$psu_type == "Backup", ])
#' pm <- psu_map_data(vcs_example_raw, frame, psu_var = "psu_id",
#'                    zone_var = "health_zone", child_marker = "EC_ID",
#'                    n_caregivers = 2, n_children = 2)
#' admin_map_data(pm, level = "zone")
#' admin_map_data(pm, level = "adm1")
admin_map_data <- function(x, level = "zone", values = NULL, values_by = NULL,
                           primary_only = TRUE) {
  assert_string(level)
  assert_flag(primary_only)
  d <- as.data.frame(x)
  need <- c("psu", "psu_type", "n_visits", "n_children", "opened")
  missing <- setdiff(need, names(d))
  if (length(missing)) {
    vcs_abort(
      sprintf("`x` must come from `psu_map_data()`; missing column(s): %s.",
              paste(missing, collapse = ", ")),
      class = "vaxsurvR_missing_variable"
    )
  }
  if (primary_only) {
    d <- d[d$psu_type == "Primary", , drop = FALSE]
  }
  if (identical(level, "overall")) {
    d$overall <- "Entire study area"
  }
  if (!level %in% names(d)) {
    vcs_abort(
      sprintf(paste0("The frame carries no \"%s\" column. Keep it with ",
                     "`read_psu_frame(adm_cols = )`, or use one of: %s."),
              level, paste(intersect(c("zone", "adm0", "adm1", "adm2", "adm3"),
                                     names(d)), collapse = ", ")),
      class = "vaxsurvR_missing_variable"
    )
  }
  if (all(is.na(d[[level]]))) {
    vcs_abort(sprintf("Column \"%s\" is entirely missing in the frame.", level),
              class = "vaxsurvR_missing_variable")
  }

  idx <- split(seq_len(nrow(d)), d[[level]])
  g <- function(f, type = integer(1)) unname(vapply(idx, f, type))
  out <- tibble::tibble(
    level = level,
    name = names(idx),
    psus_frame = g(function(i) length(i)),
    psus_opened = g(function(i) sum(d$opened[i])),
    psus_with_children = g(function(i) sum(d$opened[i] & d$n_children[i] > 0L)),
    psus_zero_children = g(function(i) sum(d$opened[i] & d$n_children[i] == 0L)),
    visits = g(function(i) sum(d$n_visits[i])),
    children = g(function(i) sum(d$n_children[i]))
  )
  out$pct_psus_opened <- out$psus_opened / out$psus_frame
  out$children_per_opened_psu <- ifelse(out$psus_opened > 0,
                                        out$children / out$psus_opened, NA_real_)
  # Carry the coarser levels so maps can be facetted or ordered by them.
  ladder <- c("adm0", "adm1", "adm2", "adm3", "zone")
  pos <- match(level, ladder)
  parents <- if (is.na(pos)) character(0) else ladder[seq_len(max(pos - 1L, 0L))]
  for (nm in intersect(parents, names(d))) {
    out[[nm]] <- g(function(i) {
      v <- stats::na.omit(unique(d[[nm]][i]))
      if (length(v)) as.character(v[1]) else NA_character_
    }, character(1))
  }

  if (!is.null(values)) {
    v <- as.data.frame(values)
    vb <- values_by %||% guess_name_column(v, out$name)
    if (is.null(vb)) {
      vcs_abort(
        "No column of `values` matches the unit names; pass `values_by`.",
        class = "vaxsurvR_missing_variable"
      )
    }
    extra <- setdiff(names(v), c(vb, names(out)))
    m <- match(out$name, as.character(v[[vb]]))
    for (nm in extra) out[[nm]] <- v[[nm]][m]
    attr(out, "values_by") <- vb
  }
  class(out) <- c("vcs_admin_map", class(out))
  out
}

#' Find the column of `values` that holds unit names
#'
#' Several columns can match -- an estimate table carries both `domain` and the
#' domain variable itself -- so a generically named column is only used when
#' nothing better matches.
#' @noRd
guess_name_column <- function(values, wanted) {
  generic <- c("domain", "name", "group", "level", "indicator", "evidence")
  hits <- character(0)
  for (nm in names(values)) {
    u <- unique(stats::na.omit(as.character(values[[nm]])))
    if (length(u) && length(intersect(u, wanted)) > 0.5 * length(u)) {
      hits <- c(hits, nm)
    }
  }
  if (!length(hits)) {
    return(NULL)
  }
  specific <- setdiff(hits, generic)
  if (length(specific)) specific[1] else hits[1]
}

#' Read administrative boundaries for mapping
#'
#' Reads a boundary layer and standardises the unit name to a `name` column, so
#' that [plot_admin_map()] can match it to [admin_map_data()] without caring
#' what the file calls its fields.
#'
#' @param path Path to a GeoJSON, shapefile or GeoPackage, or an `sf` object.
#' @param name_col Column holding the unit name. Guessed from `names_wanted`
#'   when `NULL`.
#' @param names_wanted Unit names to match against when guessing, usually
#'   `admin_map_data(...)$name`.
#' @param layer Layer name, for multi-layer sources.
#' @return An `sf` object with a `name` column.
#' @export
#' @family PSU maps
#' @examples
#' \dontrun{
#' zones <- read_admin_boundaries("health_zones.geojson",
#'                                names_wanted = am$name)
#' }
read_admin_boundaries <- function(path, name_col = NULL, names_wanted = NULL,
                                  layer = NULL) {
  assert_installed("sf", "read_admin_boundaries()")
  b <- if (inherits(path, "sf")) {
    path
  } else {
    assert_string(path)
    if (!file.exists(path)) {
      vcs_abort(sprintf("Boundary file does not exist: %s", path),
                class = "vaxsurvR_io_error")
    }
    if (is.null(layer)) sf::st_read(path, quiet = TRUE) else sf::st_read(path, layer = layer, quiet = TRUE)
  }
  nm <- name_col
  if (is.null(nm) && !is.null(names_wanted)) {
    nm <- guess_name_column(sf::st_drop_geometry(b), names_wanted)
  }
  if (is.null(nm)) {
    vcs_abort(
      paste0("Could not identify the unit-name column of the boundary file. ",
             "Pass `name_col`, or `names_wanted` to guess from."),
      class = "vaxsurvR_missing_variable"
    )
  }
  b$name <- gsub("_", " ", trimws(as.character(b[[nm]])))
  b[, "name"]
}

#' Derive approximate boundaries by dissolving a spatial frame
#'
#' When no boundary file is available, the sampling units themselves can stand
#' in for administrative units. Neither result is an administrative border, and
#' the difference matters:
#'
#' * `method = "union"` dissolves the sampling units into their **sampled
#'   footprint**. It is truthful about where the frame actually placed units,
#'   and on a gridded-population frame it is a scatter of small patches --
#'   fine for showing extent, useless as a choropleth.
#' * `method = "hull"` takes the convex hull of each unit, giving one
#'   contiguous readable shape per unit. It overstates the area covered and
#'   neighbouring hulls can overlap, but it is legible, and it is what
#'   [plot_psu_map()] already draws behind the sampling units.
#'
#' Prefer a real boundary file through [read_admin_boundaries()] whenever one
#' exists; these are fallbacks, and the function says so.
#'
#' @param frame Path to a polygon frame, or an `sf` object of sampling units.
#'   A point layer works for `method = "hull"`.
#' @param level Column of `frame` holding the administrative unit.
#' @param method `"union"` for the sampled footprint, `"hull"` for one convex
#'   hull per unit.
#' @return An `sf` object with a `name` column and a `derived` attribute
#'   recording the method.
#' @export
#' @family PSU maps
#' @examples
#' \dontrun{
#' bd <- dissolve_frame_boundaries("PSU_segments.geojson", level = "adm3")
#' bd <- dissolve_frame_boundaries("PSU_segments.geojson", method = "hull")
#' }
dissolve_frame_boundaries <- function(frame, level = "adm3",
                                      method = c("union", "hull")) {
  assert_installed("sf", "dissolve_frame_boundaries()")
  assert_string(level)
  method <- match.arg(method)
  g <- if (inherits(frame, "sf")) frame else sf::st_read(frame, quiet = TRUE)
  if (!level %in% names(g)) {
    vcs_abort(sprintf("The frame has no \"%s\" column.", level),
              class = "vaxsurvR_missing_variable")
  }
  vcs_warn(
    paste0("Deriving \"", level, "\" boundaries from the sampling frame by ",
           if (method == "hull") "convex hull" else "union",
           ". These are ", if (method == "hull")
             "the frame's extent in each unit, which overstates its area" else
             "the sampled footprint of each unit",
           " -- not an administrative border."),
    class = "vaxsurvR_derived_boundaries"
  )
  g$.unit <- gsub("_", " ", trimws(as.character(g[[level]])))
  g <- g[!is.na(g$.unit) & nzchar(g$.unit), , drop = FALSE]
  parts <- split(seq_len(nrow(g)), g$.unit)
  geoms <- lapply(parts, function(i) {
    u <- sf::st_union(sf::st_geometry(g)[i])
    if (method == "hull") u <- sf::st_convex_hull(u)
    u[[1]]
  })
  out <- sf::st_sf(name = names(parts),
                   geometry = sf::st_sfc(geoms, crs = sf::st_crs(g)))
  attr(out, "derived") <- method
  out
}

#' Map data collection by administrative unit
#'
#' Fills each administrative unit by a metric and labels it with the value and
#' its denominator: the progress and hotspot map used alongside the
#' sampling-unit map in [plot_psu_map()]. Units with no row in `x` keep their
#' outline and are filled flat, so a zone where nothing has been collected
#' reads as "not yet visited" rather than as a zero.
#'
#' Passing `points` draws the sampling units on top of the choropleth, which is
#' how hotspots are usually shown: the fill gives the zone-level rate, the
#' bubbles give where within the zone the children actually are.
#'
#' @param x Output of [admin_map_data()].
#' @param boundaries An `sf` layer with a `name` column, from
#'   [read_admin_boundaries()] or [dissolve_frame_boundaries()].
#' @param metric Column of `x` to map.
#' @param points Optional sampling units to draw on top: the output of
#'   [psu_map_data()], or any data frame with `lon` and `lat`.
#' @param points_size_by Column of `points` scaling the bubbles.
#' @param label Label each unit with its name, value and denominator.
#' @param label_n Column of `x` used as the denominator in labels.
#' @param percent Format `metric` as a percentage. Guessed from the column name
#'   and range when `NULL`.
#' @param low,high Fill colours for the low and high ends of `metric`.
#' @param missing_fill Fill for units with no data.
#' @param min_n Units whose `label_n` is below this are outlined as
#'   low-precision rather than being hidden. `NULL` disables the check.
#' @param palette A [vcs_palette()].
#' @param base_size Base font size in points.
#' @param title,subtitle,legend_title Labels. `NULL` generates a default.
#' @return A ggplot. The `sf` table actually drawn -- values joined onto the
#'   boundaries, including units with no data -- is attached as the
#'   `"vcs_map_data"` attribute.
#' @export
#' @family PSU maps
#' @seealso [admin_map_data()], [plot_psu_map()]
#' @examples
#' \dontrun{
#' am <- admin_map_data(pm, level = "zone")
#' bd <- read_admin_boundaries("zones.geojson", names_wanted = am$name)
#'
#' # Progress: how much of each zone has been opened.
#' plot_admin_map(am, bd, metric = "pct_psus_opened")
#'
#' # Hotspots: children collected per zone, with the sampling units on top.
#' plot_admin_map(am, bd, metric = "children", points = pm)
#'
#' # Any indicator joined on.
#' zd <- estimate_zero_dose(design, by = ~zone)
#' am2 <- admin_map_data(pm, level = "zone", values = zd, values_by = "zone")
#' plot_admin_map(am2, bd, metric = "estimate", points = pm)
#' }
plot_admin_map <- function(x, boundaries, metric = "pct_psus_opened",
                           points = NULL, points_size_by = "n_children",
                           label = TRUE, label_n = "children", percent = NULL,
                           low = "#F4F9F4", high = "#1F5C3A",
                           missing_fill = "#E8E8E8", min_n = 5,
                           palette = vcs_palette(), base_size = 9,
                           title = NULL, subtitle = NULL, legend_title = NULL) {
  assert_installed("ggplot2", "plot_admin_map()")
  assert_installed("sf", "plot_admin_map()")
  assert_string(metric)
  assert_flag(label)
  d <- as.data.frame(x)
  if (!metric %in% names(d)) {
    vcs_abort(sprintf("`x` has no column \"%s\".", metric),
              class = "vaxsurvR_missing_variable")
  }
  if (!inherits(boundaries, "sf")) {
    vcs_abort("`boundaries` must be an `sf` object; see `read_admin_boundaries()`.",
              class = "vaxsurvR_missing_variable")
  }
  if (!"name" %in% names(boundaries)) {
    vcs_abort("`boundaries` must carry a `name` column.",
              class = "vaxsurvR_missing_variable")
  }
  vals <- suppressWarnings(as.numeric(d[[metric]]))
  percent <- percent %||% (grepl("^pct_|^prop_", metric) ||
                             all(vals >= 0 & vals <= 1, na.rm = TRUE))
  fam <- vcs_font()

  m <- match(boundaries$name, d$name)
  map <- boundaries
  map$value <- vals[m]
  map$label_n <- if (label_n %in% names(d)) suppressWarnings(as.numeric(d[[label_n]]))[m] else NA_real_
  map$has_data <- !is.na(map$value)

  # `intToUtf8()` keeps the source ASCII, which R CMD check requires, without
  # giving up the typographic dash in the rendered label.
  em_dash <- intToUtf8(8212)
  fmt <- function(v) {
    ifelse(is.na(v), em_dash,
           if (percent) sprintf("%.0f%%", 100 * v) else format(round(v, 1), big.mark = ","))
  }
  map$label_text <- ifelse(
    map$has_data,
    sprintf("%s\n%s (n=%s)", map$name, fmt(map$value),
            ifelse(is.na(map$label_n), em_dash,
                   format(map$label_n, big.mark = ",", trim = TRUE))),
    sprintf("%s\n(no data)", map$name)
  )

  no_data <- map[!map$has_data, , drop = FALSE]
  has_data <- map[map$has_data, , drop = FALSE]

  p <- ggplot2::ggplot()
  if (nrow(no_data)) {
    p <- p + ggplot2::geom_sf(data = no_data, fill = missing_fill,
                              colour = palette$grey_dk, linewidth = 0.3)
  }
  if (nrow(has_data)) {
    p <- p + ggplot2::geom_sf(data = has_data, ggplot2::aes(fill = .data$value),
                              colour = palette$grey_dk, linewidth = 0.3)
  }
  if (!is.null(min_n)) {
    low_n <- map[map$has_data & !is.na(map$label_n) & map$label_n < min_n, , drop = FALSE]
    if (nrow(low_n)) {
      p <- p + ggplot2::geom_sf(data = low_n, fill = NA, colour = palette$danger,
                                linewidth = 0.6, linetype = "dashed")
    }
  }

  if (!is.null(points)) {
    pts <- as.data.frame(points)
    if (!all(c("lon", "lat") %in% names(pts))) {
      vcs_abort("`points` must carry `lon` and `lat` columns.",
                class = "vaxsurvR_missing_variable")
    }
    if (points_size_by %in% names(pts)) {
      pts <- pts[!is.na(pts[[points_size_by]]) & pts[[points_size_by]] > 0, , drop = FALSE]
    }
    if (nrow(pts)) {
      p <- if (points_size_by %in% names(pts)) {
        p + ggplot2::geom_point(
          data = pts,
          ggplot2::aes(x = .data$lon, y = .data$lat, size = .data[[points_size_by]]),
          shape = 21, fill = palette$danger, colour = "white", stroke = 0.25,
          alpha = 0.85) +
          ggplot2::scale_size_continuous(range = c(0.8, 4.2), name = "PSU: children")
      } else {
        p + ggplot2::geom_point(data = pts,
                                ggplot2::aes(x = .data$lon, y = .data$lat),
                                size = 1, colour = palette$danger)
      }
    }
  }

  if (label) {
    # Label positions are computed once, rather than by `geom_sf_text()`, which
    # warns about point-on-surface for every unprojected layer it draws.
    xy <- suppressWarnings(
      sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(map)))
    )
    lab <- data.frame(x = xy[, 1], y = xy[, 2], label = map$label_text,
                      stringsAsFactors = FALSE)
    p <- p + if (requireNamespace("ggrepel", quietly = TRUE)) {
      # Thirty-odd units will collide otherwise.
      ggrepel::geom_text_repel(
        data = lab,
        ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
        size = base_size / 3.2, colour = "grey15", family = fam,
        lineheight = 0.95, min.segment.length = 0.3, segment.size = 0.2,
        segment.colour = "grey55", seed = 1, max.overlaps = Inf,
        point.size = NA
      )
    } else {
      ggplot2::geom_text(
        data = lab,
        ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
        size = base_size / 3.2, colour = "grey15", family = fam, lineheight = 0.95
      )
    }
  }

  opened <- sum(d$psus_opened, na.rm = TRUE)
  framed <- sum(d$psus_frame, na.rm = TRUE)
  out <- p +
    ggplot2::scale_fill_gradient(
      low = low, high = high,
      labels = if (percent) function(v) sprintf("%.0f%%", 100 * v) else ggplot2::waiver(),
      name = legend_title %||% pretty_metric(metric)
    ) +
    ggplot2::labs(
      title = title %||% sprintf("%s by %s", pretty_metric(metric), d$level[1]),
      subtitle = subtitle %||% sprintf(
        "%s of %s units with data; %s of %s sampling units opened (%.0f%%)",
        format(sum(map$has_data), big.mark = ","), format(nrow(map), big.mark = ","),
        format(opened, big.mark = ","), format(framed, big.mark = ","),
        100 * opened / max(framed, 1)),
      x = NULL, y = NULL
    ) +
    ggplot2::theme_void(base_size = base_size, base_family = fam) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 2,
                                         colour = palette$primary, hjust = 0,
                                         margin = ggplot2::margin(b = 2)),
      plot.subtitle = ggplot2::element_text(size = base_size - 1,
                                            colour = palette$grey_dk,
                                            margin = ggplot2::margin(b = 6)),
      legend.position = "bottom",
      legend.key.height = ggplot2::unit(8, "pt"),
      legend.key.width = ggplot2::unit(26, "pt"),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin = ggplot2::margin(4, 4, 4, 4))
  # The joined table is attached so callers can tabulate exactly what was
  # drawn, including the units that came out with no data.
  attr(out, "vcs_map_data") <- map
  out
}

#' @noRd
pretty_metric <- function(x) {
  lab <- c(
    pct_psus_opened = "% of sampling units opened",
    psus_opened = "Sampling units opened",
    psus_frame = "Sampling units in the frame",
    psus_with_children = "Units with at least one child",
    psus_zero_children = "Units with no eligible child",
    children = "Children interviewed",
    visits = "Household visits",
    children_per_opened_psu = "Children per opened unit",
    estimate = "Estimate"
  )
  if (x %in% names(lab)) unname(lab[x]) else gsub("_", " ", x)
}

#' Progress by administrative unit as a bar chart
#'
#' The non-spatial companion to [plot_admin_map()], for reports that have no
#' boundary file or need a figure that survives black-and-white printing.
#'
#' @param x Output of [admin_map_data()].
#' @param metric Column to plot.
#' @param target Optional reference line, e.g. `0.8`.
#' @param fill_by Optional column colouring the bars, e.g. `"adm1"`.
#' @param palette A [vcs_palette()].
#' @param base_size Base font size.
#' @param title Plot title.
#' @return A ggplot.
#' @export
#' @family PSU maps
#' @examples
#' raw <- vcs_example_frame_raw
#' frame <- read_psu_frame(primary = raw[raw$psu_type == "Primary", ],
#'                         backup  = raw[raw$psu_type == "Backup", ])
#' pm <- psu_map_data(vcs_example_raw, frame, psu_var = "psu_id",
#'                    zone_var = "health_zone", child_marker = "EC_ID",
#'                    n_caregivers = 2, n_children = 2)
#' # Assigned rather than printed: rendering needs a font device, and the
#' # object is what callers compose into a report.
#' p <- plot_admin_progress_bars(admin_map_data(pm, "zone"), target = 0.8)
#' class(p)
plot_admin_progress_bars <- function(x, metric = "pct_psus_opened", target = NULL,
                                     fill_by = NULL, palette = vcs_palette(),
                                     base_size = 9, title = NULL) {
  assert_installed("ggplot2", "plot_admin_progress_bars()")
  assert_string(metric)
  d <- as.data.frame(x)
  if (!metric %in% names(d)) {
    vcs_abort(sprintf("`x` has no column \"%s\".", metric),
              class = "vaxsurvR_missing_variable")
  }
  d$name <- factor(d$name, levels = d$name[order(d[[metric]], na.last = FALSE)])
  percent <- grepl("^pct_|^prop_", metric)
  fam <- vcs_font()

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[metric]], y = .data$name))
  p <- if (!is.null(fill_by) && fill_by %in% names(d)) {
    p + ggplot2::geom_col(ggplot2::aes(fill = .data[[fill_by]]), width = 0.75)
  } else {
    p + ggplot2::geom_col(fill = palette$primary, width = 0.75)
  }
  if (!is.null(target)) {
    p <- p + ggplot2::geom_vline(xintercept = target, linetype = 2,
                                 colour = palette$danger, linewidth = 0.4)
  }
  if (percent) {
    p <- p + ggplot2::scale_x_continuous(
      labels = function(v) sprintf("%.0f%%", 100 * v), limits = c(0, 1))
  }
  p +
    ggplot2::labs(x = pretty_metric(metric), y = NULL,
                  title = title %||% sprintf("%s by %s", pretty_metric(metric),
                                             d$level[1])) +
    theme_vcs(base_size = base_size, base_family = fam) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}
