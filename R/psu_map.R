#' Read a gridded-population PSU sampling frame
#'
#' Reads the primary and (optionally) backup PSU files supplied with a
#' gridded-population sample and returns one tidy table of sampling units with
#' their health zone and centroid coordinates. The column names are those used
#' by Biostat Global Consulting's frame files (`project_psu_id`, `adm3`,
#' `centroid_longitude`, `centroid_latitude`), and any of them can be
#' overridden.
#'
#' @param primary Path to the primary PSU file (`.xlsx` or `.csv`), or a data
#'   frame already read in.
#' @param backup Optional path or data frame of backup PSUs.
#' @param psu_col,zone_col,lon_col,lat_col,pop_col Column names in those files.
#' @param backup_for_col Column of the backup file naming the primary PSU that
#'   each backup stands in for.
#' @param adm_cols Named character vector of administrative columns to carry
#'   through, from the largest unit to the smallest. The names become the
#'   column names of the result, so `c(adm1 = "adm1", adm3 = "adm3")` keeps
#'   province and health zone. Levels absent from the file are skipped.
#'   These are what [admin_map_data()] aggregates over; `zone` is kept
#'   separately for the PSU maps and is always `zone_col`.
#' @return A tibble with `psu`, `psu_type` (`"Primary"` or `"Backup"`),
#'   `zone`, `lon`, `lat`, `est_pop`, `backup_for` and any `adm_cols`.
#' @export
#' @family PSU maps
#' @examples
#' \dontrun{
#' frame <- read_psu_frame("03_Primary_PSUs.xlsx", "04_Backup_PSUs.xlsx")
#' }
read_psu_frame <- function(primary, backup = NULL,
                           psu_col = "project_psu_id", zone_col = "adm3",
                           lon_col = "centroid_longitude", lat_col = "centroid_latitude",
                           pop_col = "est_pop", backup_for_col = "backup_for",
                           adm_cols = c(adm0 = "adm0", adm1 = "adm1",
                                        adm2 = "adm2", adm3 = "adm3")) {
  one <- function(x, type) {
    if (is.null(x)) return(NULL)
    df <- if (is.character(x)) {
      if (grepl("\\.xlsx?$", x, ignore.case = TRUE)) {
        assert_installed("readxl", "read_psu_frame()")
        as.data.frame(readxl::read_excel(x))
      } else {
        read_vcs(x)
      }
    } else {
      as.data.frame(x)
    }
    need <- c(psu_col, zone_col, lon_col, lat_col)
    missing <- setdiff(need, names(df))
    if (length(missing)) {
      vcs_abort(sprintf("PSU frame is missing column(s): %s.", paste(missing, collapse = ", ")),
                class = "vaxsurvR_missing_variable")
    }
    out <- tibble::tibble(
      psu = trimws(as.character(df[[psu_col]])),
      psu_type = type,
      zone = gsub("_", " ", trimws(as.character(df[[zone_col]]))),
      lon = suppressWarnings(as.numeric(df[[lon_col]])),
      lat = suppressWarnings(as.numeric(df[[lat_col]])),
      est_pop = if (pop_col %in% names(df)) suppressWarnings(as.numeric(df[[pop_col]])) else NA_real_,
      backup_for = if (backup_for_col %in% names(df)) trimws(as.character(df[[backup_for_col]])) else NA_character_
    )
    for (nm in names(adm_cols)) {
      src <- adm_cols[[nm]]
      if (src %in% names(df)) {
        out[[nm]] <- gsub("_", " ", trimws(as.character(df[[src]])))
      }
    }
    out
  }
  out <- dplyr::bind_rows(one(primary, "Primary"), one(backup, "Backup"))
  out[!is.na(out$lon) & !is.na(out$lat), ]
}

#' Attach the achieved sample to a PSU frame
#'
#' Counts, for every sampling unit the field teams opened, how many household
#' visits were made and how many children were interviewed, and joins those
#' counts onto the frame returned by [read_psu_frame()]. Visits recorded
#' against a backup unit are credited to the backup unit, not to the primary
#' unit it replaced, which is the distinction the maps in [plot_psu_map()]
#' are built on.
#'
#' @param data A `vcs_data` object, or the raw wide export as a data frame.
#' @param frame Output of [read_psu_frame()].
#' @param psu_var,backup_var,zone_var Columns of the export holding the
#'   assigned PSU, the backup PSU used instead (blank when none was), and the
#'   health zone.
#' @param n_caregivers,n_children Repeat extents of the child module, used to
#'   count the children interviewed in each unit.
#' @param child_marker The child-module variable whose presence marks a
#'   completed child block.
#' @return `frame` with `n_visits`, `n_children`, `opened` and `panel` added.
#'   `panel` is the facet each unit belongs to in [plot_psu_map()].
#' @export
#' @family PSU maps
psu_map_data <- function(data, frame, psu_var = "PVT06", backup_var = "PVT06_BCKP",
                         zone_var = "PVT05", n_caregivers = 3L, n_children = 3L,
                         child_marker = "EC01") {
  raw <- if (inherits(data, "vcs_data")) data$raw else as.data.frame(data)
  if (is.null(raw)) {
    vcs_abort("`data` carries no raw export; build the `vcs_data` with `keep_raw = TRUE`.",
              class = "vaxsurvR_missing_variable")
  }
  nz <- function(x) !is.na(x) & nzchar(trimws(x))
  get <- function(v) if (v %in% names(raw)) trimws(as.character(raw[[v]])) else rep(NA_character_, nrow(raw))

  # A visit made in a backup unit belongs to that backup unit.
  psu <- get(psu_var)
  bck <- get(backup_var)
  use_bck <- nz(bck) & grepl("^[0-9]+$", bck)
  psu[use_bck] <- bck[use_bck]

  slots <- expand.grid(c = seq_len(n_caregivers), k = seq_len(n_children))
  kids <- Reduce(`+`, lapply(seq_len(nrow(slots)), function(i) {
    cn <- sprintf("%s_%d_%d", child_marker, slots$c[i], slots$k[i])
    if (cn %in% names(raw)) as.integer(nz(raw[[cn]])) else 0L
  }))

  keep <- nz(psu)
  agg <- stats::aggregate(list(n_visits = rep(1L, sum(keep)), n_children = kids[keep]),
                          by = list(psu = psu[keep]), FUN = sum)
  zone_seen <- tapply(gsub("_", " ", get(zone_var)[keep]), psu[keep],
                      function(z) names(sort(table(z), decreasing = TRUE))[1])

  out <- dplyr::left_join(frame, tibble::as_tibble(agg), by = "psu")
  out$n_visits <- ifelse(is.na(out$n_visits), 0L, out$n_visits)
  out$n_children <- ifelse(is.na(out$n_children), 0L, out$n_children)
  out$opened <- out$n_visits > 0L
  # Where the frame and the export disagree on the health zone, trust the field.
  seen <- unname(zone_seen[out$psu])
  out$zone <- ifelse(!is.na(seen), seen, out$zone)
  out$panel <- factor(
    ifelse(out$psu_type == "Backup",
           "Backup PSUs that contributed data from at least one child ages 12-23m",
           ifelse(out$n_children > 0L,
                  "Primary PSUs that contributed data from at least one child ages 12-23m",
                  "Primary PSUs that contributed data from zero children ages 12-23m")),
    levels = c("Primary PSUs that contributed data from at least one child ages 12-23m",
               "Primary PSUs that contributed data from zero children ages 12-23m",
               "Backup PSUs that contributed data from at least one child ages 12-23m"))
  out
}

#' Map of household data collection locations from primary and backup PSUs
#'
#' Draws the three-panel sampling-unit map used in vaccination coverage survey
#' reports: the primary units that yielded at least one child aged 12 to 23
#' months, the primary units that yielded none, and the backup units that were
#' activated and yielded at least one child. Point size and colour give the
#' number of children interviewed in the unit.
#'
#' The outline drawn behind the points is the boundary supplied in `boundary`
#' when there is one; otherwise it is the convex hull of every sampling unit of
#' the frame that falls in the zone, which marks the extent of the frame rather
#' than the administrative boundary.
#'
#' @param x Output of [psu_map_data()].
#' @param zone Health zone to map. `NULL` maps the whole study area.
#' @param boundary Optional `sf` polygon (or a path to one) to draw as the
#'   outline. It is subset to `zone` when it carries a matching name column.
#' @param boundary_zone_col Column of `boundary` holding the zone name.
#' @param palette A [vcs_palette()].
#' @param base_size Base font size in points.
#' @param title Optional title; defaults to the zone name.
#' @param only_opened Drop sampling units the field teams never opened, so the
#'   map shows where data collection actually happened.
#' @return A ggplot.
#' @export
#' @family PSU maps
plot_psu_map <- function(x, zone = NULL, boundary = NULL, boundary_zone_col = "zone",
                         palette = vcs_palette(), base_size = 9, title = NULL,
                         only_opened = TRUE) {
  assert_installed("ggplot2", "plot_psu_map()")
  d <- as.data.frame(x)
  if (!is.null(zone)) d <- d[!is.na(d$zone) & d$zone %in% zone, ]
  if (!nrow(d)) {
    vcs_abort("No sampling units to map for that zone.", class = "vaxsurvR_empty_input")
  }
  hull_pts <- d[, c("lon", "lat")]            # frame extent, before dropping units
  if (only_opened) d <- d[d$opened | d$psu_type == "Primary", ]
  pts <- d[d$opened, , drop = FALSE]
  fam <- vcs_font()

  outline <- NULL
  if (!is.null(boundary)) {
    assert_installed("sf", "plot_psu_map(boundary = )")
    b <- if (is.character(boundary)) sf::st_read(boundary, quiet = TRUE) else boundary
    if (!is.null(zone) && boundary_zone_col %in% names(b)) {
      b <- b[gsub("_", " ", trimws(as.character(b[[boundary_zone_col]]))) %in% zone, ]
    }
    if (nrow(b)) outline <- b
  }
  hull <- NULL
  if (is.null(outline) && nrow(hull_pts) >= 3L) {
    h <- grDevices::chull(hull_pts$lon, hull_pts$lat)
    hull <- hull_pts[c(h, h[1]), ]
    # A small outward nudge so points on the hull are not clipped by the line.
    hull$lon <- mean(hull_pts$lon) + (hull$lon - mean(hull_pts$lon)) * 1.06
    hull$lat <- mean(hull_pts$lat) + (hull$lat - mean(hull_pts$lat)) * 1.06
  }

  # Every panel gets the same outline, so the three maps are comparable.
  lv <- levels(d$panel)
  rep_panels <- function(df) {
    do.call(rbind, lapply(lv, function(p) { df$panel <- factor(p, levels = lv); df }))
  }

  p <- ggplot2::ggplot()
  if (!is.null(outline)) {
    p <- p + ggplot2::geom_sf(data = outline, fill = "white", colour = palette$grey_dk,
                              linewidth = 0.35, inherit.aes = FALSE)
  } else if (!is.null(hull)) {
    p <- p + ggplot2::geom_path(data = rep_panels(hull),
                                ggplot2::aes(x = .data$lon, y = .data$lat),
                                colour = palette$grey_dk, linewidth = 0.35)
  }
  zero <- pts[pts$panel == lv[2], , drop = FALSE]
  some <- pts[pts$panel != lv[2], , drop = FALSE]
  if (nrow(some)) {
    p <- p + ggplot2::geom_point(
      data = some,
      ggplot2::aes(x = .data$lon, y = .data$lat, size = .data$n_children,
                   colour = .data$n_children), alpha = 0.85)
  }
  if (nrow(zero)) {
    p <- p + ggplot2::geom_point(data = zero,
                                 ggplot2::aes(x = .data$lon, y = .data$lat),
                                 shape = 17, size = 1.9, colour = palette$danger)
  }
  rng <- range(c(1, pts$n_children[pts$n_children > 0]), na.rm = TRUE)
  brks <- unique(round(seq(rng[1], max(rng[2], 2), length.out = 5)))
  p +
    ggplot2::scale_colour_gradient(low = "#9FD8CB", high = "#2C5F8A",
                                   breaks = brks, guide = "legend",
                                   name = "# Children 12-23m") +
    ggplot2::scale_size_continuous(range = c(1.1, 5.2), breaks = brks,
                                   name = "# Children 12-23m") +
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)),
                    size = ggplot2::guide_legend()) +
    ggplot2::facet_wrap(~panel, nrow = 1, labeller = ggplot2::label_wrap_gen(width = 30)) +
    ggplot2::coord_fixed(ratio = 1) +
    ggplot2::labs(title = title %||% (if (is.null(zone)) NULL else paste(zone, collapse = ", ")),
                  x = NULL, y = NULL) +
    ggplot2::theme_void(base_size = base_size, base_family = fam) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 2,
                                         colour = palette$primary, hjust = 0,
                                         margin = ggplot2::margin(b = 4)),
      strip.text = ggplot2::element_text(size = base_size - 1.5, lineheight = 1.05, hjust = 0.5,
                                         margin = ggplot2::margin(b = 3)),
      legend.position = "right",
      legend.title = ggplot2::element_text(size = base_size - 0.5),
      legend.text = ggplot2::element_text(size = base_size - 1),
      legend.key.height = ggplot2::unit(10, "pt"),
      panel.spacing = ggplot2::unit(6, "pt"),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin = ggplot2::margin(4, 4, 4, 4))
}

#' Summarise the sampling-unit funnel behind the PSU maps
#'
#' @param x Output of [psu_map_data()].
#' @param by Optional grouping column, usually `"zone"`.
#' @return A tibble of unit counts: in the frame, opened, yielding at least one
#'   child, yielding none, and the children and visits those units produced.
#' @export
#' @family PSU maps
psu_map_summary <- function(x, by = NULL) {
  d <- as.data.frame(x)
  g <- if (is.null(by)) rep("Entire study area", nrow(d)) else d[[by]]
  f <- function(i) {
    s <- d[i, , drop = FALSE]
    pr <- s[s$psu_type == "Primary", , drop = FALSE]
    bk <- s[s$psu_type == "Backup", , drop = FALSE]
    tibble::tibble(
      primary_in_frame = nrow(pr),
      primary_opened = sum(pr$opened),
      primary_with_children = sum(pr$opened & pr$n_children > 0),
      primary_zero_children = sum(pr$opened & pr$n_children == 0),
      backup_opened = sum(bk$opened),
      backup_with_children = sum(bk$opened & bk$n_children > 0),
      visits = sum(s$n_visits),
      children = sum(s$n_children))
  }
  out <- dplyr::bind_rows(lapply(split(seq_len(nrow(d)), g), f))
  out <- dplyr::bind_cols(tibble::tibble(group = names(split(seq_len(nrow(d)), g))), out)
  names(out)[1] <- by %||% "group"
  out
}
