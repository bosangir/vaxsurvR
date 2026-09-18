# ---------------------------------------------------------------------------
# Caregiver questions: behavioural and social drivers of vaccination (BeSD),
# select-multiple follow-ups, reasons the child is not fully vaccinated, and
# child-health interventions. All of these are proportions of respondents,
# estimated with the survey design, over the same stratifier ladder as the
# coverage tables.
# ---------------------------------------------------------------------------

#' Describe a set of BeSD questions and their concerning responses
#'
#' @param ... Named entries, one per question. Each is a list with `var`
#'   (column in the child-level table), `label` (question text as shown in
#'   tables), `concerning` (codes counted as a concerning response), `group`
#'   (optional colour group: `intention`, `confidence`, `norms`,
#'   `motivation`, `access`, `practical`, `service`) and `short` (optional
#'   short label for the by-stratum table).
#' @return An object of class `vcs_besd_map`.
#' @export
#' @seealso [kc9_besd_map()], [estimate_besd()]
#' @examples
#' vcs_besd_map(
#'   BESD02 = list(var = "BESD02", label = "Want child to get none, some or all vaccines",
#'                 concerning = c("1", "2"), group = "intention")
#' )
vcs_besd_map <- function(...) {
  entries <- rlang::list2(...)
  if (!length(entries) || is.null(names(entries)) || any(!nzchar(names(entries)))) {
    vcs_abort("BeSD entries must be named.", class = "vaxsurvR_value_error")
  }
  for (nm in names(entries)) {
    e <- entries[[nm]]
    if (!is.list(e) || is.null(e$var) || is.null(e$concerning)) {
      vcs_abort(sprintf("Entry \"%s\" needs `var` and `concerning`.", nm),
                class = "vaxsurvR_value_error")
    }
    entries[[nm]]$label <- e$label %||% nm
    entries[[nm]]$short <- e$short %||% entries[[nm]]$label
    entries[[nm]]$group <- e$group %||% "access"
    entries[[nm]]$concerning_label <- e$concerning_label %||% paste(e$concerning, collapse = "/")
  }
  structure(entries, class = "vcs_besd_map")
}

#' BeSD map for the KC v9 questionnaire
#'
#' The WHO BeSD core questions as asked in `BESD01`--`BESD19`, with the
#' response codes the South Sudan report counted as concerning.
#'
#' @return A [vcs_besd_map()].
#' @export
#' @family KC v9 configuration
#' @examples
#' names(kc9_besd_map())
kc9_besd_map <- function() {
  vcs_besd_map(
    BESD01 = list(var = "BESD01", group = "intention", concerning = c("1", "2"),
                  concerning_label = "None or some",
                  label = "Has your child had none, some or all of the scheduled vaccines?",
                  short = "Caregiver believes child has not received all scheduled vaccines"),
    BESD02 = list(var = "BESD02", group = "intention", concerning = c("1", "2"),
                  concerning_label = "None or some",
                  label = "Do you want your child to get none, some or all of these vaccines?",
                  short = "Does not want child to get all scheduled vaccines"),
    BESD03 = list(var = "BESD03", group = "confidence", concerning = c("1", "2"),
                  concerning_label = "Not at all or a little",
                  label = "How important do you think vaccines are for your child's health?",
                  short = "Vaccines are not important to child's health"),
    BESD04 = list(var = "BESD04", group = "confidence", concerning = c("1", "2"),
                  concerning_label = "Not at all or a little",
                  label = "How safe do you think vaccines are for your child?",
                  short = "Vaccines are not safe"),
    BESD05 = list(var = "BESD05", group = "norms", concerning = c("1", "2"),
                  concerning_label = "Not at all or a little",
                  label = "How much do you trust the health workers who give children vaccines?",
                  short = "Does not trust health workers giving vaccines"),
    BESD06 = list(var = "BESD06", group = "norms", concerning = "2", concerning_label = "No",
                  label = "Do you think most parents you know get their children vaccinated?",
                  short = "Most parents they know do not get their child vaccinated"),
    BESD07 = list(var = "BESD07", group = "norms", concerning = "2", concerning_label = "No",
                  label = "Do you think most of your close family/friends want you to get your child vaccinated?",
                  short = "Family and friends do not want child to get vaccinated"),
    BESD08 = list(var = "BESD08", group = "motivation", concerning = "2", concerning_label = "No",
                  label = "Do you think your religious leaders want you to get your child vaccinated?",
                  short = "Religious leaders do not support vaccination"),
    BESD09 = list(var = "BESD09", group = "motivation", concerning = "2", concerning_label = "No",
                  label = "Do you think your community leaders want you to get your child vaccinated?",
                  short = "Community leaders do not support vaccination"),
    BESD10 = list(var = "BESD10", group = "motivation", concerning = "2", concerning_label = "No",
                  label = "Has a health worker recommended your child to be vaccinated?",
                  short = "Health worker has not recommended child be vaccinated"),
    BESD11 = list(var = "BESD11", group = "motivation", concerning = "2", concerning_label = "No",
                  label = "Have you ever been contacted about your child being due for vaccination?",
                  short = "Never contacted about child due for vaccination"),
    BESD12 = list(var = "BESD12", group = "access", concerning = "1", concerning_label = "Yes",
                  label = "Does the mother need permission from someone else in the household to vaccinate the child?",
                  short = "Mother needs permission to take child for vaccination services"),
    BESD13 = list(var = "BESD13", group = "access", concerning = "2", concerning_label = "No",
                  label = "Do you know where to go to get your child vaccinated?",
                  short = "Caregiver does not know where to go to get child vaccinated"),
    BESD14 = list(var = "BESD14", group = "access", concerning = "2", concerning_label = "No",
                  label = "Have you personally ever taken your youngest child (12-23m) to get vaccinated?",
                  short = "Never personally taken youngest child (12-23m) to get vaccinated"),
    BESD15 = list(var = "BESD15", group = "practical", concerning = "1", concerning_label = "Yes",
                  label = "Have you ever been turned away when you tried to get your child vaccinated?",
                  short = "Turned away when tried to get child vaccinated"),
    BESD17 = list(var = "BESD17", group = "practical", concerning = c("1", "2"),
                  concerning_label = "Not at all or a little",
                  label = "How easy is it to pay for vaccination services for your child?",
                  short = "Not easy for caregiver to pay for vaccination services for child"),
    BESD16 = list(var = "BESD16", group = "service", concerning = c("1", "2"),
                  concerning_label = "Not at all or a little",
                  label = "How easy is it to get vaccination services for your child?",
                  short = "Not easy for caregiver to get vaccination services for child"),
    BESD19 = list(var = "BESD19", group = "service", concerning = c("1", "2"),
                  concerning_label = "Not at all or a little",
                  label = "How satisfied are you with the vaccination services?",
                  short = "Not satisfied with vaccination services")
  )
}

#' Add 0/1 "concerning response" indicators to the child table
#'
#' @param x A [vcs_data][new_vcs_data] object whose child table carries the
#'   BeSD variables (see [attach_child_variables()]).
#' @param map A [vcs_besd_map()].
#' @param prefix Prefix of the derived columns.
#' @return `x` with one `<prefix><name>` column per question: `1` concerning,
#'   `0` not concerning, `NA` not answered.
#' @export
#' @examples
#' \dontrun{
#' vcs <- derive_besd(vcs, kc9_besd_map())
#' }
derive_besd <- function(x, map, prefix = "besd_") {
  assert_vcs_data(x)
  if (!inherits(map, "vcs_besd_map")) {
    vcs_abort("`map` must be a `vcs_besd_map`.", class = "vaxsurvR_type_error")
  }
  ch <- x$children
  for (nm in names(map)) {
    e <- map[[nm]]
    v <- if (e$var %in% names(ch)) trimws(as.character(ch[[e$var]])) else rep(NA_character_, nrow(ch))
    out <- rep(NA_integer_, nrow(ch))
    answered <- !is.na(v) & nzchar(v) & !v %in% c("98", "99")
    out[answered] <- as.integer(v[answered] %in% e$concerning)
    ch[[paste0(prefix, nm)]] <- mark_derived(
      out, sprintf("%s in {%s} = concerning response", e$var, paste(e$concerning, collapse = ", "))
    )
  }
  x$children <- ch
  x
}

#' Estimate the percent of caregivers giving concerning BeSD responses
#'
#' One row per question (and domain): the weighted share of caregivers whose
#' answer falls in the concerning set, with the question text, the concerning
#' responses and the colour group. Children are the unit of analysis in the
#' design; when several children share a caregiver the caregiver's answers
#' are counted once per child, as in VCQI.
#'
#' @param design A [vcs_design()] built on a child table that has been
#'   through [derive_besd()].
#' @param map The [vcs_besd_map()] used.
#' @param by Domains.
#' @param prefix Prefix of the derived columns.
#' @param ... Passed to the estimator (`ci_method`, `level`).
#' @return A tibble.
#' @export
estimate_besd <- function(design, map, by = NULL, prefix = "besd_", ...) {
  assert_design(design)
  by <- as_column_names(by)
  rows <- lapply(names(map), function(nm) {
    col <- paste0(prefix, nm)
    if (!col %in% names(design$data)) return(NULL)
    est <- tibble::as_tibble(estimate_indicator(design, col, by, type = "besd", ...))
    est$question <- nm
    est$label <- map[[nm]]$label
    est$short <- map[[nm]]$short
    est$concerning <- map[[nm]]$concerning_label
    est$group <- map[[nm]]$group
    est
  })
  out <- dplyr::bind_rows(rows)
  out$weighted_n <- out$denominator
  front <- c("question", "short", "label", "concerning", "group", "domain", by)
  out[, c(intersect(front, names(out)), setdiff(names(out), front)), drop = FALSE]
}

#' BeSD concerning responses by stratum, as a wide shaded table
#'
#' Builds the data behind the "percent of caregivers who gave concerning
#' responses, by county" table: rows are questions, columns are strata, each
#' cell the weighted percent, with the weighted N of each stratum in the
#' header.
#'
#' @param design A [vcs_design()] after [derive_besd()].
#' @param map A [vcs_besd_map()].
#' @param by One stratifier column.
#' @param levels Optional order of the strata.
#' @return A list with `table` (questions x strata, percentages), `n`
#'   (weighted N per stratum), `groups` (colour group per question).
#' @export
besd_by_stratum <- function(design, map, by, levels = NULL) {
  by <- as_column_names(by)
  est <- estimate_besd(design, map, by = by[1])
  strata <- levels %||% sort(unique(est[[by[1]]]))
  q <- names(map)
  tab <- matrix(NA_real_, nrow = length(q), ncol = length(strata),
                dimnames = list(q, strata))
  nn <- stats::setNames(rep(NA_real_, length(strata)), strata)
  for (s in strata) {
    sub <- est[est[[by[1]]] == s, , drop = FALSE]
    tab[sub$question, s] <- sub$estimate
    nn[s] <- max(sub$denominator, na.rm = TRUE)
  }
  list(table = tab, n = nn,
       short = vapply(map, `[[`, character(1), "short"),
       groups = vapply(map, `[[`, character(1), "group"))
}

#' Draw the BeSD-by-stratum table
#'
#' @param x Output of [besd_by_stratum()].
#' @param palette A [vcs_palette()]; `palette$besd` colours the row groups.
#' @param base_size Font size.
#' @param title Optional title.
#' @return A ggplot.
#' @export
plot_besd_by_stratum <- function(x, palette = vcs_palette(), base_size = 8.5, title = NULL) {
  assert_installed("ggplot2", "plot_besd_by_stratum()")
  tab <- x$table
  q <- rownames(tab)
  strata <- colnames(tab)
  nq <- length(q)
  ns <- length(strata)
  cell_w <- 12
  label_w <- 44
  cols <- palette$besd
  grp_cols <- cols[x$groups]
  grp_cols[is.na(grp_cols)] <- palette$secondary
  # lighten within group by order of appearance
  shade <- stats::ave(seq_len(nq), x$groups, FUN = seq_along)
  size_grp <- stats::ave(seq_len(nq), x$groups, FUN = length)
  fill <- vapply(seq_len(nq), function(i) {
    f <- if (size_grp[i] > 1) 0.15 + 0.7 * (shade[i] - 1) / (size_grp[i] - 1) else 0
    grDevices::adjustcolor(grp_cols[i], alpha.f = 1 - 0.65 * f)
  }, character(1))
  df <- expand.grid(qi = seq_len(nq), si = seq_len(ns))
  df$v <- as.numeric(tab[cbind(df$qi, df$si)])
  df$y <- nq - df$qi + 1
  df$x0 <- label_w + (df$si - 1) * cell_w
  df$w <- pmin(pmax(df$v, 0), 1) * (cell_w - 0.5)
  df$fill <- fill[df$qi]
  fam <- vcs_font()
  ts <- base_size / ggplot2::.pt
  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(data = df, ggplot2::aes(xmin = .data$x0, xmax = .data$x0 + .data$w,
                                               ymin = .data$y - 0.45, ymax = .data$y + 0.45),
                       fill = df$fill, colour = NA) +
    ggplot2::geom_text(data = df, ggplot2::aes(x = .data$x0 + cell_w - 0.6, y = .data$y,
                                               label = ifelse(is.na(.data$v), "", sprintf("%.1f", 100 * .data$v))),
                       hjust = 1, size = ts * 0.9, family = fam) +
    ggplot2::annotate("text", x = 0.5, y = nq - seq_len(nq) + 1, label = x$short[q],
                      hjust = 0, size = ts, family = fam) +
    ggplot2::annotate("text", x = label_w + (seq_len(ns) - 1) * cell_w + cell_w / 2,
                      y = nq + 1.2, label = sprintf("%s\n(Wtd N = %s)", strata, format(round(x$n), big.mark = ",")),
                      size = ts, family = fam, lineheight = 0.9, fontface = "bold") +
    ggplot2::annotate("text", x = 0.5, y = nq + 1.2, label = "Question", hjust = 0,
                      size = ts, family = fam, fontface = "bold") +
    ggplot2::annotate("segment", x = 0, xend = label_w + ns * cell_w,
                      y = seq_len(nq + 1) - 0.5, yend = seq_len(nq + 1) - 0.5,
                      colour = palette$grey_lt, linewidth = 0.3) +
    ggplot2::annotate("segment", x = label_w + (0:ns) * cell_w, xend = label_w + (0:ns) * cell_w,
                      y = 0.5, yend = nq + 2, colour = palette$grey, linewidth = 0.3) +
    ggplot2::annotate("segment", x = 0, xend = label_w + ns * cell_w, y = nq + 0.5, yend = nq + 0.5,
                      colour = palette$grey_dk, linewidth = 0.6) +
    ggplot2::scale_x_continuous(limits = c(0, label_w + ns * cell_w + 0.5), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0.4, nq + 2.1), expand = c(0, 0)) +
    ggplot2::labs(title = title, caption = "Table cells are shaded in proportion to the outcome; an outcome of 100% would fill the cell with colour. Percent of caregivers who gave a concerning response.") +
    ggplot2::theme_void(base_size = base_size, base_family = fam) +
    ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0, colour = palette$grey_dk, size = base_size * 0.85),
                   plot.caption.position = "plot",
                   plot.title = ggplot2::element_text(face = "bold", hjust = 0),
                   plot.background = ggplot2::element_rect(fill = "white", colour = NA),
                   plot.margin = ggplot2::margin(4, 6, 4, 4))
  attr(p, "vcs_size") <- c(width = (label_w + ns * cell_w) / 12.5, height = (nq + 4) / 4.2)
  p
}

#' Tabulate a select-multiple question
#'
#' Splits a space-separated SurveyCTO `select_multiple` answer into its
#' options and estimates the weighted percent of respondents choosing each
#' one, among those who answered. Percents can sum to more than 100.
#'
#' @param design A [vcs_design()].
#' @param variable Column with the space-separated codes.
#' @param labels Named character vector, names are codes.
#' @param by Domains.
#' @param min_n Options with fewer than this many respondents are still
#'   reported; the `n` column lets the reader judge.
#' @param ... Passed to the estimator.
#' @return A tibble with `option`, `code`, the domain columns, `estimate`,
#'   `conf_low`, `conf_high`, `n` (respondents), `n_selected`.
#' @export
#' @examples
#' \dontrun{
#' tabulate_multiselect(des, "BESD18", kc9_labels()$besd18)
#' }
tabulate_multiselect <- function(design, variable, labels, by = NULL, min_n = 0, ...) {
  assert_design(design)
  assert_string(variable)
  by <- as_column_names(by)
  if (!variable %in% names(design$data)) {
    vcs_abort(sprintf("Column \"%s\" not in the design data.", variable),
              class = "vaxsurvR_value_error")
  }
  raw <- trimws(as.character(design$data[[variable]]))
  answered <- !is.na(raw) & nzchar(raw)
  toks <- strsplit(raw, "[[:space:],;]+")
  codes <- names(labels)
  des <- design
  for (cd in codes) {
    col <- paste0(".ms_", make.names(cd))
    val <- rep(NA_integer_, length(raw))
    val[answered] <- vapply(toks[answered], function(t) as.integer(cd %in% t), integer(1))
    des$data[[col]] <- val
    des$design$variables[[col]] <- val
  }
  rows <- lapply(codes, function(cd) {
    col <- paste0(".ms_", make.names(cd))
    est <- tibble::as_tibble(estimate_indicator(des, col, by, type = "multiselect", ...))
    est$code <- cd
    est$option <- unname(labels[cd])
    est$n <- est$denominator
    est$n_selected <- est$numerator
    est
  })
  out <- dplyr::bind_rows(rows)
  out[, c("option", "code", intersect(c("domain", by), names(out)), "estimate",
          "conf_low", "conf_high", "n", "n_selected"), drop = FALSE]
}

#' Tabulate one categorical question
#'
#' Weighted distribution of the answers to a `select_one` question among
#' those who answered.
#'
#' @param design A [vcs_design()].
#' @param variable Column name.
#' @param labels Named character vector, names are codes; unknown codes are
#'   dropped unless `keep_unknown = TRUE`.
#' @param by Domains.
#' @param keep_unknown Keep codes absent from `labels`, labelled by code.
#' @param ... Passed to the estimator.
#' @return A tibble with `option`, `code`, domain columns, `estimate`,
#'   `conf_low`, `conf_high`, `n`, `n_selected`.
#' @export
tabulate_categorical <- function(design, variable, labels, by = NULL,
                                 keep_unknown = FALSE, ...) {
  assert_design(design)
  assert_string(variable)
  raw <- trimws(as.character(design$data[[variable]]))
  present <- unique(raw[!is.na(raw) & nzchar(raw)])
  codes <- names(labels)
  if (keep_unknown) {
    extra <- setdiff(present, codes)
    labels <- c(labels, stats::setNames(extra, extra))
    codes <- names(labels)
  }
  lab_vec <- stats::setNames(rep(NA_character_, length(raw)), NULL)
  answered <- !is.na(raw) & raw %in% codes
  ms <- ifelse(answered, raw, NA_character_)
  fake <- design
  fake$data[[".cat"]] <- ms
  fake$design$variables[[".cat"]] <- ms
  tabulate_multiselect(fake, ".cat", labels, by = by, ...)
}

#' Reasons the child is not fully vaccinated, by category and stratum
#'
#' Builds the data for the "reasons the child is not fully vaccinated" table:
#' the weighted percent of caregivers of not-fully-vaccinated children who
#' gave each reason, overall and by stratum, with reasons grouped into
#' categories (access, health centre, beliefs, social processes).
#'
#' @param design A [vcs_design()].
#' @param variable Select-multiple column with the reasons.
#' @param labels Named character vector of reason labels.
#' @param categories Named character vector mapping codes to categories.
#' @param by One stratifier column, or `NULL` for the overall column only.
#' @param levels Optional order of strata.
#' @return A list with `table` (reasons x columns, proportions), `n`
#'   (respondents per column), `category` (per reason).
#' @export
reasons_by_stratum <- function(design, variable, labels, categories, by = NULL,
                               levels = NULL) {
  by <- as_column_names(by)
  overall <- tabulate_multiselect(design, variable, labels)
  cols <- list("All combined" = overall)
  if (length(by)) {
    st <- tabulate_multiselect(design, variable, labels, by = by[1])
    strata <- levels %||% sort(unique(st[[by[1]]]))
    for (s in strata) cols[[s]] <- st[st[[by[1]]] == s, , drop = FALSE]
  }
  codes <- names(labels)
  tab <- matrix(NA_real_, nrow = length(codes), ncol = length(cols),
                dimnames = list(unname(labels), names(cols)))
  nn <- stats::setNames(rep(NA_real_, length(cols)), names(cols))
  for (cn in names(cols)) {
    d <- cols[[cn]]
    tab[match(d$code, codes), cn] <- d$estimate
    nn[cn] <- if (nrow(d)) max(d$n, na.rm = TRUE) else NA_real_
  }
  cat <- unname(categories[codes])
  cat[is.na(cat)] <- "Other"
  ord <- order(match(cat, unique(c(categories, "Other"))), seq_along(codes))
  list(table = tab[ord, , drop = FALSE], n = nn, category = cat[ord])
}

#' Draw the reasons-not-vaccinated table
#'
#' @param x Output of [reasons_by_stratum()].
#' @param palette A [vcs_palette()].
#' @param base_size Font size.
#' @param title Optional title.
#' @return A ggplot.
#' @export
plot_reasons_table <- function(x, palette = vcs_palette(), base_size = 8, title = NULL) {
  assert_installed("ggplot2", "plot_reasons_table()")
  tab <- x$table
  reasons <- rownames(tab)
  cols <- colnames(tab)
  nr <- length(reasons)
  nc <- length(cols)
  cat_w <- 11
  label_w <- 46
  cell_w <- 11
  x0 <- cat_w + label_w
  df <- expand.grid(ri = seq_len(nr), ci = seq_len(nc))
  df$v <- as.numeric(tab[cbind(df$ri, df$ci)])
  df$y <- nr - df$ri + 1
  df$xs <- x0 + (df$ci - 1) * cell_w
  df$w <- pmin(pmax(df$v, 0), 1) * (cell_w - 0.4)
  fam <- vcs_font()
  ts <- base_size / ggplot2::.pt
  cat_runs <- rle(x$category)
  cat_end <- cumsum(cat_runs$lengths)
  cat_start <- cat_end - cat_runs$lengths + 1
  cat_df <- tibble::tibble(label = cat_runs$values,
                           y = nr - (cat_start + cat_end) / 2 + 1,
                           ybot = nr - cat_end + 0.5)
  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(data = df, ggplot2::aes(xmin = .data$xs, xmax = .data$xs + .data$w,
                                               ymin = .data$y - 0.45, ymax = .data$y + 0.45),
                       fill = palette$bar_fill, colour = NA) +
    ggplot2::geom_text(data = df, ggplot2::aes(x = .data$xs + cell_w - 0.5, y = .data$y,
                                               label = ifelse(is.na(.data$v), "", sprintf("%.1f", 100 * .data$v))),
                       hjust = 1, size = ts, family = fam) +
    ggplot2::annotate("text", x = x0 - 0.6, y = nr - seq_len(nr) + 1, label = reasons,
                      hjust = 1, size = ts, family = fam) +
    ggplot2::geom_text(data = cat_df, ggplot2::aes(x = cat_w / 2, y = .data$y, label = .data$label),
                       size = ts, family = fam, lineheight = 0.9) +
    ggplot2::annotate("segment", x = 0, xend = x0 + nc * cell_w, y = cat_df$ybot, yend = cat_df$ybot,
                      colour = palette$grey_dk, linewidth = 0.5) +
    ggplot2::annotate("segment", x = cat_w, xend = x0 + nc * cell_w,
                      y = seq_len(nr) - 0.5, yend = seq_len(nr) - 0.5,
                      colour = palette$grey_lt, linewidth = 0.25) +
    ggplot2::annotate("segment", x = c(cat_w, x0 + (0:nc) * cell_w), xend = c(cat_w, x0 + (0:nc) * cell_w),
                      y = -0.5, yend = nr + 1.5, colour = palette$grey, linewidth = 0.3) +
    ggplot2::annotate("text", x = x0 + (seq_len(nc) - 1) * cell_w + cell_w - 0.5, y = nr + 1,
                      label = cols, hjust = 1, size = ts, family = fam, fontface = "bold") +
    ggplot2::annotate("text", x = c(cat_w / 2, x0 - 0.6), y = nr + 1, label = c("Category", "Reason"),
                      hjust = c(0.5, 1), size = ts, family = fam, fontface = "bold") +
    ggplot2::annotate("text", x = x0 + (seq_len(nc) - 1) * cell_w + cell_w - 0.5, y = 0,
                      label = format(round(x$n), big.mark = ","), hjust = 1, size = ts, family = fam) +
    ggplot2::annotate("text", x = x0 - 0.6, y = 0, label = "N", hjust = 1, size = ts, family = fam) +
    ggplot2::annotate("segment", x = 0, xend = x0 + nc * cell_w, y = c(nr + 0.5, 0.5, -0.5),
                      yend = c(nr + 0.5, 0.5, -0.5), colour = palette$grey_dk, linewidth = 0.5) +
    ggplot2::scale_x_continuous(limits = c(0, x0 + nc * cell_w + 0.5), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(-0.6, nr + 1.6), expand = c(0, 0)) +
    ggplot2::labs(title = title, caption = "Table cells are shaded in proportion to the outcome; an outcome of 100% would fill the cell with colour. Caregivers could give more than one reason.") +
    ggplot2::theme_void(base_size = base_size, base_family = fam) +
    ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0, colour = palette$grey_dk, size = base_size * 0.85),
                   plot.caption.position = "plot",
                   plot.title = ggplot2::element_text(face = "bold", hjust = 0),
                   plot.background = ggplot2::element_rect(fill = "white", colour = NA),
                   plot.margin = ggplot2::margin(4, 6, 4, 4))
  attr(p, "vcs_size") <- c(width = (x0 + nc * cell_w) / 12.5, height = (nr + 5) / 4.4)
  p
}

#' Estimate a yes/no question over the design
#'
#' Weighted proportion answering `yes` among those who answered (don't-know
#' and refusal codes are excluded), optionally by domain. A convenience for
#' the many single questions a report quotes.
#'
#' @param design A [vcs_design()].
#' @param variable Column name in the design data.
#' @param yes Codes counted as yes.
#' @param dk Codes treated as not answered.
#' @param by Domains.
#' @param ... Passed to the estimator (`ci_method`, `level`).
#' @return A `vcs_estimate` tibble.
#' @export
#' @examples
#' \dontrun{
#' estimate_yes_no(des, "ORS01")
#' }
estimate_yes_no <- function(design, variable, yes = "1", dk = c("98", "99"), by = NULL, ...) {
  assert_design(design)
  assert_string(variable)
  if (!variable %in% names(design$data)) {
    vcs_abort(sprintf("Column \"%s\" not in the design data.", variable), class = "vaxsurvR_value_error")
  }
  v <- trimws(as.character(design$data[[variable]]))
  ind <- ifelse(is.na(v) | !nzchar(v) | v %in% dk, NA_integer_, as.integer(v %in% yes))
  design$data[[".yn"]] <- ind
  design$design$variables[[".yn"]] <- ind
  out <- estimate_indicator(design, ".yn", by, type = "yes_no", ...)
  out$indicator <- variable
  out
}
