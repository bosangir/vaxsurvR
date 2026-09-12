# ---------------------------------------------------------------------------
# De-identification and disclosure control.
#
# Two invariants hold throughout this module:
#   * the salt is never written anywhere -- not to the output data, not to the
#     log, not to the console, not into an object that gets saved;
#   * there is no default salt. Deterministic hashing without a secret is
#     reversible by dictionary attack on a small identifier space, so the user
#     must supply one.
# ---------------------------------------------------------------------------

#' Patterns that identify likely direct identifiers
#' @noRd
pii_patterns <- function() {
  list(
    name = "(^|_)(name|nom|prenom|surname|firstname|lastname)($|_)",
    telephone = "(^|_)(phone|tel|telephone|mobile|cell|gsm)($|_)",
    address = "(^|_)(address|adresse|street|rue|avenue|village_name|house_no)($|_)",
    gps = "(^|_)(gps|lat|latitude|lon|lng|longitude|geopoint|coord)($|_)",
    device = "(^|_)(device_id|imei|sim|subscriber|deviceid)($|_)",
    media = "(^|_)(photo|image|audio|recording|signature)($|_)",
    free_text = "(^|_)(note|notes|comment|observation|remarque|other|autre)($|_)",
    date_of_birth = "(^|_)(dob|birth|naissance)($|_)"
  )
}

#' Identify potential personal information in a dataset
#'
#' Scans column names against a pattern library and inspects values for
#' telephone-like and coordinate-like content. The result is a *starting point
#' for human review*, not a guarantee: free-text fields can hold names that no
#' automated scan will catch, so read them before release.
#'
#' @param x A [vcs_data][new_vcs_data] object, or a data frame.
#' @param dictionary An optional [vcs_dictionary()]. Concepts known to be direct
#'   identifiers (`child_name`, `caregiver_name`, `telephone`, `address`,
#'   `gps_lat`, `gps_lon`) are reported with certainty rather than by pattern.
#' @param extra_patterns Named character vector of additional regular
#'   expressions to match against column names.
#' @return A tibble with columns `variable`, `category`, `basis`, `confidence`
#'   and `n_nonmissing`.
#' @export
#' @seealso [deidentify_vcs()], [assess_reidentification_risk()]
#' @examples
#' identify_pii(vcs_example_raw, vcs_example_dictionary)
identify_pii <- function(x, dictionary = NULL, extra_patterns = NULL) {
  tab <- if (is_vcs_data(x)) {
    dictionary <- dictionary %||% x$dictionary
    if (!is.null(x$raw)) x$raw else vcs_children(x)
  } else {
    assert_data(x)
    x
  }
  assert_character(extra_patterns)

  patterns <- c(pii_patterns(), as.list(extra_patterns %||% character(0)))
  out <- list()

  # 1. Concepts the dictionary names outright.
  if (!is.null(dictionary) && is_vcs_dictionary(dictionary)) {
    direct <- c(child_name = "name", caregiver_name = "name",
                telephone = "telephone", address = "address",
                gps_lat = "gps", gps_lon = "gps")
    for (concept in intersect(names(direct), names(dictionary$map))) {
      res <- resolve_concept(dictionary, concept, NULL)
      cols <- intersect(res$column, names(tab))
      if (length(cols)) {
        out[[length(out) + 1L]] <- tibble::tibble(
          variable = cols,
          category = unname(direct[concept]),
          basis = sprintf("dictionary concept \"%s\"", concept),
          confidence = "certain"
        )
      }
    }
  }

  # 2. Column names matching the pattern library.
  for (cat in names(patterns)) {
    hits <- grep(patterns[[cat]], names(tab), value = TRUE, ignore.case = TRUE)
    if (length(hits)) {
      out[[length(out) + 1L]] <- tibble::tibble(
        variable = hits, category = cat, basis = "column name pattern",
        confidence = "likely"
      )
    }
  }

  # 3. Values that look like telephone numbers or coordinates.
  for (nm in names(tab)) {
    v <- tab[[nm]]
    if (!is.character(v) || !length(v)) next
    ok <- v[!is_blank(v)]
    if (!length(ok)) next
    sample_v <- utils::head(ok, 200)
    if (mean(grepl("^\\+?[0-9][0-9 .()-]{7,}$", sample_v)) > 0.8) {
      out[[length(out) + 1L]] <- tibble::tibble(
        variable = nm, category = "telephone", basis = "values look like phone numbers",
        confidence = "likely"
      )
    }
  }

  if (!length(out)) {
    return(tibble::tibble(variable = character(0), category = character(0),
                          basis = character(0), confidence = character(0),
                          n_nonmissing = integer(0)))
  }
  res <- dplyr::bind_rows(out)
  res <- res[order(res$variable, match(res$confidence, c("certain", "likely"))), ]
  res <- res[!duplicated(paste(res$variable, res$category)), , drop = FALSE]
  res$n_nonmissing <- vapply(res$variable, function(v) {
    sum(!is_blank(tab[[v]]))
  }, integer(1))
  tibble::as_tibble(res)
}

#' Deterministically hash an identifier
#'
#' Produces a stable pseudonym: the same input and salt always give the same
#' output, so records can be linked across files and survey rounds, while the
#' original value cannot be recovered without the salt.
#'
#' @param x Vector of identifiers.
#' @param salt Secret salt. There is no default: without a salt, a hash of a
#'   short identifier is trivially reversible.
#' @param prefix Prefix prepended to each code, e.g. `"CH"`.
#' @param length Number of hexadecimal characters to keep. Shorter codes are
#'   more readable but collide sooner; the default of 12 is safe for the
#'   identifier volumes household surveys produce.
#' @param namespace Optional string mixed into the hash so that the same
#'   identifier hashed for two different purposes gives two different codes.
#' @param algo Hash algorithm passed to [digest::digest()].
#' @return A character vector of pseudonyms. Missing and blank inputs stay
#'   missing.
#' @export
#' @seealso [deidentify_vcs()]
#' @examples
#' hash_identifier(c("CH0001", "CH0002"), salt = "example-salt-not-for-production")
#'
#' # Deterministic: the same input and salt give the same code.
#' identical(
#'   hash_identifier("CH0001", salt = "s"),
#'   hash_identifier("CH0001", salt = "s")
#' )
#'
#' # A different namespace gives a different code for the same input.
#' hash_identifier("CH0001", salt = "s", namespace = "household")
hash_identifier <- function(x, salt, prefix = "", length = 12L,
                            namespace = NULL, algo = "sha256") {
  if (missing(salt)) {
    vcs_abort(
      paste0("`salt` is required. Read it from an environment variable, ",
             "e.g. `Sys.getenv(\"VAXSURVR_SALT\")`; never hard-code it."),
      class = "vaxsurvR_salt_error"
    )
  }
  assert_string(salt)
  if (!nzchar(salt)) {
    vcs_abort(
      "`salt` is empty. Set a real secret salt before de-identifying.",
      class = "vaxsurvR_salt_error"
    )
  }
  assert_string(prefix)
  assert_number(length, lower = 4, upper = 64)
  assert_string(namespace, allow_null = TRUE)
  assert_string(algo)

  chr <- as.character(x)
  blank <- is_blank(chr)
  out <- rep(NA_character_, length(chr))
  if (!all(blank)) {
    keys <- paste0(salt, "|", namespace %||% "", "|", chr[!blank])
    digests <- vapply(keys, function(k) {
      digest::digest(k, algo = algo, serialize = FALSE)
    }, character(1), USE.NAMES = FALSE)
    out[!blank] <- paste0(prefix, toupper(substr(digests, 1L, as.integer(length))))
  }
  out
}

#' Remove direct identifiers from a dataset
#'
#' @param x A [vcs_data][new_vcs_data] object, or a data frame.
#' @param variables Columns to remove. Defaults to everything
#'   [identify_pii()] reports with `confidence == "certain"`.
#' @param dictionary An optional [vcs_dictionary()] used to find identifiers.
#' @return The input with the columns removed.
#' @export
#' @examples
#' d <- remove_direct_identifiers(vcs_example)
#' "child_name" %in% names(d$children)
remove_direct_identifiers <- function(x, variables = NULL, dictionary = NULL) {
  if (is.null(variables)) {
    concepts <- c("child_name", "caregiver_name", "telephone", "address")
    if (is_vcs_data(x)) {
      variables <- intersect(concepts, c(names(x$children), names(x$households)))
    } else {
      pii <- identify_pii(x, dictionary)
      variables <- pii$variable[pii$confidence == "certain"]
    }
  }
  assert_character(variables, allow_null = FALSE)
  if (is_vcs_data(x)) {
    for (tb in c("children", "households", "vaccinations")) {
      drop <- intersect(variables, names(x[[tb]]))
      if (length(drop)) {
        x[[tb]] <- x[[tb]][, setdiff(names(x[[tb]]), drop), drop = FALSE]
      }
    }
    return(x)
  }
  assert_data(x)
  x[, setdiff(names(x), variables), drop = FALSE]
}

#' Generalise age to coarser bands
#'
#' @param x A [vcs_data][new_vcs_data] object, or a numeric vector of ages in
#'   months.
#' @param breaks Band boundaries in months.
#' @param labels Optional band labels.
#' @param variable Name of the age column when `x` is a `vcs_data`.
#' @param new_name Name of the banded column.
#' @param drop_exact Remove the exact age and date-of-birth columns after
#'   banding. `TRUE` by default: an exact date of birth is a quasi-identifier.
#' @return A banded factor, or the input with the banded column added.
#' @export
#' @examples
#' generalize_age(c(11, 13, 18, 23), breaks = c(0, 12, 18, 24))
#' d <- generalize_age(vcs_example)
#' table(d$children$age_band)
generalize_age <- function(x, breaks = c(0, 12, 18, 24, Inf), labels = NULL,
                           variable = "age_months", new_name = "age_band",
                           drop_exact = TRUE) {
  assert_string(variable)
  assert_string(new_name)
  assert_flag(drop_exact)
  if (!is.numeric(breaks) || length(breaks) < 2L) {
    vcs_abort("`breaks` must be a numeric vector of at least two boundaries.",
              class = "vaxsurvR_value_error")
  }
  band <- function(v) {
    cut(suppressWarnings(as.numeric(v)), breaks = breaks, labels = labels,
        right = FALSE, include.lowest = TRUE)
  }
  if (!is_vcs_data(x)) {
    return(band(x))
  }
  ch <- x$children
  if (!variable %in% names(ch)) {
    if ("child_dob" %in% names(ch) && "interview_date" %in% names(vcs_children(x))) {
      ch[[variable]] <- child_age_months(vcs_children(x))
    } else {
      vcs_abort(
        sprintf("Column \"%s\" not found and age cannot be derived.", variable),
        class = "vaxsurvR_value_error"
      )
    }
  }
  ch[[new_name]] <- mark_derived(
    band(ch[[variable]]),
    sprintf("%s banded at %s months", variable, paste(breaks, collapse = "/"))
  )
  if (drop_exact) {
    ch <- ch[, setdiff(names(ch), c(variable, "child_dob")), drop = FALSE]
  }
  x$children <- ch
  x
}

#' Generalise or suppress geography
#'
#' Coarsens the geographic detail released with a dataset. GPS coordinates are
#' the highest-risk variable in a household survey: a coordinate pair plus a
#' health zone identifies a dwelling.
#'
#' @param x A [vcs_data][new_vcs_data] object, or a data frame.
#' @param drop Geographic columns to remove outright. Defaults to GPS
#'   coordinates and accuracy.
#' @param round_gps Number of decimal places to keep on coordinates instead of
#'   dropping them. `NULL` (default) means drop rather than round. Two decimals
#'   is roughly a kilometre.
#' @param lowest_level Finest administrative level to keep: any of
#'   `"province"`, `"district"`, `"health_zone"`, `"health_area"`. Levels below
#'   it are removed.
#' @param min_cell Suppress any geographic value observed in fewer than this
#'   many records, replacing it with `NA`. Small cells are what makes
#'   geography identifying.
#' @return The input with geography coarsened.
#' @export
#' @seealso [assess_reidentification_risk()]
#' @examples
#' d <- generalize_geography(vcs_example, lowest_level = "district")
#' names(d$households)
#'
#' # Round rather than drop coordinates.
#' d2 <- generalize_geography(vcs_example, round_gps = 2)
#' head(d2$households$gps_lat)
generalize_geography <- function(x,
                                 drop = NULL,
                                 round_gps = NULL,
                                 lowest_level = NULL,
                                 min_cell = 0L) {
  assert_character(drop)
  assert_number(round_gps, lower = 0, upper = 10, allow_null = TRUE)
  assert_string(lowest_level, allow_null = TRUE)
  assert_number(min_cell, lower = 0)

  hierarchy <- c("province", "district", "health_zone", "health_area")
  if (!is.null(lowest_level)) {
    assert_choice(lowest_level, hierarchy)
  }
  gps_cols <- c("gps_lat", "gps_lon", "gps_accuracy")
  if (is.null(drop)) {
    drop <- if (is.null(round_gps)) gps_cols else "gps_accuracy"
  }
  if (!is.null(lowest_level)) {
    keep_to <- match(lowest_level, hierarchy)
    drop <- unique(c(drop, hierarchy[-seq_len(keep_to)]))
  }

  apply_to <- function(tab) {
    if (!is.null(round_gps)) {
      for (nm in intersect(c("gps_lat", "gps_lon"), names(tab))) {
        tab[[nm]] <- round(suppressWarnings(as.numeric(tab[[nm]])), round_gps)
      }
    }
    if (min_cell > 0) {
      for (nm in intersect(hierarchy, names(tab))) {
        v <- as.character(tab[[nm]])
        counts <- table(v)
        small <- names(counts)[counts < min_cell]
        v[v %in% small] <- NA_character_
        tab[[nm]] <- v
      }
    }
    tab[, setdiff(names(tab), intersect(drop, names(tab))), drop = FALSE]
  }

  if (is_vcs_data(x)) {
    x$households <- apply_to(x$households)
    x$children <- apply_to(x$children)
    return(x)
  }
  assert_data(x)
  apply_to(x)
}

#' Shift dates by a constant, record-specific offset
#'
#' Applies the same offset to every date belonging to one record, so that
#' intervals between doses -- the analytically useful quantity -- are preserved
#' exactly while calendar dates are no longer the real ones.
#'
#' The offsets are derived from the salt, so a shift is reproducible for a team
#' holding the salt and irrecoverable for anyone else. They are never returned
#' or written.
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param salt Secret salt. Required.
#' @param max_days Maximum absolute shift, in days.
#' @param by Record level at which the offset is constant: `"household"`
#'   (default), `"child"` or `"psu"`. A coarser level preserves more
#'   within-group structure.
#' @param variables Date columns to shift. Defaults to every `Date` column in
#'   the child, household and vaccination tables.
#' @return `x`, with the dates shifted.
#' @export
#' @examples
#' d <- shift_dates(vcs_example, salt = "example-salt-not-for-production")
#' # Intervals between doses are unchanged.
#' a <- vcs_example$vaccinations
#' b <- d$vaccinations
#' identical(
#'   diff(a$card_date[a$child_id == a$child_id[1]]),
#'   diff(b$card_date[b$child_id == b$child_id[1]])
#' )
shift_dates <- function(x, salt, max_days = 30L,
                        by = c("household", "child", "psu"),
                        variables = NULL) {
  assert_vcs_data(x)
  if (missing(salt)) {
    vcs_abort("`salt` is required so that the shift is reproducible for the data owner.",
              class = "vaxsurvR_salt_error")
  }
  assert_string(salt)
  if (!nzchar(salt)) {
    vcs_abort("`salt` is empty.", class = "vaxsurvR_salt_error")
  }
  assert_number(max_days, lower = 1)
  by <- match.arg(by)
  assert_character(variables)

  key_col <- switch(by, household = "household_id", child = "child_id", psu = "psu")
  ch <- vcs_children(x)
  if (!key_col %in% names(ch)) {
    vcs_abort(sprintf("Column \"%s\" is needed to shift dates by %s.", key_col, by),
              class = "vaxsurvR_value_error")
  }

  # A deterministic integer offset per key, derived from the salt without
  # exposing it.
  offset_for <- function(keys) {
    uk <- unique(as.character(keys))
    digests <- vapply(uk, function(k) {
      digest::digest(paste0(salt, "|dateshift|", k), algo = "xxhash64",
                     serialize = FALSE)
    }, character(1), USE.NAMES = FALSE)
    span <- 2L * as.integer(max_days) + 1L
    off <- as.integer(strtoi(substr(digests, 1L, 7L), 16L) %% span) - as.integer(max_days)
    stats::setNames(off, uk)
  }
  lut <- offset_for(ch[[key_col]])

  child_offsets <- unname(lut[as.character(ch[[key_col]])])
  names(child_offsets) <- ch$child_id
  hh_offsets <- if (key_col == "household_id") {
    lut
  } else {
    stats::setNames(child_offsets[match(x$households$interview_id, ch$interview_id)],
                    x$households$interview_id)
  }

  shift_table <- function(tab, offsets) {
    cols <- if (is.null(variables)) {
      names(tab)[vapply(tab, inherits, logical(1), "Date")]
    } else {
      intersect(variables, names(tab))
    }
    for (nm in cols) {
      tab[[nm]] <- tab[[nm]] + offsets
    }
    tab
  }

  x$children <- shift_table(x$children, unname(child_offsets[x$children$child_id]))
  x$households <- shift_table(
    x$households,
    unname(hh_offsets[as.character(x$households$interview_id)])
  )
  if (nrow(x$vaccinations)) {
    x$vaccinations <- shift_table(
      x$vaccinations,
      unname(child_offsets[x$vaccinations$child_id])
    )
  }
  x$meta$dates_shifted <- sprintf("shifted by a constant offset per %s, +/- %d days",
                                  by, as.integer(max_days))
  x
}

#' De-identify a survey dataset
#'
#' Applies the whole privacy pipeline in one call: removal of direct
#' identifiers, deterministic hashing of the identifiers needed for linkage,
#' geographic coarsening and optional date shifting.
#'
#' The salt is used and discarded. It is never written to the output data, the
#' de-identification report, the object's metadata, or the console.
#'
#' @param x A [vcs_data][new_vcs_data] object.
#' @param remove Columns to delete outright. Defaults to the direct identifiers
#'   the dictionary names.
#' @param hash Identifier columns to replace with deterministic pseudonyms.
#' @param salt Secret salt, required whenever `hash` is non-empty or
#'   `shift_dates = TRUE`. Read it from the environment; never hard-code it.
#' @param geography Arguments passed to [generalize_geography()], as a list.
#'   `NULL` disables geographic treatment.
#' @param age_bands Breaks passed to [generalize_age()]. `NULL` leaves age
#'   alone.
#' @param shift_dates Shift dates with [shift_dates()].
#' @param max_shift_days Maximum absolute date shift.
#' @param drop_raw Drop the preserved raw import, which still contains the
#'   identifiers. `TRUE` by default, and it should stay `TRUE` for anything
#'   that leaves the data-management team.
#' @return `x`, de-identified, with a `deidentification` element recording what
#'   was done (but never the salt).
#' @export
#' @seealso [identify_pii()], [assess_reidentification_risk()]
#' @examples
#' # In production, read the salt from the environment:
#' #   salt <- Sys.getenv("VAXSURVR_SALT")
#' anon <- deidentify_vcs(
#'   vcs_example,
#'   hash = c("child_id", "household_id"),
#'   salt = "example-salt-not-for-production"
#' )
#' anon$deidentification
#' head(anon$children$child_id)
deidentify_vcs <- function(x,
                           remove = NULL,
                           hash = character(0),
                           salt = NULL,
                           geography = list(round_gps = NULL),
                           age_bands = NULL,
                           shift_dates = FALSE,
                           max_shift_days = 30L,
                           drop_raw = TRUE) {
  assert_vcs_data(x)
  assert_character(hash, allow_null = FALSE)
  assert_flag(shift_dates)
  assert_flag(drop_raw)
  needs_salt <- length(hash) > 0L || isTRUE(shift_dates)
  if (needs_salt) {
    if (is.null(salt) || !is.character(salt) || length(salt) != 1L || !nzchar(salt)) {
      vcs_abort(
        paste0("A non-empty `salt` is required to hash identifiers or shift dates. ",
               "Read it from an environment variable such as ",
               "`Sys.getenv(\"VAXSURVR_SALT\")`; vaxsurvR ships no default salt."),
        class = "vaxsurvR_salt_error"
      )
    }
  }

  actions <- character(0)

  removed <- if (is.null(remove)) {
    intersect(c("child_name", "caregiver_name", "telephone", "address"),
              c(names(x$children), names(x$households)))
  } else {
    remove
  }
  if (length(removed)) {
    x <- remove_direct_identifiers(x, variables = removed)
    actions <- c(actions, sprintf("removed direct identifiers: %s",
                                  paste(removed, collapse = ", ")))
  }

  if (length(hash)) {
    for (nm in hash) {
      for (tb in c("children", "households", "vaccinations")) {
        if (nm %in% names(x[[tb]])) {
          x[[tb]][[nm]] <- hash_identifier(x[[tb]][[nm]], salt = salt,
                                           namespace = nm)
        }
      }
    }
    actions <- c(actions, sprintf("hashed identifiers (salted SHA-256): %s",
                                  paste(hash, collapse = ", ")))
  }

  if (isTRUE(shift_dates)) {
    x <- shift_dates(x, salt = salt, max_days = max_shift_days)
    actions <- c(actions, sprintf("shifted dates by a per-household offset of +/- %d days",
                                  as.integer(max_shift_days)))
  }

  if (!is.null(age_bands)) {
    x <- generalize_age(x, breaks = age_bands)
    actions <- c(actions, sprintf("banded age at %s months",
                                  paste(age_bands, collapse = "/")))
  }

  if (!is.null(geography)) {
    x <- do.call(generalize_geography, c(list(x), geography))
    actions <- c(actions, "coarsened geography")
  }

  if (drop_raw && !is.null(x$raw)) {
    x$raw <- NULL
    actions <- c(actions, "dropped the preserved raw import")
  }

  # The salt is deliberately absent from everything recorded here.
  x$deidentification <- list(
    actions = actions,
    salt_used = needs_salt,
    salt = NULL,
    timestamp = vcs_timestamp(),
    vaxsurvR_version = vcs_version()
  )
  x
}

#' Assess re-identification risk
#'
#' Computes k-anonymity over a set of quasi-identifiers: for each record, how
#' many other records share its combination of values. Records in small groups
#' are the ones a person with background knowledge could single out.
#'
#' This is a screening tool. It measures only what the chosen quasi-identifiers
#' capture, and says nothing about identifiers hiding in free text.
#'
#' @param x A [vcs_data][new_vcs_data] object, or a data frame.
#' @param quasi_identifiers Columns to treat as quasi-identifiers. Defaults to
#'   the geographic, demographic and design variables usually released.
#' @param k Threshold below which a group is considered risky.
#' @return A list of class `vcs_risk` with the k-anonymity distribution, the
#'   risky groups, and the proportion of records below `k`.
#' @export
#' @seealso [generalize_geography()], [generalize_age()]
#' @examples
#' r <- assess_reidentification_risk(vcs_example)
#' r
#' # Coarsening geography raises k.
#' coarse <- generalize_geography(vcs_example, lowest_level = "province")
#' assess_reidentification_risk(coarse)$prop_below_k
assess_reidentification_risk <- function(x, quasi_identifiers = NULL, k = 5L) {
  assert_number(k, lower = 1)
  tab <- if (is_vcs_data(x)) vcs_children(x) else {
    assert_data(x)
    tibble::as_tibble(x)
  }
  if (is.null(quasi_identifiers)) {
    quasi_identifiers <- intersect(
      c("province", "district", "health_zone", "health_area", "residence",
        "stratum", "sex", "age_months", "age_band"),
      names(tab)
    )
  }
  assert_character(quasi_identifiers, allow_null = FALSE)
  quasi_identifiers <- intersect(quasi_identifiers, names(tab))
  if (!length(quasi_identifiers)) {
    vcs_abort("None of the requested quasi-identifiers is present in the data.",
              class = "vaxsurvR_value_error")
  }
  if (!nrow(tab)) {
    vcs_abort("Cannot assess risk on zero records.", class = "vaxsurvR_value_error")
  }

  key <- do.call(paste, c(
    lapply(quasi_identifiers, function(v) as.character(tab[[v]])), sep = "\r"
  ))
  counts <- table(key)
  k_per_record <- as.integer(counts[key])

  risky <- names(counts)[counts < k]
  risky_tab <- if (length(risky)) {
    parts <- do.call(rbind, strsplit(risky, "\r", fixed = TRUE))
    out <- tibble::as_tibble(
      stats::setNames(as.data.frame(parts, stringsAsFactors = FALSE),
                      quasi_identifiers)
    )
    out$k <- as.integer(counts[risky])
    out[order(out$k), , drop = FALSE]
  } else {
    tibble::tibble()
  }

  structure(
    list(
      quasi_identifiers = quasi_identifiers,
      k = as.integer(k),
      n_records = nrow(tab),
      min_k = min(k_per_record),
      median_k = stats::median(k_per_record),
      n_unique = sum(k_per_record == 1L),
      n_below_k = sum(k_per_record < k),
      prop_below_k = mean(k_per_record < k),
      k_distribution = tibble::tibble(
        k = as.integer(names(table(k_per_record))),
        n_records = as.integer(table(k_per_record))
      ),
      risky_groups = risky_tab
    ),
    class = "vcs_risk"
  )
}

#' @export
print.vcs_risk <- function(x, ...) {
  cat("<vcs_risk: k-anonymity screening>\n")
  cat(sprintf("  quasi-identifiers: %s\n", paste(x$quasi_identifiers, collapse = ", ")))
  cat(sprintf("  records          : %d\n", x$n_records))
  cat(sprintf("  minimum k        : %d\n", x$min_k))
  cat(sprintf("  median k         : %g\n", x$median_k))
  cat(sprintf("  unique records   : %d (%.1f%%)\n", x$n_unique,
              100 * x$n_unique / x$n_records))
  cat(sprintf("  below k = %d      : %d (%.1f%%)\n", x$k, x$n_below_k,
              100 * x$prop_below_k))
  if (x$prop_below_k > 0) {
    cat("  Consider generalize_geography() or generalize_age() before release.\n")
  }
  invisible(x)
}
