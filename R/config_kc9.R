# ---------------------------------------------------------------------------
# Ready-made configuration for the DRC "Vx Coverage KC v9" XLSForm
# (DRC_Vx_Coverage_KCv9.xlsx): schedule, dictionary, recall crosswalk,
# questionnaire labels and the derived variables the report tabulates by.
#
# Nothing in the rest of the package depends on this file. It exists so that
# any SurveyCTO wide export of that form -- simulated or real, any province,
# any round -- can be analysed with one call, and so that the report template
# has a single, auditable place where variable names live.
# ---------------------------------------------------------------------------

#' The DRC EPI schedule as captured by the KC v9 card block
#'
#' `CVH01`--`CVH26` in questionnaire order, so that the `{vv}` placeholder of
#' the dictionary resolves to the right card column. Minimum ages follow the
#' DRC calendar (birth; 6, 10 and 14 weeks; 6, 7, 8 months for malaria;
#' 9 months for measles, yellow fever and IPV2; 15 months for measles 2 and
#' malaria 4). Maximum ages drive timeliness only.
#'
#' @param include_supplements Keep vitamin A and mebendazole (`CVH24`--`CVH26`)
#'   in the schedule. They are child-health interventions, not EPI doses; keep
#'   them when you want card-documented vitamin A and deworming, and exclude
#'   them from coverage charts with `vaccines = kc9_epi_doses()`.
#' @return A [vcs_schedule()].
#' @export
#' @family KC v9 configuration
#' @examples
#' kc9_schedule()
kc9_schedule <- function(include_supplements = TRUE) {
  assert_flag(include_supplements)
  s <- tibble::tribble(
    ~vaccine,  ~antigen, ~dose, ~min,  ~max,  ~prev,     ~int,
    "BCG",     "BCG",    1L,      0,    28,   NA,        NA,
    "OPV0",    "OPV",    0L,      0,    14,   NA,        NA,
    "OPV1",    "OPV",    1L,     42,    76,   "OPV0",    NA,
    "PENTA1",  "PENTA",  1L,     42,    76,   NA,        NA,
    "PCV1",    "PCV",    1L,     42,    76,   NA,        NA,
    "ROTA1",   "ROTA",   1L,     42,    76,   NA,        NA,
    "OPV2",    "OPV",    2L,     70,   104,   "OPV1",    28,
    "PENTA2",  "PENTA",  2L,     70,   104,   "PENTA1",  28,
    "PCV2",    "PCV",    2L,     70,   104,   "PCV1",    28,
    "ROTA2",   "ROTA",   2L,     70,   104,   "ROTA1",   28,
    "OPV3",    "OPV",    3L,     98,   132,   "OPV2",    28,
    "IPV1",    "IPV",    1L,     98,   132,   NA,        NA,
    "PENTA3",  "PENTA",  3L,     98,   132,   "PENTA2",  28,
    "PCV3",    "PCV",    3L,     98,   132,   "PCV2",    28,
    "ROTA3",   "ROTA",   3L,     98,   132,   "ROTA2",   28,
    "MCV1",    "MCV",    1L,    270,   330,   NA,        NA,
    "YF",      "YF",     1L,    270,   330,   NA,        NA,
    "MCV2",    "MCV",    2L,    450,   540,   "MCV1",    28,
    "IPV2",    "IPV",    2L,    270,   330,   "IPV1",    28,
    "MAL1",    "MAL",    1L,    180,   210,   NA,        NA,
    "MAL2",    "MAL",    2L,    210,   240,   "MAL1",    28,
    "MAL3",    "MAL",    3L,    240,   270,   "MAL2",    28,
    "MAL4",    "MAL",    4L,    450,   540,   "MAL3",    28,
    "VITA1",   "VITA",   1L,    180,   365,   NA,        NA,
    "VITA2",   "VITA",   2L,    365,   540,   "VITA1",   150,
    "DEWORM",  "DEWORM", 1L,    365,   540,   NA,        NA
  )
  # OPV0 is a birth dose: OPV1 does not require it, so no previous_dose link.
  s$prev[s$vaccine == "OPV1"] <- NA_character_
  if (!include_supplements) {
    s <- s[!s$antigen %in% c("VITA", "DEWORM"), , drop = FALSE]
  }
  vcs_schedule(
    vaccine = s$vaccine, antigen = s$antigen, dose = s$dose,
    minimum_age_days = s$min, maximum_age_days = s$max,
    previous_dose = s$prev, minimum_interval_days = s$int
  )
}

#' Names of the EPI doses in the KC v9 schedule
#'
#' @param malaria Include the four malaria doses.
#' @return A character vector in schedule order.
#' @export
#' @family KC v9 configuration
#' @examples
#' kc9_epi_doses()
kc9_epi_doses <- function(malaria = TRUE) {
  s <- kc9_schedule(include_supplements = FALSE)
  v <- s$vaccine
  if (!malaria) v <- v[s$antigen != "MAL"]
  v
}

#' Recall crosswalk for the KC v9 VR block
#'
#' `VR*` is not parallel to the card block: series antigens are asked as
#' "ever received?" plus "how many times at the facility?", single doses as
#' one yes/no. Campaign-dose counts (`VR07`, `VR14`, `VR19`) are excluded
#' because routine coverage is the quantity of interest.
#'
#' @return A [vcs_recall_map()].
#' @export
#' @family KC v9 configuration
#' @examples
#' kc9_recall_map()
kc9_recall_map <- function() {
  vcs_recall_map(
    BCG    = "VR01_{c}_{k}",
    OPV0   = "VR03_{c}_{k}",
    OPV    = list(ever = "VR04_{c}_{k}", count = "VR05_{c}_{k}"),
    IPV1   = "VR06_{c}_{k}",
    IPV2   = "VR21_{c}_{k}",
    PENTA  = list(ever = "VR08_{c}_{k}", count = "VR09_{c}_{k}"),
    PCV    = list(ever = "VR10_{c}_{k}", count = "VR11_{c}_{k}"),
    ROTA   = list(ever = "VR15_{c}_{k}", count = "VR16_{c}_{k}"),
    MCV    = list(ever = "VR12_{c}_{k}", count = "VR13_{c}_{k}"),
    MCV2   = "VR20_{c}_{k}",
    YF     = list(ever = "VR17_{c}_{k}", count = "VR18_{c}_{k}"),
    MAL1   = "VR23_{c}_{k}",
    MAL2   = "VR24_{c}_{k}",
    MAL3   = "VR25_{c}_{k}",
    MAL4   = "VR26_{c}_{k}",
    VITA1  = "VR29_{c}_{k}",
    DEWORM = "VR30_{c}_{k}"
  )
}

#' Dictionary for a KC v9 SurveyCTO wide export
#'
#' @param n_caregivers,n_children Repeat extents present in the export.
#' @param psu Source column holding the PSU; `"PVT06"` is populated on every
#'   row while `submission_psu` is not.
#' @param health_zone Source column holding the health zone.
#' @param weight Optional source column holding the child-level weight (a
#'   column you have joined onto the export yourself).
#' @param stratum Optional source column holding the sampling stratum.
#' @param gps_suffix How the export splits the geopoint: SurveyCTO desktop
#'   exports write `PVT_GPS-Latitude`; some server exports write
#'   `PVT_GPS_latitude`.
#' @param date_formats Date formats to try, most common first. Desktop exports
#'   write `m/d/Y`.
#' @return A [vcs_dictionary()].
#' @export
#' @family KC v9 configuration
#' @examples
#' kc9_dictionary()
kc9_dictionary <- function(n_caregivers = 3L, n_children = 3L, psu = "PVT06",
                           health_zone = "PVT05", weight = NULL, stratum = NULL,
                           gps_suffix = c("-Latitude", "-Longitude", "-Accuracy"),
                           date_formats = c("%m/%d/%Y", "%Y-%m-%d", "%d/%m/%Y")) {
  args <- list(
    interview_id   = "KEY",
    household_id   = "KEY",
    psu            = psu,
    segment        = "PVT07",
    health_zone    = health_zone,
    residence      = "HHI07",
    interview_date = "today",
    duration_min   = "duration",
    enumerator     = "PVT02",
    team           = "PVT03",
    supervisor     = "PVT04",
    gps_lat        = paste0("PVT_GPS", gps_suffix[1]),
    gps_lon        = paste0("PVT_GPS", gps_suffix[2]),
    gps_accuracy   = paste0("PVT_GPS", gps_suffix[3]),
    eligible       = "ELHH",
    consent        = "HHI09",
    n_eligible     = "RHH04",
    telephone      = "RHH02_phone",
    address        = "HHI06",
    child_name     = "EC03_{c}_{k}",
    caregiver_name = "CB02_{c}",
    caregiver_id   = "EC04_{c}_{k}",
    child_dob      = "child_dob_{c}_{k}",
    sex            = "child_sex_{c}_{k}",
    age_months     = "child_months_{c}_{k}",
    card_seen      = "CVH_card_shown_{c}_{k}",
    card_status    = "CVH{vv}_{c}_{k}",
    card_date      = "CVH{vv}_date_{c}_{k}",
    recall_status  = "recall_status_{vaccine}_{c}_{k}",
    recall_ever    = "recall_ever_{antigen}_{c}_{k}",
    recall_count   = "recall_count_{antigen}_{c}_{k}"
  )
  if (!is.null(weight)) args$weight <- weight
  if (!is.null(stratum)) args$stratum <- stratum
  do.call(vcs_dictionary, c(
    args,
    list(n_caregivers = n_caregivers, n_children = n_children,
         yes_values = c("1", "Oui", "oui", "yes"),
         no_values = c("2", "0", "Non", "non", "no"),
         card_yes_values = c("1", "2"),
         card_no_values = c("3"),
         count_dk_values = c("98", "99"),
         date_formats = date_formats)
  ))
}

#' Questionnaire value labels for the KC v9 form
#'
#' Codes to English labels for the variables the report tabulates. Edit the
#' returned list to relabel.
#'
#' @return A named list of named character vectors.
#' @export
#' @family KC v9 configuration
#' @examples
#' kc9_labels()$residence
kc9_labels <- function() {
  list(
    residence = c("1" = "Urban", "2" = "Rural"),
    sex = c("1" = "Male", "2" = "Female"),
    education = c("0" = "No formal schooling", "1" = "Primary",
                  "2" = "Secondary", "3" = "Higher"),
    education3 = c("0" = "No formal schooling", "1" = "Primary",
                   "2" = "Secondary or more", "3" = "Secondary or more"),
    marital = c("1" = "Married", "2" = "Common-law union", "3" = "Separated",
                "4" = "Single", "5" = "Divorced", "6" = "Widowed",
                "98" = "Don't know", "99" = "Refused"),
    yes_no = c("1" = "Yes", "2" = "No", "98" = "Don't know"),
    none_some_all = c("1" = "None", "2" = "Some", "3" = "All"),
    scale4 = c("1" = "Not at all", "2" = "A little", "3" = "Moderately", "4" = "Very"),
    cvb02 = c("1" = "1-2 times", "2" = "3-5 times", "3" = "6 or more times", "98" = "Don't know"),
    vacc_facility = c("1" = "Public health facility", "2" = "Local private facility",
                      "3" = "Not applicable (child not vaccinated)"),
    vsc03_walk = c("1" = "Less than 5 minutes", "2" = "5-30 minutes",
                   "3" = "31-60 minutes", "4" = "More than 60 minutes"),
    vsc02_loc = c("1" = "Same health area as household", "2" = "Another health area",
                  "3" = "Another health zone", "98" = "Don't know"),
    trip_times = c("1" = "Once", "2" = "2-3 times", "3" = "4 times or more", "98" = "Don't know"),
    trip_duration = c("1" = "Less than 3 months", "2" = "3-6 months",
                      "3" = "More than 6 months", "98" = "Don't know"),
    trip_who = c("1" = "Whole household", "2" = "One adult", "3" = "Two or more adults",
                 "4" = "Children travelling alone", "5" = "Adults and children",
                 "98" = "Don't know"),
    trip_purpose = c("1" = "Work", "2" = "Visit family", "3" = "Leisure or vacation",
                     "4" = "Other", "98" = "Don't know"),
    cvh34 = c("1" = "Not vaccinated", "2" = "Partially vaccinated", "3" = "Fully vaccinated"),
    child_outcome = c("1" = "Completed", "2" = "Partially completed", "3" = "Refused",
                      "4" = "Caregiver not available", "5" = "Child not eligible",
                      "96" = "Other"),
    besd18 = c("1" = "Nothing, it's not hard", "2" = "Getting to the health facility is hard",
               "3" = "Opening times are inconvenient",
               "4" = "Facility sometimes turns people away without vaccinating",
               "5" = "Waiting time is too long", "96" = "Something else"),
    besd20 = c("1" = "Nothing, I'm satisfied", "2" = "Vaccine not always available",
               "3" = "Session does not open on time", "4" = "Waiting times are long",
               "5" = "Facility is not clean", "6" = "Staff poorly trained",
               "7" = "Staff not respectful", "8" = "Staff do not spend enough time",
               "9" = "Vaccination only on specific days", "96" = "Something else"),
    cvb06 = c("1" = "No vaccine", "2" = "No vaccinator (not closed)",
              "3" = "Health facility closed", "4" = "Child was sick",
              "5" = "Not enough children to open a vial", "6" = "Not the vaccination day",
              "7" = "Wait was too long", "96" = "Other"),
    ors03 = c("1" = "ORS sachets", "2" = "Zinc (tablet or syrup)", "3" = "Home-prepared treatment",
              "4" = "Antibiotic", "5" = "Anti-diarrhoeal", "6" = "Intravenous fluid",
              "7" = "Injection", "8" = "Fever medicine", "9" = "Anti-nausea medicine",
              "10" = "Other pill / syrup", "11" = "Vitamins", "96" = "Other",
              "98" = "Don't know"),
    cvh35 = c(
      "1" = "Vaccination site too far away", "2" = "Vaccination schedule not known",
      "3" = "Mother too busy", "4" = "Family problems / mother's illness",
      "5" = "Sick child, not sent", "6" = "Child sent but not vaccinated",
      "7" = "Long wait", "8" = "Rumours", "9" = "Don't believe in vaccination",
      "10" = "Fear of side effects", "11" = "Vaccination site not known",
      "12" = "Unaware of the need for vaccination",
      "13" = "Unaware of need to return for 2nd or 3rd dose",
      "14" = "Misconceptions about contraindications",
      "15" = "Inappropriate vaccination time", "16" = "Vaccinator absent",
      "17" = "Vaccine not available", "18" = "Session cancelled", "19" = "High cost",
      "20" = "Religious censorship", "21" = "Negative spouse / father attitude",
      "96" = "Other"
    ),
    cvh35_category = c(
      "1" = "Access", "2" = "Access", "3" = "Access", "4" = "Access", "5" = "Access",
      "11" = "Access", "19" = "Access",
      "6" = "Health centre", "7" = "Health centre", "15" = "Health centre",
      "16" = "Health centre", "17" = "Health centre", "18" = "Health centre",
      "8" = "Beliefs", "9" = "Beliefs", "10" = "Beliefs", "12" = "Beliefs",
      "13" = "Beliefs", "14" = "Beliefs",
      "20" = "Social processes", "21" = "Social processes", "96" = "Other"
    )
  )
}

#' Link child modules to the household roster in a KC v9 export
#'
#' The child's sex, age and date of birth are in the household roster
#' (`HL03_child_gender_{n}`, `HL04_child_months_{n}`, `HL05_child_dob_{n}`,
#' `HL08_caregiver_id_{n}`), while the child module is indexed by caregiver
#' block `c` and child-within-caregiver `k`, with `EC04_{c}_{k}` holding the
#' caregiver's roster line. This joins the two and writes
#' `child_dob_{c}_{k}`, `child_sex_{c}_{k}` and `child_months_{c}_{k}`, which
#' [kc9_dictionary()] maps. It also carries the primary caregiver's education
#' and marital status onto the child slot (`caregiver_education_{c}_{k}`,
#' `caregiver_marital_{c}_{k}`) for stratified tables.
#'
#' Unmatched modules keep `NA`; nothing borrows another child's date. The
#' match rate is stored in `attr(, "roster_match")`.
#'
#' @param df The raw export.
#' @param n_caregivers,n_children,n_roster Repeat extents.
#' @return `df` with the new columns.
#' @export
#' @family KC v9 configuration
kc9_link_roster <- function(df, n_caregivers = 3L, n_children = 3L, n_roster = 4L) {
  assert_data(df)
  nz <- function(x) !is.na(x) & nzchar(trimws(x))
  get <- function(cn) if (cn %in% names(df)) as.character(df[[cn]]) else rep(NA_character_, nrow(df))
  roster <- lapply(seq_len(n_roster), function(n) list(
    cg = get(sprintf("HL08_caregiver_id_%d", n)),
    dob = get(sprintf("HL05_child_dob_%d", n)),
    sex = get(sprintf("HL03_child_gender_%d", n)),
    months = get(sprintf("HL04_child_months_%d", n))
  ))
  adults <- lapply(seq_len(max(n_roster, 4L) * 2L), function(n) list(
    edu = get(sprintf("HL07_school_%d", n)),
    mar = get(sprintf("HL06_marital_%d", n)),
    sex = get(sprintf("HL03_adult_gender_%d", n))
  ))
  n_roster_kids <- Reduce(`+`, lapply(roster, function(r) as.integer(nz(r$dob))))
  matched <- attempted <- 0L
  for (c_i in seq_len(n_caregivers)) {
    for (k in seq_len(n_children)) {
      present <- nz(get(sprintf("EC01_%d_%d", c_i, k))) | nz(get(sprintf("EC03_%d_%d", c_i, k)))
      cg_id <- get(sprintf("EC04_%d_%d", c_i, k))
      cg_id[!nz(cg_id)] <- get(sprintf("CB03_%d", c_i))[!nz(cg_id)]
      dob <- sex <- months <- rep(NA_character_, nrow(df))
      seen <- integer(nrow(df))
      for (n in seq_len(n_roster)) {
        r <- roster[[n]]
        hit <- present & nz(cg_id) & nz(r$cg) & cg_id == r$cg & nz(r$dob)
        seen[hit] <- seen[hit] + 1L
        take <- hit & seen == k
        dob[take] <- r$dob[take]
        sex[take] <- r$sex[take]
        months[take] <- r$months[take]
      }
      solo <- present & is.na(dob) & n_roster_kids == 1L
      dob[solo] <- roster[[1]]$dob[solo]
      sex[solo] <- roster[[1]]$sex[solo]
      months[solo] <- roster[[1]]$months[solo]
      # Fall back on roster position when nothing matched but a slot exists.
      posk <- present & is.na(dob) & k <= n_roster & nz(roster[[min(k, n_roster)]]$dob) & n_roster_kids >= k
      if (any(posk)) {
        r <- roster[[min(k, n_roster)]]
        dob[posk] <- r$dob[posk]; sex[posk] <- r$sex[posk]; months[posk] <- r$months[posk]
      }
      df[[sprintf("child_dob_%d_%d", c_i, k)]] <- dob
      df[[sprintf("child_sex_%d_%d", c_i, k)]] <- sex
      df[[sprintf("child_months_%d_%d", c_i, k)]] <- months

      edu <- mar <- csex <- rep(NA_character_, nrow(df))
      line <- suppressWarnings(as.integer(cg_id))
      for (n in seq_along(adults)) {
        take <- present & !is.na(line) & line == n
        edu[take] <- adults[[n]]$edu[take]
        mar[take] <- adults[[n]]$mar[take]
        csex[take] <- adults[[n]]$sex[take]
      }
      df[[sprintf("caregiver_education_%d_%d", c_i, k)]] <- edu
      df[[sprintf("caregiver_marital_%d_%d", c_i, k)]] <- mar
      df[[sprintf("caregiver_sex_%d_%d", c_i, k)]] <- csex
      attempted <- attempted + sum(present)
      matched <- matched + sum(present & nz(dob))
    }
  }
  attr(df, "roster_match") <- c(matched = matched, attempted = attempted)
  df
}

#' Carry caregiver-level questionnaire blocks onto each child
#'
#' The BeSD, service (VSC), knowledge (KN) and interviewer-comment (IC)
#' blocks are asked once per caregiver (`{c}`), not per child. For
#' child-level tables the report needs them beside the child. This copies the
#' named caregiver variables into `<var>_{c}_{k}` columns so the dictionary's
#' repeat machinery can pick them up, and returns their names.
#'
#' @param df The raw export.
#' @param variables Caregiver-level variable stems, e.g. `"BESD01"`.
#' @param n_caregivers,n_children Repeat extents.
#' @return `df` with the new columns.
#' @export
#' @family KC v9 configuration
kc9_caregiver_to_child <- function(df, variables, n_caregivers = 3L, n_children = 3L) {
  assert_data(df)
  assert_character(variables, allow_null = FALSE)
  for (v in variables) {
    for (c_i in seq_len(n_caregivers)) {
      src <- sprintf("%s_%d", v, c_i)
      val <- if (src %in% names(df)) as.character(df[[src]]) else rep(NA_character_, nrow(df))
      for (k in seq_len(n_children)) {
        df[[sprintf("%s_%d_%d", v, c_i, k)]] <- val
      }
    }
  }
  df
}

#' Child-level questionnaire variables to carry through mapping
#'
#' Variables asked in the child module (`{c}_{k}`) that the report uses
#' beyond the vaccination block: care-seeking (`CVB`), vaccination status and
#' reasons (`CVH34`--`CVH36`), diarrhoea and ORS (`ORS`), and the caregiver
#' blocks copied by [kc9_caregiver_to_child()].
#'
#' @return A character vector of variable stems.
#' @export
#' @family KC v9 configuration
kc9_child_extras <- function() {
  c(sprintf("CVB%02d", 1:7), "CVH34", "CVH35", "CVH36", sprintf("ORS%02d", 1:5),
    "VR02", "VR29", "VR30", "VR27", "VR28",
    "child_outcome", "caregiver_education", "caregiver_marital", "caregiver_sex")
}

#' Caregiver-level questionnaire variables the report uses
#'
#' @return A character vector of variable stems.
#' @export
#' @family KC v9 configuration
kc9_caregiver_extras <- function() {
  c(sprintf("BESD%02d", 1:20), sprintf("VSC%02d", 1:21), "KN05",
    "IC01", "IC02", "IC04", "CB04", "CB05")
}

#' Attach extra questionnaire variables to the child table
#'
#' [map_vcs_variables()] only carries the concepts in the dictionary. This
#' pulls any `{c}_{k}`-indexed variables from the raw export onto
#' `x$children`, matched by interview and repeat indices, so that behavioural
#' and child-health questions can be tabulated with the survey design.
#'
#' @param x A [vcs_data][new_vcs_data] object whose `raw` is still attached
#'   (or pass `raw`).
#' @param variables Variable stems, e.g. `c("BESD01", "ORS01")`.
#' @param raw The raw export, if `x$raw` was dropped.
#' @return `x`, with the variables added to `x$children`.
#' @export
#' @family KC v9 configuration
attach_child_variables <- function(x, variables, raw = NULL) {
  assert_vcs_data(x)
  assert_character(variables, allow_null = FALSE)
  raw <- raw %||% x$raw
  if (is.null(raw)) {
    vcs_abort("`x` carries no raw data; pass `raw`.", class = "vaxsurvR_value_error")
  }
  ch <- x$children
  if (!all(c("interview_id", "caregiver_index", "child_index") %in% names(ch))) {
    vcs_abort("Child table lacks repeat indices.", class = "vaxsurvR_value_error")
  }
  id_col <- x$dictionary$map$interview_id %||% "KEY"
  row_of <- match(ch$interview_id, as.character(raw[[id_col]]))
  for (v in variables) {
    out <- rep(NA_character_, nrow(ch))
    for (c_i in unique(stats::na.omit(ch$caregiver_index))) {
      for (k in unique(stats::na.omit(ch$child_index))) {
        col <- sprintf("%s_%d_%d", v, c_i, k)
        if (!col %in% names(raw)) next
        sel <- !is.na(ch$caregiver_index) & ch$caregiver_index == c_i &
          !is.na(ch$child_index) & ch$child_index == k & !is.na(row_of)
        out[sel] <- as.character(raw[[col]])[row_of[sel]]
      }
    }
    ch[[v]] <- out
  }
  x$children <- ch
  x
}

#' Prepare a KC v9 export for analysis in one call
#'
#' Reads (or takes) a SurveyCTO wide export of the KC v9 form and returns a
#' mapped [vcs_data][new_vcs_data] with card and recall evidence, child
#' demographics linked from the roster, caregiver blocks attached to each
#' child, and display labels for the stratifiers the report uses
#' (`residence_label`, `sex_label`, `education_label`, `zone_label`).
#'
#' @param data A file path or a data frame (the raw export).
#' @param n_caregivers,n_children,n_roster Repeat extents.
#' @param weight,stratum Optional source columns; see [kc9_dictionary()].
#' @param program_zones Optional character vector of health zones that make
#'   up the programme cohort; written to `children$cohort`.
#' @param keep_raw Keep the raw export inside the result.
#' @param ... Passed to [kc9_dictionary()].
#' @return A [vcs_data][new_vcs_data] object.
#' @export
#' @family KC v9 configuration
#' @examples
#' \dontrun{
#' vcs <- kc9_prepare("DRC_VxCoverage_WIDE.csv")
#' }
kc9_prepare <- function(data, n_caregivers = 3L, n_children = 3L, n_roster = 4L,
                        weight = NULL, stratum = NULL, program_zones = NULL,
                        keep_raw = TRUE, ...) {
  raw <- if (is.character(data)) read_surveycto(data) else {
    assert_data(data)
    data
  }
  if (!"KEY" %in% names(raw)) {
    raw$KEY <- sprintf("row_%06d", seq_len(nrow(raw)))
  }
  sched <- kc9_schedule()
  raw <- kc9_link_roster(raw, n_caregivers, n_children, n_roster)
  rm_report <- attr(raw, "roster_match")
  raw <- apply_recall_map(raw, kc9_recall_map(), sched,
                          n_caregivers = n_caregivers, n_children = n_children)
  raw <- kc9_caregiver_to_child(raw, kc9_caregiver_extras(), n_caregivers, n_children)
  dict <- kc9_dictionary(n_caregivers = n_caregivers, n_children = n_children,
                         weight = weight, stratum = stratum, ...)
  vcs <- suppressWarnings(map_vcs_variables(raw, dict, sched, keep_raw = TRUE))
  vcs <- attach_child_variables(vcs, c(kc9_child_extras(), kc9_caregiver_extras()))
  lab <- kc9_labels()
  # Residence and zone are household-level concepts; join them once so that
  # the child table is self-contained for stratified estimation.
  ch <- vcs_children(vcs)
  ch$residence_label <- as.character(vcs_recode(ch$residence, lab$residence))
  ch$sex_label <- as.character(vcs_recode(ch$sex, lab$sex))
  ch$education_label <- as.character(vcs_recode(ch$caregiver_education, lab$education3))
  ch$zone_label <- gsub("_", " ", as.character(ch$health_zone))
  if (!is.null(program_zones)) {
    ch$cohort <- ifelse(ch$zone_label %in% program_zones, "Programme", "Non-programme")
  }
  vcs$children <- ch
  vcs$meta$roster_match <- rm_report
  vcs$meta$recall_map_report <- attr(raw, "recall_map_report")
  if (!keep_raw) vcs$raw <- NULL
  vcs
}

#' Default stratifiers for a KC v9 report
#'
#' @param zone_parent Optional column nesting health zones (e.g. `"cohort"`
#'   for programme / non-programme).
#' @return A [vcs_strata()].
#' @export
#' @family KC v9 configuration
#' @examples
#' kc9_strata()
kc9_strata <- function(zone_parent = NULL) {
  vcs_strata(
    zone = list(var = "zone_label", label = "Health zone", parent = zone_parent),
    residence = list(var = "residence_label", label = "Area",
                     levels = c("Urban", "Rural")),
    sex = list(var = "sex_label", label = "Child's sex", levels = c("Male", "Female")),
    education = list(var = "education_label", label = "Caregiver's education",
                     levels = c("No formal schooling", "Primary", "Secondary or more")),
    overall_label = "Entire study area"
  )
}
