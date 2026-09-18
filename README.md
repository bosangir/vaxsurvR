# vaxsurvR

<!-- badges: start -->
[![R-CMD-check](https://github.com/bosangir/vaxsurvR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bosangir/vaxsurvR/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**Management, quality assurance, de-identification and analysis of vaccination
coverage surveys.**

`vaxsurvR` takes a raw household survey export and carries it, reproducibly, to
validated, de-identified, analysis-ready data and design-based coverage
estimates. It is country-independent: no form structure, vaccination schedule
or administrative hierarchy is hard-coded.

## Installation

```r
# install.packages("remotes")
remotes::install_github("bosangir/vaxsurvR")
```

## The pipeline

```
vcs_schedule() + vcs_dictionary() [+ vcs_recall_map()]      describe the survey once
        |
read_vcs()  ->  apply_recall_map()  ->  map_vcs_variables()   import and standardise
        |
validate_vcs()  ->  clean_vcs()                                check, flag, log
        |
derive_vaccination_status()  ->  deidentify_vcs()              define, then protect
        |
vcs_design()  ->  estimate_coverage_by_evidence()              estimate with the design
             ->  estimate_dropout()  ->  vcs_summary()
```

Each step is a function you can run, inspect and test on its own. There is no
single `clean_data()` that does everything and shows you nothing.

## End-to-end example

```r
library(vaxsurvR)

# 1. Describe the survey once: the schedule and the variable mapping.
schedule <- vcs_schedule(
  vaccine               = c("BCG", "PENTA1", "PENTA2", "PENTA3", "MCV1"),
  minimum_age_days      = c(0, 42, 70, 98, 270),
  maximum_age_days      = c(28, 76, 104, 132, 330),
  previous_dose         = c(NA, NA, "PENTA1", "PENTA2", NA),
  minimum_interval_days = c(NA, NA, 28, 28, NA)
)

# 2. Recall is rarely one column per dose. Say how the questionnaire asked it:
#    a yes/no per dose, or "ever received?" + "how many times?" per series.
recall <- vcs_recall_map(
  BCG   = "VR01_{c}_{k}",
  PENTA = list(ever = "VR08_{c}_{k}", count = "VR09_{c}_{k}"),
  MCV   = list(ever = "VR12_{c}_{k}", count = "VR13_{c}_{k}")
)
dictionary <- do.call(vcs_dictionary, c(
  list(interview_id = "KEY", psu = "submission_psu", stratum = "stratum",
       weight = "final_weight", health_zone = "health_zone",
       child_dob = "EC_DOB_{c}_{k}", card_seen = "CVH_card_shown_{c}_{k}",
       card_status = "CVH{vv}_{c}_{k}", card_date = "CVH{vv}_date_{c}_{k}",
       n_caregivers = 3, n_children = 2),
  recall_dictionary_entries()
))

# 3. Import and map. The raw export is preserved untouched inside the result.
raw    <- read_vcs("survey.csv")
raw    <- apply_recall_map(raw, recall, schedule, n_caregivers = 3, n_children = 2)
mapped <- map_vcs_variables(raw, dictionary, schedule)

# 4. Check. Structured issues, not printed warnings.
qa <- validate_vcs(mapped)
summary(qa, by = "severity")
issues(qa, severity = "CRITICAL")

# 5. Clean. The default ruleset flags; it changes nothing.
clean <- clean_vcs(mapped)
audit_changes(clean)          # record, variable, before, after, rule, time

# 6. Derive. Say which evidence counts; the rule is stored on the column.
clean <- derive_vaccination_status(clean, evidence = "card_or_recall")
clean <- derive_zero_dose(clean)

# 7. De-identify. The salt comes from the environment and is never written out.
anon <- deidentify_vcs(
  clean,
  remove = c("child_name", "caregiver_name", "telephone"),
  hash   = c("child_id", "household_id"),
  salt   = Sys.getenv("VAXSURVR_SALT")
)

# 8. Estimate, with the design preserved throughout. Card, recall and either
#    source side by side, every child in every denominator.
results <- estimate_coverage_by_evidence(
  anon,
  vaccines    = c("BCG", "PENTA1", "PENTA3", "MCV1"),
  by          = ~health_zone,
  design_args = list(ids = ~psu, strata = ~stratum, weights = ~weight)
)

design <- vcs_design(anon, ids = ~psu, strata = ~stratum, weights = ~weight)
estimate_dropout(design, first = "PENTA1", last = "PENTA3", by = ~health_zone)
```

Every function above runs on the synthetic dataset shipped with the package, so
you can try the whole pipeline before you have data:

```r
library(vaxsurvR)

vcs_example                                    # 600 households, 256 children
summary(validate_vcs(vcs_example), by = "severity")

d   <- derive_vaccination_status(vcs_example, evidence = "card_or_recall")
des <- vcs_design(d)

estimate_coverage(des, vaccines = c("BCG", "PENTA1", "PENTA3", "MCV1"))
estimate_coverage_by_evidence(vcs_example, vaccines = c("BCG", "PENTA3", "MCV1"))
estimate_dropout(des, first = "PENTA1", last = "PENTA3", by = ~stratum)
```

## The full report in one call

Version 0.2.0 adds every indicator of a WHO/VCQI-style coverage report --
coverage and timeliness charts, dropout, interval and missed-opportunity
tables shaded in proportion to the outcome, cumulative coverage curves,
organ-pipe plots, BeSD tables, reasons not vaccinated -- and a parameterised
R Markdown report that renders them to Word.

```r
library(vaxsurvR)

# A SurveyCTO wide export of the DRC KC v9 questionnaire, mapped in one call.
vcs <- kc9_prepare("DRC_VxCoverage_WIDE.csv", n_caregivers = 3, n_children = 2)
vcs <- erase_illogical_dates(vcs)                 # VCQI date logic, audited
des <- vcs_design(vcs, ids = ~psu)

v <- estimate_vctc(vcs, design = des, vaccines = kc9_epi_doses())
plot_vctc(v)                                      # the VCTC

st  <- kc9_strata()
tab <- estimate_stratified(des, function(x, by = NULL) estimate_dropout(x, "PENTA1", "PENTA3", by = by), st)
plot_bar_table(tab, bar_measure("estimate", "denominator", label = "PENTA1-PENTA3
Dropout (%)"))

m <- derive_mosv(vcs)
plot_mosv_children(m, by = "zone_label")

render_coverage_report("DRC_VxCoverage_WIDE.csv", "Coverage_Report.docx", output_dir = "reports")
use_coverage_report("reports/my_report.Rmd")     # copy the template to edit it
```

## Design principles

* **The raw import is never modified.** It is carried alongside the
  standardised tables so any transformation can be traced back to its source.
* **Nothing is corrected silently.** The default cleaning ruleset flags.
  Any rule that changes a value writes a log line naming the record, the
  variable, the value before and after, the rule, its severity and the time.
* **Absence of evidence is not evidence of absence.** A dose with no card and
  no recall is `NA`, never `0`, unless you explicitly ask otherwise.
* **Card and recall are never conflated.** Every coverage figure states which
  evidence definition produced it, and the rule is stored on the column.
* **Observed and derived variables stay distinguishable.** `derivation_rules()`
  lists every derived column and the rule behind it.
* **The design survives to the end.** Estimation runs on a `survey::svydesign`;
  vaxsurvR does not implement a competing variance estimator.
* **No default salt, ever.** Deterministic hashing requires a secret you
  supply, and the package never writes it to data, logs, metadata or console.

## Configurability

Nothing about a particular survey is baked in.

| Concern | How you configure it |
|---|---|
| Source variable names | `vcs_dictionary()`, or `read_vcs_dictionary()` from a spreadsheet |
| Repeat groups in wide exports | `{c}` / `{k}` placeholders in the dictionary |
| Antigens, ages, intervals | `vcs_schedule()` |
| What counts as vaccinated | `evidence =` in `derive_vaccination_status()` |
| How recall was asked | `vcs_recall_map()`: per dose, or "ever" + "how many" per series |
| Denominator convention | `denominator =` in `estimate_coverage_by_evidence()` |
| Fully vaccinated | `vaccines =` in `derive_fully_vaccinated()` |
| Zero-dose marker | `marker =` in `derive_zero_dose()` |
| Cleaning behaviour | `vcs_rule()` and `vcs_ruleset()` |
| Disclosure control | `generalize_geography()`, `generalize_age()`, `shift_dates()` |

## Vignettes

```r
vignette("vaxsurvR", package = "vaxsurvR")            # getting started
vignette("preparing-a-survey", package = "vaxsurvR")
vignette("data-quality", package = "vaxsurvR")
vignette("deidentification", package = "vaxsurvR")
vignette("survey-analysis", package = "vaxsurvR")
vignette("coverage-indicators", package = "vaxsurvR")
vignette("end-to-end", package = "vaxsurvR")
```

## Origins

The package grew out of methodological and operational work on vaccination
coverage surveys carried out in collaboration between the University of Kinshasa
(UNIKIN), Biostatistics for Global Health collaborators and GiveWell. It is
released as a generic toolkit: no institution, country, survey or project
assumption is encoded anywhere in the code.

## Contributing

Bug reports and pull requests are welcome; see `CONTRIBUTING.md`. This project
is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md).

## License

MIT © vaxsurvR authors.
