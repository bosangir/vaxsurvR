# vaxsurvR 0.1.1

## Caregiver recall

* Recall questions rarely mirror the card block one-for-one. New concepts
  `recall_ever` and `recall_count` describe the common "ever received?" +
  "how many times?" structure asked once per antigen series; dose *n* is
  recalled when the count is at least *n*, and a per-dose `recall_status`
  answer overrides the series answer where both exist.
* A new `{antigen}` placeholder resolves vaccine-level templates once per
  antigen series.
* `vcs_recall_map()` and `apply_recall_map()` turn a questionnaire crosswalk
  into the standardised columns the dictionary picks up via
  `recall_dictionary_entries()`.
* `count_dk_values` on the dictionary keeps don't-know codes out of dose
  counts.

## Estimation

* `estimate_coverage_by_evidence()` reports each antigen three ways -- card,
  recall, card or recall -- stacked in one table, with an explicit choice of
  denominator convention (`"all"` children, as in published coverage tables,
  or `"determinable"` status only).
* Design effects use the with-replacement reference variance. The previous
  default returned `Inf` whenever weights summed to the sample size.

## Fixes found on real survey exports

* Unpadded `m/d/Y` dates such as `5/6/2025` were rejected by a strict
  round-trip check.
* A concept keyed only by `{c}` (a caregiver name, say) could conjure phantom
  children into every empty `{k}` slot.
* A card code positively meaning "not given" is now evidence of absence even
  when no recall variable is mapped; `card_no_values` separates it from
  don't-know codes, which stay unknown.

# vaxsurvR 0.1.0

First release. Phase 1 of the roadmap: the reproducible pipeline from a raw
survey export to design-based coverage estimates.

## Import and mapping

* `read_vcs()` reads CSV, TSV, Excel, RDS and Stata exports, auto-detecting the
  delimiter and reading everything as text by default so that codes and partial
  dates survive import. `read_surveycto()` adds the defaults wide SurveyCTO
  exports need.
* `vcs_dictionary()` maps arbitrary source variable names onto standardised
  concepts, with `{c}` / `{k}` / `{vv}` placeholders for repeat groups and
  per-vaccine columns. `read_vcs_dictionary()` reads the same mapping from a
  spreadsheet.
* `map_vcs_variables()` turns a wide or long export into a `vcs_data` object
  with separate household, child and long vaccination tables, keeping the raw
  import untouched.

## Schedules

* `vcs_schedule()` describes antigens, minimum ages, dose sequences and minimum
  intervals as data. `vcs_schedule_who()` provides a template to adapt.

## Validation

* `validate_vcs()` runs the structural and vaccination check suites and returns
  a `vcs_validation` object with `issues()`, `summary()` and `plot()` methods
  and four severity levels.
* Structural checks: `check_required_variables()`, `check_unique_ids()`,
  `check_duplicates()`, `check_household_structure()`,
  `check_child_eligibility()`, `check_psu_structure()`,
  `check_segment_structure()`.
* Vaccination checks: `check_vaccine_dates()`, `check_future_dates()`,
  `check_prebirth_vaccination()`, `check_age_at_vaccination()`,
  `check_vaccine_sequence()`, `check_dose_intervals()`,
  `check_duplicate_doses()`, `check_card_transcription()`, and
  `flag_vaccine_inconsistencies()` to collapse them to record level.

## Dates

* `parse_vaccine_date()` classifies each value as a complete, partial, missing
  or unparseable date without imputing anything.
* `validate_partial_date()` resolves partial dates only under an explicitly
  chosen rule, and records which one.
* `derive_age_at_vaccination()` and `classify_vaccination_timeliness()`.

## Derivation

* `derive_vaccination_status()` with five evidence definitions, defaulting to
  treating absent evidence as unknown rather than as non-vaccination.
* `derive_fully_vaccinated()`, `derive_zero_dose()`, `derive_dropout()` and
  `derive_timeliness()`, all with user-supplied definitions.
* `derivation_rules()` lists every derived column and the rule that made it.

## Audit trail

* `vcs_rule()` and `vcs_ruleset()` define cleaning rules; `vcs_default_rules()`
  flags without changing values.
* `clean_vcs()` applies them and attaches a `vcs_audit` trail;
  `audit_changes()` and `export_cleaning_log()` read and export it.

## De-identification

* `identify_pii()`, `remove_direct_identifiers()`, `hash_identifier()`,
  `generalize_age()`, `generalize_geography()`, `shift_dates()` and
  `deidentify_vcs()`.
* `assess_reidentification_risk()` computes k-anonymity over quasi-identifiers.
* No default salt is shipped, and the salt is never written to data, logs,
  metadata or console.

## Design and estimation

* `vcs_design()` builds a `survey::svydesign` over the child-level table;
  `validate_weights()`, `check_weight_distribution()`, `trim_weights()` and
  `design_effect()` support it.
* `estimate_coverage()`, `estimate_antigen_coverage()`,
  `estimate_full_coverage()`, `estimate_zero_dose()`,
  `estimate_card_availability()`, `estimate_timely_coverage()` and
  `estimate_dropout()` return tidy estimates with numerators, denominators,
  standard errors, confidence limits, unweighted sample sizes and design
  effects.
* `estimate_dropout()` supports both the individual-history and the
  coverage-difference definitions, and labels which produced each row.

## Completeness and reporting

* `vcs_missingness()`, `coverage_completeness()`, `card_completeness()` and
  `date_completeness()`.
* `vcs_summary()`, `vcs_quality_report()`, `vcs_coverage_report()` and
  `write_vcs_report()`.

## Data

* `vcs_example`, `vcs_example_raw`, `vcs_example_dictionary` and
  `vcs_example_schedule`: a fully synthetic survey with a documented set of
  planted data-quality problems. No real participant data is used anywhere in
  the package.
