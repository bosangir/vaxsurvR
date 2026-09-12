# Contributing to vaxsurvR

Thank you for taking the time to contribute.

## Reporting a bug

Please open an issue with a minimal reproducible example built on
`vcs_example` or on synthetic data you generate in the report itself.
**Never paste real survey data, identifiers or coordinates into an issue.**

## Pull requests

1. Fork and create a branch from `main`.
2. Add or update `testthat` (edition 3) tests for every behaviour you change.
   Functions that modify data or determine vaccination status need edge-case
   tests as well as happy-path ones.
3. Document exported functions with roxygen2 and run `roxygen2::roxygenise()`.
4. Run `devtools::test()` and `devtools::check()`. Pull requests are expected
   to leave the package at zero errors and zero warnings.
5. Keep to the existing style: no `library()` calls inside package code,
   explicit `pkg::fun()` for anything outside base and the Imports, and lines
   under 90 characters.

## Design rules that reviews enforce

Changes that break any of these will be asked to change, however convenient
they are:

* the raw import is never modified in place;
* no value is corrected without a log line recording record, variable,
  before, after, rule, severity and timestamp;
* missing vaccination evidence is never treated as evidence of
  non-vaccination unless the caller asks for it explicitly;
* caregiver recall is never silently equated with card documentation;
* no country vaccination schedule is hard-coded;
* survey weights are never ignored for population-level estimates;
* records are never dropped merely for triggering a QA flag;
* no direct identifier appears in a log, and no hashing salt is ever printed,
  stored or given a default;
* no real survey data enters tests, examples or vignettes.

## Adding a dependency

Check first whether base R or an existing import already does the job. Heavy
or specialised packages (`sf`, `readxl`, `haven`, `openxlsx`, reporting
libraries) belong in `Suggests`, guarded with `requireNamespace()`.
