# ---------------------------------------------------------------------------
# Rendering the full coverage report. The R Markdown template lives in
# inst/rmarkdown/templates/coverage_report and is also available through
# RStudio's "New R Markdown > From Template" dialog.
# ---------------------------------------------------------------------------

#' Path to the coverage report template and its Word reference document
#'
#' @return A named character vector with `rmd` and `reference_docx`.
#' @export
#' @examples
#' coverage_report_template()
coverage_report_template <- function() {
  c(rmd = system.file("rmarkdown", "templates", "coverage_report", "skeleton",
                      "skeleton.Rmd", package = "vaxsurvR"),
    reference_docx = system.file("templates", "vcs_reference.docx", package = "vaxsurvR"))
}

#' Copy the coverage report template into a folder for editing
#'
#' The report is a parameterised R Markdown document. Copy it, edit the text
#' or the parameters, and knit it -- or render it unchanged with
#' [render_coverage_report()].
#'
#' @param path Destination file (`.Rmd`).
#' @param overwrite Overwrite an existing file.
#' @return `path`, invisibly.
#' @export
#' @examples
#' \dontrun{
#' use_coverage_report("reports/kongo_central_coverage_report.Rmd")
#' }
use_coverage_report <- function(path = "coverage_report.Rmd", overwrite = FALSE) {
  assert_string(path)
  src <- coverage_report_template()[["rmd"]]
  if (!nzchar(src)) {
    vcs_abort("The report template is not installed with this copy of vaxsurvR.",
              class = "vaxsurvR_value_error")
  }
  if (file.exists(path) && !overwrite) {
    vcs_abort(sprintf("\"%s\" exists; set `overwrite = TRUE` to replace it.", path),
              class = "vaxsurvR_value_error")
  }
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  file.copy(src, path, overwrite = TRUE)
  ref <- file.path(dirname(path), "vcs_reference.docx")
  file.copy(coverage_report_template()[["reference_docx"]], ref, overwrite = TRUE)
  invisible(path)
}

#' Render the full coverage report to Word
#'
#' Runs the package's report template against a SurveyCTO wide export of the
#' KC v9 questionnaire (or any export prepared the same way) and writes a
#' `.docx` styled like the Kongo Central survey reports, plus a folder of
#' every figure as PNG.
#'
#' @param data_file Path to the wide export (CSV/TSV/XLSX).
#' @param output_file Name of the Word file.
#' @param output_dir Folder for the report and its `figures/` sub-folder.
#' @param params Named list overriding the template parameters: `title`,
#'   `subtitle`, `study_area`, `country`, `survey_year`, `prepared_by`,
#'   `partners`, `n_caregivers`, `n_children`, `n_roster`, `weight_var`,
#'   `stratum_var`, `frame_file`, `program_zones`, `targets_file`,
#'   `evidence`, `fully_vaccinated_doses`, `mosv_doses`, `assumed`,
#'   `suppress_n`, `font`, `simulated`. See the template's YAML header.
#' @param rmd Path to an edited copy of the template; defaults to the
#'   installed one.
#' @param quiet Passed to [rmarkdown::render()].
#' @param update_fields Fill in the table of contents after rendering with
#'   [update_docx_fields()] (Windows with Word only; silently skipped
#'   elsewhere).
#' @return The path of the rendered file, invisibly.
#' @export
#' @examples
#' \dontrun{
#' render_coverage_report(
#'   "DRC_VxCoverage_WIDE.csv", "Kongo_Central_Coverage_Report.docx",
#'   output_dir = "reports",
#'   params = list(study_area = "Kongo Central Province", survey_year = 2026)
#' )
#' }
render_coverage_report <- function(data_file, output_file = "coverage_report.docx",
                                   output_dir = ".", params = list(), rmd = NULL,
                                   quiet = FALSE, update_fields = TRUE) {
  assert_installed(c("rmarkdown", "knitr", "flextable", "officer", "ggplot2"),
                   "render_coverage_report()")
  assert_string(data_file)
  if (!file.exists(data_file)) {
    vcs_abort(sprintf("Data file not found: %s", data_file), class = "vaxsurvR_value_error")
  }
  rmd <- rmd %||% coverage_report_template()[["rmd"]]
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  output_dir <- normalizePath(output_dir, winslash = "/")
  params$data_file <- normalizePath(data_file, winslash = "/")
  params$fig_dir <- params$fig_dir %||% "figures"
  # Knit in the output folder so that figures land beside the report and
  # relative image links resolve for pandoc.
  rmarkdown::render(
    rmd, output_file = output_file, output_dir = output_dir,
    knit_root_dir = output_dir,
    intermediates_dir = output_dir,
    params = params, envir = new.env(parent = globalenv()), quiet = quiet
  )
  out <- file.path(output_dir, output_file)
  if (update_fields) update_docx_fields(out)
  invisible(out)
}

#' Update the table of contents and other fields of a Word document
#'
#' Pandoc writes the table of contents as a field that Word fills in the
#' first time the document is opened. On Windows with Microsoft Word
#' installed this opens the file invisibly, updates every field and saves
#' it, so the report is complete the moment it is rendered. Elsewhere it does
#' nothing and returns `FALSE`.
#'
#' @param path Path to a `.docx` file.
#' @return `TRUE` if the fields were updated, `FALSE` otherwise (invisibly).
#' @export
update_docx_fields <- function(path) {
  assert_string(path)
  if (.Platform$OS.type != "windows" || !nzchar(Sys.which("powershell"))) {
    return(invisible(FALSE))
  }
  full <- normalizePath(path, winslash = "\\", mustWork = TRUE)
  script <- paste(
    "$w = New-Object -ComObject Word.Application; $w.Visible = $false;",
    sprintf("$d = $w.Documents.Open('%s');", gsub("'", "''", full)),
    "$d.Fields.Update() | Out-Null;",
    "foreach ($t in $d.TablesOfContents) { $t.Update() };",
    "$d.Save(); $d.Close(); $w.Quit();"
  )
  ok <- tryCatch({
    system2("powershell", c("-NoProfile", "-NonInteractive", "-Command", shQuote(script)),
            stdout = FALSE, stderr = FALSE, timeout = 300) == 0
  }, error = function(e) FALSE)
  invisible(isTRUE(ok))
}
