test_that("vcs_pipeline() describes every stage in order", {
  p <- vcs_pipeline()
  expect_s3_class(p, "tbl_df")
  expect_named(p, c("stage", "step", "fn", "optional", "caption"))
  expect_equal(unique(p$stage),
               c("Configure", "Import", "Check", "Derive", "Estimate"))
  # Steps number from 1 within each stage, with no gaps.
  for (s in unique(p$stage)) {
    steps <- p$step[p$stage == s]
    expect_equal(sort(steps), seq_along(steps))
  }
  # One caption per stage, not per row.
  caps <- tapply(p$caption, p$stage, function(x) length(unique(x)))
  expect_true(all(caps == 1L))
  expect_type(p$optional, "logical")
  expect_true(any(p$optional))
})

test_that("every function named in the pipeline actually exists", {
  p <- vcs_pipeline()
  fns <- sub("\\(\\)$", "", p$fn)
  missing <- fns[!vapply(fns, exists, logical(1), where = asNamespace("vaxsurvR"),
                         mode = "function", inherits = FALSE)]
  expect_equal(missing, character(0))
})

test_that("the optional steps are the recall ones", {
  p <- vcs_pipeline()
  expect_setequal(p$fn[p$optional], c("vcs_recall_map()", "apply_recall_map()"))
})

test_that("plot_vcs_pipeline() builds a ggplot that renders", {
  skip_if_not_installed("ggplot2")
  g <- plot_vcs_pipeline()
  expect_s3_class(g, "ggplot")
  expect_silent(invisible(ggplot2::ggplot_build(g)))

  # One box per pipeline row, and arrows linking them.
  layers <- vapply(g$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomRect" %in% layers)
  expect_true("GeomSegment" %in% layers)
  expect_gte(sum(layers == "GeomText"), 3L)
})

test_that("plot_vcs_pipeline() accepts a trimmed pipeline", {
  skip_if_not_installed("ggplot2")
  p <- vcs_pipeline()
  short <- p[!p$optional, ]
  g <- plot_vcs_pipeline(short)
  expect_s3_class(g, "ggplot")
  expect_silent(invisible(ggplot2::ggplot_build(g)))

  one <- p[p$stage == "Check", ]
  expect_s3_class(plot_vcs_pipeline(one), "ggplot")
})

test_that("plot_vcs_pipeline() rejects a malformed pipeline", {
  expect_error(plot_vcs_pipeline(data.frame(stage = "x")),
               class = "vaxsurvR_missing_variable")
})

test_that("the diagram writes to PDF without font warnings", {
  skip_if_not_installed("ggplot2")
  f <- withr::local_tempfile(fileext = ".pdf")
  expect_silent(
    ggplot2::ggsave(f, plot_vcs_pipeline(), width = 10, height = 6,
                    device = grDevices::cairo_pdf)
  )
  expect_true(file.exists(f))
  expect_gt(file.size(f), 1000)
})
