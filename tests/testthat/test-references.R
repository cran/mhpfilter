test_that("references of every result and of the package", {
  all <- mhp_cite()
  expect_s3_class(all, "bibentry")
  expect_gte(length(all), 12L)
  m <- mhp_filter(log(mhpfilter::us_macro$gdp), max_lambda = 1000, as_dt = FALSE)
  expect_output(print(mhp_cite(m)), "Choudhary")
  expect_output(print(mhp_cite(m, style = "bibtex")), "@Article")
  keys <- unique(unlist(lapply(c("hp", "mhp", "fmhp", "weights", "gain", "loss", "simulation"), function(k) {
    x <- structure(list(), class = c(paste0("mhp_", k), "mhp_result", "list"))
    mhpfilter:::.mhp_sources_of(x)
  })))
  expect_true(all(keys %in% names(mhpfilter:::.mhp_refs_list())))
  expect_error(mhp_cite(1), "result")
})
