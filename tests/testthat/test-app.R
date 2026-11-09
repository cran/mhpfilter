test_that("the application runs every page", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  shiny::testServer(mhp_app_object(), {
    session$setInputs(source = "example", col = "gdp", freq = "4", start = 1959, start_period = 1, logs = TRUE,
                      method = "fmhp", max_lambda = 3000, k_linear = 15, k_quadratic = NA,
                      schemes = c("linear", "quadratic"), form = "rows", fit_plot = "decomposition", lambda = 1600,
                      criterion = "approximate", diag = "loss", cmp_max = 3000, end_share = 0.1, b_method = "mhp",
                      b_max = 3000, b_show = "cycle", s_models = "3", s_rep = 1, s_T = 100, s_trend = "linear",
                      s_agg = "none", s_seed = 1)
    session$setInputs(run = 1)
    expect_s3_class(fit(), "mhp_fmhp")
    expect_match(.mhp_app_code(input), "fmhp_filter\\(")
    session$setInputs(method = "mhp")
    session$setInputs(run = 2)
    expect_s3_class(fit(), "mhp_mhp")
    for (d in c("loss", "gain", "weights")) {
      session$setInputs(diag = d)
      expect_true(nzchar(mhp_interpret(dg())$headline))
    }
    session$setInputs(compare = 1)
    expect_identical(nrow(cmp()), 3L)
    session$setInputs(batch = 1)
    expect_identical(nrow(attr(bat(), "lambdas")), 3L)
    session$setInputs(simulate = 1)
    expect_s3_class(sim(), "mhp_simulation")
  })
})
