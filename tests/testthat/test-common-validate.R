test_that("argument checks accept valid input and name the function and argument in errors", {
  expect_identical(.mhp_check_number(3, "n", "f", min = 1, integer = TRUE), 3L)
  expect_error(.mhp_check_number(0, "n", "f", min = 1), "f\\(\\): `n` must be")
  expect_error(.mhp_check_number(c(1, 2), "n", "f"), "single")
  expect_true(.mhp_check_flag(TRUE, "x", "f"))
  expect_error(.mhp_check_flag(NA, "x", "f"), "TRUE or FALSE")
  expect_identical(.mhp_check_choice(2, c("a", "b"), "m", "f", codes = TRUE), "b")
  expect_identical(.mhp_check_choice("a", c("a", "b"), "m", "f"), "a")
  expect_error(.mhp_check_choice(3, c("a", "b"), "m", "f", codes = TRUE), "one of")
  expect_error(.mhp_check_data(list(a = 1), "f"), "data frame")
})

test_that("columns are captured as bare names, strings or vectors", {
  d <- data.frame(Yield = c(1, 2, 3), Rep = c(1, 1, 2))
  f <- function(y, data) .mhp_column(data, substitute(y), "y", "f", parent.frame())
  expect_identical(f(Yield, d)$value, d$Yield)
  expect_identical(f("Yield", d)$name, "Yield")
  expect_identical(f(c(4, 5, 6), d)$value, c(4, 5, 6))
  col <- "Yield"
  expect_identical(f(col, d)$value, d$Yield)
  expect_error(f("Nope", d), "no column")
  expect_error(f(c(1, 2), d), "must name a column")
})
