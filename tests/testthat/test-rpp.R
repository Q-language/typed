test_that("Argument types", {
  expect_rpp_snapshot("?function(a = 0L) {}")
  expect_rpp_snapshot("?function(a = ?Integer()) {}")
  expect_rpp_snapshot("?function(a = 0L ?Integer()) {}")
})

test_that("Return values", {
  expect_rpp_snapshot("Integer()? function(a = 0L ?Integer()) {}")
  expect_rpp_snapshot("fun <- (Integer()? function(a = 0L ?Integer()) {})")
  expect_rpp_snapshot("fun <- Integer()? function(a = 0L ?Integer()) {}")
  code <- c(
    "fun <- Integer()? function(a = 0L ?Integer(),",
    "                           b = '' ?Character()) {",
    "}"
  )
  expect_rpp_snapshot(code)
})

test_that("Variable types", {
  expect_rpp_snapshot("Integer()? x <- 0L")
})
