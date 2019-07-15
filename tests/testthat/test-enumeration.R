key = Sys.getenv("PREVEDERE_API_KEY")

skip_if_offline()
skip_on_cran()
skip_on_appveyor()

test_that("aggregations work", {

  result <- prevedere_aggregations(key)

  expect_type(result,"character")
  expect_gte(length(result),1)

})

test_that("calculations work", {

  result <- prevedere_calculations(key)

  expect_type(result,"character")
  expect_gte(length(result),1)

})

test_that("frequencies work", {

  result <- prevedere_frequencies(key)

  expect_type(result,"character")
  expect_gte(length(result),1)

})

test_that("seasonalities work", {

  result <- prevedere_seasonalities(key)

  expect_type(result,"character")
  expect_gte(length(result),1)

})
