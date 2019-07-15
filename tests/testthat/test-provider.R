key = Sys.getenv("PREVEDERE_API_KEY")

skip_if_offline()
skip_on_cran()
skip_on_travis()
skip_on_appveyor()

test_that("Fetch provider works ", {

  result <- prevedere_providers(key)

  expect_s3_class(result,"data.frame")
})

test_that("Fetch provider raw works ", {

  result <- prevedere_providers(key,raw = TRUE)

  expect_type(result,"list")
  expect_gte(length(result),1)
})