key = Sys.getenv("PREVEDERE_API_KEY")

skip_if_offline()
skip_on_cran()
skip_on_travis()
skip_on_appveyor()

test_that("Fetching forecast model works ", {

  result <- prevedere_forecast(
    key,
    model_id = "e0dee904db714c2fbcd33d4c7a7cea6a"
  )

  expect_s3_class(result,"data.frame")

})

test_that("Fetching forecast model with raw result works ", {

  result <- prevedere_forecast(
    key,
    model_id = "e0dee904db714c2fbcd33d4c7a7cea6a",
    raw = TRUE
  )

  expect_type(result,"list")
  expect_gte(length(result),1)
})

test_that("Fetching raw forecast model works ", {

  result <- prevedere_raw_model(
    key,
    model_id = "e0dee904db714c2fbcd33d4c7a7cea6a",
  )

  expect_s3_class(result$Indicators[[1]]$Values,"data.frame")

})

test_that("Fetching raw forecast model with raw results works ", {

  result <- prevedere_raw_model(
    key,
    model_id = "e0dee904db714c2fbcd33d4c7a7cea6a",
    raw = TRUE
  )

  expect_type(result,"list")
  expect_gte(length(result),1)
})