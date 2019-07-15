key = Sys.getenv("PREVEDERE_API_KEY")

skip_if_offline()
skip_on_cran()
skip_on_travis()
skip_on_appveyor()

test_that("Fetching indicator works ", {

  result <- prevedere_indicator(
    key,
    provider = "BLS",
    provider_id = "CES3133231058"
    )

  expect_type(result,"list")
  expect_gte(length(result),1)
})

test_that("Fetching indicator series works ", {

  result <- prevedere_indicator_series(
    key,
    provider = "BLS",
    provider_id = "CES3133231058",
    calculation = "None",
    start_date = "2010-01-01",
    offset_periods = 0
    )

  expect_s3_class(result,"data.frame")
})

test_that("Fetching raw indicator series works ", {

  result <- prevedere_indicator_series(
    key,
    provider = "BLS",
    provider_id = "CES3133231058",
    calculation = "None",
    start_date = "2010-01-01",
    offset_periods = 0,
    raw = TRUE
  )

  expect_type(result,"list")
  expect_gte(length(result),1)
})
