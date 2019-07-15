key = Sys.getenv("PREVEDERE_API_KEY")

skip_if_offline()
skip_on_cran()
skip_on_appveyor()

test_that("Workbench works ", {

  workbenchid <- "573a534d13284de0ab987fdbfc415e14"

  result <- prevedere_workbench(key,workbenchid)

  expect_type(result,"list")
  expect_gte(length(result),1)
})
