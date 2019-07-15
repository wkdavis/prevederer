key = Sys.getenv("PREVEDERE_API_KEY")

skip_if_offline()
skip_on_cran()
skip_on_travis()
skip_on_appveyor()

test_that("Fetch works", {

  expect_equal(
    prevedere_fetch(key,path = "test",payload = list(Echo = "teststring")),
    "teststring"
  )

})
