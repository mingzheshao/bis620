test_that("The hello function works", {
  expect_equal(hello(), "Hello, world!")
})

test_that("The hello function works with other parameters." ,{
  expect_equal(hello(FALSE, TRUE), "Hello!")
})
