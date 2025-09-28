library(testthat)
library(mockery)

test_that("parse_test_actions correctly identifies action types", {
  # Test click action
  prompt <- "Click the submit button"
  actions <- parse_test_actions(prompt)
  expect_equal(actions[[1]]$type, "click")
  expect_equal(actions[[1]]$params[[1]], "submit")
  
  # Test select action
  prompt <- "Select 'Option A' from dropdown"
  actions <- parse_test_actions(prompt)
  expect_equal(actions[[1]]$type, "select")
  expect_equal(actions[[1]]$params[[1]], "Option A")
  expect_equal(actions[[1]]$params[[2]], "dropdown")
  
  # Test multiple actions
  prompt <- "Click submit and wait 2 seconds"
  actions <- parse_test_actions(prompt)
  expect_equal(length(actions), 2)
  expect_equal(actions[[1]]$type, "click")
  expect_equal(actions[[2]]$type, "wait")
})

test_that("generate_selector creates valid CSS selectors", {
  selector <- generate_selector("submit-button", "button")
  expect_match(selector, "#submit-button")
  expect_match(selector, "\\[id='submit-button'\\]")
})

test_that("generate_test_code produces valid test code", {
  actions <- list(
    list(type = "click", params = list("submit")),
    list(type = "wait", params = list("2"))
  )
  
  code <- generate_test_code(actions)
  expect_type(code, "character")
  expect_true(any(grepl("app\\$click", code)))
  expect_true(any(grepl("app\\$wait_for_idle", code)))
})

test_that("generate_shinytest2_test handles invalid inputs", {
  expect_error(generate_shinytest2_test(NULL, "path"))
  expect_error(generate_shinytest2_test("prompt", NULL))
  expect_error(generate_shinytest2_test("prompt", "nonexistent/path"))
})

test_that("generate_shinytest2_test returns correct structure", {
  # Mock file.exists to avoid filesystem dependency
  mockery::stub(generate_shinytest2_test, "file.exists", TRUE)
  
  result <- generate_shinytest2_test(
    "Click submit button and wait 2 seconds",
    "mock/path",
    "test_submit"
  )
  
  expect_type(result, "list")
  expect_true(all(c("success", "data", "message", "error") %in% names(result)))
  expect_true(result$success)
  expect_type(result$data, "character")
})