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
  
  # Test input action  
  prompt <- "Enter 'John Smith' in name-input"
  actions <- parse_test_actions(prompt)
  expect_equal(actions[[1]]$type, "input")
  expect_equal(actions[[1]]$params[[1]], "John Smith")
  expect_equal(actions[[1]]$params[[2]], "name-input")
  
  # Test expect action
  prompt <- "Verify success message appears"
  actions <- parse_test_actions(prompt)
  expect_equal(actions[[1]]$type, "expect")
  expect_equal(actions[[1]]$params[[1]], "success message appears")
  
  # Test wait action
  prompt <- "Wait 3 seconds"
  actions <- parse_test_actions(prompt)
  expect_equal(actions[[1]]$type, "wait")
  expect_equal(actions[[1]]$params[[1]], "3")
  
  # Test multiple actions
  prompt <- "Click submit and wait 2 seconds then verify success"
  actions <- parse_test_actions(prompt)
  expect_equal(length(actions), 3)
  expect_equal(actions[[1]]$type, "click")
  expect_equal(actions[[2]]$type, "wait")
  expect_equal(actions[[3]]$type, "expect")
})

test_that("generate_selector creates valid CSS selectors", {
  selector <- generate_selector("submit-button", "button")
  expect_match(selector, "#submit-button")
  expect_match(selector, "\\[id='submit-button'\\]")
  expect_match(selector, "\\[name='submit-button'\\]")
  expect_match(selector, "\\[aria-label='submit-button'\\]")
  
  # Test normalization
  selector <- generate_selector("User Name", "input")
  expect_match(selector, "#User-Name")
  expect_match(selector, "\\[id='User-Name'\\]")
})

test_that("generate_test_code produces valid test code", {
  actions <- list(
    list(type = "click", params = list("submit")),
    list(type = "wait", params = list("2")),
    list(type = "input", params = list("John", "name")),
    list(type = "select", params = list("Blue", "color")),
    list(type = "expect", params = list("success"))
  )
  
  code <- generate_test_code(actions)
  expect_type(code, "character")
  
  # Verify scaffolding
  expect_true(any(grepl("test_that\\(", code)))
  expect_true(any(grepl("AppDriver\\$new\\(", code)))
  expect_true(any(grepl("app\\$stop\\(\\)", code)))
  
  # Verify action translations
  expect_true(any(grepl("app\\$click", code)))
  expect_true(any(grepl("app\\$wait_for_idle", code)))
  expect_true(any(grepl("app\\$set_inputs", code)))
  expect_true(any(grepl("app\\$expect_values", code)))
})

test_that("generate_shinytest2_test handles invalid inputs", {
  expect_error(generate_shinytest2_test(NULL, "path"), 
              "Prompt must be a non-empty character string")
  expect_error(generate_shinytest2_test("", "path"), 
              "Prompt must be a non-empty character string")
  expect_error(generate_shinytest2_test("prompt", NULL), 
              "App path must be a non-empty character string")
  expect_error(generate_shinytest2_test("prompt", ""), 
              "App path must be a non-empty character string")
  expect_error(generate_shinytest2_test("prompt", "nonexistent/path"),
              "App path does not exist")
  expect_error(generate_shinytest2_test("prompt", ".", 123),
              "Test name must be NULL or a character string")
  expect_error(generate_shinytest2_test("prompt", ".", test_name = list()),
              "Test name must be NULL or a character string")
  expect_error(generate_shinytest2_test("prompt", ".", options = "invalid"),
              "Options must be a list")
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
  expect_type(result$message, "character")
  expect_null(result$error)
  
  # Test error case with unparseable prompt
  result <- generate_shinytest2_test(
    "do something invalid",
    "mock/path",
    "test_invalid"
  )
  
  expect_false(result$success)
  expect_null(result$data)
  expect_type(result$message, "character")
  expect_type(result$error, "character")
})