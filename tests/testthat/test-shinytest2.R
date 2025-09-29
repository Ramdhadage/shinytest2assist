library(shinytest2)
library(here)

test_that('test_form_submission', {
  app_dir <- here("demo-app")

  if (!dir.exists(app_dir)) {
    skip(paste("App directory", app_dir, "does not exist"))
  }

  app <- AppDriver$new(
    app_dir = app_dir,
    name = 'test_form_submission',
    variant = platform_variant(),
    load_timeout = 5000
  )

  app$set_inputs("name" = "John Doe")
  app$set_inputs("color" = "Blue")
  app$set_inputs("age" = "30")
  app$click("submit")
  app$expect_values(output = "greeting")

  app$stop()
})
