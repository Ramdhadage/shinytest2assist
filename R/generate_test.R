#' @importFrom mcpr mcp_tool mcp_http
#' @importFrom rlang arg_match
#' @importFrom cli cli_alert_success cli_alert_warning cli_alert_danger
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom stringr str_match str_extract_all str_trim
#' @importFrom purrr map_chr
#' @importFrom glue glue
NULL

#' Parse Natural Language Prompt into Test Actions
#' 
#' @param prompt Character string containing test description
#' @return List of parsed actions with parameters
#' @keywords internal
parse_test_actions <- function(prompt) {
  # Define regex patterns for common actions
  patterns <- list(
    click = "click(?:ing)? (?:the )?([\\w-]+)(?: button)?",
    select = "select(?:ing)? ['\"](.*?)['\"] from (\\w+)",
    input = "enter(?:ing)? ['\"](.*?)['\"] in(?: the)? ([\\w-]+)",
    expect = "(?:verify|expect|check)(?: that)? (.*?)(?:$|,|\\sand)",
    wait = "wait(?:ing)? (\\d+)(?: seconds?)?"
  )
  
  actions <- list()
  
  # Extract actions using regex patterns
  for (action_type in names(patterns)) {
    matches <- str_match_all(prompt, patterns[[action_type]])[[1]]
    if (nrow(matches) > 0) {
      for (i in seq_len(nrow(matches))) {
        actions[[length(actions) + 1]] <- list(
          type = action_type,
          params = as.list(matches[i, -1])
        )
      }
    }
  }
  
  actions
}

#' Generate CSS Selector from Element Description
#' 
#' @param element Character string describing the element
#' @param type Character string indicating element type
#' @return Character string containing CSS selector
#' @keywords internal
generate_selector <- function(element, type = "input") {
  # Clean and normalize element name
  element <- str_trim(element)
  element <- gsub("[^[:alnum:]]", "-", element)
  
  # Generate selectors with fallbacks
  selectors <- c(
    sprintf("#%s", element),
    sprintf("[id='%s']", element),
    sprintf("[name='%s']", element),
    sprintf("[aria-label='%s']", element)
  )
  
  # Return all selectors joined with comma
  paste(selectors, collapse = ", ")
}

#' Convert Test Actions to Shinytest2 Code
#' 
#' @param actions List of parsed test actions
#' @return Character string containing generated test code
#' @keywords internal
generate_test_code <- function(actions) {
  code_lines <- character(0)
  
  for (action in actions) {
    code <- switch(action$type,
      click = sprintf('app$click("%s")', 
                     generate_selector(action$params[[1]], "button")),
      select = sprintf('app$set_inputs("%s" = "%s")', 
                      action$params[[2]], action$params[[1]]),
      input = sprintf('app$set_inputs("%s" = "%s")', 
                     action$params[[2]], action$params[[1]]),
      expect = sprintf('app$expect_values(output = "%s")', 
                      generate_selector(action$params[[1]], "output")),
      wait = sprintf("app$wait_for_idle(duration = %d)", 
                    as.numeric(action$params[[1]]) * 1000)
    )
    code_lines <- c(code_lines, code)
  }
  
  # Add necessary scaffolding
  c(
    "test_that(sprintf('%s', test_name), {",
    "  # Initialize app driver with proper platform variant",
    "  app <- AppDriver$new(",
    "    variant = platform_variant(),",
    "    name = test_name,",
    sprintf("    load_timeout = %d", getOption("shinytest2.load_timeout", 5000)),
    "  )",
    "",
    "  # Run test steps",
    paste("  ", code_lines),
    "",
    "  # Take final snapshot",
    "  app$expect_values()",
    "",
    "  # Cleanup",
    "  app$stop()",
    "})"
  )
}

#' Generate Shinytest2 Test from Natural Language Prompt
#'
#' @description This MCP tool converts natural language descriptions into
#' production-ready shinytest2 test code using advanced prompt parsing
#'
#' @param prompt Character string describing the test scenario in natural language
#' @param app_path Character string path to Shiny application directory or app.R file
#' @param test_name Optional character string for test name (auto-generated if NULL)
#' @param options Named list of additional options (browser, headless, timeout)
#'
#' @return List with structure: list(success = TRUE/FALSE, data = test_code, 
#'         message = "...", error = NULL)
#'
#' @examples
#' \dontrun{
#' result <- generate_shinytest2_test(
#'   prompt = "Click submit button and verify success message appears",
#'   app_path = "./my-shiny-app",
#'   test_name = "submit_workflow"
#' )
#' }
#'
#' @export
generate_shinytest2_test <- function(prompt, app_path, test_name = NULL, 
                                   options = list()) {
  # Validate inputs
  tryCatch({
    if (!is.character(prompt) || length(prompt) != 1 || nchar(prompt) == 0) {
      stop("Prompt must be a non-empty character string")
    }
    
    if (!is.character(app_path) || length(app_path) != 1 || nchar(app_path) == 0) {
      stop("App path must be a non-empty character string")
    }
    
    if (!is.null(test_name) && 
        (!is.character(test_name) || length(test_name) != 1)) {
      stop("Test name must be NULL or a character string")
    }
    
    if (!is.list(options)) {
      stop("Options must be a list")
    }
    
    # Set default options
    default_options <- list(
      browser = "chromium",
      headless = TRUE,
      timeout = 5000
    )
    options <- modifyList(default_options, options)
    
    # Validate app path exists
    if (!file.exists(app_path)) {
      stop("App path does not exist: ", app_path)
    }
    
    # Generate test name if not provided
    if (is.null(test_name)) {
      test_name <- sprintf("test_%s", format(Sys.time(), "%Y%m%d_%H%M%S"))
    }
    
    # Parse natural language prompt into test actions
    cli::cli_alert_info("Parsing test actions from prompt...")
    actions <- parse_test_actions(prompt)
    
    if (length(actions) == 0) {
      stop("No test actions could be parsed from prompt")
    }
    
    # Generate test code
    cli::cli_alert_info("Generating test code...")
    test_code <- generate_test_code(actions)
    
    # Return success response
    list(
      success = TRUE,
      data = paste(test_code, collapse = "\n"),
      message = sprintf("Successfully generated test with %d actions", 
                       length(actions)),
      error = NULL
    )
    
  }, error = function(e) {
    # Return error response
    cli::cli_alert_danger(conditionMessage(e))
    list(
      success = FALSE,
      data = NULL,
      message = "Failed to generate test",
      error = conditionMessage(e)
    )
  })
}

#' Create MCP Server for Shinytest2 Test Generation
#'
#' @description Creates and starts an MCP server that provides the
#' generate_shinytest2_test tool
#'
#' @param port Numeric port number to run server on
#' @param host Character string host address to bind to
#' @return mcpr server object
#' @export
create_shinytest2_mcp_server <- function(port = 8080, host = "127.0.0.1") {
  # Create MCP tool definition
  tool <- mcp_tool(
    name = "generate_shinytest2_test",
    description = paste(
      "Generates automated test cases for Shiny applications using natural",
      "language prompts. Converts descriptions into shinytest2 code."
    ),
    parameters = list(
      prompt = list(
        type = "string",
        description = "Natural language description of test scenario"
      ),
      app_path = list(
        type = "string", 
        description = "Path to Shiny application"
      ),
      test_name = list(
        type = "string",
        description = "Optional name for generated test",
        required = FALSE
      ),
      options = list(
        type = "object",
        description = "Additional options for test generation",
        required = FALSE
      )
    ),
    handler = generate_shinytest2_test
  )
  
  # Create and start MCP server
  server <- mcp_http(
    host = host,
    port = port,
    tools = list(tool)
  )
  
  cli::cli_alert_success(sprintf(
    "MCP server started at http://%s:%d", 
    host, 
    port
  ))
  
  server
}