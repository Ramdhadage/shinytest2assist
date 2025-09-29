#' @import shiny mcpr
#' @importFrom rlang arg_match
#' @importFrom cli cli_alert_success cli_alert_warning cli_alert_danger cli_alert_info
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom stringr str_match str_extract_all str_trim
#' @importFrom purrr map_chr
#' @importFrom glue glue
NULL

#' Parse Natural Language Prompt into Test Actions
#' @import stringr
#' @param prompt Character string containing test description
#' @return List of parsed actions with parameters in order
#' @keywords internal
#' @export
parse_test_actions <- function(prompt) {
  # Define regex patterns for common actions with more flexible matching
  patterns <- list(
    input = "(?:enter(?:ing)?|type(?:ing)?|input(?:ting)?)\\s+['\"]([^'\"]+)['\"]\\s+(?:in(?:to)?(?:\\s+the)?|to(?:\\s+the)?)\\s+([\\w\\s-]+?)(?:\\s+field|\\s+input|\\s+box)?(?=\\s*[,.]|\\s+and|$)",
    select = "select(?:ing)?\\s+['\"]([^'\"]+)['\"]\\s+from(?:\\s+the)?\\s+([\\w\\s-]+?)(?:\\s+dropdown|\\s+select|\\s+list)?(?=\\s*[,.]|\\s+and|$)",
    click = "click(?:ing)?(?:\\s+(?:the|on))?\\s+([\\w\\s-]+?)(?:\\s+button)?(?=\\s*[,.]|\\s+and|$)",
    expect = "(?:verify|expect|check)(?:\\s+that)?\\s+(.*?)(?=\\s*[,.]|\\s+and|$)",
    wait = "wait(?:ing)?\\s+(\\d+)(?:\\s+seconds?)?(?=\\s*[,.]|\\s+and|$)"
  )

  # Find all matches with their positions to maintain order
  all_matches <- list()

  for (action_type in names(patterns)) {
    matches <- gregexpr(patterns[[action_type]], prompt, ignore.case = TRUE, perl = TRUE)[[1]]

    if (matches[1] != -1) {
      for (i in seq_along(matches)) {
        match_start <- matches[i]
        match_length <- attr(matches, "match.length")[i]
        match_text <- substr(prompt, match_start, match_start + match_length - 1)

        # Extract captured groups
        capture_starts <- attr(matches, "capture.start")[i, ]
        capture_lengths <- attr(matches, "capture.length")[i, ]

        params <- list()
        for (j in seq_along(capture_starts)) {
          if (capture_starts[j] != -1) {
            param_text <- substr(prompt, capture_starts[j], capture_starts[j] + capture_lengths[j] - 1)
            params[[j]] <- trimws(param_text)
          }
        }

        all_matches[[length(all_matches) + 1]] <- list(
          type = action_type,
          params = params,
          position = match_start
        )
      }
    }
  }

  # Sort by position to maintain order
  if (length(all_matches) > 0) {
    positions <- sapply(all_matches, function(x) x$position)
    sorted_indices <- order(positions)
    all_matches <- all_matches[sorted_indices]

    # Remove position info from final output
    actions <- lapply(all_matches, function(x) {
      list(type = x$type, params = x$params)
    })
  } else {
    actions <- list()
  }

  return(actions)
}

#' Generate CSS Selector from Element Description
#'
#' @param element Character string describing the element
#' @param type Character string indicating element type
#' @return Character string containing CSS selector
#' @keywords internal
#' @export
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
#' @export
generate_test_code <- function(actions, app_path = ".", test_name = "generated_test") {
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
    "library(shinytest2)",
    sprintf("test_that( '%s ', {", test_name),
    "  # Initialize app driver with proper platform variant",
    "  app <- AppDriver$new(",
    sprintf("    app_dir = '%s',", app_path),
    sprintf("    name = '%s',", test_name),
    "    variant = platform_variant(),",
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
#' @param params List containing:
#'   \itemize{
#'     \item prompt Character string describing the test scenario
#'     \item app_path Character string path to Shiny application
#'     \item test_name Optional character string for test name
#'     \item options Named list of additional options
#'   }
#' @return List with structure: list(success = TRUE/FALSE, data = test_code,
#'         message = "...", error = NULL)
#' @export
generate_shinytest2_test <- function(params) {
  # Extract parameters from the params list
  prompt <- params$prompt
  app_path <- params$app_path
  test_name <- params$test_name
  options <- params$options %||% list()
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
    test_code <- generate_test_code(actions, app_path, test_name)

    # Return response in MCP format
    mcpr::response_text(list(
      success = TRUE,
      data = paste(test_code, collapse = "\n"),
      message = sprintf("Successfully generated test with %d actions", length(actions)),
      error = NULL
    ))

  }, error = function(e) {
    # Return error response in MCP format
    cli::cli_alert_danger(conditionMessage(e))
    mcpr::response_text(list(
      success = FALSE,
      data = NULL,
      message = "Failed to generate test",
      error = conditionMessage(e)
    ))
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
  tool <- new_tool(
    name = "generate_shinytest2_test",
    description = paste(
      "Generates automated test cases for Shiny applications using natural",
      "language prompts. Converts descriptions into shinytest2 code."
    ),
    input_schema = schema(
      properties = list(
        prompt = property_string(
          "Prompt",
          "Natural language description of test scenario",
          required = TRUE
        ),
        app_path = property_string(
          "App path",
          "Path to Shiny application",
          required = TRUE
        ),
        test_name = property_string(
          "Test name",
          "Optional name for generated test",
          required = FALSE
        ),
        options = property_object(
          "Options",
          "Additional options for test generation",
          properties = list(
            browser = property_string(
              "Browser",
              "Browser to use for testing",
              required = FALSE
            ),
            headless = property_boolean(
              "Headless",
              "Whether to run browser in headless mode",
              required = FALSE
            ),
            timeout = property_number(
              "Timeout",
              "Timeout in milliseconds",
              required = FALSE
            )
          ),
          required = FALSE
        )
      )
    ),
    handler = generate_shinytest2_test
  )

  # Create MCP server
  server <- new_server(
    name = "Shinytest2 Generator",
    description = "MCP server for generating shinytest2 test cases",
    version = "1.0.0"
  )

  server <- add_capability(server, tool)

  # Create custom HTTP server using httpuv
  httpuv::startServer(
    host = host,
    port = port,
    app = list(
      call = function(req) {
        if (req$REQUEST_METHOD == "POST") {
          # Read the request body
          body <- rawToChar(req$rook.input$read())

          # Parse JSON-RPC request
          request <- jsonlite::fromJSON(body)

          # Process using mcpr internals
          result <- mcpr:::process_request(server, request)

          # Return response
          list(
            status = 200L,
            headers = list(
              "Content-Type" = "application/json"
            ),
            body = jsonlite::toJSON(result, auto_unbox = TRUE)
          )
        } else {
          list(
            status = 405L,
            headers = list(
              "Content-Type" = "text/plain"
            ),
            body = "Method not allowed"
          )
        }
      }
    )
  )

  cli::cli_alert_success(sprintf(
    "MCP server started at http://%s:%d",
    host,
    port
  ))

  invisible(server)
}
