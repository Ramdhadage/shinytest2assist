# Shinytest2 Test Generation with MCP

## Overview

This package provides a Model Context Protocol (MCP) server that generates automated test cases for Shiny applications using natural language prompts. It leverages shinytest2 for browser automation and implements sophisticated prompt parsing to convert English descriptions into executable test code.

## Installation

```R
remotes::install_github("Ramdhadage/shinytest2assist")
```

## Usage

1. Start the MCP server:

```R
library(shinytest2assist)
server <- create_shinytest2_mcp_server(port = 8080)
```

2. Generate test cases by sending prompts:

```R
params <- list(prompt = "Enter 'John Doe' in name field, select 'Blue' from color dropdown, enter '30' in age field, click submit button, and verify greeting shows 'Hello John Doe!'",
  app_path = "demo-app",  # Update this path to where you saved app.R
  test_name = "test_form_submission")
# Generate test cases
test_result <- generate_shinytest2_test(
  params = params
)
cat(test_result$text[2])
```

## Features

- Natural language parsing for common UI interactions
- Robust CSS selector generation with fallbacks
- Platform-aware test generation
- Comprehensive error handling
- Built-in logging and debugging support
- MCP protocol compliance

## Contributing

Contributions welcome! Please read CONTRIBUTING.md for details.

## License

MIT