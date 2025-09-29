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
result <- generate_shinytest2_test(
  prompt = "Click submit button and verify success message appears",
  app_path = "./my-shiny-app",
  test_name = "test_submit_workflow"
)
```

## Features

- Natural language parsing for common UI interactions:
  - Click buttons and links
  - Enter text in inputs
  - Select dropdown options  
  - Wait for specific durations
  - Verify output values
  
- Robust CSS selector generation with fallbacks:
  - ID-based selectors
  - Name attributes
  - ARIA labels
  - Automatic fallback chain
  
- Platform-aware test generation:
  - Browser compatibility handling
  - Platform-specific variants
  - Configurable timeouts
  - Screenshot comparisons

- Comprehensive error handling:
  - Detailed error messages
  - Invalid input validation
  - Missing element detection
  - Path verification

- Built-in logging and debugging:
  - CLI progress indicators
  - Action validation messages
  - Timing information
  - Failure diagnostics

- MCP protocol compliance:
  - Standard tool definition
  - Schema validation
  - Parameter documentation
  - Response formatting

## Contributing

Contributions welcome! Please read CONTRIBUTING.md for details.

## License

MIT