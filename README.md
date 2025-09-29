# shinytest2assist

Generate automated test cases for Shiny applications using natural language prompts.

## Overview

`shinytest2assist` is an R package that simplifies the creation of automated tests for Shiny applications. By leveraging the power of natural language processing and the shinytest2 framework, it enables developers to write test cases using plain English descriptions, which are then automatically converted into executable test code.

## Installation

```R
# Install from GitHub
remotes::install_github("Ramdhadage/shinytest2assist")
```

## Quick Start

1. First, load the library and create an MCP server:

```R
library(shinytest2assist)

# Start the MCP server
server <- create_shinytest2_mcp_server(port = 8080)
```

2. Generate test cases using natural language:

```R
# Define test parameters
params <- list(
  prompt = "Enter 'John Doe' in name field, select 'Blue' from color dropdown, 
           enter '30' in age field, click submit button, 
           and verify greeting shows 'Hello John Doe!'",
  app_path = "demo-app",
  test_name = "test_form_submission"
)

# Generate the test
test_result <- generate_shinytest2_test(params = params)
```

## Key Features

- **Natural Language Test Generation**
  - Write tests in plain English
  - Support for common UI interactions
  - Automatic conversion to executable code

- **Smart Selector Generation**
  - ID-based selectors
  - Name attribute matching
  - ARIA label support
  - Fallback chain strategy

- **Robust Test Framework**
  - Browser compatibility handling
  - Platform-specific adaptations
  - Configurable timeouts
  - Screenshot comparison support

- **Error Handling & Debugging**
  - Detailed error messages
  - Missing element detection
  - Validation checking
  - Built-in logging

## Demo

[Coming soon: GIF demonstrations of test generation and execution]

## Contributing

Contributions are welcome! Please read [CONTRIBUTING.md](CONTRIBUTING.md) for contribution guidelines.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.