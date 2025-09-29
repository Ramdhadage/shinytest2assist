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

Try it out yourself with our [demo app](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAYgAI1MsAdAJyYgBsBLAIxahYBPABQBnABZcIggJSt28iAFcutADwBaWgDMOKgCYAFKAHM4wxbVqkupDnGMQ4HC2AAicGEVoBlSdNoAQXQdIhZaABU4URsIEyYwGVxLWhTRLn04HgEAGShBIiVSCzYIKyt0zOyWR2cS9nLy8gAPUgBJCFQi12h4BNxUsABRMjhwgqVw3rhEfsGEpJTG0Wc4AnbO7oTiDjC5hIBhcSIiFZ0oADcw2zhaHbDZvCXGl5eCY64CaNoAXjvXLBwfT7MAAIT0cBBAHEWHAKAtFqVXsp4CxPh0usUEqZIXhBiNyONCuEcY8Bs9XpTaBcoBDfrQAEwAVgGMCk9IADKyoM16QBGBkcxENF5QdZcEigoqkEiuURKHhs0ggnwKpULZ7CsrlZ4wKBSWouCm0cQAZgB0SUHFIogWySRLwuY2yNhgUVaAHkipjXCZYXBYvFEvaRY1ULtSF7SD7tkRdjUI3bjaQoDx7FGY2B5TA9UINQ6rHIHUX2CWIIoViwneFNDolBBxbKpJiBoVo0UZLQQCk6AdYVByLR++KndTaUpvjLaDEwrd5YquKJ0iRpynyCkaRDROotMObE6AGrj6JiNW2cj6ekAMUCOR8QzLVh7tAAqqh9AO52fSBfV5-aAA7uIFC0Dw0oroudzcAQADWQIpEQPCVk6QxOmQwjNkUAAk85KgM3YFmOW44d+v61hEWAvkMKQAL6PqkDp0FCFBjP+fpwoGCHeth7EBlIJg7kOFCZCw7rFARoZCQAjsIm4TqIJELj+QL0RUqBomQ2iuAAEs4uy0AApKIACEcyYaQWHTPRdEpM+fZwP+UDTlwMDhrc4ZEKQoFQCsl4rvc4TvEQnyQg6baYlhAWGBGgmwhAInRZ5wgSdqViwjJcnRIpSoXqpdxxmEAD6mXbrWBAWsCYD0gksKVeSYIQgk1VgGmE7Qv65ZVX8CS8Z1eXPL2-aDl4sJCbSi42AQ9BmLQ+gTWiYE2CuH4pgNvguVaA78fQtAQGEeocBo3BwbN828EUEplF8oy1fQLCFPF023AALByzwrKQGArECwgCqana0HQ2hhEJ6lEPoSgELwXDcKQgjPDipVaOVxppXtLAwMITJcrQ8BQGUfymjjoiXn8TJJFYdATHEYy0H6hSoKjQ7o5jfIcjjeME7Qr0DCT9IABwU4DuMZPo9hPXT91KIzhGNCwLPCETrIOVzABsxOk7QADsAOU7QcYiZLDOagjZhI7QqB6s0wiCgMltSMI-M41L8XCIjMge3rxAQDE-DsojoEBgBcJlIK9CPU7zxrfZjkzfT0vbvjl4ELSBCbYO2jpRODZcNEptwIV8eoObafFIjAx8A5MHbn8KwybbtBOxXgj8kKeWZ3AUnZ1D3y1imabmDihdS8X-WEYNDmDqQwHUouSjjQAXltJDPKgAjCDwAndWAQE3Pmkk8KggnVB5xQd13FA96I5Ky5SOz0gFxXHqIwDmZFBUsAAuiGqVUrj+pc1EOpKQpAtIJECDNNwZ1FqXSYOwA8udg6XnZEZMymwLIBS1H-cozQOCpmahA24MIGa2ieLfV4gg8E8GagAOSUDAHgtMiDaFoIYOARA3L9CZo0HgYRDbb13uuMhkkqSUJcg-YQHMeTCHPt3XOohOwACpaB8gwAyD2Udx5BH0JeTIxB+D7luKseAZBbSET9BkYQEBeR-BoYEAYARbEvhyDkAY99t5+nyAATjenia0Ldt76E8hefev81qBB0URCctAqHOG3CuGUR9mG0Acu8bybBCItGKIfAYsjL7yNoAAan-tbPJOdohKNoByDAHIWQMREY0MpV8b4NPKO4wYH4WAwU8fDYRv8XjAzIPSdRmjJJ0AiZeJQlYADkicZq5jghkySXAWHCGMouDA0AMLoKwjiT2KUqRTLGMPBmglS7bIijiCu-Zq70jrjbHGTdQIBJUW3Jm1RCpSEyLyWsu93jCHsE6Dgog3ZmBOQnTsPw-hHJYOC1AeVGgrNoACigJhp4bwEJ8+KcBmidgAHxVM7Ac7BFtgqmI3qgYAHyvk4u-vU-pJLyhNPkcAGFcLP5FJKTIrO+SKm0GUdUvkLSGWMotmkv4zJhWiteJvZqgg9JEAAlw8hjL2kJDTGKGCyrWmqpxcMhFlIskUqpZimlzRv7cKpMy6IrLKzss5Vbblnc5F8oFRgPkLJLWUgSAATUKHdW4wFYSmT6dKtpcYH4fyfluV+OyAoWpVX-QZXkJUGqsDRZ4GbSg2QdHZIatwnLZlzC3fu9guLtgskWgQLdaxxREhEVM9hkrPHSrJZ+2VzwqWeCtKAGBM6wHMMaK8ucOCaxRmAGhA6QSEJBFeS41xBwHA-kmW+R46R-HKm-aYAw35XNoG-TBJsHQ5vYFmhQpQJBSEEMEVAwgVD0hULzMY1Y7nPrGGWMANFP5AA).

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