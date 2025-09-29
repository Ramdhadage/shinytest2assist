# app.R
library(shiny)

ui <- fluidPage(
  titlePanel("Demo Shiny App for Testing"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("name", "Enter your name:", ""),
      selectInput("color", "Choose favorite color:",
                 choices = c("Red", "Blue", "Green")),
      numericInput("age", "Enter your age:", 
                  value = 25, min = 0, max = 120),
      actionButton("submit", "Submit")
    ),
    
    mainPanel(
      h3("Results"),
      verbatimTextOutput("greeting"),
      plotOutput("colorPlot"),
      tableOutput("summary")
    )
  )
)

server <- function(input, output) {
  # Create reactive values to store submission state
  values <- reactiveValues(submitted = FALSE)
  
  # Update submitted state when button is clicked
  observeEvent(input$submit, {
    values$submitted <- TRUE
  })
  
  # Generate greeting
  output$greeting <- renderText({
    req(values$submitted)
    sprintf("Hello %s!", input$name)
  })
  
  # Create a simple plot based on color choice
  output$colorPlot <- renderPlot({
    req(values$submitted)
    color_values <- c("Red" = "red", "Blue" = "blue", "Green" = "green")
    
    # Create more realistic age distribution data
    # Simulating a normal-like distribution centered around age 40
    set.seed(123)  # for reproducibility
    ages <- c(
        rnorm(50, mean = 30, sd = 5),  # younger group
        rnorm(100, mean = 40, sd = 8),  # middle age group
        rnorm(30, mean = 60, sd = 7)    # older group
    )
    ages <- pmax(20, pmin(80, round(ages)))  # constrain ages between 20 and 80
    
    # Create age groups and calculate frequencies
    age_groups <- cut(ages, breaks = seq(20, 80, by = 10))
    frequencies <- table(age_groups)
    
    # Create the visualization
    par(bg = "white")
    bp <- barplot(frequencies, 
            col = color_values[input$color],
            main = sprintf("Age Distribution\nViewed in %s", input$color),
            xlab = "Age Groups",
            ylab = "Number of People",
            border = "white",
            ylim = c(0, max(frequencies) * 1.2))
    
    # Add decorative elements
    grid(nx = NA, ny = NULL, col = "gray90", lty = "dotted")
    
    # Add value labels on top of each bar
    text(bp, frequencies + max(frequencies) * 0.05, 
         frequencies, 
         col = "darkgray",
         font = 2)
    
    # Add user's age marker
    if (!is.na(input$age)) {
        user_group <- cut(input$age, breaks = seq(20, 80, by = 10))
        bar_index <- which(levels(age_groups) == user_group)
        if (length(bar_index) > 0) {
            points(bp[bar_index], 
                   frequencies[user_group] + max(frequencies) * 0.1, 
                   pch = 25, 
                   bg = "yellow",
                   col = "black",
                   cex = 2)
            text(bp[bar_index],
                 frequencies[user_group] + max(frequencies) * 0.15,
                 "You are here!",
                 col = color_values[input$color],
                 font = 2)
        }
    }
})
  
  # Create a summary table
  output$summary <- renderTable({
    req(values$submitted)
    data.frame(
      Field = c("Name", "Age", "Favorite Color"),
      Value = c(input$name, input$age, input$color)
    )
  })
}

shinyApp(ui = ui, server = server)
