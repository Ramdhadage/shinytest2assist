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
    
    plot(1:10, 1:10, 
         col = color_values[input$color],
         pch = 19,
         cex = 2,
         main = "Your Color Choice")
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
