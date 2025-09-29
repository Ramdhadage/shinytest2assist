library(shiny)

# Define UI
ui <- fluidPage(
    titlePanel("Demo App for Shinytest2assist"),
    
    sidebarLayout(
        sidebarPanel(
            textInput("name", "Enter your name:", ""),
            selectInput("color", "Choose a color:",
                       choices = c("Red", "Blue", "Green")),
            actionButton("submit", "Submit")
        ),
        
        mainPanel(
            h3("Output"),
            verbatimTextOutput("result"),
            div(id = "success-message", style = "display:none;",
                class = "alert alert-success",
                "Successfully submitted!")
        )
    )
)

# Define server
server <- function(input, output, session) {
    observeEvent(input$submit, {
        output$result <- renderText({
            paste("Hello", input$name, "! Your favorite color is", 
                  tolower(input$color))
        })
        
        # Show success message
        runjs("document.getElementById('success-message').style.display = 'block';")
    })
}

# Run app
shinyApp(ui = ui, server = server)