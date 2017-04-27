library(shiny)
#ui <- fluidPage("Hello World")
ui <- fluidPage(sliderInput(inputId = "num",
                            label = "Choose a number",
                            value = 25, min = 1, max = 1000),
                plotOutput("hist")
                )

#input/output are list like objects
#output has something built with render functions - place R objects as HTML in shiny web page

server <- function(input, output) {
output$hist <- renderPlot({
  title <- "Selected random normal values"
  #accessing input values #reactivity
  hist(rnorm(input$num), main = title)
  })  
  
}
shinyApp(ui = ui, server = server)