library(shiny)
f <- function(x) g(x)
g <- function(x) h(x)
h <- function(x) x * 2

ui <- fluidPage(
    numericInput("n", "N", value = 2, min = 1, max = 10,step = 1),
    plotOutput("plot")
)
server <- function(input, output, session) {
    output$plot <- renderPlot({
        # d <- f(input$n)
        print(input$n)
        plot(head(cars, input$n))
    })
}
shinyApp(ui, server)

head(cars, 7) %>% plot()

f(2)
debugonce(f)
