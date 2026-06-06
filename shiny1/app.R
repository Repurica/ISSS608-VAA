#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
exam <- read_csv("data/Exam_data.csv")
print(exam)
ui <- fluidPage(
  titlePanel("Pupils Exam Result Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "variable",
        label = "Subject: ",
        choices = c(
          "English" = "ENGLISH",
          "Maths" = "MATHS",
          "Science" = "SCIENCE"
        ),
        selected = "English"
      ),
      sliderInput(
        inputId = "bins",
        label = "Number of Bins",
        min = 5,
        max = 25,
        value = 10
      )
    ),
    mainPanel(plotOutput("distPlot")),
    position = "right"
  )
)


server <- function(input, output) {
  output$distPlot = renderPlot({
    ggplot(exam, aes_string(x=input$variable)) + geom_histogram(bins = input$bins,
                                                color = "black",
                                                fill = "turquoise")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
