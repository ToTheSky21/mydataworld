# loading Libraries
library(shiny)
library(tidyverse)
library(tidytext)
library(plotly)
library(glue)
# reading data
expense_actuals <- read.csv("Expense_Actuals.csv")
head(expense_actuals)
# defining ui(user interface objects)
ui <- fluidPage(
  titlePanel("My First Shiny App"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "FiscalYear",
        label = "Choose a fiscal year",
        choices = c("2015", "2016", "2017")
      )),
    mainPanel(
      plotlyOutput(outputId = "funds_plot")
    )
  
 )
)

# Updating Server with the Plot
server <- function(input, output) {
  output$funds_plot <- renderPlotly({
    p <- expense_actuals %>%
      filter(FISCAL.YEAR == input$FiscalYear) %>%
      head(5) %>%
      
      ggplot(aes(x = AGENCY.NAME, y = ALL.FUNDS)) +
      geom_line(color = "gray") +
      geom_point(aes(color = ALL.FUNDS > 10000), size = 2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "lightgray") +
      labs(
        title = glue("Agency Summary, {input$FiscalYear} total funded actual expenses"),
        x = "Agency Name", y = "Fund Amount ($)"
      ) + 
      theme_minimal() +
      theme(legend.position = "none")
      ggplotly(p, tooltip = c("x", "y"))
  })
}
# calling Shiny App
shinyApp(ui = ui, server = server)