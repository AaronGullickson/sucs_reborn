#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(googlesheets4)
library(tidyverse)
library(here)
library(plotly)
source("functions.R")
load("sucs_data.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Battletech Universe Faction Map"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          dateInput(
            inputId = "date",
            label = h3("Date input"),
            value = date("2830-01-01"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotlyOutput(outputId = "plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$plot <- renderPlotly({ 
    plot_planets(input$date, as.character(input$date)) |>
      layout(height = 800, width = 1000)
  }) 
}

# Run the application 
shinyApp(ui = ui, server = server)
