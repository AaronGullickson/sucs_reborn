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
source_types <- unique(sucs_data$source_type)
names(source_types) <- str_to_title(source_types)
sources <- sort(unique(sucs_data$source_title))
special_factions <- c("Abandoned" = "A", "Inhabited"= "I","Undiscovered" = "U")

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
            value = date("3079-08-01")),
          checkboxInput(
            "remove_undiscovered", 
            "Remove Undiscovered?", 
            TRUE
          ), 
          checkboxGroupInput( 
            "source_types", 
            "Source Types:", 
            source_types,
            selected = source_types
          ),
          checkboxGroupInput( 
            "sources", 
            "Sources:", 
            sources,
            selected = sources
          )
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
    sucs_data |>
      filter(!input$remove_undiscovered | faction != "U") |>
      filter(source_type %in% input$source_types) |>
      filter(source_title %in% input$sources) |>
      plot_planets(input$date, as.character(input$date),
                   faction_data = sucs_factions) |>
      layout(height = 800, width = 1000)
  }) 
}

# Run the application 
shinyApp(ui = ui, server = server)
