#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(googlesheets4)
library(tidyverse)
library(here)
library(plotly)
library(randomcoloR)
source("functions.R")
load("sucs_data.RData")
source_types <- unique(sucs_data$source_type)
names(source_types) <- str_to_title(source_types)
sources <- sort(unique(sucs_data$source_title))
time_periods <- tribble(
  ~period,  ~date,
  "Free Worlds League Founding", date("2271-06-02"),
  "Federated Suns Founding", date("2317-06-26"),
)

# Define UI for application that draws a histogram
#ui <- fluidPage(
ui <- page_fillable(
  title = "Battletech Universe Faction Map",
  theme = bs_theme(bootswatch = "superhero"),
  card(
    card_header("Battletech Universe Faction Map"),
    layout_sidebar(
      sidebar = sidebar(
        width = 350,
        dateInput(
          inputId = "date",
          label = "Choose a date",
          value = date("3085-10-31")),
        selectInput( 
          "select_color", 
          "Color by:", 
          list("Faction" = "faction", "Source" = "source") 
        ),
        downloadButton("download", "Download CSV file"),
        input_switch(
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
      plotlyOutput(outputId = "plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # uncomment out to test out bootstrap themes
  #bs_themer()
  
  output$plot <- renderPlotly({ 
    sucs_data |>
      filter(!input$remove_undiscovered | faction != "U") |>
      filter(source_type %in% input$source_types) |>
      filter(source_title %in% input$sources) |>
      plot_planets(input$date, 
                   choice_color = input$select_color,
                   faction_data = sucs_factions)
  }) 
  
  output$download <- downloadHandler(
    filename = paste0("battletech_map_", input$date, ".csv"),
    content = function(file) {
      sucs_data |>
        filter(!input$remove_undiscovered | faction != "U") |>
        filter(source_type %in% input$source_types) |>
        filter(source_title %in% input$sources) |>
        faction_snapshot(input$date) |>
        write_csv(file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
