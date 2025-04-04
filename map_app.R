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
isp_list <- c("Interstellar Players", "IE: Interstellar Players 3")
time_periods <- list(
  "Major Houses Founded (2366)" = "2366-12-31",
  "Founding of the Star League (2571)" = "2571-12-31",
  "End of Reunification War (2596)" = "2596-12-31",
  "Height of the Star League (2750)" = "2750-12-31",
  "Start of First Succession War (2786)" = "2786-01-01",
  "End of First Succession War (2822)" = "2822-12-31",
  "Start of Second Succession War (2830)" = "2830-01-01",
  "End of Second Succession War (2864)" = "2864-12-31",
  "End of Third Succession War (3025)" = "3025-12-31",
  "End of Fourth Succession War (3030)" = "3030-01-31",
  "End of War of 3039 (3040)" = "3040-01-31",
  "Start of Clan Invasion (3049)" = "3049-06-01",
  "End of Clan Invasion (3052)" = "3052-10-31",
  "Start of FedCom Civil War (3063)" = "3063-12-31",
  "End of FedCom Civil War (3067)" = "3067-10-31",
  "Mid Jihad (3075)" = "3075-12-31",
  "Late Jihad (3079)" = "3079-12-31",
  "End of the Jihad (3081)" = "3081-12-31",
  "Early Republic of the Sphere (3085)" = "3085-12-31",
  "Late Republic of the Sphere (3130)" = "3130-01-01",
  "Early Dark Age (3135)" = "3135-01-01",
  "Late Dark Age (3145)" = "3145-01-01",
  "The Fall of the Republic (3151)" = "3151-01-01",
  "Dawn of the IlClan (3152)" = "3152-07-01"
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
        selectizeInput( 
          inputId = "era",
          label = h4("Pick an era..."), 
          choices = time_periods,
          selected = "3152-07-01"
        ),
        sliderInput(
          inputId = "year", 
          label = h4("Or select a year..."), 
          min = 2271, max = 3152, value = 3152, sep = ""), 
        dateInput(
          inputId = "date",
          label = h4("Or choose a specific date..."),
          value = date("3152-07-01"),
        ),
        selectInput( 
          "select_color", 
          h4("Color by:"), 
          list("Faction" = "faction", "Source" = "source") 
        ),
        downloadButton("download", "Download CSV file"),
        h4("Further Map Controls"),
        checkboxGroupInput( 
          "source_types", 
          h5("Source Types:"), 
          source_types,
          selected = source_types
        ),
        input_switch(
          "show_unsettled", 
          "Show unsettled planets?", 
          FALSE
        ), 
        input_switch(
          "show_hidden", 
          "Show hidden/secret planets?", 
          TRUE
        ),
        input_switch(
          "show_isp", 
          "Show Interstellar Players data?", 
          TRUE
        ),
        input_switch(
          "show_arano", 
          "Show House Arano data?", 
          TRUE
        )
      ),
      plotlyOutput(outputId = "plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # uncomment out to test out bootstrap themes
  #bs_themer()
  
  observeEvent(input$year,{
    updateDateInput(session, "date", 
                    value = date(paste(input$year, "01", "01", sep = "-")))
    
  })
  
  observeEvent(input$era,{
    updateDateInput(session, "date", value = date(input$era))
    
  })
  
  output$plot <- renderPlotly({ 
    sucs_data |>
      filter(input$show_unsettled | faction != "U") |>
      filter(input$show_hidden | !hidden) |>
      filter(input$show_isp | !(source_title %in% isp_list)) |>
      filter(input$show_arano | source_title != "Handbook: House Arano") |>
      filter(source_type %in% input$source_types) |>
      plot_planets(input$date, 
                   choice_color = input$select_color,
                   faction_data = sucs_factions)
  }) 
  
  output$download <- downloadHandler(
    filename = paste0("battletech_map_", input$date, ".csv"),
    content = function(file) {
      sucs_data |>
        filter(input$show_unsettled | faction != "U") |>
        filter(input$show_hidden | !hidden) |>
        filter(input$show_isp | !(source_title %in% isp_list)) |>
        filter(input$show_arano | source_title != "Handbook: House Arano") |>
        filter(source_type %in% input$source_types) |>
        filter(source_title %in% input$sources) |>
        faction_snapshot(input$date) |>
        write_csv(file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
