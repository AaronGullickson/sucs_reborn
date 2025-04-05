

# Load libraries ----------------------------------------------------------

library(shiny)
library(bslib)
library(googlesheets4)
library(tidyverse)
library(here)
library(plotly)
library(randomcoloR)

# Functions ---------------------------------------------------------------

# load custom functions
source("functions.R")

# javascript code for dynamic labels
js_dynamic_labels <- paste(readLines("add_dynamic_labels.js"), collapse = "\n")

# the plotting function
plot_planets <- function(map_data,
                         date, 
                         title = NULL, 
                         xrange = c(-610, 795),
                         yrange = c(-595, 600),
                         choice_color = "faction",
                         faction_data = sucs_factions) {
  
  # Take a snapshot
  map_data <- map_data |>
    faction_snapshot(date)
  
  # Determine color palette - give a named vector to make sure colors match
  # in subsets
  if(choice_color == "faction") {
    map_data$var_color <- map_data$faction
    color_palette <- faction_data |> select(name, color) |> deframe()
    legend_name <- "Faction"
  } else {
    map_data$var_color <- map_data$source_title
    color_palette <- randomColor(length(unique(map_data$source_title)))
    names(color_palette) <- unique(map_data$source_title)
    legend_name <- "Source"
  }
  
  
  plot_title <- ifelse(is.null(title), as.character(date), title)
  
  faction_capital_data <- map_data |> 
    filter(capital == "Faction")
  
  major_capital_data <- map_data |> 
    filter(capital == "Major")
  
  minor_capital_data <- map_data |> 
    filter(capital == "Minor")
  
  # Base ggplot
  map <- map_data |>
    ggplot(aes(x = x, y = y, text = text_plotly, color = var_color,
               customdata = id_mhq)) +
    # some fancy stuff here for capital rings
    geom_point(data = faction_capital_data, size = 4)+
    geom_point(data = faction_capital_data, color = "grey20", size = 2.5)+
    geom_point(data = major_capital_data, size = 3.5)+
    geom_point(data = major_capital_data, color = "grey20", size = 2.5)+
    geom_point(data = minor_capital_data, size = 3)+
    geom_point(data = minor_capital_data, color = "grey20", size = 2.5)+
    geom_point(size = 2) +
    scale_color_manual(values = color_palette)+
    labs(title = plot_title, color = legend_name)+
    theme_void() +
    theme(panel.background = element_rect(fill = "grey20"),
          panel.grid = element_blank(),
          # these colors work well with superhero theme - change if it changes
          plot.background = element_rect(fill = "#3B4D5B"),
          text = element_text(color = "#EBEBEB"))
  
  map <- ggplotly(map, tooltip = "text") |>
    config(scrollZoom = TRUE)  |> 
    layout(dragmode = "pan",
           xaxis = list(range = xrange), 
           yaxis = list(range = yrange)) |>
    htmlwidgets::onRender(js_dynamic_labels)
  
  return(map)
}


# Preliminary setup --------------------------------------------------------

# unofficial way to determin if local but from Yihui Xie 
is_local <- Sys.getenv('SHINY_PORT') == ""

# load the data - comment out one to read locally or remotely
if(is_local) {
  data_address <- here("data", "sucs_data.csv")
  factions_address <- here("data", "sucs_factions.csv")
} else {
  data_address <- "https://raw.githubusercontent.com/AaronGullickson/sucs_reborn/refs/heads/master/data/sucs_data.csv"
  factions_address <- "https://raw.githubusercontent.com/AaronGullickson/sucs_reborn/refs/heads/master/data/sucs_factions.csv"
}
sucs_data <- read_csv(data_address)
sucs_factions <- read_csv(factions_address)

# get required lists and vectors
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

# default settings that may get changed and need to be tracked
# Use the whole Inner Sphere as the default range
current_range <- list(xrange = c(-610, 795), yrange = c(-595, 600))

# Organize the data for ease of use in the plot - this way this code only
# has to be run once

map_data <- sucs_data |>
  # get strings for disputed cases
  mutate(disputed = str_extract(faction, "(?<=\\()[^()]+(?=\\))")) |>
  separate_wider_delim(disputed, ",", too_few = "align_start", 
                       names_sep = "") |>
  # we don't know how many there are so pivot longer to get names
  pivot_longer(starts_with("disputed")) |>
  mutate(value = factor(value, 
                        levels = sucs_factions$id_sucs, 
                        labels = sucs_factions$name)) |>
  # now reshape back wider and concatenate disputed cases
  pivot_wider() |>
  unite("disputed", starts_with("disputed"), sep = "/", na.rm = TRUE) |>
  # now organize the rest of the labels
  mutate(
    # first clean the disputed parenthetical away
    faction = str_remove(faction, "\\s*\\([^\\)]+\\)"),
    # now turn faction into factor
    faction = factor(faction, 
                     levels = sucs_factions$id_sucs, 
                     labels = sucs_factions$name),
    # construct strings for the map display
    faction_str = if_else(disputed == "", 
                          paste0(faction, "<br>"),
                          paste0(faction, " (", disputed, ")<br>")),
    capital_str = if_else(is.na(capital), "", paste0(capital, " Capital<br>")),
    region1_str = if_else(is.na(region1), "", paste0(region1, "<br>")),
    region2_str = if_else(is.na(region2), "", paste0(region2, "<br>")),
    region3_str = if_else(is.na(region3), "", paste0(region3, "<br>")),
    source_str = paste0("<i>Source:</i> ", 
                        paste(source_type, source_title, source_loc, 
                              sep = ", ")),
    source_date_str = paste0("<br><i>Source Date:</i> ", source_date),
    text_plotly = paste0("<b>", id_mhq, "</b><br>",
                         faction_str,
                         capital_str, region1_str, region2_str, region3_str,
                         source_str, source_date_str)
  )



# Shiny app ---------------------------------------------------------------

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
  
  # Capture range changes using the relayout event
  observeEvent(event_data("plotly_relayout"), {
    
    event <- event_data("plotly_relayout")
    
    # Update the current_range value with new ranges
    new_xrange <- c(event$`xaxis.range[0]`, event$`xaxis.range[1]`)
    new_yrange <- c(event$`yaxis.range[0]`, event$`yaxis.range[1]`)
    if (!is.null(new_xrange) && !is.null(new_yrange)) {
      current_range <<- list(xrange = new_xrange, yrange = new_yrange)
    }
  })
  
  output$plot <- renderPlotly({ 
    
    map_data |>
      filter(input$show_unsettled | faction != "U") |>
      filter(input$show_hidden | !hidden) |>
      filter(input$show_isp | !(source_title %in% isp_list)) |>
      filter(input$show_arano | source_title != "Handbook: House Arano") |>
      filter(source_type %in% input$source_types) |>
      plot_planets(input$date, 
                   choice_color = input$select_color,
                   faction_data = sucs_factions,
                   xrange = current_range$xrange,
                   yrange = current_range$yrange)
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
        faction_snapshot(input$date) |>
        write_csv(file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
