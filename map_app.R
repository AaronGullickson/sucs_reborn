

# Load libraries ----------------------------------------------------------

library(shiny)
library(bslib)
library(googlesheets4)
library(tidyverse)
library(here)
library(plotly)
library(randomcoloR)
library(sf)
library(smoothr)
library(concaveman)

# Functions ---------------------------------------------------------------

# load custom functions
source("functions.R")

# javascript code for dynamic labels
js_dynamic_labels <- paste(readLines("add_dynamic_labels.js"), collapse = "\n")

# the plotting function
plot_planets <- function(snapshot_data,
                         title = "Battletech Faction Map", 
                         use_polygons = FALSE,
                         xrange = c(-610, 795),
                         yrange = c(-595, 600),
                         choice_color = "faction",
                         palette_color = palettes[["faction"]]) {
  
  legend_name <- choice_color |> str_replace_all("_", " ") |> str_to_title()
  
  faction_capital_data <- snapshot_data |> 
    filter(capital == "Faction")
  
  major_capital_data <- snapshot_data |> 
    filter(capital == "Major")
  
  minor_capital_data <- snapshot_data |> 
    filter(capital == "Minor")
  
  # Base ggplot
  map <- snapshot_data |>
    ggplot(aes(x = x, y = y, color = !!sym(choice_color),
               customdata = id_mhq))
  
  if(use_polygons & choice_color == "faction") {
    # add hulls
    hull_data <- snapshot_data |>
      group_by(faction) |>
      filter(!(faction %in% c("Abandoned", "Unsettled", "Independent",
                              "Interstellar Expeditions", "Disputed",
                              "Clan Sea Fox", "ComStar"))) |>
      generate_hull()
    map <- map +
      suppressWarnings(
        geom_polygon(data = hull_data, 
                     aes(x = X, y = Y, fill = faction, group = faction,
                         text = faction), 
                     alpha = 0.2,
                     inherit.aes = FALSE,
                     show.legend = FALSE)
        )
  }
  
  map <- map + 
    # some fancy stuff here for capital rings
    geom_point(data = faction_capital_data, size = 4)+
    geom_point(data = faction_capital_data, color = "grey20", size = 2.5)+
    geom_point(data = major_capital_data, size = 3.5)+
    geom_point(data = major_capital_data, color = "grey20", size = 2.5)+
    geom_point(data = minor_capital_data, size = 3)+
    geom_point(data = minor_capital_data, color = "grey20", size = 2.5)+
    suppressWarnings(geom_point(size = 2, aes(text = text_plotly))) +
    scale_color_manual(values = palette_color)+
    scale_fill_manual(values = palette_color)+
    labs(title = title, color = legend_name)+
    guides(fill = "none")+
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

# generate hulls
generate_hull <- function(map_data) {
  map_data |>
    select(x, y, faction) |>
    # Convert to sf POINT, CRS is NA for cartesian plot
    st_as_sf(coords = c("x", "y"), crs = NA) |>
    group_by(faction) |>
    # need more than 3 planets to do it
    filter(n() > 3) |>
    group_split() |>
    map(function(group_data) {
      # remove outliers that are more than 500 LY from centroid
      group_data <- remove_outliers(group_data, 500)
      if(nrow(group_data) == 0) {
        return(NULL)
      }
      
      # concaveman will find a concave hull
      hull <- concaveman(group_data) |>
        # buffer out to 30 LY
        st_buffer(dist = 30) |>
        # Smooth the edges
        smooth(method = "ksmooth", smoothness = 5) |>
        # convert to coordinates for plotting
        st_coordinates() |>
        as_tibble() |>
        # add faction back in
        mutate(faction = group_data$faction[1])
    }) |>
    bind_rows()
}

# remove observations that are a long way from the centroid
remove_outliers <- function(group_data, max_distance) {
  # Calculate the centroid (center of mass) for the group
  centroid <- st_centroid(st_union(group_data))
  # Calculate the distance of each point from the centroid
  distances <- st_distance(group_data, centroid)
  # Keep only the points within the maximum distance threshold
  filtered_points <- group_data[distances <= max_distance, ]
  return(filtered_points)
}



# Load and organize data --------------------------------------------------

# unofficial way to determin if local but from Yihui Xie 
is_local <- Sys.getenv('SHINY_PORT') == ""

# load the data - comment out one to read locally or remotely
if(is_local) {
  data_address <- here("data", "sucs_data.csv")
  factions_address <- here("data", "sucs_factions.csv")
  sources_address <- here("data", "sucs_sources.csv")
} else {
  data_address <- "https://raw.githubusercontent.com/AaronGullickson/sucs_reborn/refs/heads/master/data/sucs_data.csv"
  factions_address <- "https://raw.githubusercontent.com/AaronGullickson/sucs_reborn/refs/heads/master/data/sucs_factions.csv"
  sources_address <- "https://raw.githubusercontent.com/AaronGullickson/sucs_reborn/refs/heads/master/data/sucs_sources.csv"
}
sucs_data <- read_csv(data_address)
sucs_factions <- read_csv(factions_address)
sucs_sources <- read_csv(sources_address)

# Organize the data for ease of use in the plot
map_data <- sucs_data |>
  # remove any duplicates (shouldn't be, but good to check)
  distinct() |>
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
    # now turn source_title into a factor
    source_title = factor(source_title,
                           levels = sucs_sources$source),
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

# Define globals --------------------------------------------------------

# what source types do we have?
source_types <- unique(map_data$source_type)
names(source_types) <- str_to_title(source_types)

# what planets do we have
planet_ids <- sort(unique(map_data$id_mhq))
system_coords <- map_data |>
  select(id_mhq, x, y) |>
  distinct()

# list of ISP sources to potentially exclude
isp_list <- c("Interstellar Players", "IE: Interstellar Players 3")

# list of eras
eras <- list(
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

# define color palettes. The palettes object should be a named list 
# with the names corresponding to the the different color_choice options.
# Each palette should be a named vector with names corresponding to all
# possible options for a given color_choice
palette_faction <- sucs_factions |> select(name, color) |> deframe()
palette_source <- randomColor(length(unique(map_data$source_title)))
names(palette_source) <- unique(map_data$source_title)
palettes = list(faction = palette_faction, source_title = palette_source)

# Set the initial zoom level/layout - use whole Inner Sphere as default
current_range <- list(xrange = c(-610, 795), yrange = c(-595, 600))

# Shiny app ---------------------------------------------------------------

ui <- page_fillable(
  title = "Battletech Universe Faction Map",
  theme = bs_theme(bootswatch = "superhero"),
  card(
    card_header("Battletech Universe Faction Map"),
    layout_sidebar(
      sidebar = sidebar(
        width = 375,
        card(
          card_header("Choose time period"), 
          selectizeInput( 
            inputId = "era",
            label = "Pick an era...", 
            choices = eras,
            selected = "3152-07-01"
          ),
          dateInput(
            inputId = "date",
            label = "Or choose a specific date...",
            value = date("3152-07-01"),
          ),
          p("Advance date by a ..."),
          fluidRow(
            actionButton("increment_year", "year", style = 'display: inline-block; margin-left: 15px;', width = 100), 
            actionButton("increment_month", "month", style = 'display: inline-block;', width = 100), 
            actionButton("increment_day", "day", style = 'display: inline-block;', width = 100)
          ),
          p("Reverse date by a ..."),
          fluidRow(
            actionButton("decrement_year", "year", style = 'display: inline-block; margin-left: 15px;', width = 100), 
            actionButton("decrement_month", "month", style = 'display: inline-block;', width = 100), 
            actionButton("decrement_day", "day", style = 'display: inline-block;', width = 100)
          )
        ),
        card(
          card_header("Map controls"),
          selectizeInput( 
            "select_planet", 
            "Center on:", 
            choices = NULL
          ),
          selectInput( 
            "select_color", 
            "Color by:", 
            list("Faction" = "faction", "Source" = "source_title") 
          ),
          checkboxGroupInput( 
            "source_types", 
            "Source Types:", 
            source_types,
            selected = source_types
          ),
          input_switch(
            "show_boundaries", 
            "Show boundaries (slower)?", 
            TRUE
          ), 
          input_switch(
            "show_unsettled", 
            "Show unsettled planets?", 
            FALSE
          ), 
          input_switch(
            "show_abandoned", 
            "Show abandoned planets?", 
            FALSE
          ), 
          input_switch(
            "show_hidden", 
            "Show hidden/secret planets?", 
            TRUE
          ),
          input_switch(
            "use_isp", 
            "Use Interstellar Players data?", 
            TRUE
          ),
          input_switch(
            "use_arano", 
            "Use House Arano data?", 
            TRUE
          )
        ),
        downloadButton("download", "Download CSV file"),
      ),
      plotlyOutput(outputId = "plot")
    )
  )
)

server <- function(input, output, session) {

  # uncomment out to test out bootstrap themes
  #bs_themer()
  
  trigger_recenter <- reactiveVal(0)
  
  # use server side selectize for speed
  updateSelectizeInput(session, 'select_planet', 
                       choices = c("Choose planet...", planet_ids),
                       server = TRUE)
  
  observeEvent(input$year,{
    req(input$date)
    updateDateInput(session, "date", 
                    value = date(paste(input$year, 
                                       month(input$date), 
                                       day(input$date), sep = "-")))
    
  })
  
  observeEvent(input$era,{
    updateDateInput(session, "date", value = date(input$era))
    
  })
  
  observeEvent(input$increment_year,{
    updateDateInput(session, "date", value = (input$date %m+% years(1)))
    
  })
  
  observeEvent(input$increment_month,{
    updateDateInput(session, "date", value = (input$date %m+% months(1)))
    
  })
  
  observeEvent(input$increment_day,{
    updateDateInput(session, "date", value = (input$date %m+% days(1)))
    
  })
  
  observeEvent(input$decrement_year,{
    updateDateInput(session, "date", value = (input$date %m-% years(1)))
    
  })
  
  observeEvent(input$decrement_month,{
    updateDateInput(session, "date", value = (input$date %m-% months(1)))
    
  })
  
  observeEvent(input$decrement_day,{
    updateDateInput(session, "date", value = (input$date %m-% days(1)))
    
  })
  
  observeEvent(input$select_planet, {
    # check for recentering
    if(input$select_planet != "" & input$select_planet != "Choose planet...") {
      coords <- system_coords |> filter(id_mhq == input$select_planet)
      if(nrow(coords) == 1) {
        new_xrange <- c(coords$x - diff(current_range$xrange)/2,
                        coords$x + diff(current_range$xrange)/2)
        new_yrange <- c(coords$y - diff(current_range$yrange)/2,
                        coords$y + diff(current_range$yrange)/2)
        current_range <<- list(xrange = new_xrange, yrange = new_yrange)
        # trigger a redraw with a recentering
        trigger_recenter(trigger_recenter() + 1)
      }
    }
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

    # check for recenter
    trigger_recenter()
    
    map_data |>
      filter(input$use_isp | !(source_title %in% isp_list)) |>
      filter(input$use_arano | source_title != "Handbook: House Arano") |>
      filter(source_type %in% input$source_types) |>
      faction_snapshot(input$date) |>
      filter(input$show_unsettled | faction != "Unsettled") |>
      filter(input$show_abandoned | faction != "Abandoned") |>
      filter(input$show_hidden | !hidden) |>
      plot_planets(paste(input$date), 
                   use_polygons = input$show_boundaries,
                   choice_color = input$select_color,
                   palette_color = palettes[[input$select_color]],
                   xrange = current_range$xrange,
                   yrange = current_range$yrange)
  }) 
  
  output$download <- downloadHandler(
    filename = paste0("battletech_map_", input$date, ".csv"),
    content = function(file) {
      sucs_data |>
        filter(input$use_isp | !(source_title %in% isp_list)) |>
        filter(input$use_arano | source_title != "Handbook: House Arano") |>
        filter(source_type %in% input$source_types) |>
        faction_snapshot(input$date) |>
        filter(input$show_unsettled | faction != "Unsettled") |>
        filter(input$show_abandoned | faction != "Abandoned") |>
        filter(input$show_hidden | !hidden) |>
        write_csv(file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
