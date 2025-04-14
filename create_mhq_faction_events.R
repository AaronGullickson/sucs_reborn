# DL and organize the faction events from MekHQ once so I don't have to wait
# on it to process each time. Only run this if something changes on the MHQ
# side

library(megamekR)
library(tidyverse)
library(xml2)
library(here)

YAML_PATH <- "~/personal/Battletech/mek_project/mekhq/MekHQ/data/universe/planetary_systems/canon_systems"

planet_files <- list.files(YAML_PATH, pattern = "*.yml")

faction_events <- map(planet_files, function(planet_file) {
  planet_data <- read_planetary_data(here(YAML_PATH, planet_file))
  
  if(is.na(planet_data$system$primarySlot)) {
    return(NULL)
  }
  planet_events <- planet_data$planetary_events[[planet_data$system$primarySlot]]
  if(is.null(planet_events) | nrow(planet_events) == 0) {
    return(NULL)
  }
  planet_events |>
    filter(!is.na(faction)) |>
    select(date, faction, source_faction) |>
    add_column(id_mhq = planet_data$system$id, 
               id_sucs = planet_data$system$sucsId,
               x = planet_data$system$xcood,
               y = planet_data$system$ycood,
               .before = "date")
}) |> bind_rows()


# remove any duplicates
faction_events <- faction_events |>
  distinct(.keep_all = TRUE)

# remove sucs data as we will add it back in properly later
faction_events <- faction_events |>
  filter(is.na(source_faction) | source_faction != "sucs") 

# TODO: remove missing sucs ids but figure out whats missing
faction_events <- faction_events |>
  filter(!is.na(id_sucs))

# everything gets marked as noncanon unless proven otherwise
faction_events <- faction_events |>
  mutate(source_faction = NA)

save(faction_events, file = here("data", "mhq_faction_events.RData"))
