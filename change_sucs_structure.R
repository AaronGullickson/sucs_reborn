library(googlesheets4)
library(tidyverse)
library(here)


# Helper functions ---------------------------------------------------

# define a bounding box by planet entries
create_box <- function(planet_left, planet_right, planet_high, planet_low) {
  x_left <- system_coords |> 
    filter(id_mhq == planet_left) |>
    pull(x)
  x_right <- system_coords |> 
    filter(id_mhq == planet_right) |>
    pull(x)
  y_high <- system_coords |> 
    filter(id_mhq == planet_high) |>
    pull(y)
  y_low <- system_coords |> 
    filter(id_mhq == planet_low) |>
    pull(y)
  
  list(x_left = x_left, x_right = x_right, y_high = y_high, y_low = y_low)
}

is_in_box <- function(x, y, box) {
  if(is.null(box)) {
    return(TRUE)
  }
  (x >= box$x_left & x <= box$x_right & y <= box$y_high & y >= box$y_low)
}


update_sources <- function(target, title, loc, date, 
                           box = NULL, factions = NULL) {
  sucs_data |> 
    # First, drop any values from sucs_data from the target_time that are not in 
    # the bounding box and do not come from acceptable factions
    filter(
      (time_point != target) | 
        (is_in_box(x, y, box) & (is.null(faction) | faction %in% factions))) |>
    # add source information
    mutate(
      source_title = case_when(time_point == target ~ title),
      source_loc = case_when(time_point == target ~ loc),
      source_date = case_when(time_point == target ~ date)
    )
  
}

#create_box("Loongana", "Palos", "Chaffee (LC)", "Rohinjan")

# Read in data -------------------------------------------------------

gs4_deauth()

sucs_data <- read_sheet("1uO6aZ20rfEcAZJ-nDRhCnaNUiCPemuoOOd67Zqi1MVM", 
                        sheet = "Systems", skip = 1)

id_crosswalk <- read_sheet("17GFFFp1sGvSYcs8DBryleQGvqgppX8EZ8MuF6Oq3or8")


# Reshape and clean SUCS data ---------------------------------------------

sucs_data <- sucs_data |>
  select(-systemName, -alternateName, -size, -sarnaLink, -`distance (LY)`) |>
  rename(id_sucs = systemId)

# pivoting longer will get it in the format we want
sucs_data <- sucs_data |>
  pivot_longer(cols = !c(id_sucs, x, y), 
               names_to = "time_point", 
               values_to = "faction") |>
  # remove missing values in faction at this point
  filter(!is.na(faction))

# get faction how we like it 
# TODO: ultimately this should also allow expanding out the region data so we 
# don't lose itm and probably keeping the disputed code for now
sucs_data <- sucs_data |>
  mutate(
    faction = case_when(
      str_detect(faction, "^D\\(") ~ str_extract(faction, "(?<=\\()[^()]+(?=\\))"),
      TRUE ~ str_split_i(faction, ",", 1)
    ),
    faction = str_trim(faction), # remove any leading/trailing whitespace
    faction = str_remove(faction, "\\s*\\([^\\)]+\\)")
  )

# add in MekHQ ids
sucs_data <- sucs_data |>
  left_join(id_crosswalk) |>
  select(id_sucs, x, y, id_mhq, time_point, faction)

# get the coordinates by mekhq id for bounding boxes
system_coords <- sucs_data |>
  select(id_mhq, x, y) |>
  distinct()

# add in empty columns for what we need and re-organize a bit
sucs_data <- sucs_data |>
  mutate(source_type = "map", 
         source_title = as.character(NA),
         source_loc = as.character(NA), 
         source_date = as_date(NA)) |>
  select(starts_with("id_"), x, y, time_point, starts_with("source_"), faction)

# Founding state maps from handbooks ----------------------------------------

# Lets start with the founding cases. These are complicated by the fact that 
# changes are sometimes clearly made outside the range of these maps. I can 
# fix some of that by specifying a bounding box for changes. I can also restrict
# to certain kinds of changes


# Handbook: House Marik
bounding_box <- create_box("Loongana", "Palos", "Chaffee (LC)", "Rohinjan")
sucs_data <- update_sources(
  target = "2271", 
  title = "Handbook: House Marik", 
  loc = "p. 16",
  date = date("2271-06-01"), 
  box = bounding_box, 
  factions = c("I", "U", "MCM", "SC", "FO", "PR", "TA")
)

# Handbook: House Davion
bounding_box <- create_box("Perkasie", "Niquinohomo", "Rowe", "Islamabad")
sucs_data <- update_sources(
  target = "2317", 
  title = "Handbook: House Davion", 
  loc = "p. 18",
  date = date("2317-06-26"), 
  box = bounding_box, 
  factions = c("I", "U", "FS")
)

# Create final data --------------------------------------------------------

sucs_data |>
  filter(!is.na(source_title)) |>
  select(id_sucs, id_mhq, starts_with("source_"), faction) |>
  arrange(id_sucs, source_date)
