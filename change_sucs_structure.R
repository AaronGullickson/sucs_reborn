library(googlesheets4)
library(tidyverse)
library(here)


# Helper functions ---------------------------------------------------

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
  select(id_sucs, id_mekhq, time_point, faction)

# Add map source information ----------------------------------------------

# Lets start with the founding cases.
test <- sucs_data |>
  mutate(
    source_type = "map",
    source_title = case_when(
      time_point == "2271" ~ "Handbook: House Marik",
      time_point == "2317" ~ "Handbook: House Davion"
    ),
    source_loc = case_when(
      time_point == "2271" ~ "p. 16",
      time_point == "2317" ~ "p. 18"
    ),
    source_date = case_when(
      time_point == "2271" ~ date("2271-06-01"), # day before FWL founding
      time_point == "2317" ~ date("2317-06-26")  # founding day for FS
    )
    
  )
  
