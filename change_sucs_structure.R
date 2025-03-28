library(googlesheets4)
library(tidyverse)
library(here)



# Read in SUCS data -------------------------------------------------------

gs4_deauth()

sucs_data <- read_sheet("1uO6aZ20rfEcAZJ-nDRhCnaNUiCPemuoOOd67Zqi1MVM", 
                        sheet = "Systems", skip = 1) |>
  select(-systemName, -alternateName, -x, -y, -size, -sarnaLink, -`distance (LY)`) |>
  rename(sucsId = systemId)


# Reshape and clean SUCS data ---------------------------------------------

# pivoting longer will get it in the format we want
sucs_data <- sucs_data |>
  pivot_longer(cols = !sucsId, names_to = "time_point", values_to = "faction") |>
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


# Add map source information ----------------------------------------------

# we will start with the easy cases where we have a single map source for the
# column
test <- sucs_data |>
  mutate(
    source_type = "map",
    source_title = case_when(
      time_point == "2271" ~ "Handbook: House Marik"
    ),
    source_loc = case_when(
      time_point == "2271" ~ "p. 16"
    ),
    source_date = case_when(
      time_point == "2271" ~ date("2271-06-01")
    )
    
  )
  
