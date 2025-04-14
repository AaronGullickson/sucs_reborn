library(megamekR)
library(tidyverse)
library(xml2)
library(here) 

detect_faction <- function(factions, pattern) {
  factions |>
    map_lgl(function(x) {
      x |>
        str_split_1(",") |>
        str_trim() |>
        str_detect(pattern) |>
        any()
    })
}


# Load data---------------------------------------------------------------

# load the reshaped SUCS data
sucs_data <- read_csv(here("data", "sucs_data.csv"))

# load the mhq faction events data
load(here("data", "mhq_faction_events.RData"))

# TODO: Figure out missing id_sucs values
faction_events <- faction_events |>
  filter(!is.na(id_sucs))

# read in faction crosswalk
gs4_deauth()
faction_crosswalk <- read_sheet("117Mmhf7TtyumwCzB9bGKq05-SeTFKboZ7Ef2ZbyOEPk",
                                sheet = "translator") |>
  rename(sucs_faction = their_code, faction = our_code) |>
  select(sucs_faction, faction) |>
  filter(!is.na(faction))

# Specific projects -------------------------------------------------------

# Invading Clans project - somebody put in a bunch of stuff for the transfer
# stations in the deep periphery which is definitely **not** in the Invading
# Clans Sourcebook (maybe IE:ISP3?). However the furthest north we have an
# actual entry is Von Strang's world at y == 515.891, so only do cases at or
# below that in y.
faction_events <- faction_events |>
  mutate(
    source_faction = if_else(year(date) >= 3049 & 
                               year(date) <= 3052 & 
                               y <= 515.891 & # Von Stang's World
                               y >= 210.42 & # Tukkayid
                               detect_faction(faction, "(CW|CGB|CJF|CSJ|CSV|CNC|FC|DC)"),
                             "Invading Clans", source_faction))

# Shattered Fortress
faction_events <- faction_events |>
  mutate(source_faction = if_else(year(date) >= 3146 & year(date) <= 3150,
                                  "Shattered Fortress", source_faction))

# FCCW - it looks like only FS and LA were coded, from 3062 to 3067?
faction_events <- faction_events |>
  mutate(source_faction = if_else(year(date) >= 3062 & date <= "3067-04-20" & 
                                    detect_faction(faction, "FC|LA|FS"),
                                  "FedCom Civil War", source_faction))


# Format as sucs data -----------------------------------------------------

faction_events <- faction_events |>
    filter(!is.na(source_faction)) |>
  mutate(
    source_type = "text",
    source_loc = "<b>page needed</b>"
  ) |>
  select(id_sucs,
         id_mhq,
         x, 
         y,
         source_type, 
         source_title = source_faction, 
         source_date = date, 
         source_loc, 
         faction)


# Convert to SUCS faction codes -------------------------------------------

# first we need to deal with disputed cases
faction_events <- faction_events |>
  mutate(disputed = if_else(str_detect(faction, ","), faction, ""),
         faction = if_else(str_detect(faction, ","), "DIS", faction),
         # a bit of a cheat but we can assume any FC should be FCF
         faction = if_else(faction == "FC", "FCF", faction)) |>
  left_join(faction_crosswalk) |>
  mutate(faction = if_else(is.na(sucs_faction), faction, sucs_faction))


# separate out disputed cases
disputed_cases <- faction_events |> filter(faction == "D")
faction_events <- faction_events |> filter(faction != "D")

# deal with disputed cases separately
disputed_cases <- disputed_cases |>
  select(-sucs_faction) |>
  separate_wider_delim(disputed, ", ", too_few = "align_start", names_sep = "") |> 
  pivot_longer(starts_with("disputed")) |>
  mutate(value = if_else(value == "", NA, value),
         value = if_else(value == "FC", "FCF", value)) |>
  left_join(faction_crosswalk, by = join_by(value == faction)) |>
  mutate(value = if_else(is.na(sucs_faction), value, sucs_faction)) |> 
  select(-sucs_faction) |>
  pivot_wider() |>
  unite("disputed", starts_with("disputed"), sep = "/", na.rm = TRUE) |>
  mutate(faction = paste0(faction, "(", disputed, ")")) |>
  select(-disputed)

faction_events <- faction_events |>
  select(-disputed, -sucs_faction) |>
  bind_rows(disputed_cases)

# Combine with sucs -------------------------------------------------------

sucs_data <- sucs_data |>
  bind_rows(faction_events) |>
  mutate(hidden = if_else(is.na(hidden), FALSE, hidden)) |>
  arrange(id_sucs, source_date)

write_csv(sucs_data, file = here("data", "sucs_data.csv"))
