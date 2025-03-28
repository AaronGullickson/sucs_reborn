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
      source_title = case_when(
        time_point == target ~ title,
        TRUE ~ source_title),
      source_loc = case_when(
        time_point == target ~ loc,
        TRUE ~ source_loc),
      source_date = case_when(
        time_point == target ~ date,
        TRUE ~ source_date)
    )
  
}

#create_box("Loongana", "Palos", "Chaffee (LC)", "Rohinjan")

# Read in data -------------------------------------------------------

gs4_deauth()

sucs_data <- read_sheet("1uO6aZ20rfEcAZJ-nDRhCnaNUiCPemuoOOd67Zqi1MVM", 
                        sheet = "Systems", skip = 1)

sucs_factions <- read_sheet("1uO6aZ20rfEcAZJ-nDRhCnaNUiCPemuoOOd67Zqi1MVM", 
                        sheet = "Factions") |>
  select(factionId, factionName, color) |>
  rename(id_sucs = factionId, name = factionName) |>
  filter(!is.na(id_sucs))

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

# break up faction data into faction and regions. The max depth here is 5
sucs_data <- sucs_data |>
  separate_wider_delim(faction, ",", too_few = "align_start",
                       names = c("faction", paste("region", 1:4, sep="")))

# add in MekHQ ids
sucs_data <- sucs_data |>
  left_join(id_crosswalk) |>
  select(id_sucs, x, y, id_mhq, time_point, faction, starts_with("region"))

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
  select(starts_with("id_"), x, y, time_point, starts_with("source_"), 
         faction, starts_with("region"))

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

# for the House Marik data, we also know that the planets with FO, MCM, SC, and
# PR formed the FWL on 2271-06-02. So add that data
fwl_founders <- sucs_data |>
  filter(time_point == "2271" & faction %in% c("FO", "MCM", "PR", "SC")) |>
  mutate(source_date = date("2271-06-02"),
         faction = "FWL")

sucs_data <- sucs_data |>
  bind_rows(fwl_founders)

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

# Handbook: House Kurita
bounding_box <- create_box("Nathan", "Ottumwa", "Thule", "Edwards")
sucs_data <- update_sources(
  target = "2319", 
  title = "Handbook: House Kurita", 
  loc = "p. 18",
  # HBHK says Shiro Kurita had achieved his objectives by November (pg. 18), 
  # so lets set it at the midpoint of the month.
  date = date("2319-09-15"), 
  box = bounding_box, 
  factions = c("I", "U", "AG", "DC", "TH", "PoR", "TamP", "FoS")
)

# like the FWL, the current data has this as AG for the Alliance of Galedon
# We know Shiro shortly afterwards changed it to DC. Lets date it to end of 
# the month.
dc_founders <- sucs_data |>
  filter(time_point == "2319" & faction == "AG") |>
  mutate(source_date = date("2319-09-30"),
         faction = "DC")

sucs_data <- sucs_data |>
  bind_rows(dc_founders)

# Handbook: House Steiner
bounding_box <- create_box("Cavanaugh II", "Errai", "Zhongshan", "Savannah")
sucs_data <- update_sources(
  target = "2341", 
  title = "Handbook: House Steiner", 
  loc = "p. 13",
  # HBHS p. 12 says the foundation date was Jan 1, so set the original 
  # member states to 12-31
  date = date("2340-12-31"), 
  box = bounding_box, 
  factions = c("I", "U", "PD", "TamP", "FoS", "TH")
)

# Convert to LC on Jan 1 2341
lc_founders <- sucs_data |>
  filter(time_point == "2341" & faction %in% c("TamP", "FoS", "PD")) |>
  mutate(source_date = date("2341-01-01"),
         faction = "LC")

sucs_data <- sucs_data |>
  bind_rows(lc_founders)

# Handbook: House Liao
bounding_box <- create_box("Gannett", "Ridgebrook", "Ronel", "Ghorepani")
sucs_data <- update_sources(
  target = "2366", 
  title = "Handbook: House Liao", 
  loc = "p. 17",
  # we know the Capellan Confederation was declared in July 2366, lets mark
  # the pre-states as beginning of the year.
  date = date("2366-01-01"), 
  box = bounding_box, 
  factions = c("I", "U", "DL", "SS", "SIML", "CCom", "SiS", "TGU", "TH", "TC",
               "FWL", "FS", "LC", "CC")
)

# Now change them to CC.
# p. 15 of HBHL tells us that this happened during a convention on St. Andre
# in July of 2366, but not specific date 
cc_founders <- sucs_data |>
  filter(time_point == "2366" & faction %in% c( "DL", "SS", "SIML", "CCom", "SiS", "TGU")) |>
  mutate(source_date = date("2366-07-15"),
         faction = "CC")

sucs_data <- sucs_data |>
  bind_rows(cc_founders)

# Create final data --------------------------------------------------------

sucs_data <- sucs_data |>
  filter(!is.na(source_title)) |>
  select(id_sucs, id_mhq, x, y, starts_with("source_"), faction, starts_with("region")) |>
  arrange(id_sucs, source_date)

#gs4_auth()
#gs4_create("SUCS reborn", sheets = sucs_data)

# Create plots to test ----------------------------------------------------

plot_planets <- function(date) {
  # get the date for each planet closest to the date but not over
  temp <- sucs_data |>
    filter(source_date <= date) |>
    # arrange with most recent date at the top
    arrange(id_sucs, desc(source_date)) |>
    # remove duplicate planet entries
    filter(!duplicated(id_sucs)) |>
    select(x, y, faction) |>
    mutate(faction = factor(faction, 
                            levels = sucs_factions$id_sucs,
                            labels = sucs_factions$name))
  
  # determine color palette
  faction_colors <- sucs_factions |>
    filter(name %in% unique(temp$faction)) |>
    pull("color")
  
  temp |>
    filter(faction != "Undiscovered") |>
    ggplot(aes(x = x, y = y, color = faction))+
    geom_point()+
    scale_x_continuous(limits = c(-460, 460))+
    scale_y_continuous(limits = c(-460, 460))+
    scale_color_manual(values = faction_colors)+
    labs(title = as.character(date))+
    theme_void()+
    theme(panel.background = element_rect(fill = "grey20"),
          panel.grid = element_blank())
}

plot_planets(date("2367-01-01"))
