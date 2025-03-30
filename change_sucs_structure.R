library(googlesheets4)
library(tidyverse)
library(here)
library(plotly)
library(ggpubr)
library(ggrepel)

# TODO: we could do an "errata" type to handle corrections


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
    # First, drop any values from sucs_data from the target_time 
    mutate(
      # only change values that are from the target time and in
      # the bounding box and come from acceptable factions
      change_source = (time_point == target) & is_in_box(x, y, box) & 
        (is.null(faction) | faction %in% factions),
      # add source information
      source_title = case_when(
        !change_source ~ source_title,
        time_point == target ~ title),
      source_loc = case_when(
        !change_source ~ source_loc,
        time_point == target ~ loc),
      source_date = case_when(
        !change_source ~ source_date,
        time_point == target ~ date)
    )
  
}

correct_faction <- function(id, time_target, new_faction) {
  sucs_data |>
    mutate(faction = if_else(id_mhq %in% id & time_point %in% time_target, 
                             new_faction, faction))
}

correct_sources <- function(id, time_target,
                            new_source_title, new_source_loc, new_source_date) {
  sucs_data |>
    mutate(
      source_title = if_else(id_mhq %in% id & time_point %in% time_target, 
                             new_source_title, source_title),
      source_loc = if_else(id_mhq %in% id & time_point %in% time_target, 
                           new_source_loc, source_loc),
      source_date = if_else(id_mhq %in% id & time_point %in% time_target, 
                            new_source_date, source_date)
    )
}

faction_snapshot <- function(base_data, date) {
  # get the date for each planet closest to the date but not over
  base_data |>
    filter(source_date <= date) |>
    # create a type priority
    mutate(source_type = factor(source_type, 
                                levels = c("errata", "text", "map"))) |>
    # arrange with most recent date at the top, and then break date
    # ties by source_type
    arrange(id_sucs, desc(source_date), source_type) |>
    # remove duplicate planet entries
    filter(!duplicated(id_sucs)) |>
    select(starts_with("id_"), x, y, faction, 
           source_type, source_title, source_loc, source_date)
}

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
# TODO: we are missing a few new ones
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


# Fix some cases ----------------------------------------------------------

# There are some cases that if we fix them at the top, it will be easier
# some of these I might want to check on for clarification and errata but
# they definitely should not be in the maps

## Randis IV ##
# Randis IV is listed as I as early as 2750, but its described in Sarna as 
# being settled by refugees from the succession wars, so not clear what is 
# going on here. The earliest map I have it on is 3025, so change all 
# years before that to U
sucs_data <- correct_faction("Randis IV (Hope IV 2988-)", 
                             c("2750", "2765", "2767", "2783", "2786",
                               "2821", "2822", "2830", "2864"),
                             "U")

## McEvans' Sacrifice ## 
# Does not show up on this map. Listed as I based on OTP: Fronc Reaches 
# which I don't have. The first map entry is from the House Arano book
# (for 3025 map but not before), although the first "real" entry I can find is 
# from the 3063 map in the Era Report 3062 book. I think the Arano entry 
# should be handled with those other cases. So, I am going to list as U
# for all entries before 3025, and then remove entries betweent 3025 and 3063
# to handle them with Arano case
sucs_data <- correct_faction("McEvans' Sacrifice", 
                             c("2596", "2750", "2765", "2767", "2783", "2786",
                               "2821", "2822", "2830", "2864"),
                             "U")
sucs_data <- sucs_data |>
  filter(!(time_point %in% c("3025", "3030", "3040", "3049", "3050a", "3050b",
                             "3050c", "3051", "3052", "3057", "3058", "3059a",
                             "3059b", "3059c", "3059d") &
             id_mhq == "McEvans' Sacrifice"))

## Brasha ##
# This is showing up as independent based on p. 151 of Major Periphery
# States which says it was found in the "early 26th century" - but it does not
# show up on the map in Major Periphery States in 2571 or the 2596 maps from
# the Reunification War. My guess is this is a typo meant to be 27th century. 
# In any case, since we are going by maps only here, it needs to be U.
sucs_data <- correct_faction("Brasha", "2596", "U")

## Gibraltar ##
# this is showing up as independent because of a planet write up
# in Empire Alone that says it was independent until 2610, despite maps to the
# contrary
sucs_data <- correct_faction("Gibraltar", "2596", "FWL")

## Sherwood ##
# Seems to be "corrected" to independent from write up in Touring the 
# Stars. However, Sherwood does not show up at all in map, so it should be
# changed to U
sucs_data <- correct_faction("Sherwood", "2596", "U")

## Stotzing ##
# This one is listed as independent but doesn't show up on the map.
# I am guessing this is based on Touring the Stars: Stotzing, but this document
# says quite clearly "The world was considered officially settled in 2598, 
# "when its new capital of Alt-Eisenstadt—now known as Sophia—was founded."
# That would be more than a year after the Reunification War map, so this 
# should be listed as U
sucs_data <- correct_faction("Stotzing", "2596", "U")

## Alfirk ##
# this is listed as an independent world, apparently from very early
# on based on an entry from the Periphery handbook (2nd edition) and being
# put on maps in Era Report 3145. But on this map, it is not present. I think 
# it should be listed as U until 3145 where it makes its first map appearance
# we can correct with errata if necessary
sucs_data <- correct_faction("Alfirk", 
                             c("2596", "2750", "2765", "2767", "2783", "2786",
                               "2821", "2822", "2830", "2864", "3025", "3030", 
                               "3040", "3049", "3050a", "3050b", "3050c", 
                               "3051", "3052", "3057", "3058", "3059a", "3059b",
                               "3059c", "3059d", "3063", "3067", "3068", "3075",
                               "3079", "3081", "3085", "3095", "3130", "3135",
                               "3145", "3151", "3152"),
                             "U")

## Ward ## 
# The text on pg. 88 of HBHL says it was founded during "the Exodus
# from Terra" but it doesn't show up on maps in the same document until the 
# First Succession War map. It also shows up in the Era Report 2750 map. 
# Technically, the only named date in the entry on pg. 88 is for 2644, so 
# its not totally inconsistent that it wasn't founded until after Reunification
# War. For map it should be U
sucs_data <- correct_faction("Ward", "2596", "U")


## McEvedy's Folly ##
# This is shown as SL in 2765 based on Touring the Stars - McEvedy's Folly, but it 
# does not show up on maps until 3067, so make it until thens
# record as "U" here.
sucs_data <- correct_faction("McEvedy's Folly", 
                             c("2765", "2767", "2783", "2786",
                               "2821", "2822", "2830", "2864", "3025", "3030", 
                               "3040", "3049", "3050a", "3050b", "3050c", 
                               "3051", "3052", "3057", "3058", "3059a", "3059b",
                               "3059c", "3059d", "3063"), 
                             "U")

## Antallos (Port Krin) ##
# It is listed as SL. This is from Merc Supplemental II where
# is is listed as a joint venture of DC, FS, OA, and TH founded in 2674. Lets
# change the map reference to "I" and add a text entry
sucs_data <- correct_faction("Antallos (Port Krin)", 
                             c("2750", "2765"), "I")

sucs_data <- sucs_data |>
  bind_rows(
    tibble(
      id_sucs = 145,
      id_mhq = "Antallos (Port Krin)",
      x = 463.228,
      y = 281.417,
      time_point = "special",
      source_type = "text",
      source_title = "Mercenary FM Supplemental 2",
      source_loc = "p. 12",
      source_date = date("2674-01-01"),
      faction = "SL"
    )
  )

## Oberon Confederation ##
# Sigurd, Oberon VI, and Crellacor are showing up as OC from 2783 but
# they shouldn't be there until 3025
sucs_data <- correct_faction(c("Sigurd", "Oberon VI", "Crellacor"),
                             c("2783", "2786", "2821", "2822", "2830", "2864"),
                             "I")

## Joppa ##
# According to Empire Alone planetary write up, Joppa was settled during
# the Periphery Uprising campaign. However, it does not show up on a map
# until 3067 as part of MOC. So I think we should remove all of the SL and I 
# entries from 2596 to 3063 to make room for a text entry.
sucs_data <- sucs_data |>
  filter(!(id_mhq == "Joppa" & 
             time_point %in% c("2596", "2750", "2765", "2767", "2783", "2786",
                               "2821", "2822", "2830", "2864", "3025", "3030", 
                               "3040", "3049", "3050a", "3050b", "3050c", 
                               "3051", "3052", "3057", "3058", "3059a", "3059b",
                               "3059c", "3059d", "3063")))

## St. Andreas ##
# The St. Andreas entry is from Interstellar Expeditions: Interstellar Players 3
# and provides an exact date of settlement of 1st of February 2768. It doesn't 
# change after that, so we should remove all entries from 2786 forward and 
# replace with a text entry
sucs_data <- sucs_data |>
  filter(!(id_mhq == "St. Andreas" & 
             time_point %in% c("2783", "2786", "2821", "2822", "2830", 
                               "2864", "3025", "3030", "3040", "3049", "3050a",
                               "3050b", "3050c", "3051", "3052", "3057", "3058", 
                               "3059a", "3059b", "3059c", "3059d", "3063", 
                               "3068", "3075", "3079", "3081", "3085", "3095", 
                               "3130", "3135", "3145", "3151", "3152")))
sucs_data <- sucs_data |>
  bind_rows(
    tibble(
      id_sucs = 3060,
      id_mhq = "St. Andreas",
      x = -582.627,
      y = -365.812,
      time_point = "special",
      source_type = "text",
      source_title = "IE: Interstellar Players 3",
      source_loc = "pp. 57-61",
      source_date = date("2768-02-01"),
      faction = "I"
    )
  )


# Founding state maps from handbooks ----------------------------------------

# Lets start with the founding cases. 

# Handbook: House Marik
bounding_box <- create_box("Loongana", "Palos", "Chaffee (LC)", "Prix")
sucs_data <- update_sources(
  target = "2271", 
  title = "Handbook: House Marik", 
  loc = "p. 16",
  date = date("2271-06-01"), 
  box = bounding_box, 
  factions = c("I", "U", "A", "MCM", "SC", "FO", "PR", "TA")
)

# for the House Marik data, we also know that the planets with FO, MCM, SC, and
# PR formed the FWL on 2271-06-02. So add that data
fwl_founders <- sucs_data |>
  filter(time_point == "2271" & faction %in% c("FO", "MCM", "PR", "SC")) |>
  mutate(source_date = date("2271-06-02"),
         region1 = case_when(
           faction == "MCM" ~ "Marik Commonwealth",
           faction == "PR" ~ "Prinicipality of Regulus",
           faction == "SC" ~ "Steward Confederation",
           faction == "FO" ~ "Federation of Oriente"
         ),
         faction = "FWL")

sucs_data <- sucs_data |>
  bind_rows(fwl_founders)

# Handbook: House Davion
bounding_box <- create_box("Perkasie", "Niquinohomo", "Rowe", "Safe Port")
sucs_data <- update_sources(
  target = "2317", 
  title = "Handbook: House Davion", 
  loc = "p. 18",
  date = date("2317-06-26"), 
  box = bounding_box, 
  factions = c("I", "U", "A", "FS")
)

# Handbook: House Kurita
bounding_box <- create_box("Nathan", "Ottumwa", "Thule", "Avellaneda")
sucs_data <- update_sources(
  target = "2319", 
  title = "Handbook: House Kurita", 
  loc = "p. 18",
  # HBHK says Shiro Kurita had achieved his objectives by November (pg. 18), 
  # so lets set it at the midpoint of the month.
  date = date("2319-09-15"), 
  box = bounding_box, 
  factions = c("I", "U", "A", "AG", "DC", "TH", "PoR", "TamP", "FoS")
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
bounding_box <- create_box("Cavanaugh II", "Errai", "Zhongshan", "Gei-Fu")
sucs_data <- update_sources(
  target = "2341", 
  title = "Handbook: House Steiner", 
  loc = "p. 13",
  # HBHS p. 12 says the foundation date was Jan 1, so set the original 
  # member states to 12-31
  date = date("2340-12-31"), 
  box = bounding_box, 
  factions = c("I", "U", "A", "PD", "TamP", "FoS", "TH")
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
  factions = c("I", "U", "A", "DL", "SS", "SIML", "CCom", "SiS", "TGU", "TH", "TC",
               "FWL", "FS", "LC", "CC", "A")
)

# Now change them to CC.
# p. 15 of HBHL tells us that this happened during a convention on St. Andre
# in July of 2366, but not specific date 
# There are also several cases of independent planets that become part of the 
# CC at this time according to map, so add them manually here
independent_cc <- sucs_data |> 
  # most of the independents on this map should be CC
  filter(time_point == "2366" & faction == "I") |>
  # remove the few cases that are not
  filter((!id_mhq %in% c("Aspropirgos", "Gouderak", "Calseraigne", "Ghorepani",
                         "Sunnywood", "Scheuerheck")))

cc_founders <- sucs_data |>
  filter(time_point == "2366" & 
           faction %in% c( "DL", "SS", "SIML", "CCom", "SiS", "TGU")) |>
  bind_rows(independent_cc) |>
  mutate(source_date = date("2366-07-15"),
         faction = "CC")

sucs_data <- sucs_data |>
  bind_rows(cc_founders)

# Add UHC -----------------------------------------------------------------

# add UHC in properly from the Era Digest: Age of war map
uhc_worlds <- c("Islamabad", "Songgang", "Gambarare", "As Samik", "Panpour",
                "Naka Pabni", "Agliana", "Birmensdorf", "Gambarare", "Kaiyuh",
                "Niquinohomo", "Ildrong (Sanurcha 2750-)", "Gurrnazovo", 
                "Neukirchen (Jodipur 3025-)", "June", "Hoonaar", "Vackisujfalu")
uhc_worlds[!(uhc_worlds %in% sucs_data$id_mhq)]
fs_worlds <- c("Tegaldanas", "Belaire", "Darwendale", "Alsek", "Nizina", 
               "Chirikof", "Ingenstrem", "Noatak", "Kotzebue", "Colorado",
               "Killarney", "Eustatius", "Jaboatao", "Olindo", "Fetsund",
               "Great Gorge", "Semichi")
fs_worlds[!(fs_worlds %in% sucs_data$id_mhq)]
tc_worlds <- c("Weippe", "Mavegh", "Caldwell", "Pierce", "Tentativa", "Montour",
               "Cohagen", "Verdigreis", "Cyrton", "Estuan", "Dumassas",
               "Armington", "Csomad", "Anaheim")
tc_worlds[!(tc_worlds %in% sucs_data$id_mhq)]

data_2540 <- tibble(
  id_mhq = c(uhc_worlds, fs_worlds, tc_worlds),
  time_point = "2540",
  source_type = "map",
  source_title = "Era Digest: Age of War",
  source_loc = "p. 7",
  # we know they joined FS in 2540 but not when so use Jan 1
  source_date = date("2540-01-01"),
  faction = c(rep("UHC", length(uhc_worlds)), 
              rep("FS", length(fs_worlds)),
              rep("TC", length(tc_worlds)))
)

# add in the transfer to FS by end of year
data_2540 <- data_2540 |>
  filter(faction == "UHC") |>
  mutate(source_date = date("2540-12-31"),
         faction = "FS") |>
  bind_rows(data_2540)

# integrate sucs id and x, y values
data_2540 <- sucs_data |>
  filter(id_mhq %in% unique(data_2540$id_mhq)) |>
  select(id_sucs, id_mhq, x, y) |>
  distinct() |> 
  right_join(data_2540, by = "id_mhq")

# now add to sucs data
sucs_data <- sucs_data |>
  bind_rows(data_2540)

# Add 2571 data -----------------------------------------------------------

# All the House Handbooks, plus Periphery book
# will use the creation of the Star League on 2571-07-09 as "end" of Age of War

#Handbook: House Marik
bounding_box <- create_box("Trondheimal", "Kashilla", "Premana", "Tarol IV")
sucs_data <- update_sources(
   target = "2571",
   title = "Handbook: House Marik", 
   loc = "p. 24",
   date = date("2571-07-09"), 
   box = bounding_box, 
   factions = c("I", "U", "A", "TH", "FWL", "LC", "MOC", "RW", "A")
)

# Handbook: House Davion
bounding_box <- create_box("Otho", "Delos IV", "Tiflis", "New Vandenburg")
sucs_data <- update_sources(
   target = "2571", 
   title = "Handbook: House Davion", 
   loc = "p. 48",
   date = date("2571-07-09"), 
   box = bounding_box, 
   factions = c("I", "U", "A", "TH", "TC", "DC", "CC", "FS", "OA")
)

# Handbook: House Kurita
bounding_box <- create_box("Elektrougli", "Xenia", "Taran's World", "Loeches")
sucs_data <- update_sources(
  target = "2571", 
  title = "Handbook: House Kurita", 
  loc = "p. 31",
  date = date("2571-07-09"), 
  box = bounding_box, 
  factions = c("I", "U", "A", "TH", "DC", "CC", "FS", "OA", "LC", "FWL", "RW")
)

# Handbook: House Steiner
bounding_box <- create_box("Slewis", "Achernar", "Crellacor", "Gei-Fu")
sucs_data <- update_sources(
  target = "2571", 
  title = "Handbook: House Steiner", 
  loc = "p. 25",
  date = date("2571-07-09"), 
  box = bounding_box, 
  factions = c("I", "U", "A", "TH", "DC", "CC", "OA", "LC", "FWL", "RW")
)

# Handbook: House Steiner
bounding_box <- create_box("Slewis", "Achernar", "Crellacor", "Gei-Fu")
sucs_data <- update_sources(
  target = "2571", 
  title = "Handbook: House Steiner", 
  loc = "p. 25",
  date = date("2571-07-09"), 
  box = bounding_box, 
  factions = c("I", "U", "A", "TH", "DC", "CC", "OA", "LC", "FWL", "RW")
)

# Handbook: House Liao
bounding_box <- create_box("Sheridan (FWL)", "Ridgebrook", "Caph", "Harminous")
sucs_data <- update_sources(
  target = "2571", 
  title = "Handbook: House Liao", 
  loc = "p. 25",
  date = date("2571-07-09"), 
  box = bounding_box, 
  factions = c("I", "U", "A", "TH", "TC", "CC", "MC", "FWL", "LC", "FS")
)

# Periphery: Magistracy of Canopus
bounding_box <- create_box("Marantha", "Buenos Aires", "Kanata", "Tetski")
sucs_data <- update_sources(
  target = "2571", 
  title = "Handbook: Major Periphery States", 
  loc = "p. 93",
  date = date("2571-07-09"), 
  box = bounding_box, 
  factions = c("I", "U", "A", "MOC", "CC", "FWL")
)

# Periphery: Taurian Concordat
bounding_box <- create_box("Xieng Khouang", "Great Gorge", "Kumqwat", "New Vandenburg")
sucs_data <- update_sources(
  target = "2571", 
  title = "Handbook: Major Periphery States", 
  loc = "p. 121",
  date = date("2571-07-09"), 
  box = bounding_box, 
  factions = c("I", "U", "A", "TC", "CC", "FS")
)

# Periphery: Outworlds Alliance
bounding_box <- create_box("Shaul Khala", "Delos IV", "Monywa", "Glenmora")
sucs_data <- update_sources(
  target = "2571", 
  title = "Handbook: Major Periphery States", 
  loc = "p. 147",
  date = date("2571-07-09"), 
  box = bounding_box, 
  factions = c("I", "U", "A", "OA", "DC", "FS")
)

# Add 2596 End of Reunification War data -----------------------------------

bounding_box <- create_box("Malaga", "Tortuga Prime", "Jápminboddu", "Kossandra's Memory")
sucs_data <- update_sources(
  target = "2596", 
  title = "Historical: Reunification War", 
  loc = "pp. 158-159",
  # reunification war ended with surrender of TC on 2596-09-23, p. 93
  date = date("2596-09-23"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "TH", "DC", "FS", "FWL", "LC", "CC",
               "OA", "TC", "MOC", "RW", "IP")
)

# Add 2750 data -----------------------------------------------------------

bounding_box <- create_box("Hunter's Paradise", "Pilon", "Syrstart", "Helvetica")
sucs_data <- update_sources(
  target = "2750", 
  title = "Era Report 2750", 
  loc = "pp. 36-37",
  # we don't really know the date so assume Jan 1
  date = date("2750-01-01"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "TH", "DC", "FS", "FWL", "LC", "CC",
               "OA", "TC", "MOC", "RW", "IP", "TD", "LL")
)

# Issues
# Alfrik - same as problem above
sucs_data <- correct_faction("Alfirk", "2750", "U")

# Add 2765 Lib of Terra Data -------------------------------------------------

bounding_box <- create_box("Hunter's Paradise", "Pilon", "Syrstart", "Helvetica")
sucs_data <- update_sources(
  target = "2765", 
  title = "Liberation of Terra, Vol. I", 
  loc = "pp. 10-11",
  date = date("2765-01-01"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "TH", "DC", "FS", "FWL", "LC", "CC",
               "OA", "TC", "MOC", "RW", "IP", "TD", "LL")
)

# we are getting quite a bit of Aurigan Coalition stuff mixed in here - lets
# filter all of that out and put it in a separate Aurigan map
aurigan_cases <- c("Alloway", "Bellerophon", "Bonavista", "Chaadan",
                   "Don't", "Sacromonte", "Tiburon", 
                   "Wheeler (Perian 2822+/Mystras 3022+)")
aurigan_planets_2765 <- sucs_data |>
  filter(id_mhq %in% aurigan_cases & time_point == "2765") |>
  mutate(source_title = "Handbook: House Arano",
         source_loc = "p. 10",
         source_date = date("2765-01-01"))
sucs_data <- sucs_data |>
  filter(!(id_mhq %in% aurigan_cases & time_point == "2765"))

# Amaris Empire data -------------------------------------------------------

# This is the data labeled as 2767 in the SUCS and described as "2765 map with 
# the core TH worlds shifted to AE because of the Coup". Its unclear if it comes
# from an actual map or not, but we do get the extent of the Amaris Empire on
# a map on p. 138 of the Liberation of Terra Volume 1. I wonder if that is what
# was used? If so, the date should not be 2767 but rather July 1 2772. 

bounding_box <- create_box("Gacrux", "Tawas", "Kannon", "Lacadon")
sucs_data <- update_sources(
  target = "2767", 
  title = "Liberation of Terra, Vol. 1", 
  loc = "p. 138",
  date = date("2772-07-01"), 
  box = bounding_box, 
  factions = c("I", "U", "A", "CC", "FWL", "FS", "DC", "LC", "AE")
)

# looking at the map, this seems to overlap perfectly

# TODO: we could add the wave data here


# Add 2783 Lib Terra II Data ----------------------------------------------

# This comes from Lib of Terra, p. 119, before houses gobble up Hegemony worlds

bounding_box <- create_box("Gacrux", "Junction", "Altais", "Lacadon")
sucs_data <- update_sources(
  target = "2783", 
  title = "Liberation of Terra, Vol. 2", 
  loc = "p. 119",
  date = date("2783-01-01"), 
  box = bounding_box, 
  factions = c("I", "U", "A", "CC", "FWL", "FS", "DC", "LC", "AE", "TH")
)

# Add 2786 Lib Terra II Data ----------------------------------------------

# This comes from Lib of Terra, p. 119, after houses gobble up Hegemony worlds
# I think this should go through the end of 2786

bounding_box <- create_box("Gacrux", "Junction", "Altais", "Lacadon")
sucs_data <- update_sources(
  target = "2786", 
  title = "Liberation of Terra, Vol. 2", 
  loc = "p. 119",
  date = date("2786-12-31"), 
  box = bounding_box, 
  factions = c("I", "U", "A", "CC", "FWL", "FS", "DC", "LC", "AE", "TH")
)

# TODO: We also have a 2786 map in 1SW for the whole IS. Should we use that
# where available?

# Add Operation Klondike maps data -----------------------------------------

# TODO: Do this


# Add 2822 End of 1SW data -------------------------------------------------

# The map in 1SW is missing some periphery planets that are shown in the 
# later handbooks and have already been integrated into SUCS, so I will go
# and and use the Handbooks and skip the 1SW
# The map says 2822, but end of 1SW was 2821-09-24

# Handbook: House Davion
bounding_box <- create_box("Otho", "Shiri", "Tiflis", "Portland")
sucs_data <- update_sources(
  target = "2822", 
  title = "Handbook: House Davion", 
  loc = "p. 54",
  date = date("2821-09-24"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "CC", "DC", "FS", "TH", "OA", "TD", "CS")
)

# Handbook: House Kurita
bounding_box <- create_box("Dustball", "Quiberas", "Ichmandu", "New Praha")
sucs_data <- update_sources(
  target = "2822", 
  title = "Handbook: House Kurita", 
  loc = "p. 43",
  date = date("2821-09-24"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "CC", "DC", "FS", "LC", "FWL", "OA", "CS")
)

# Handbook: House Steiner
bounding_box <- create_box("Lothario", "Brighton", "Paulus Prime", "Brighton")
sucs_data <- update_sources(
  target = "2822", 
  title = "Handbook: House Steiner", 
  loc = "p. 40",
  date = date("2821-09-24"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "CC", "DC", "LC", "FWL", "CF", "CS")
)

# Handbook: House Marik
bounding_box <- create_box("Trondheimal", "Primus", "Glengarry", "Brighton")
sucs_data <- update_sources(
  target = "2822", 
  title = "Handbook: House Marik", 
  loc = "p. 34",
  date = date("2821-09-24"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "CC", "DC", "LC", "FWL", "MOC", "IP", "CF", "CS")
)

# Handbook: House Liao
bounding_box <- create_box("Bethonolog", "Firgrove", "Rochester", "Herotitus")
sucs_data <- update_sources(
  target = "2822", 
  title = "Handbook: House Liao", 
  loc = "p. 31",
  date = date("2821-09-24"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "CC", "DC", "LC", "FWL", "FS", "MOC", "TC", "CS")
)

# OK now do the full 1SW map
bounding_box <- create_box("Hunter's Paradise", "Maripa", "Syrstart", "Helvetica")
sucs_data <- update_sources(
  target = "2822", 
  title = "Historicals: First Succession War", 
  loc = "p. 112-113",
  date = date("2821-09-24"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "CC", "DC", "LC", "FWL", "FS", 
               "MOC", "OA", "TC", "LL", "IP", "CF",
               "CS")
)

# Add 2830 Start of 2SW data -----------------------------------------------

bounding_box <- create_box("Hunter's Paradise", "Maripa", "Syrstart", "Helvetica")
sucs_data <- update_sources(
  target = "2830", 
  title = "Historicals: Second Succession War", 
  loc = "pp. 18-19",
  date = date("2830-01-01"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "CC", "DC", "LC", "FWL", "FS", 
               "MOC", "OA", "TC", "LL", "IP", "CF",
               "CS")
)

# Add 2864 End of 2SW data ------------------------------------------------


# Add 3025 End of 3SW data ------------------------------------------------

# this comes from a poster map?

# need to deal with House Arano again


# Add 3030 End of 4SW data ------------------------------------------------

# I think the only source for this is the House Handbooks so will need to do
# it in steps

# Add 3039 end of War of 3039 data ----------------------------------------



# Handle House Arano data -------------------------------------------------

# We do Arano last to fix whatever other messes it might have made.
# The Arano stuff is quite a mess. I think the best way to handle this 
# is to identify all the names that are for specifically Arano sourcebook
# planets that weren't there before and pull them all out of the SUCS as it is,
# and then create a specific Arano subset of the data from the three maps in
# the handbook and add it back in.

arano_planets_2765 <- c("Alloway", "Bonavista", "Sacromonte", "Polybius",
                        "Bellerophon", "Don't (Mantharaka 3022+)", 
                        "Tiburon (Tiverton 3022+)", 
                        "Wheeler (Perian 2822+/Mystras 3022+)", 
                        "Chaadan (Chadan 2864+/Chandan 3022+)",
                        "Cassilda", "Balawat")

arano_planets_2890 <- c("Amnesty", "Nuncavoy", "Contrilla", "Peratallada",
                        "Pyrrhus", "Eliat", "Abeline (Taygete 2890+)")

arano_planets_3026 <- c("Highwater", "Tarragona", "Fairuza", "Gaucin", "Ahlat")

all_arano_planets <- c(arano_planets_2765, arano_planets_2890, 
                       arano_planets_3026)

# pull these entries out into a separate tibble
arano_data <- sucs_data |>
  filter(id_mhq %in% all_arano_planets)

sucs_data <- sucs_data |>
  filter(!(id_mhq %in% all_arano_planets))

# because of the nature of the data, I don't think you can really say when 
# these planets were undiscovered, except for in earlier periods from the same
# time. So lets just keep three data points and relabel the time point on 2864
# to 2890 
arano_data <- arano_data |>
  filter(time_point %in% c("2765", "2864", "3025")) |>
  mutate(time_point = if_else(time_point == "2864", "2890", time_point)) |>
  mutate(source_title = "Handbook: House Arano",
         source_loc = case_when(
           time_point == "2765" ~ "p. 10",
           time_point == "2890" ~ "p. 12",
           time_point == "3025" ~ "pp. 14-15"
         ),
         source_date = case_when(
           time_point == "2765" ~ date("2765-01-01"),
           time_point == "2890" ~ date("2890-07-21"),
           time_point == "3025" ~ date("3025-01-01")
         ))

# going by the map in 2890, four planets already belonged to AuC
auc_2890 <- tibble(id_mhq = c("Coromodir", "Itrom", "Guldra", "Tyrlon")) |>
  left_join(id_crosswalk) |>
  left_join(system_coords)

arano_data <- arano_data |>
  bind_rows(
    tibble(
      tibble(
        id_sucs = auc_2890$id_sucs,
        id_mhq = auc_2890$id_mhq,
        x = auc_2890$x,
        y = auc_2890$y,
        source_type = "map",
        source_title = "Handbook: House Arano",
        source_loc = "p. 12",
        source_date = date("2890-07-21"),
        faction = "AuC"
      )
    )
  )

sucs_data <- sucs_data |>
  bind_rows(arano_data)

# change source for any 3025 AuC entries to Handbook: House Arano
sucs_data |>
  mutate(
    source_title = if_else(time_point == "3025" & faction == "AuC", 
                           "Handbook: House Arano", source_title),
    source_loc = if_else(time_point == "3025" & faction == "AuC", 
                         "pp. 14-15", source_title)
  )




# Create final data --------------------------------------------------------

sucs_data <- sucs_data |>
  filter(!is.na(source_title)) |>
  select(id_sucs, id_mhq, x, y, starts_with("source_"), faction, starts_with("region")) |>
  arrange(id_sucs, source_date)

#gs4_auth()
#gs4_create("SUCS reborn", sheets = sucs_data)

# Create plots to test ----------------------------------------------------

plot_planets <- function(date, 
                         title = NULL, 
                         xlimits = c(-600, 780), 
                         ylimits = c(-580, 580),
                         faction_filter = c("Undiscovered"),
                         source_filter = NULL,
                         show_id = FALSE,
                         interactive = TRUE) {
  
  temp <- sucs_data
  
  # Apply filters
  if (!is.null(faction_filter)) {
    temp <- temp |> filter(!(faction %in% faction_filter))
  }
  if (!is.null(source_filter)) {
    temp <- temp |> filter(!(source_title %in% source_filter))
  }
  
  # Take a snapshot & create labels
  temp <- temp |>
    faction_snapshot(date) |>
    mutate(
      faction = factor(faction, levels = sucs_factions$id_sucs, labels = sucs_factions$name),
      text_plotly = paste0("<b>", id_mhq, "</b><br>",
                           "<i>", faction, "</i><br>",
                           source_type, ": ", source_title, 
                           ", ", source_loc, "<br>", 
                           source_date)
    )
  
  # Determine color palette
  faction_colors <- sucs_factions |>
    filter(name %in% unique(temp$faction)) |>
    pull("color")
  
  plot_title <- if_else(is.null(title), as.character(date), title)
  
  # Base ggplot
  map <- ggplot(temp, aes(x = x, y = y, text = text_plotly, customdata = id_mhq)) +
    geom_point(aes(color = faction)) +
    scale_x_continuous(limits = xlimits) +
    scale_y_continuous(limits = ylimits) +
    scale_color_manual(values = faction_colors) +
    labs(title = plot_title) +
    theme_void() +
    theme(panel.background = element_rect(fill = "grey20"),
          panel.grid = element_blank())
  
  # Add ID labels if required
  if (show_id) {
    if (interactive) {
      # Hide labels initially
      map <- map + geom_text(aes(label = ""), size = 3, hjust = 1, vjust = 1)
    } else {
      map <- map + geom_text_repel(aes(label = id_mhq), color = "grey95", 
                                   size = 3)
    }
  }
  
  # Convert to plotly
  if (interactive) {
    map <- ggplotly(map, tooltip = "text") |>
      config(scrollZoom = TRUE) |>
      layout(dragmode = "pan")
    
    # JavaScript to show labels when zoomed in
    map <- map |> htmlwidgets::onRender("
    function(el, x) {
      console.log('Binding plotly_relayout event...');
      var plot = document.getElementById(el.id);
   
   let throttleTimeout = null;
let lastZoomLevel = null;

el.on('plotly_relayout', function(eventData) {
  console.log('Plotly relayout event detected:', eventData);

  // Access the xaxis and yaxis range directly
  var xaxisMin = eventData['xaxis.range[0]'];
  var xaxisMax = eventData['xaxis.range[1]'];
  var yaxisMin = eventData['yaxis.range[0]'];
  var yaxisMax = eventData['yaxis.range[1]'];

  // Throttle the event by using requestAnimationFrame for efficient updating
  if (throttleTimeout) return; // If we are already waiting for an update, skip this event

  throttleTimeout = requestAnimationFrame(function() {
    // Check if the range values are available
    if (xaxisMin !== undefined && xaxisMax !== undefined && yaxisMin !== undefined && yaxisMax !== undefined) {
      var zoomLevelX = Math.abs(xaxisMax - xaxisMin);  // X-axis zoom level
      var zoomLevelY = Math.abs(yaxisMax - yaxisMin);  // Y-axis zoom level
      var zoomLevel = Math.max(zoomLevelX, zoomLevelY); // Use the max zoom level between X and Y axes

      var zoomThreshold = 500;  // Zoom threshold set to 300
      var annotations = [];

      // Only add annotations if zoomed in beyond the threshold
      if (zoomLevel < zoomThreshold) {
        console.log('Zoomed in: Showing labels');
        // Loop through the traces and create annotations only for visible points
        for (var i = 0; i < x.data.length; i++) {
          var trace = x.data[i];

          // Loop through each point in the trace and check if it is visible within the current zoom range
          for (var j = 0; j < trace.x.length; j++) {
            if (trace.x[j] >= xaxisMin && trace.x[j] <= xaxisMax && trace.y[j] >= yaxisMin && trace.y[j] <= yaxisMax) {
              annotations.push({
                x: trace.x[j],             // x value for the point
                y: trace.y[j],             // y value for the point
                text: trace.customdata[j], // customdata holds the label text
                showarrow: false           // Don't show arrows, just the label
              });
            }
          }
        }
      } else {
        console.log('Zoomed out: Hiding labels');
      }

      // Apply the annotations to the plot only if the zoom level has changed
      if (lastZoomLevel !== zoomLevel) {
        lastZoomLevel = zoomLevel;  // Update last zoom level
        Plotly.relayout(el, {annotations: annotations});
      }
    }

    throttleTimeout = null;  // Reset the throttle timeout
  });
});




    }
  ")
  }
  
  return(map)
}


# change some colors for better comparison
sucs_factions <- sucs_factions |>
  mutate(color = ifelse(id_sucs == "UHC", "#90EE90", color),
         color = ifelse(id_sucs == "A", "grey70", color))

g1 <- plot_planets(date("2271-06-01"), "2271-06-01, Eve of FWL Founding")
g2 <- plot_planets(date("2271-06-02"), "2271-06-02, FWL Founding")
g3 <- plot_planets(date("2317-06-26"), "2317-06-26, FedSuns Founding")
g4 <- plot_planets(date("2319-09-15"), "2319-09-15, Eve of DC Founding (approximate)")
g5 <- plot_planets(date("2319-09-30"), "2319-09-30, DC Founding (approximate)")
g6 <- plot_planets(date("2340-12-31"), "2340-12-31, Eve of LC Founding")
g7 <- plot_planets(date("2341-01-01"), "2341-01-01, LC Founding")
g8 <- plot_planets(date("2366-01-01"), "2366-01-01, Eve of CC Founding (approximate)")
g9 <- plot_planets(date("2366-07-15"), "2366-07-15, CC Founding (approximate)")

ggarrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, ncol = 3, nrow = 3)

plot_planets(date("2540-01-01"), "2540-01-01, UHC Pre-Merge")
plot_planets(date("2540-12-31"), "2540-12-31, after UHC merge")
plot_planets(date("2571-07-09"), "2571-07-09, Founding of Star League")
plot_planets(date("2596-09-30"), "2596-09-30, End of Reunification War")
plot_planets(date("2750-01-01"), "2750-01-01, Height of Star League")
plot_planets(date("2765-01-01"), "2765-01-01, Eve of Amaris Coup",
             source_filter = "Handbook: House Arano")
plot_planets(date("2765-01-01"), "2765-01-01, Eve of Amaris Coup")
plot_planets(date("2772-07-01"), "2772-07-01, Amaris Empire")
plot_planets(date("2783-01-01"), "2783-01-01, Pre-Great House Encroachment")
plot_planets(date("2786-12-31"), "2786-12-31, Great House Encroachment")
plot_planets(date("2822-01-01"), "2822-01-01, End of 1st SW",
             source_filter = "Handbook: House Arano")
plot_planets(date("2830-01-01"), "2830-01-01, Start of 2nd SW",
             source_filter = "Handbook: House Arano", show_id = TRUE)


x <- sucs_data |> 
  faction_snapshot(date("2822-01-01")) |>
  filter(faction == "I")

# get close in view
#plot_planets(date("2786-12-31"), "2786-12-31, Great House Encroachment",
#             xlimits = c(-100, 150), ylimits = c(-125, 135),
#             show_id = TRUE)

#plot_planets(date("2822-01-01"), "Test",
#             xlimits = c(500, 700), ylimits = c(-450, 200),
#             show_id = TRUE)

#plot_planets(date("2890-07-21"), "Test",
#             #source_filter = "Handbook: House Arano",
#             xlimits = c(-150, 350), ylimits = c(-550,-400),
#             show_id = TRUE)

#plot_planets(date("2822-01-01"), "Test",
#             xlimits = c(-300, 0), ylimits = c(400,650),
#             show_id = TRUE)

#plot_planets(date("2830-01-01"), "Test",
#             source_filter = "Handbook: House Arano",
#             xlimits = c(-700, -300), ylimits = c(-500,-150),
#             show_id = TRUE)

