library(googlesheets4)
library(tidyverse)
library(here)
library(plotly)

source("functions.R")

# TODO: we could do an "errata" type to handle corrections

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
  # first remove parentheticals for disputed cases, so commas won't get 
  # processed incorrectly
  mutate(disputed_cases = str_extract(faction, "(?<=\\()[^()]+(?=\\))"),
         faction = str_remove(faction, "\\s*\\([^\\)]+\\)")) |>
  # now get capital information
  mutate(capital = case_when(
           str_detect(faction, "Faction Capital") ~ "Faction",
           str_detect(faction, "Major Capital") ~ "Major",
           str_detect(faction, "Minor Capital") ~ "Minor"
         ),
         faction = str_remove(
           faction, 
           "(,Faction Capital|,Major Capital|,Minor Capital)"
         )) |>
  # now split faction into separate variables by commas
  separate_wider_delim(faction, ",", too_few = "align_start",
                       names = c("faction", paste("region", 1:3, sep=""))) |>
  # now put back in disputed cases
  mutate(faction = if_else(is.na(disputed_cases), 
                           faction, 
                           paste0(faction, "(", disputed_cases, ")")))
  
# add in MekHQ ids
# TODO: we are missing a few new ones
sucs_data <- sucs_data |>
  left_join(id_crosswalk) |>
  select(id_sucs, x, y, id_mhq, time_point, 
         faction, starts_with("region"), capital)

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
         faction, starts_with("region"), capital)


# Address disputed faction codes ------------------------------------------

disputed <- sucs_data |> filter(str_detect(faction, "^D\\("))
disputed |> pull(faction) |> unique() |> sort()

table(disputed$faction, disputed$time_point)


## Malagrotta Cooperative ##
# The three planets of the Malagrotta Cooperative are listed as D(MC,FS) in
# 3079. The AFFS and Periphery maps from Field Reports show them as FS, but 
# the AFFS map marks them off as "disputed."


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
# to handle them with Arano case.
# TODO: This is currently not showing up in Jihad era maps even though it should
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
# on based on an entry from the Periphery handbook (2nd edition), but it does
# not appear on any maps until War of 3039. All cases before that should be
# removed. 
# It also does not show up on the 3052 and 3062 Era Report maps, although that 
# may be due to legend placement
# It shows up again in Jihad: Final Reckoning, FM 3085, and Jihad Secrets, 
# Era Report 3145, Shattered Fortress, and Ilkhan's Eyes Only, (not sure 
# about Era Digest: Dark Ages)
sucs_data <- sucs_data |>
  filter(!(id_mhq == "Alfirk" & 
             time_point %in% c("2596", "2750", "2765", "2767", "2783", "2786",
                               "2821", "2822", "2830", "2864", "3025", "3030",
                               "3049", "3050a", "3050b", "3050c", "3051", 
                               "3052", "3057", "3058", "3059a", "3059b", 
                               "3059c", "3059d" ,"3062", "3063")))

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
# Sigurd, Oberon VI, and Crellacor are showing up as OC from 2783. The OG 
# Oberon Confederation only shows up on the 2786 map from 1SW, but as 
# independent in the 2822 maps for that same source. The OC only shows up again
# in 3025. So I think these planets should be marked as independent for every
# time_point between 2783 and 2864 except for 2786.
sucs_data <- correct_faction(c("Sigurd", "Oberon VI", "Crellacor"),
                             c("2783", "2821", "2822", "2830", "2864"),
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
                               "3067", "3068", "3075", "3079", "3081", "3085", 
                               "3095", "3130", "3135", "3145", "3151", "3152")))
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

## Frobisher ##
# This is an IE: ISP3 entry and it says the colony of ... fish people ...
# was founded in the "late 2690s". It is still existing today, although in a 
# somewhat ... unusual state. So, like St. Andreas above, we should wipe out
# all the map references and make a two entries - one for an SL faction and 
# then a fall of the SL date to I.
sucs_data <- sucs_data |>
  filter(!(id_mhq == "Frobisher" & 
             time_point %in% c("2750", "2765", "2767", "2783", "2786", "2821", 
                               "2822", "2830", "2864", "3025", "3030", "3040", 
                               "3049", "3050a", "3050b", "3050c", "3051", 
                               "3052", "3057", "3058", "3059a", "3059b", 
                               "3059c", "3059d", "3063", "3067", "3068", "3075", 
                               "3079", "3081", "3085", "3095", 
                               "3130", "3135", "3145", "3151", "3152")))
sucs_data <- sucs_data |>
  bind_rows(
    tibble(
      id_sucs = 3111,
      id_mhq = "Frobisher",
      x = -370.480,
      y = -470.316,
      time_point = "special",
      source_type = "text",
      source_title = "IE: Interstellar Players 3",
      source_loc = "p. 81",
      source_date = date("2699-12-31"),
      faction = "SL"
    )
  )

sucs_data <- sucs_data |>
  bind_rows(
    tibble(
      id_sucs = 3111,
      id_mhq = "Frobisher",
      x = -370.480,
      y = -470.316,
      time_point = "special",
      source_type = "text",
      source_title = "IE: Interstellar Players 3",
      source_loc = "p. 81",
      source_date = date("2786-12-31"),
      faction = "I"
    )
  )

## Andurien Wars ##
# Several worlds that should be FWL/CC are listed as MOC/DA presumably due
# to the Andurien Wars, but thats not what shows on this map. That doesn't 
# start until September anyway, so should be handled by properly entering text
# entries from Brush Wars whenever we do that as a project.
# All DA worlds should be changed. We will change them to FWL but a few will
# then be corrected to CC below
andurien_3030 <- sucs_data |> 
  filter(faction == "DA" & time_point == "3030") |> 
  pull(id_mhq)
sucs_data <- correct_faction(andurien_3030, "3030", "FWL")
sucs_data <- correct_faction(c("Prix", "Primus", "New Roland", "Andarmax", 
                               "Jacomarle", "Drozan", "Renown", "Sax"),
                             c("3030"),
                             "CC")

## New St. Andrews ##
# This planet first appears on the map in 2930 and needs three cases removed
sucs_data <- sucs_data |>
  filter(!(id_mhq == "New St. Andrews" & 
             time_point %in% c("2822", "2830", "2864")))

## Kleinwelt ##
# This was originally part of MOC but was abandoned by 2864 map. The 
# SUCK has it as independent from 3040 map onward, but it is only from IE:ISP3
# entry regarding Marian Hegemony slaves refounding a colony there in 3044.
# So remove all entries after 2864 and then add a text entry in 3044.
sucs_data <- sucs_data |>
  filter(!(id_mhq == "Kleinwelt" & 
             time_point %in% c("3025", "3030", "3040", 
                               "3049", "3050a", "3050b", "3050c", "3051", 
                               "3052", "3057", "3058", "3059a", "3059b", 
                               "3059c", "3059d", "3063", "3067", "3068", "3075", 
                               "3079", "3081", "3085", "3095", 
                               "3130", "3135", "3145", "3151", "3152")))
sucs_data <- sucs_data |>
  bind_rows(
    tibble(
      id_sucs = 1376,
      id_mhq = "Kleinwelt",
      x = -334.286,
      y = -423.676,
      time_point = "special",
      source_type = "text",
      source_title = "IE: Interstellar Players 3",
      source_loc = "p. 82",
      source_date = date("3044-12-31"),
      faction = "I"
    )
  )

## Star's End ## 
# This is listed as independent in 3081 but the map does not show this. Sarna
# describes the Hell's Horses as leaving at some point, but the references
# listed don't actually indicate this. The same problem exists in 3085.
sucs_data <- correct_faction("Star's End (Novo Cressidas)", 
                             c("3081", "3085"), "CHH")


## Farstar ##
# Farstar is listed as part of Clan Snow Raven in in 3063, 3067,	3068,	and 3075
# but the Sarna entry clearly says:
# "Notably, there is no indication that the Clan actually took control of the world."
sucs_data <- correct_faction("Farstar", 
                             c("3063", "3067", "3068", "3075"), "I")

## Tharkad ##
# Tharkad is listed as WOB in 3067 and 3068, but the 3067 map date is in 
# October 3067 which is before the Whitting Conference, and even after that 
# it was in dispute at most. 
sucs_data <- correct_faction("Tharkad", 
                             c( "3067", "3068"), "LA")

## Wynn's Roost ##
# This shows up on maps through the 2864 2nd Succession War maps and 
# then drops off from all subsequent maps. However ToS: Wynn's Roost indicates
# that the planet is still around in at least 3030. So we should just remove
# entries past 2864 to indicate that this is the last map entry we have.
sucs_data <- sucs_data |>
  filter(!(id_mhq == "Wynn's Roost" & 
             time_point %in% c("3025", "3030", "3040", 
                               "3049", "3050a", "3050b", "3050c", "3051", 
                               "3052", "3057", "3058", "3059a", "3059b", 
                               "3059c", "3059d", "3063", "3067", "3068", "3075", 
                               "3079", "3081", "3085", "3095", 
                               "3130", "3135", "3145", "3151", "3152")))

# TODO: Skyfog is showing as part of MOC long after it is abandoned
# because of an out of frame issue. It is also coded in the SUCK data as 
# IE which I think is a little weird. Its area does seem to come back into frame
# in the Field Report: Periphery map for the MOC and its not there, so maybe
# it should be listed as abandoned?

## Sentarus ## 
# Sentarus is showing as independent long after they drop off maps due to an
# out of frame issue. According to Sarna "none of the maps from later eras 
# present in the Handbooks series of sourcebooks extend far enough into the 
# Periphery to confirm or deny the continued presence of this system during 
# the Succession Wars era." However the SUCK data has the planet abandoned
# in 3025, but there is no reference for this decision. The area it is in
# does come back into frame in 3151 with Shattered Fortress, so we will pick
# up the abandoned code then - I will leave as is until I hear more about why
# it was listed as abandoned in 3025.

## Vannes and Far Reach ##
# Like Sentarus, these are showing as independent long after they drop off 
# maps due to an out of frame issue. However, these are later confirmed as of 
# FM 3085 to belong to the Chainelane Islands, so we should pick them up there

# Add Founding House Maps------------------------------------------------

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

# We also have a 2786 map in 1SW for the whole IS. Lets use that as well
bounding_box <- create_box("Hunter's Paradise", "Pilon", "Syrstart", "Helvetica")
sucs_data <- update_sources(
  target = "2786", 
  title = "Historicals: First Succession War", 
  loc = "pp. 24-25",
  date = date("2786-12-31"), 
  box = bounding_box, 
  factions = c("I", "U", "A", "CC", "FWL", "FS", "DC", "LC", "AE", "TH", "CS",
               "MOC", "TC", "OA", "IP", "LL", "TD", "OC", "FFR", "CF")
)

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
bounding_box <- create_box("Hunter's Paradise", "Pilon", "Syrstart", "Helvetica")
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

bounding_box <- create_box("Hunter's Paradise", "Maripa", "Syrstart", "Helvetica")
sucs_data <- update_sources(
  target = "2864", 
  title = "Historicals: Second Succession War", 
  loc = "pp. 78-79",
  date = date("2864-01-01"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "CC", "DC", "LC", "FWL", "FS", 
               "MOC", "OA", "TC", "LL", "IP", "CF",
               "CS")
)


# Add 2930 Marian Hegemony Map --------------------------------------------

# From Major Periphery States
bounding_box <- create_box("New St. Andrews", "Negushevo", 
                           "Edmondson", "Algenib")
sucs_data <- update_sources(
  target = "3025", 
  title = "Handbook: Major Periphery States", 
  loc = "p. 163",
  date = date("2930-01-01"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "MH", "FWL", "MOC", "LL", "IP", "CF")
)

# Add 3025 End of 3SW data ------------------------------------------------

# Lets start with House Handbooks

# Handbook: House Davion
bounding_box <- create_box("Al Hillah", "New Haiti (New Hati)", 
                           "Kagoshima", "Rockwellawan")
sucs_data <- update_sources(
  target = "3025", 
  title = "Handbook: House Davion", 
  loc = "p. 70",
  date = date("3025-01-01"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "CC", "DC", "FS", "FWL", "LC", "CS",
               "OA", "TC", "TD")
)

# Handbook: House Kurita
bounding_box <- create_box("Garrison", "Antallos (Port Krin)", 
                           "Manaringaine", "Zion")
sucs_data <- update_sources(
  target = "3025", 
  title = "Handbook: House Kurita", 
  loc = "p. 64",
  date = date("3025-01-01"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "CC", "DC", "FS", "FWL", "LC", "CS",
               "OA", "TFR", "OC", "EF")
)

# Handbook: House Steiner
bounding_box <- create_box("Haggard", "Otho", 
                           "Botany Bay", "Maximillian")
sucs_data <- update_sources(
  target = "3025", 
  title = "Handbook: House Steiner", 
  loc = "p. 47",
  date = date("3025-01-01"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "CC", "DC", "FS", "FWL", "LC", "CS",
               "OA", "TFR", "OC", "EF", "MV", "CF")
)

# Handbook: House Marik
bounding_box <- create_box("Florida", "Styk", "Glengarry", "Lockton")
sucs_data <- update_sources(
  target = "3025", 
  title = "Handbook: House Marik", 
  loc = "p. 42",
  date = date("3025-01-01"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "CC", "DC", "FS", "FWL", "LC", "CS",
               "MOC", "TFR", "CF", "LL", "IP", "MH")
)

# Handbook: House Liao
bounding_box <- create_box("McAffe", "New Avalon", "Clovis", "Rockwellawan")
sucs_data <- update_sources(
  target = "3025", 
  title = "Handbook: House Liao", 
  loc = "p. 40",
  date = date("3025-01-01"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "CC", "FS", "FWL", "LC", "CS",
               "MOC", "TC", "TFR", "SIC")
)

# Periphery: Magistracy of Canopus
bounding_box <- create_box("Thraxa", "Borden", "Andurien", "Gettorf")
sucs_data <- update_sources(
  target = "3025", 
  title = "Handbook: Major Periphery States", 
  loc = "p. 95",
  date = date("3025-01-01"), 
  box = bounding_box, 
  factions = c("I", "U", "A", "MOC", "CC", "FWL")
)

# Periphery: Taurian Concordat
bounding_box <- create_box("Betelgeuse", "Great Gorge", "Darwendale", "Spencer")
sucs_data <- update_sources(
  target = "3025", 
  title = "Handbook: Major Periphery States", 
  loc = "p. 121",
  date = date("3025-01-01"), 
  box = bounding_box, 
  factions = c("I", "U", "A", "TC", "CC", "FS")
)

# Ok, we had a few areas outside the frame of all these maps
# So lets zoom in on areas outside frame and use the Inner Sphere at War map 
# as a reference
# East of Outworlds Alliance
bounding_box <- create_box("Shiri", "Puttalam", "Feijo", "Kent")
sucs_data <- update_sources(
  target = "3025", 
  title = "Inner Sphere at War", 
  loc = "map",
  date = date("3025-01-01"), 
  box = bounding_box, 
  factions = c("I", "U", "A")
)

# North of Outworlds Alliance
bounding_box <- create_box("Michtal", "Blueys", "Blueys", "Azur")
sucs_data <- update_sources(
  target = "3025", 
  title = "Inner Sphere at War", 
  loc = "map",
  date = date("3025-01-01"), 
  box = bounding_box, 
  factions = c("I", "U", "A")
)

# Azur and Vanburg
bounding_box <- create_box("Vangburg", "Azur", "Vangburg", "Azur")
sucs_data <- update_sources(
  target = "3025", 
  title = "Inner Sphere at War", 
  loc = "map",
  date = date("3025-01-01"), 
  box = bounding_box, 
  factions = c("I", "U", "A")
)

# North of Lyran Commonwealth
bounding_box <- create_box("Mearra", "Beowulf", "Givrodat", "Battaraigi")
sucs_data <- update_sources(
  target = "3025", 
  title = "Inner Sphere at War", 
  loc = "map",
  date = date("3025-01-01"), 
  box = bounding_box, 
  factions = c("I", "U", "A")
)

# West of Lyran Commonwealth
bounding_box <- create_box("Hunter's Paradise", "Sialkot", "Rypful", "Lande")
sucs_data <- update_sources(
  target = "3025", 
  title = "Inner Sphere at War", 
  loc = "map",
  date = date("3025-01-01"), 
  box = bounding_box, 
  factions = c("I", "U", "A")
)

# Add 3030 End of 4SW data ------------------------------------------------

# The only source for this is the House Handbooks so will need to do it in steps

# Handbook: House Davion
bounding_box <- create_box("Al Hillah", "New Haiti (New Hati)", 
                           "Kagoshima", "Rockwellawan")
sucs_data <- update_sources(
  target = "3030", 
  title = "Handbook: House Davion", 
  loc = "p. 72",
  date = date("3030-01-31"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "CC", "DC", "FS", "FWL", "LC", "CS",
               "OA", "TC", "TD", "SIC", "TFR")
)

# Issues
# TODO: four inhabited system south of TC that are not on map; Aea, Regis Roost,
# Carthage, Spitz. They do show up on the House Liao book, but they should be 
# removed here to ensure we get proper sourcing of the disagreement


# Handbook: House Kurita
bounding_box <- create_box("Garrison", "Antallos (Port Krin)", 
                           "Manaringaine", "Zion")
sucs_data <- update_sources(
  target = "3030", 
  title = "Handbook: House Kurita", 
  loc = "p. 66",
  date = date("3030-01-31"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "CC", "DC", "FS", "FWL", "LC", "CS",
               "OA", "TFR", "OC", "EF")
)

# Handbook: House Steiner
bounding_box <- create_box("Gillfillan's Gold", "Basalt", 
                           "Placida", "Shasta")
sucs_data <- update_sources(
  target = "3030", 
  title = "Handbook: House Steiner", 
  loc = "p. 56",
  date = date("3030-01-31"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "CC", "DC", "FS", "FWL", "LC", "CS",
               "OA", "TFR", "OC", "EF", "GV", "CF")
)

# Handbook: House Marik
bounding_box <- create_box("Florida", "Gan Singh", "Florida", "Lockton")
sucs_data <- update_sources(
  target = "3030", 
  title = "Handbook: House Marik", 
  loc = "p. 51",
  date = date("3030-01-31"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "CC", "DC", "FS", "FWL", "LC", "CS",
               "MOC", "TFR", "CF", "LL", "IP", "MH")
)

# Handbook: House Liao
bounding_box <- create_box("Faleolo", "Argyle", "Clovis", "Hellespont")
sucs_data <- update_sources(
  target = "3030", 
  title = "Handbook: House Liao", 
  loc = "p. 49",
  date = date("3030-01-31"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "CC", "FS", "FWL", "LC", "CS",
               "MOC", "TC", "TFR", "SIC")
)

# Add 3039 end of War of 3039 data ----------------------------------------

# date from end of war 3040-01-19
bounding_box <- create_box("New St. Andrews", "Micanos (Mica II, V, VII)", 
                           "Manaringaine", "Skyfog")
sucs_data <- update_sources(
  target = "3040", 
  title = "Historicals: War of 3039", 
  loc = "pp. 132-133",
  date = date("3040-01-19"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "CC", "FS", "FWL", "FCL", "FCF", "DC", "CS",
               "SIC", "FR",
               "MOC", "TC", "OA", "CF", "LL" , "IP", "MH",
               "OC", "EF", "GV", "TD")
)

# Add Era Report 3052 data ----------------------------------------------------

# The first map says 3050, but the wave data indicates that the periphery
# wave started in August 3049, so lets date it to 3049-07-30
# Goddammit! The first 3049 column already incorporates the periphery wave
# of clan invasions. So I am going to have to do that one first and then
# correct the periphery planets back to what they should be and do it 
# again for the whole IS

# Periphery: Operation Revival, Wave 1
bounding_box <- create_box("Miquelon", "Miyada", 
                           "Manaringaine", "Tukayyid")
sucs_data <- update_sources(
  target = "3049", 
  title = "Era Report 3052", 
  loc = "pp. 43, 47, 51, 55, Periphery Campaign",
  date = date("3049-12-31"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "FS", "FCL", "FCF", "DC", "CS",
               "SIC", "FR",
               "CWF", "CJF", "CGB", "CSJ",
               "OC", "EF", "GV")
)

# ok, save these cases to a separate dataset that will get re-merged back in
# because otherwise this will get overwritten by the pre-invasion data
periphery_3049 <- sucs_data |>
  filter(time_point == "3049") |>
  mutate(time_point = "3049p")

# ok now go back and correct periphery factions for before clan invasion
# Independents
sucs_data <- correct_faction(c("Santander V (Santander's World)",
                               "Von Strang's World (Erin 2830-)",
                               "Star's End (Novo Cressidas)"),
                             "3049",
                             "I")
# Greater Valkyrate
sucs_data <- correct_faction(c("Erewhon", "Lackhove", "Gotterdammerung",
                               "Last Chance", "Botany Bay", "Butte Hold"),
                             "3049",
                             "GV")
# Oberon Confederation
sucs_data <- correct_faction(c("Placida", "The Rock", "Ferris (OC)", 
                               "Blackstone", "Sigurd", "Oberon VI",
                               "Crellacor", "Gustrell", "Paulus Prime",
                               "Drask's Den"),
                             "3049",
                             "OC")
# Elysian Fields
sucs_data <- correct_faction(c("Manaringaine", "Nyserta", "Elissa", "Porthos"),
                             "3049",
                             "EF")

# Now run 3049 again for the whole IS for 3049-07-31
bounding_box <- create_box("New St. Andrews", "Micanos (Mica II, V, VII)", 
                           "Manaringaine", "Crawford's Delight")
sucs_data <- update_sources(
  target = "3049", 
  title = "Era Report 3052", 
  loc = "pp. 10-11",
  date = date("3049-07-31"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "CC", "FS", "FWL", "FCL", "FCF", "DC", "CS",
               "SIC", "FR",
               "MOC", "TC", "OA", "CF", "LL" , "IP", "MH",
               "OC", "EF", "GV", "TD", "RC")
)

# now re-add the periphery wave data
sucs_data <- sucs_data |>
  bind_rows(periphery_3049)

# Now do the remaining waves

# Wave I
bounding_box <- create_box("Miquelon", "Miyada", 
                           "Manaringaine", "Tukayyid")
sucs_data <- update_sources(
  target = "3050a", 
  title = "Era Report 3052", 
  loc = "pp. 43, 47, 51, 55, Wave 1",
  date = date("3050-04-30"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "FS", "FCL", "FCF", "DC", "CS",
               "SIC", "FR",
               "CWF", "CJF", "CGB", "CSJ",
               "OC", "EF", "GV")
)
# Wave II
sucs_data <- update_sources(
  target = "3050b", 
  title = "Era Report 3052", 
  loc = "pp. 43, 47, 51, 55, Wave 2",
  date = date("3050-05-31"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "FS", "FCL", "FCF", "DC", "CS",
               "SIC", "FR",
               "CWF", "CJF", "CGB", "CSJ",
               "OC", "EF", "GV")
)
# Wave III
sucs_data <- update_sources(
  target = "3050c", 
  title = "Era Report 3052", 
  loc = "pp. 43, 47, 51, 55, Wave 3",
  date = date("3050-07-15"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "FS", "FCL", "FCF", "DC", "CS",
               "SIC", "FR",
               "CWF", "CJF", "CGB", "CSJ",
               "OC", "EF", "GV")
)
# Wave IV
sucs_data <- update_sources(
  target = "3051", 
  title = "Era Report 3052", 
  loc = "pp. 43, 47, 51, 55, Wave 4",
  date = date("3050-10-31"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "FS", "FCL", "FCF", "DC", "CS",
               "SIC", "FR",
               "CWF", "CJF", "CGB", "CSJ",
               "OC", "EF", "GV")
)
# Wave V - This is the same as large April Map, so just use that
bounding_box <- create_box("New St. Andrews", "Micanos (Mica II, V, VII)", 
                           "Manaringaine", "Crawford's Delight")
sucs_data <- update_sources(
  target = "3052", 
  title = "Era Report 3052", 
  loc = "pp. 22-23",
  date = date("3052-04-30"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "CC", "FS", "FWL", "FCL", "FCF", "DC", "CS",
               "SIC", "FR",
               "CWF", "CJF", "CGB", "CSJ", "CDS", "CNC", "CSV",
               "MOC", "TC", "OA", "CF", "LL" , "IP", "MH",
               "TD", "RC")
)

# TODO: We are missing jointly administered worlds, part of the Disputed
# faction code issue

# Add Operation Guerrero Era Report 3062 data -------------------------------

# The 3057 column is the August 3057 map but after Chaos March and break off of
# Lyran Alliance, but it would be nice to recover the August 3057 map before 
# all that happened. However, it looks almost identical, except for a few things
# in the clan zone. Plus the regions will be screwed up for the FCL, so lets
# just go with what we have here.
# Ok more complications, it looks like the LA regions were not changed until 
# the 3058 column, so our regions are wrong in 3057. However 3058 has other 
# changes in it that are clearly not map based - like the Falcon incursion and 
# creation of NCR (or at least not for a map of this time period)

# lets start by taking the map on pg. 16 and setting that to the end of the 
# year but do it as a separate map
bounding_box <- create_box("Marik", "Sanilac", 
                           "Dromini VI", "No Return")
chaos_march <- update_sources(
  target = "3057", 
  title = "Era Report 3062", 
  loc = "p. 16",
  date = date("3057-12-31"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "CC", "FS", "FWL", "LA", "FCF", "DC", "CS",
               "TCC", "SS", "DS", "ST", "SKC")) |>
  mutate(time_point = "3057cm")

# now lets switch LA back to FCL and chaos march back to respective factions
sucs_data <- sucs_data |>
  mutate(faction = if_else(time_point == "3057" & faction == "LA", "FCL", faction))
# chaos march is a bit harder, but lets try just swapping in values from 3052
# for everything in the chaos march bounding box. 
pre_chaos_march <- sucs_data |>
  filter(is_in_box(x, y, bounding_box) & time_point == "3052") |>
  mutate(time_point = "3057",
         source_title = NA, source_loc = NA, source_date = NA)

# remove the actual 3057 values in this box and swap back in the new ones
sucs_data <- sucs_data |>
  filter(!(is_in_box(x, y, bounding_box) & time_point == "3057")) |>
  bind_rows(pre_chaos_march)

bounding_box <- create_box("New St. Andrews", "Micanos (Mica II, V, VII)", 
                            "Manaringaine", "Crawford's Delight")
sucs_data <- update_sources(
  target = "3057", 
  title = "Era Report 3062", 
  loc = "pp. 10-11",
  date = date("3057-07-31"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "CC", "FS", "FWL", "FCL", "FCF", "DC", "CS",
               "SIC", "FR",
               "CWF", "CJF", "CGB", "CSJ", "CDS", "CNC", "CSV",
               "TCC", "SS", "DS", "ST", "SKC",
               "MOC", "TC", "OA", "CF", "LL" , "IP", "MH",
               "TD", "RC")
)

# add back in chaos march data
sucs_data <- sucs_data |>
  bind_rows(chaos_march)

# The final step is to add a text entry for the switch over of all FCL to LA
# on 18 Sept 3057, but leave out the Sarna March
lyran_alliance <- sucs_data |>
  filter(time_point == "3057" & faction == "FCL" & region1 != "Sarna March") |>
  mutate(time_point = "3057la", 
         source_type = "text", 
         source_title = "Era Report 3062",
         source_loc = "p. 15",
         source_date = date("3057-09-18"),
         faction = "LA")

sucs_data <- sucs_data |>
  bind_rows(lyran_alliance)


# Add Era Report 3062 Final Map data --------------------------------------

bounding_box <- create_box("New St. Andrews", "Micanos (Mica II, V, VII)", 
                           "Manaringaine", "Crawford's Delight")
sucs_data <- update_sources(
  target = "3063", 
  title = "Era Report 3062", 
  loc = "pp. 28-29",
  date = date("3063-07-01"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "CC", "FS", "FWL", "LA", "FCF", "DC", "CS",
               "SIC", "FR",
               "CWF", "CJF", "CGB", "CSJ", "CDS", "CNC", "CSV",
               "TCC", "SS", "DS", "ST", "SKC",
               "MOC", "TC", "OA", "CF", "LL" , "IP", "MH",
               "TD", "RC", "NCR")
)

# TODO: Noticing Skyfog is still on this map and presumably others, but it 
# seems like it might be within frame to declare it abandoned, needs a check.


# Add Operation Bulldog Wave data -----------------------------------------

bounding_box <- create_box("Skallevoll", "Loysville", 
                           "Santander V (Santander's World)", "Arkab")
sucs_data <- update_sources(
  target = "3059a", 
  title = "Era Report 3062", 
  loc = "p. 21, wave 1",
  date = date("3059-06-25"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "CSJ", "CGB", "CNC", "CWF", 
               "DC")
)

sucs_data <- update_sources(
  target = "3059b", 
  title = "Era Report 3062", 
  loc = "p. 21, wave 2",
  date = date("3059-07-07"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "CSJ", "CGB", "CNC", "CWF", 
               "DC")
)

sucs_data <- update_sources(
  target = "3059c", 
  title = "Era Report 3062", 
  loc = "p. 21, wave 3",
  date = date("3059-08-13"),  # TODO: this is a guess
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "CSJ", "CGB", "CNC", "CWF", 
               "DC")
)

sucs_data <- update_sources(
  target = "3059d", 
  title = "Era Report 3062", 
  loc = "p. 21, wave 4",
  date = date("3059-11-01"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "CSJ", "CGB", "CNC", "CWF", 
               "DC")
)

# Add Jihad Final Reckoning data ----------------------------------------------

# Add 3067 map
bounding_box <- create_box("Hunter's Paradise", "Micanos (Mica II, V, VII)", 
                           "Manaringaine", "Crawford's Delight")
sucs_data <- update_sources(
  target = "3067", 
  title = "Jihad: Final Reckoning", 
  loc = "pp. 42-43",
  date = date("3067-10-31"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "CC", "FS", "FWL", "LA", "DC", 
               "CS", "FR", "WB",
               "CWF", "CJF", "CGB", "CNC",
               "TCC", "SS", "DS", "ST", "SKC",
               "MOC", "TC", "CDP", "OA", "CF", "MH",
               "TD", "RC", "FrR")
)

# TODO: McEvedy's Folly is showing up as MOC controlled
# TODO: The Saiph Triumvarate is showing up here but it shows as independent 
# on the map

# 3081 map
bounding_box <- create_box("Hunter's Paradise", "Micanos (Mica II, V, VII)", 
                           "Manaringaine", "Crawford's Delight")
sucs_data <- update_sources(
  target = "3081", 
  title = "Jihad: Final Reckoning", 
  loc = "pp. 62-63",
  date = date("3081-03-01"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "CC", "FS", "LA", "DC", "RS",
               "CS", "WB",
               "CWF", "CJF", "CGB", "CNC", "CHH",
               "MCM", "PR", "DO", "DA", "DGM", "MSC", "TP", "DoO", "RFS", "PG", 
               "MA", "DTA", "OZP", "SHC", 
               "RCM", "FvC",
               "MOC", "TC", "CDP", "OA", "MH",
               "TD", "RC", "FrR")
)

# TODO: McEvedy's Folly should be a dead world according to map

# TODO: 3068 data? Where does it come from?


# Jihad Secrets: The Blake Documents --------------------------------------

# There are a couple of CoF entries in here but it is too early for that. All
# of the CoF entries that I can see should be WB
sucs_data <- sucs_data |>
  mutate(faction = if_else(time_point == "3075" & faction == "CoF", 
                           "WB", faction))

#3075ish?
bounding_box <- create_box("Hunter's Paradise", "Micanos (Mica II, V, VII)", 
                           "Manaringaine", "Crawford's Delight")
sucs_data <- update_sources(
  target = "3075", 
  title = "Jihad Secrets: The Blake Documents", 
  loc = "pp. 64-65",
  date = date("3075-01-01"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "CC", "FS", "LA", "DC", "FWL", "CoF",
               "CS", "WB",
               "CWF", "CJF", "CGB", "CNC", "CHH",
               "AB", "RCM", "FvC", "MC", "KP",
               "MOC", "TC", "CDP", "OA", "MH", "CF",
               "TD", "RC", "FrR")
)

# Add Field Reports data ------------------------------------------------------

# August 3079

# DCMS
bounding_box <- create_box("Leskovik", "Sterlington", 
                           "Elissa", "Towne")
sucs_data <- update_sources(
  target = "3079", 
  title = "Field Report: DCMS", 
  loc = "p. 21",
  date = date("3079-08-01"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "FS", "LA", "DC", "RS", "CoF",
               "CS", "WB",
               "CWF", "CJF", "CGB", "CNC", "CHH",
               "AB", "OA")
)

# CCAF
bounding_box <- create_box("Deschenes", "New Valencia", 
                           "Marcus", "Flaum")
sucs_data <- update_sources(
  target = "3079", 
  title = "Field Report: CCAF", 
  loc = "p. 21",
  date = date("3079-08-01"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "FS", "FWL", "CC", "RS", "CoF",
               "CS", "WB",
               "TC")
)

# FWLM
bounding_box <- create_box("Valerius", "Milos", 
                           "Maisons", "Canopus IV")
sucs_data <- update_sources(
  target = "3079", 
  title = "Field Report: FWLM", 
  loc = "p. 17",
  date = date("3079-08-01"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "LA", "FWL", "CC", "RS", "CoF",
               "CS", "WB",
               "CF", "MOC", "MH", "RCM")
)

# LAAF
bounding_box <- create_box("Otisberg", "Kawich", 
                           "New Caledonia", "Sterling")
sucs_data <- update_sources(
  target = "3079", 
  title = "Field Report: LAAF", 
  loc = "p. 19",
  date = date("3079-08-01"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "LA", "FWL", "CC", "DC", "RS", "CoF",
               "CJF", "CWF", "CHH", "CGB",
               "CS", "WB",
               "CF", "MH", "AB")
)

# AFFS
bounding_box <- create_box("Errai", "New Haiti (New Hati)", 
                           "Umijiri", "Hyalite")
sucs_data <- update_sources(
  target = "3079", 
  title = "Field Report: AFFS", 
  loc = "p. 21",
  date = date("3079-08-01"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "FS", "CC", "DC", "RS", "CoF",
               "CS", "WB",
               "TC", "AB", "TD", "OA", "FvC")
)

# Periphery - MOC
bounding_box <- create_box("Thraxa", "Jacson", 
                           "Palladaine", "Crawford's Delight")
sucs_data <- update_sources(
  target = "3079", 
  title = "Field Report: Periphery", 
  loc = "p. 20",
  date = date("3079-08-01"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "FS", "CC", "FWL",
               "CS", "WB",
               "MOC", "FrR")
)

# Periphery - TC
bounding_box <- create_box("Safe Port", "Lastpost", 
                           "Kigamboni", "Hellespont")
sucs_data <- update_sources(
  target = "3079", 
  title = "Field Report: Periphery", 
  loc = "p. 21",
  date = date("3079-08-01"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "FS", "CC", "FWL",
               "CS", "WB",
               "TC", "CDP")
)

# Periphery - Filtvelt Coalition
bounding_box <- create_box("Gurrnazovo", "Micanos (Mica II, V, VII)", 
                           "Brookeland", "Pirates Haven (50) (Badlands Cluster 3025-)")
sucs_data <- update_sources(
  target = "3079", 
  title = "Field Report: Periphery", 
  loc = "p. 22",
  date = date("3079-08-01"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "FS",
               "TD", "FvC")
)

# Periphery - MH
bounding_box <- create_box("New St. Andrews", "Eromanga", 
                           "Galisteo", "Vixen")
sucs_data <- update_sources(
  target = "3079", 
  title = "Field Report: Periphery", 
  loc = "p. 23",
  date = date("3079-08-01"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "FWL",
               "CS", "WB",
               "MH", "CF", "OA", "RCM")
)

# Periphery - Hanseatic League
bounding_box <- create_box("Anklan", "Hamburg", 
                           "Granada", "Gateway")
sucs_data <- update_sources(
  target = "3079", 
  title = "Field Report: Periphery", 
  loc = "p. 24",
  date = date("3079-08-01"), 
  box = bounding_box, 
  factions = c("I", "U", "A", 
               "HL", "NC")
)

# TODO: Add Field Report: Clans data


# Field Manual 3085 data --------------------------------------------------

# Ignore the 3095 column for now - changes from FM 3085 are mostly IE:ISP 3

# TODO: What about wars of reaving? I don't think we have a map.

# Era Digest Dark Ages data -----------------------------------------------

# 3135?

# Era Report 3145 data ----------------------------------------------------

# 3145

# Shattered Fortress data -------------------------------------------------


# Ilclan First Round data -------------------------------------------------

# Tamar Rising, Empire Alone, Dominions Divided, Ilkan's Eyes Only


# OTP: Hanseatic Crusade --------------------------------------------------

# Where else can I get Hanseatic League area maps?


# Interstellar Expeditions: ISP3 data -------------------------------------

# integrate whatever else I have here - none of it should be considered maps

# Handle House Arano data -------------------------------------------------

# TODO: These are still kind of a cluster - waiting to finish the rest before
# fixing

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

# TODO: it seems like most of these planets are reported as abandoned by
# 3030 - is that canon? from where?

# Create final data --------------------------------------------------------

sucs_data <- sucs_data |>
  filter(!is.na(source_title)) |>
  select(id_sucs, id_mhq, x, y, starts_with("source_"), 
         faction, starts_with("region"), capital) |>
  arrange(id_sucs, source_date)

# change some colors for better comparison
sucs_factions <- sucs_factions |>
  mutate(color = if_else(id_sucs == "UHC", "#90EE90", color),
         color = if_else(id_sucs == "U", "hotpink", color),
         color = if_else(id_sucs == "CSJ", "grey40", color),
         color = if_else(id_sucs == "FCL" | id_sucs == "FCF", "#ffcf40", color),
         color = if_else(id_sucs == "SS", "#CEFF00", color))

save(sucs_data, sucs_factions, file = "sucs_data.RData")
#gs4_auth()
#gs4_create("SUCS reborn", sheets = sucs_data)

# Create plots to test ----------------------------------------------------

