library(googlesheets4)
library(tidyverse)
library(here)
library(ggpubr)


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

# a few errors need to be corrected
# Brasha - this is showing up as independent based on p. 151 of Major Periphery
# States which says it was found in the "early 26th century" - but it does not
# show up on the map in Major Periphery States in 2571 or the 2596 maps from
# the Reunification War. My guess is this is a typo meant to be 27th century. 
# In any case, since we are going by maps only here, it needs to be U.
sucs_data <- correct_faction("Brasha", "2596", "U")
#sucs_data <- sucs_data |>
#  mutate(faction = if_else(id_mhq == "Brasha" & time_point == "2596", "U", faction))
# Gibraltar - this is showing up as independent because of a planet write up
# in Empire Alone that says it was independent until 2610, despite maps to the
# contrary
sucs_data <- correct_faction("Gibraltar", "2596", "FWL")
#sucs_data <- sucs_data |>
#  mutate(faction = if_else(id_mhq == "Gibraltar" & time_point == "2596", "FWL", faction))
# Sherwood - seems to be "corrected" to independent from write up in Touring the 
# Stars. However, Sherwood does not show up at all in map, so it should be
# changed to U
sucs_data <- correct_faction("Sherwood", "2596", "U")
# Stotzing - This one is listed as independent but doesn't show up on the map.
# I am guessing this is based on Touring the Stars: Stotzing, but this document
# says quite clearly "The world was considered officially settled in 2598, 
# "when its new capital of Alt-Eisenstadt—now known as Sophia—was founded."
# That would be more than a year after the Reunification War map, so this 
# should be listed as U
sucs_data <- correct_faction("Stotzing", "2596", "U")
# Alfirk - this is listed as an independent world, apparently from very early
# on based on an entry from the Periphery handbook (2nd edition) and being
# put on maps in Era Report 3145. But on this map, it is not present, so it
# should be listed as U. This is the first map where it could be present
sucs_data <- correct_faction("Alfirk", "2596", "U")
# Ward - The text on pg. 88 of HBHL says it was founded during "the Exodus
# from Terra" but it doesn't show up on maps in the same document until the 
# First Succession War map. It also shows up in the Era Report 2750 map. 
# Technically, the only named date in the entry on pg. 88 is for 2644, so 
# its not totally inconsistent that it wasn't founded until after Reunification
# War. For map it should be U
sucs_data <- correct_faction("Ward", "2596", "U")

# Create final data --------------------------------------------------------

sucs_data <- sucs_data |>
  filter(!is.na(source_title)) |>
  select(id_sucs, id_mhq, x, y, starts_with("source_"), faction, starts_with("region")) |>
  arrange(id_sucs, source_date)

#gs4_auth()
#gs4_create("SUCS reborn", sheets = sucs_data)

# Create plots to test ----------------------------------------------------

plot_planets <- function(date, title = NULL) {
  # get the date for each planet closest to the date but not over
  temp <- sucs_data |>
    filter(source_date <= date) |>
    # arrange with most recent date at the top
    arrange(id_sucs, desc(source_date)) |>
    # remove duplicate planet entries
    filter(!duplicated(id_sucs)) |>
    select(x, y, faction, id_mhq) |>
    mutate(faction = factor(faction, 
                            levels = sucs_factions$id_sucs,
                            labels = sucs_factions$name))
  
  # determine color palette
  faction_colors <- sucs_factions |>
    filter(name %in% unique(temp$faction)) |>
    pull("color")
  
  plot_title <- if_else(is.null(title), as.character(date), title)
  
  temp |>
    filter(faction != "Undiscovered") |>
    ggplot(aes(x = x, y = y, color = faction))+
    geom_point()+
    scale_x_continuous(limits = c(-490, 670))+
    scale_y_continuous(limits = c(-480, 515))+
    scale_color_manual(values = faction_colors)+
    labs(title = plot_title)+
    theme_void()+
    theme(panel.background = element_rect(fill = "grey20"),
          panel.grid = element_blank())
}

# change some colors for better comparison
sucs_factions <- sucs_factions |>
  mutate(color = ifelse(id_sucs == "UHC", "#90EE90", color))

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
