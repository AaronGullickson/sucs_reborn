TIME_POINTS <- c("2271", "2317", "2319", "2341", "2366", "2571", "2596", "2750",
                 "2765", "2767", "2783", "2786", "2821", "2822", "2830", "2864",
                 "3025", "3030", "3040", "3049", "3050a", "3050b", "3050c", 
                 "3051", "3052", "3057", "3058", "3059a", "3059b", "3059c", 
                 "3059d", "3063", "3067", "3068", "3075", "3079", "3081", 
                 "3085", "3095", "3130", "3135", "3145", "3151", "3152")


# Helper functions --------------------------------------------------------

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

# check if x and y are in the box
is_in_box <- function(x, y, box) {
  if(is.null(box)) {
    return(TRUE)
  }
  (x >= box$x_left & x <= box$x_right & y <= box$y_high & y >= box$y_low)
}

# get all time points within a range
time_point_range <- function(start, 
                             end = TIME_POINTS[length(TIME_POINTS)]) {
  TIME_POINTS[which(TIME_POINTS == start):which(TIME_POINTS == end)]
}


# Modify data functions ---------------------------------------------------

update_sources <- function(map_data, target, title, loc, date, 
                           box = NULL, factions = NULL) {
  map_data |> 
    # First, drop any values from sucs_data from the target_time 
    mutate(
      # only change values that are from the target time and in
      # the bounding box and come from acceptable factions
      change_source = (time_point == target) & is_in_box(x, y, box) & 
        (is.null(faction) | 
           faction %in% factions | 
           # always accept disputed codes
           str_detect(faction, "^D\\(")),
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

correct_faction <- function(map_data, id, time_target, new_faction) {
  map_data |>
    mutate(faction = if_else(id_mhq %in% id & time_point %in% time_target, 
                             new_faction, faction))
}

correct_sources <- function(map_data, id, time_target,
                            new_source_title, new_source_loc, new_source_date) {
  map_data |>
    mutate(
      source_title = if_else(id_mhq %in% id & time_point %in% time_target, 
                             new_source_title, source_title),
      source_loc = if_else(id_mhq %in% id & time_point %in% time_target, 
                           new_source_loc, source_loc),
      source_date = if_else(id_mhq %in% id & time_point %in% time_target, 
                            new_source_date, source_date)
    )
}

remove_cases <- function(map_data, id, time_target) {
  map_data |>
    filter(!(id_mhq == id & time_point %in% time_target))
}

make_new_entry <- function(map_data, id, time, type, title, loc, date, faction,
                           hidden = FALSE) {
  case <- map_data |> filter(id_mhq == id)
  map_data <- map_data |>
    bind_rows(
      tibble(
        id_sucs = case$id_sucs[1],
        id_mhq = id,
        x = case$x[1],
        y = case$y[1],
        time_point = time,
        source_type =type,
        source_title = title,
        source_loc = loc,
        source_date = date,
        faction = faction,
        hidden = hidden
      )
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
    select(starts_with("id_"), x, y, faction, starts_with("region"), capital,
           source_type, source_title, source_loc, source_date)
}
