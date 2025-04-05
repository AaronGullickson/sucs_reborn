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


# plotting functions ------------------------------------------------------

# read in javascript code
js_dynamic_labels <- paste(readLines("add_dynamic_labels.js"), collapse = "\n")

plot_planets <- function(map_data,
                         date, 
                         title = NULL, 
                         show_id = TRUE,
                         interactive = TRUE,
                         choice_color = "faction",
                         faction_data = sucs_factions) {
  
  # Take a snapshot
  map_data <- map_data |>
    faction_snapshot(date)
  
  # get string for disputed cases
  map_data <- map_data |>
    mutate(disputed = str_extract(faction, "(?<=\\()[^()]+(?=\\))")) |>
    separate_wider_delim(disputed, ",", too_few = "align_start", 
                         names_sep = "") |>
    # we don't know how many there are so pivot longer to get names
    pivot_longer(starts_with("disputed")) |>
    mutate(value = factor(value, 
                          levels = faction_data$id_sucs, 
                          labels = faction_data$name)) |>
    # now reshape back wider and concatenate disputed cases
    pivot_wider() |>
    unite("disputed", starts_with("disputed"), sep = "/", na.rm = TRUE)
    
  
  # now organize the rest of the labels
  map_data <- map_data |>
    mutate(
      # first clean the disputed parenthetical away
      faction = str_remove(faction, "\\s*\\([^\\)]+\\)"),
      # now turn faction into factor
      faction = factor(faction, 
                       levels = faction_data$id_sucs, 
                       labels = faction_data$name),
      # construct strings for the map display
      faction_str = if_else(disputed == "", 
                            paste0(faction, "<br>"),
                            paste0(faction, " (", disputed, ")<br>")),
      capital_str = if_else(is.na(capital), "", paste0(capital, " Capital<br>")),
      region1_str = if_else(is.na(region1), "", paste0(region1, "<br>")),
      region2_str = if_else(is.na(region2), "", paste0(region2, "<br>")),
      region3_str = if_else(is.na(region3), "", paste0(region3, "<br>")),
      source_str = paste0("<i>Source:</i> ", 
                          paste(source_type, source_title, source_loc, 
                                sep = ", ")),
      source_date_str = paste0("<br><i>Source Date:</i> ", source_date),
      text_plotly = paste0("<b>", id_mhq, "</b><br>",
                           faction_str,
                           capital_str, region1_str, region2_str, region3_str,
                           source_str, source_date_str)
    )
  
  
  # Determine color palette - give a named vector to make sure colors match
  # in subsets
  if(choice_color == "faction") {
    map_data$var_color <- map_data$faction
    color_palette <- faction_data |> select(name, color) |> deframe()
    legend_name <- "Faction"
  } else {
    map_data$var_color <- map_data$source_title
    color_palette <- randomColor(length(unique(map_data$source_title)))
    names(color_palette) <- unique(map_data$source_title)
    legend_name <- "Source"
  }
  
  
  plot_title <- ifelse(is.null(title), as.character(date), title)
  
  faction_capital_data <- map_data |> 
    filter(capital == "Faction")
  
  major_capital_data <- map_data |> 
    filter(capital == "Major")
  
  minor_capital_data <- map_data |> 
    filter(capital == "Minor")
  
  # Base ggplot
  map <- map_data |>
    ggplot(aes(x = x, y = y, text = text_plotly, color = var_color,
               customdata = id_mhq)) +
    # some fancy stuff here for capital rings
    geom_point(data = faction_capital_data, size = 4)+
    geom_point(data = faction_capital_data, color = "grey20", size = 2.5)+
    geom_point(data = major_capital_data, size = 3.5)+
    geom_point(data = major_capital_data, color = "grey20", size = 2.5)+
    geom_point(data = minor_capital_data, size = 3)+
    geom_point(data = minor_capital_data, color = "grey20", size = 2.5)+
    geom_point(size = 2) +
    scale_color_manual(values = color_palette)+
    labs(title = plot_title, color = legend_name)+
    theme_void() +
    theme(panel.background = element_rect(fill = "grey20"),
          panel.grid = element_blank(),
          # these colors work well with superhero theme - change if it changes
          plot.background = element_rect(fill = "#3B4D5B"),
          text = element_text(color = "#EBEBEB"))
  
  # Add ID labels if required
  if (show_id) {
    if (!interactive) {
      map <- map + geom_text_repel(aes(label = id_mhq), color = "grey95", 
                                   size = 3)
    }
  }
  
  if (interactive) {
    # Convert to plotly
    map <- ggplotly(map, tooltip = "text") |>
      config(scrollZoom = TRUE)
    
    if(show_id) {
      #map <- map |> htmlwidgets::onRender(js_dynamic_labels)
    }
  }
  
  return(map)
}
