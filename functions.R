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


# plotting functions ------------------------------------------------------

# read in javascript code
js_dynamic_labels <- paste(readLines("add_dynamic_labels.js"), collapse = "\n")

plot_planets <- function(date, 
                         title = NULL, 
                         xlimits = c(-600, 780), 
                         ylimits = c(-580, 580),
                         faction_filter = NULL,
                         source_filter = NULL,
                         show_id = TRUE,
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
    if (!interactive) {
      map <- map + geom_text_repel(aes(label = id_mhq), color = "grey95", 
                                   size = 3)
    }
  }
  
  if (interactive) {
    # Convert to plotly
    map <- ggplotly(map, tooltip = "text") |>
      config(scrollZoom = TRUE) |>
      layout(dragmode = "pan")
    
    if(show_id) {
      map <- map |> htmlwidgets::onRender(js_dynamic_labels)
    }
  }
  
  return(map)
}
