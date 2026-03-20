# scripts/10_visualize_spatially_interpolated_data_get_means.R
# This script visualizes and gets mean values of spatial profiles.

options(project_clean_workspace = FALSE)
# This prevents accidental workspace wiping when you run this script by itself during testing.

source("scripts/01_header.R")
# This loads packages (via setup.R if needed) and loads Utils functions into oxydebt_funs (if you have Utils/).

if (is.null(getOption("project_assessment"))) {
  # This checks whether the assessment settings (years + folders) have been defined.
  
  source("scripts/03_define_assessment.R")
  # If not defined, this sets the period and creates Input/Output folders for that period.
}

# This reads the assessment settings into a variable we can use in this script.
assessment <- getOption("project_assessment")

# Remove unnecessary data/values/functions
# List the object names you want to keep (write yours here).
keep <- c("years","assessment", "end_year", "start_year", "outputPath", "inputPath", "proj", "repo_url", "years","obs_sf","auxilliaryFile")

# Removes everything in the global environment EXCEPT the objects in keep.
rm(list = setdiff(ls(envir = .GlobalEnv), keep), envir = .GlobalEnv)

# Variables for which figures and/or statistics is found
vars <- c(
  "sali_surf_interp",
  "sali_dif_interp",
  "halocline_interp",                                       
  "halocline_final",
  "halocline_final_2",
  "depth_gradient_interp",                                  
  "O2def_below_halocline_interp",                           
  "O2def_slope_below_halocline_interp",                     
  "O2def_H2S_NH4_INTERPOLATED_below_halocline_interp",      
  "O2def_H2S_NH4_INTERPOLATED_slope_below_halocline_interp"
)

sum_vars <- c(
  "grid_volume_m3",
  "O2def_in_gridcell",
  "O2def_H2S_NH4_INTERPOLATED_in_gridcell"
)

# This will store one means table per year.
means_list <- list()
# Stores sum tables per year.
sums_list <- list()

# LOOP THROUGH YEARS
for (yr in years) {
  # Prints progress.
  message("Means for year: ", yr)
  
  # Builds the filename for this year.
  grid_path <- file.path(outputPath, paste0("grid_interpolated_", yr, ".rds"))
 
  # Loads the interpolated grid for this year (it still has geometry).
  #grid <- readRDS(grid_path)
  grid_path <- file.path(outputPath, paste0("grid_interpolated_", yr, ".rds"))
  # Build the filename for this year.
  
  if (!file.exists(grid_path)) {
    message("No file for year ", yr, " (", grid_path, ") — skipping this year.")
    next
  }
  # If the file doesn't exist, print a message and jump to the next year in the loop.
  
  grid <- tryCatch(
    readRDS(grid_path),
    error = function(e) {
      message("Failed to read file for year ", yr, ": ", e$message, " — skipping this year.")
      return(NULL)
    }
  )
  # Try to read the file; if it errors, return NULL.
  
  if (is.null(grid)) next
  # If reading failed, jump to the next year.
  
  
  # creates a unique row id for joining
  grid$id <- seq_len(nrow(grid))
  
  # Create a new column in 'grid', where some halocline_final values are set to NA
  # Remove geometry to make calculations faster
  grid_no_geo <- st_drop_geometry(grid)
  # Get the depth below halocline layer
  grid_no_geo$halocline_final_2 <- grid_no_geo$halocline_interp + grid_no_geo$depth_gradient_interp
  # If the depth below halocline layer is bigger than the actual bathymetry depth, then it's NA
  grid_no_geo$halocline_final_2[grid_no_geo$halocline_final_2 > grid_no_geo$depth] <- NA
  
  
  # Calculate the volume of water (in m3) below halocline in each grid cell.
  # This means, get the difference between depth and halocline_final_2 (in meters)
  grid_no_geo$distance_to_bottom <- grid_no_geo$depth - grid_no_geo$halocline_final_2
  grid_no_geo$distance_to_bottom[is.na(grid_no_geo$halocline_final_2)] <- NA
  # And then multiply the depth difference by grid area, which should be 1km x 1km
  grid_no_geo$grid_volume_m3 <- grid_no_geo$distance_to_bottom * 1000 * 1000
  
  # Get the DO deficit value per grid cell
  # Get DO deficit at bottom (mg/l):
  grid_no_geo$O2def_at_bottom <- (grid_no_geo$O2def_slope_below_halocline_interp * grid_no_geo$distance_to_bottom) + grid_no_geo$O2def_below_halocline_interp
  
  grid_no_geo$O2def_H2S_NH4_INTERPOLATED_at_bottom <- (grid_no_geo$O2def_H2S_NH4_INTERPOLATED_slope_below_halocline_interp * grid_no_geo$distance_to_bottom) + grid_no_geo$O2def_H2S_NH4_INTERPOLATED_below_halocline_interp
  
  # Get mean DO deficit (mg/l): 
  grid_no_geo$O2def_mean <- (grid_no_geo$O2def_below_halocline_interp + grid_no_geo$O2def_at_bottom) / 2 
  
  grid_no_geo$O2def_H2S_NH4_INTERPOLATED_mean <- (grid_no_geo$O2def_H2S_NH4_INTERPOLATED_below_halocline_interp + grid_no_geo$O2def_H2S_NH4_INTERPOLATED_at_bottom) / 2 
  
  # Get DO deficit in grid cell below halocline (mg). First: DO def mg/l to mg/m3 by multiplying by 1000. And then multiply by the volume of water below halocline in grid cell
  grid_no_geo$O2def_in_gridcell <- (grid_no_geo$O2def_mean * 1000) *  grid_no_geo$grid_volume_m3
  
  grid_no_geo$O2def_H2S_NH4_INTERPOLATED_in_gridcell <- (grid_no_geo$O2def_H2S_NH4_INTERPOLATED_mean * 1000) *  grid_no_geo$grid_volume_m3
  
  
   # This gets the basins that actually have observations in this year.
  basins_with_obs <- obs_sf %>%
    sf::st_drop_geometry() %>%
    filter(Year == yr) %>%
    distinct(Basin) %>%
    pull(Basin)
 
   # This removes basins with no observations (e.g., Bornholm in 1950).
  grid_stats <- grid_no_geo %>%
    filter(Basin %in% basins_with_obs)
 
  
  
  # GET MEANS
  means_year <- grid_stats  %>%
    group_by(Basin) %>%                                      # one group per basin
    summarise(across(all_of(vars), ~ mean(.x, na.rm = TRUE)), # mean of each variable
              n_grid = n(),                                  # optional: number of grid rows per basin
              .groups = "drop") %>%                          # remove grouping
    mutate(Year = yr) %>%                                    # add year column
    relocate(Year, Basin)                                    # put Year and Basin first (nice layout)
  
  means_list[[as.character(yr)]] <- means_year
  # Store this year's table into the list.
  
  # GET SUMS
  sums_year <- grid_stats  %>%
    group_by(Basin) %>%                                              # one group per basin
    summarise(across(all_of(sum_vars), ~ sum(.x, na.rm = TRUE)),       # sum of each chosen variable
              n_grid = n(),                                           # how many grid cells in that basin
              .groups = "drop") %>%
    mutate(Year = yr) %>%                                             # add year column
    relocate(Year, Basin)
  # This creates basin-year sums for your chosen variables.
  
  sums_list[[as.character(yr)]] <- sums_year
  # Store this year's sums table.
  
  # left_join adds the geometry column from grid (matched by id)
  # st_as_sf turns the result into an sf object
  grid_add <- grid_no_geo %>%
    left_join(
      grid %>% select(id, geometry),
      by = "id"
    ) %>%
    st_as_sf()
  
  
  
  # REMOVE DATA FROM BASINS WHERE IT WAS CREATED WITH INTERPOLATION BUT WHERE NO OBSERVATIONS EXIST
  # 1) Find basins that actually have observations in this year
  basins_with_obs <- obs_sf %>%
    sf::st_drop_geometry() %>%     # remove geometry (faster)
    dplyr::filter(Year == yr) %>%  # this year only
    dplyr::distinct(Basin) %>%     # unique basins with obs
    dplyr::pull(Basin)             # turn into a character vector
  
  # 2) Mark grid rows that are in basins WITHOUT observations
  no_data_mask <- !(grid_add$Basin %in% basins_with_obs)
  # TRUE = basin has no obs this year (we want to remove/mask its interpolated values)
  
  # 3) Decide which columns should be wiped out for those basins
  cols_to_na <- c(vars, sum_vars,
                  "halocline_final_2", "distance_to_bottom", "grid_volume_m3",
                  "O2def_at_bottom", "O2def_mean", "O2def_in_gridcell",
                  "O2def_H2S_NH4_INTERPOLATED_at_bottom", "O2def_H2S_NH4_INTERPOLATED_mean",
                  "O2def_H2S_NH4_INTERPOLATED_in_gridcell")
  # Add/remove names here depending on which derived columns you created.
  
  cols_to_na <- intersect(cols_to_na, names(grid_add))
  # Keeps only the columns that actually exist in grid_add (prevents errors)
  
  # 4) Mask those columns to NA for basins with no observations
  grid_add[no_data_mask, cols_to_na] <- NA
  # Now those basins have "no interpolated data" for this year
  
  
  
  
  grid_outpath <- file.path(outputPath, paste0("grid_interpolated_additional_", yr, ".rds"))
  # Create a filename for saving the interpolated grid for this year.
  
  saveRDS(grid_add, grid_outpath)
  # Save the full year grid (with all interpolated variables) to disk.
  
}

# Combines all years into tables.
means_table_all_years <- bind_rows(means_list)
sums_table_all_years <- bind_rows(sums_list)

# Joins the mean and sum results side-by-side.
final_table <- left_join(means_table_all_years, sums_table_all_years, by = c("Year", "Basin"))

# Get volume specific DO deficit values, the oxygen debt indicator. Since the DO def values are in mg/m3, then divide by 1000
final_table$ODEBT_volsp <- final_table$O2def_in_gridcell / final_table$grid_volume_m3 / 1000
final_table$ODEBT_H2S_NH4_INTERPOLATED_volsp <- final_table$O2def_H2S_NH4_INTERPOLATED_in_gridcell / final_table$grid_volume_m3 / 1000


write.csv(
  final_table,
  file.path(outputPath, "ODEBT_by_year_and_basin.csv"),
  row.names = FALSE
)












#FIGURES
# Get some figures to visualize results
# LOOP THROUGH YEARS
for (yr in years) {
  # Prints progress.
  message("Figures for year: ", yr)

  # Builds the filename for this year.
  grid_path <- file.path(outputPath, paste0("grid_interpolated_additional_", yr, ".rds"))

  # Loads the interpolated grid for this year (it still has geometry).
  grid <- readRDS(grid_path)

  # Figure of halocline_final_2, which is the depth below the halocline layer
  # Keep only grid cells that have data
  z <- grid[!is.na(grid$halocline_final_2), ]

  if (nrow(z) == 0) {
    message("No data for year ", yr, " — skipping.")
    next
  }
  # If z has zero rows, print a message and jump to the next year in the loop.

  # Read in shape file of assessment areas
  if (!exists("areas")) {
    areas <- st_read(inputPath,"HELCOM_subbasin_with_coastal_WFD_waterbodies_or_watertypes_2022_eutro")
    # Converts the shapefile to the same CRS as z so it lines up correctly.
    areas <- st_transform(areas, st_crs(z))
  }

  # FIGURE OF DEPTH BELOW HALOCLINE LAYER
  p <- ggplot(data = z) +
    geom_sf(aes(color = halocline_final_2), size = 0.5) +     # points coloured by halocline depth
    geom_sf(data = areas, fill = NA, linewidth = 0.6) +     # polygon outlines (visual only)
    scale_color_viridis_c(
      option = "mako",
      direction = -1,
      name = "Depth (m)",
      limits = c(30,100)
    ) +
    theme_minimal() +
    labs(
      title = "Depth below halocline layer (m)",
      #subtitle = paste0(yr, " | Capped by Seafloor"),
      x = "Easting", y = "Northing"
    )

  # create the output filename
  full_path <- file.path(outputPath, paste0("Depth_below_halocline_map_", yr, ".jpg"))

  # save the map
  ggsave(
    filename = full_path,
    plot = p,
    device = "jpg",
    width = 10,
    height = 8,
    dpi = 300
  )

# TILED FIGURE OF DO DEFICIT
  # These figures have data on the mean do deficit below the halocline layer, which is found based on the do deficit value below the halocline layer and the slope of DO deficit.
  z_long <- z %>%
    #st_drop_geometry() %>%
    select(geometry, O2def_mean, O2def_H2S_NH4_INTERPOLATED_mean) %>%
    pivot_longer(
      cols = c(O2def_mean, O2def_H2S_NH4_INTERPOLATED_mean),
      names_to = "variable",
      values_to = "value"
    ) %>%
    st_as_sf()
  # Makes one column called "value" that contains whichever variable is being plotted,
  # and a column "variable" that tells which one it is.

  z_long$variable <- recode(
    z_long$variable,
    O2def_mean = "O2 deficit (mean)",
    O2def_H2S_NH4_INTERPOLATED_mean = "O2 deficit (mean) incl. H2S + NH4"
  )
  # Makes nicer labels for the facet titles.

  p <- ggplot(data = z_long) +
    geom_sf(aes(color = value), size = 0.5) +                # same layer, different values per facet
    geom_sf(data = areas, fill = NA, linewidth = 0.6) +      # outlines
    scale_color_viridis_c(
      option = "mako",
      direction = -1,
      name = "O2 deficit (mg/l)",
      limits = c(0, 15)
    ) +
    facet_wrap(~ variable, nrow = 1) +                       # 1 row = left/right tiles
    theme_minimal() +
    labs(
      title = "O2 deficit (mean)",
      subtitle = as.character(yr),
      x = "Easting", y = "Northing"
    )

  full_path <- file.path(outputPath, paste0("O2_deficit_mean_map_tiled_", yr, ".jpg"))

  ggsave(
    filename = full_path,
    plot = p,
    device = "jpg",
    width = 14,     # a bit wider for two panels
    height = 8,
    dpi = 300
  )

}







