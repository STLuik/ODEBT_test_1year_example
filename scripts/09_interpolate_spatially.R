# scripts/09_interpolate_spatially.R
# This script models spatial profiles based on the modeled profiles.

options(project_clean_workspace = FALSE)
# This prevents accidental workspace wiping when you run this script by itself during testing.

source("scripts/01_header.R")
# This loads packages (via setup.R if needed) and loads Utils functions into oxydebt_funs (if you have Utils/).

if (is.null(getOption("project_assessment"))) {
  # This checks whether the assessment settings (years + folders) have been defined.
  
  source("scripts/03_define_assessment.R")
  # If not defined, this sets the period and creates Input/Output folders for that period.
}

assessment <- getOption("project_assessment")
# This reads the assessment settings into a variable we can use in this script.

# Remove unnecessary data/values/functions
keep <- c("assessment", "end_year", "start_year", "outputPath", "inputPath", "proj", "repo_url", "O2satFun","auxilliaryFile")
# List the object names you want to keep (write yours here).

rm(list = setdiff(ls(envir = .GlobalEnv), keep), envir = .GlobalEnv)
# Removes everything in the global environment EXCEPT the objects in keep.

# Read profile fits
profiles_all <- read.csv(file.path(outputPath, "oxy_profiles_H2S_NH4.csv"))

# Get data from profiles, where halocline was found
profiles <- profiles_all[profiles_all$notes == "Check 7: Success.",]

# MODEL SPATIAL DISTRIBUTION OF PARAMETERS
# Prepare your bathymetry grid
grid <- read_sf(outputPath, "oxy_bathymetry") 

# Make a clean list of basin names from profiles.
basins <- unique(na.omit(profiles$Name))

# This keeps only the two columns we need and removes duplicate rows.
lookup <- unique(profiles[, c("Name", "Basin")])

# This removes rows where Name is missing (can't match those).
lookup <- lookup[!is.na(lookup$Name), ]

# match(...) finds, for each grid$Basin, the row in lookup where lookup$Name is equal.
# Then we pull the corresponding lookup$Basin into a new column.
grid$Basin_from_profiles <- lookup$Basin[match(grid$Basin, lookup$Name)]

# Filter grid to only those basins.
grid_sub <- grid[grid$Basin %in% basins, ]
# Rename columns
setnames(grid_sub, "Basin", "Basin_all")
setnames(grid_sub, "Basin_from_profiles", "Basin")

# Ensure your observation data is also 'sf' and matches the grid CRS
# Assuming profiles_subset has x and y in the same units as the grid
obs_sf <- st_as_sf(profiles, coords = c("x", "y"), crs = st_crs(grid_sub))




# SPATIAL INTERPOLATION FUNCTION
# Function: interpolate one year for ANY variable (IDW), optionally cap by bathymetry
interpolate_year_idw <- function(
    target_year,
    observations,
    target_grid,
    value_col,
    year_col = "Year",
    idp = 1,
    nmax = 30,
    min_n = 5,
    cap_by_depth = FALSE,
    depth_col = "depth"
) {
  if (!value_col %in% names(observations)) stop("Missing column in observations: ", value_col)
  # Stop if the variable is not in the observations table.
  
  if (!year_col %in% names(observations)) stop("Missing year column in observations: ", year_col)
  # Stop if the Year column is missing.
  
  yearly_data <- observations |>
    dplyr::filter(.data[[year_col]] == target_year) |>
    dplyr::filter(!is.na(.data[[value_col]]))
  # Keep only the chosen year and only rows where the variable is not NA.
  
  if (nrow(yearly_data) < min_n) {
    message("Not enough data for year ", target_year, " and variable ", value_col, " (n=", nrow(yearly_data), ")")
    return(NULL)
  }
  # If we have too few points to interpolate, return NULL.
  
  f <- stats::as.formula(paste(value_col, "~ 1"))
  # Build a formula like "halocline ~ 1".
  
  idw_out <- gstat::idw(
    formula = f,
    locations = yearly_data,
    newdata = target_grid,
    idp = idp,
    nmax = nmax
  )
  # IMPORTANT: idw() returns the predictions directly (it is NOT a model object).
  
  out <- target_grid
  # Copy the grid so we add columns without changing the original grid.
  
  pred_col <- paste0(value_col, "_interp")
  # Name for the interpolated column, e.g. "halocline_interp".
  
  out[[pred_col]] <- idw_out[["var1.pred"]]
  # Store the predicted values from idw() on the grid.
  
  if (cap_by_depth) {
    if (!depth_col %in% names(out)) stop("Missing depth column in target_grid: ", depth_col)
    # Stop if depth is missing (we need it to cap).
    
    final_col <- paste0(value_col, "_final")
    # Name for the capped column, e.g. "halocline_final".
    
    out[[final_col]] <- ifelse(out[[pred_col]] > out[[depth_col]], out[[depth_col]], out[[pred_col]])
    # Cap predicted values so they cannot be deeper than the seafloor.
  }
  
  out[[year_col]] <- target_year
  # Add Year to the grid output.
  
  out
  # Return the interpolated grid for this year and variable.
}


# Run MANY variables for ALL years + save maps + compute means
vars_to_interp <- c(
  "sali_surf",
  "sali_dif",
  "halocline",
  "depth_gradient",
  "O2def_below_halocline",
  "O2def_slope_below_halocline",
  "O2def_H2S_NH4_INTERPOLATED_below_halocline",
  "O2def_H2S_NH4_INTERPOLATED_slope_below_halocline"
)
# These are the variable names (columns) in obs_sf that you want to interpolate.

cap_vars <- c("halocline")
# These are the variables that should be capped by bathymetry depth after interpolation.

years <- sort(unique(obs_sf$Year))
# Get all unique years from your observation data.



for (yr in years) {
  # Loop over each year.
  
  message("Interpolating year: ", yr)
  # Print progress so you know which year is running.
  
  grid_year <- grid_sub
  # Start with a fresh copy of the grid for this year.
  
  grid_year$Year <- yr
  # Store the year on the grid.
  
  for (v in vars_to_interp) {
    # Loop over each variable you want to interpolate.
    
    tmp <- interpolate_year_idw(
      target_year = yr,
      observations = obs_sf,
      target_grid = grid_sub,
      value_col = v,
      cap_by_depth = v %in% cap_vars
    )
    # Interpolate this variable for this year (and cap it if it's in cap_vars).
    
    pred_col <- paste0(v, "_interp")
    # The column name where predictions will be stored.
    
    final_col <- paste0(v, "_final")
    # The column name for capped values (only exists if v is in cap_vars).
    
    if (is.null(tmp)) {
      # If interpolation failed (not enough data), fill with NA.
      
      grid_year[[pred_col]] <- NA_real_
      # Create the interpolated column but fill it with NA.
      
      if (v %in% cap_vars) grid_year[[final_col]] <- NA_real_
      # If this variable normally has a capped version, create it as NA too.
      
    } else {
      # If interpolation worked, copy the new columns onto grid_year.
      
      grid_year[[pred_col]] <- tmp[[pred_col]]
      # Copy interpolated values for this variable onto the year grid.
      
      if (v %in% cap_vars) grid_year[[final_col]] <- tmp[[final_col]]
      # Copy capped values too (only for variables that are capped).
    }
  }
  
  grid_path <- file.path(outputPath, paste0("grid_interpolated_", yr, ".rds"))
  # Create a filename for saving the interpolated grid for this year.
  
  saveRDS(grid_year, grid_path)
  # Save the full year grid (with all interpolated variables) to disk.
}



# Quick “good defaults” for your situation:
# With ~500 stations/year and uneven spatial coverage:
# Start: idp = 1.5, nmax = 30
# If still too bullseye/noisy: idp = 1.0, nmax = 50
# If too smooth / washed out: idp = 1.75, nmax = 20

