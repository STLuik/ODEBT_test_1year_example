# scripts/06_make_bathymetry_layer.R
# This script creates spatial layer of depth points.

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

# Define paths
inputPath <- "Input/master"
outputPath <- output_subset_dir

# Function to clean column names
cleanColumnNames <- function(x) {
  x <- iconv(x, "UTF-8", "ASCII", sub="") # clean odd characters in stationID
  x <- gsub("\\[[^\\]]*\\]|\\:.*$|\\.", "", x, perl=TRUE) # remove text between [...]
  #  \[                       # '['
  #    [^\]]*                 # any character except: '\]' (0 or more
  #                           # times (matching the most amount possible))
  #    \]                     # ']'
  #    \\:.*$                 # remove everything after colon
  #    \\.                    # remove .
  x <- gsub("[ ]+$", "", x)   # now remove trailing space
  x <- gsub("[ ]+", "_", x) # replace spaces with _
  x
}

# Read raw depth points
bathy <- read.csv(file.path(inputPath, "BALTIC_BATHY_BALTSEM.csv"))
names(bathy) <- cleanColumnNames(names(bathy))
bathy <- dplyr::rename(bathy, depth = dybde)
bathy <- bathy[c("x", "y", "depth")]

# Make into spatial points dataframe (note implicit utm34 in BALTIC_BATHY_BALTSEM.csv)
bathy <- st_as_sf(bathy, coords = c("x", "y"), crs = "+proj=utm +zone=34 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Trim to extent of assessment units
bathy_filtered <- bathy |>
  sf::st_filter(st_union(helcom))

# Join points with new HELCOM polygons
bathy <- st_join(bathy_filtered, helcom[, c("Name","F2_Name")])

# Write out bathymetry shapefile
# Rename columns to be shapefile-safe
bathy_clean <- bathy
names(bathy_clean)[names(bathy_clean) == "Name"] <- "Basin"

write_sf(bathy_clean[c("depth", "Basin")], dsn = outputPath, layer = "oxy_bathymetry", driver = "ESRI Shapefile", append = T)

# Add to zip
zip(file.path(outputPath, "oxy_bathymetry.zip"), file.path(outputPath, dir(outputPath, pattern = "^oxy_bathymetry*")))









