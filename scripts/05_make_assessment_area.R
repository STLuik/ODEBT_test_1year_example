# scripts/05_make_assessment_area.R
# This script creates assessment areas

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

# Create new shapefile for HELCOM areas to be used for assessment
# Read HELCOM assessment unit data and drop non SEA areas, i.e., keep only open sea areas ################################################################## THIS HAS TO BE ADDED TO data_1_download.R, where spatial info is downloaded
helcom <- sf::st_read(inputPath, "HELCOM_subbasin_with_coastal_WFD_waterbodies_or_watertypes_2022_eutro")
helcom <- helcom[grep("^SEA-", helcom$HELCOM_ID),]
# Get only specific areas
helcom <- helcom[helcom$Name %in% c("Opensea Gulf of Gdansk",
                                    "Opensea Eastern Gotland Basin",
                                    "Opensea Western Gotland Basin",
                                    "Opensea Northern Baltic Proper",
                                    "Opensea Gulf of Finland Western",
                                    "Opensea Bornholm Basin"),]
# For assessment purposes (only have 2 areas so to say) let's change the names of basins to represent these 2 areas.
helcom$F2_Name[helcom$F2_Name == "Opensea Gulf of Gdansk"] <- "Central BS"
helcom$F2_Name[helcom$F2_Name == "Opensea Eastern Gotland Basin"] <- "Central BS"
helcom$F2_Name[helcom$F2_Name == "Opensea Western Gotland Basin"] <- "Central BS"
helcom$F2_Name[helcom$F2_Name == "Opensea Northern Baltic Proper"] <- "Central BS"
helcom$F2_Name[helcom$F2_Name == "Opensea Gulf of Finland"] <- "Central BS"
helcom$F2_Name[helcom$F2_Name == "Opensea Bornholm Basin"] <- "Bornholm"


# Transform to utm34 
helcom <- sf::st_transform(helcom, sp::CRS("+proj=utm +zone=34 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Convert sf to sp
helcom_sp <- as(helcom, "Spatial")    

# Assign unique IDs
for (i in 1:length(helcom_sp@polygons)) {
  helcom_sp@polygons[[i]]@ID <- as.character(i)
}    

# Sample one point per polygon and extract attributes from BALTSEM
data <- do.call(rbind, lapply(1:length(helcom_sp), function(i) {
  pt <- sp::spsample(helcom_sp[i,], 1, type = "random")
  sp::over(pt, helcom_sp)
}))    

# Create SpatialPolygonsDataFrame
helcom_spdf <- sp::SpatialPolygonsDataFrame(helcom_sp, data)

# Fix names
helcom_sp$Name <- gsub("Å", "A", helcom_sp$Name)

# Explode multipolygons into individual polygons
helcom_sp_single <- helcom %>%
  st_cast("POLYGON") %>%
  mutate(id = row_number())

# Get centroids or points on surface
centroids <- st_point_on_surface(helcom_sp_single)
coords <- st_coordinates(centroids)

# Write out assessment area shapefile
# Convert to sf
helcom_sf <- st_as_sf(helcom_sp)

sf::write_sf(helcom_sf[c("Name", "F2_Name")], outputPath, "oxy_areas", driver = "ESRI Shapefile", overwrite_layer = TRUE)

# Add to zip
zip(file.path(outputPath, "oxy_areas.zip"), file.path(outputPath, dir(outputPath, pattern = "^oxy_areas*")))
