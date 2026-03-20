# scripts/11_assessment_confidence.R
# This script models profiles based on the cleaned data.

options(project_clean_workspace = FALSE)
# This prevents accidental workspace wiping when you run this script by itself during testing.

source("scripts/header.R")
# This loads packages (via setup.R if needed) and loads Utils functions into oxydebt_funs (if you have Utils/).

if (is.null(getOption("project_assessment"))) {
  # This checks whether the assessment settings (years + folders) have been defined.
  
  source("scripts/00_define_assessment.R")
  # If not defined, this sets the period and creates Input/Output folders for that period.
}

assessment <- getOption("project_assessment")
# This reads the assessment settings into a variable we can use in this script.

# Define paths
inputPath <- "Input/master"


# Remove unnecessary data/values/functions
keep <- c("years","assessment", "end_year", "start_year", "outputPath", "inputPath", "proj", "repo_url", "O2satFun","auxilliaryFile")
# List the object names you want to keep (write yours here).

rm(list = setdiff(ls(envir = .GlobalEnv), keep), envir = .GlobalEnv)
# Removes everything in the global environment EXCEPT the objects in keep.

# Read in data used in confidence assessment
success_summary <- read.csv(file.path(outputPath, "summary_of_oxy_profiles_success_by_year_and_2areas.csv"))

df <- success_summary
# Make a working copy.

check_cols <- grep("^(CHECK|Check)\\.[0-9]+\\.", names(df), value = TRUE)
# Finds columns that start with CHECK.<number>. or Check.<number>.

check_nums <- sub("^(?:CHECK|Check)\\.([0-9]+)\\..*$", "\\1", check_cols, perl = TRUE)
# Extracts the check number from each CHECK column name.

names(df)[match(check_cols, names(df))] <- paste0("CHECK ", check_nums)
# Renames long CHECK columns to short ones: CHECK 1, CHECK 2, ... CHECK 7.

check_order <- paste0("CHECK ", sort(as.integer(check_nums)))
# Makes an ordered list: CHECK 1, CHECK 2, ... CHECK 7.

df <- df[, c("Year", "Basin", check_order, "number_of_profiles", "success_percent")]
# Reorders columns: Year, Basin, CHECK 1..7, then totals.

df <- df[order(df$Year, df$Basin), ]
# Sorts rows by Year then Basin (optional).



# Read profile fits
profiles_all <- read.csv(file.path(outputPath, "oxy_profiles_H2S_NH4.csv"))

# Get data from profiles, where halocline was found
profiles <- profiles_all[profiles_all$notes == "Check 7: Success.",]

# Read the basin-year assessment results
results <- read.csv(file.path(outputPath, "ODEBT_by_year_and_basin.csv"))

#------------------------------------------------------------------------------#
# Create vars to hold the confidence information
years <- unique(df$Year)
# Unique years (better than df$Year because df$Year may contain repeats).

ub <- unique(df$Basin)
# Unique basins.

df_sum <- expand.grid(Year = years, Basin = ub)
# Creates all combinations: each Year appears length(ub) times.
# Result has columns Year and Basin.
# Columns added later:
  # spatial_sal_model
  # spatial_temp_unique_stations

#------------------------------------------------------------------------------#

# Get confidence aspects per year and later summarize per assessment period
# SPATIAL (& METHODICAL)
# 1. Salinity model success % per year and basin. Data in: summary_of_oxy_profiles_success_by_5basins.csv
# This is directly as % already and ce used in the assessment period final confidence
key_sum <- paste(df_sum$Year, df_sum$Basin, sep = "||")
df_sum$spatial_sal_model <- df$success_percent[match(key_sum, paste(df$Year, df$Basin, sep = "||"))]

df_sum$spatial_sal_model[df_sum$spatial_sal_model == 0] <- NA

# SPATIAL
# 2. Unique stations per year. If more than 100, then 100%, if more than 50, then 50%, if less than 50, then 0%
un_stations_tbl <- profiles %>%
  mutate(
    Longitude_r = round(Longitude, 5),
    Latitude_r  = round(Latitude, 5)
  ) %>%
  group_by(Year, Basin) %>%
  summarise(
    un_stations = n_distinct(Longitude_r, Latitude_r),
    .groups = "drop"
  )
# Creates a table with one row per Year×Basin and a count of unique IDs.

# Assign % values based on preset conditions.
# If more than 100 unique stations per year, then 100%, if more than 50, then 50%, if less than 50, then 0%
spatial_temp_unique_stations <- un_stations_tbl
spatial_temp_unique_stations$un_stations[un_stations_tbl$un_stations < 50] <- 0
spatial_temp_unique_stations$un_stations[un_stations_tbl$un_stations >= 50] <- 50
spatial_temp_unique_stations$un_stations[un_stations_tbl$un_stations >= 100] <- 100

df_sum <- df_sum %>%
  left_join(spatial_temp_unique_stations, by = c("Year", "Basin"))
# Adds un_stations into df_sum based on matching Year and Basin.





# TEMPORAL.
# Months per year. If 0 missing, then 100%, if 1-2 missing, then 50%, else 0%. If at least half of assessment years have 0 missing months, then 100%.
library(dplyr)

months_tbl <- profiles %>%
  group_by(Year, Basin) %>%
  summarise(n_months = n_distinct(Month), .groups = "drop")
# Counts unique months per Year × Basin.

temporal_months <- months_tbl %>%
  mutate(
    n_months = case_when(
      n_months == 12 ~ 100L,
      n_months >= 10 ~ 50L,
      TRUE ~ 0L
    )
  )
# Converts month counts into 100/50/0 without overwriting 100.

period_length <- length(years)
# Total number of assessment years.

temporal_months <- temporal_months %>%
  group_by(Basin) %>%
  mutate(
    temporal_months = if (sum(n_months == 100L, na.rm = TRUE) >= ceiling(period_length / 2)) 100L else n_months
  ) %>%
  ungroup()
temporal_months$n_months <- NULL
# If at least half of the assessment years in this basin are 100, set ALL years to 100 for that basin.
df_sum <- df_sum %>%
  left_join(temporal_months, by = c("Year", "Basin"))




# METHODICAL
# If regionally agreed monitoring and analysis methods, then 100%, locally agreed/accredited etc. - 50%, else 0%.
df_sum$methodical <- 100
df_sum$methodical[is.na(df_sum$spatial_sal_model)] <- NA




# ACCURACY
# You have an estimated value (your mean), but it’s uncertain.
# The probability of correct classification is the chance that the true value is on the same side of the GES threshold as your classification (good vs not-good).
# The easiest way to turn that into math is to assume your estimate has a bell-curve uncertainty around it.
# First, get the period mean values from yearly assessment results


means_by_basin <- results %>%
  group_by(Basin) %>%
  summarise(
    mean_ODEBT = mean(ODEBT_H2S_NH4_INTERPOLATED_volsp, na.rm = TRUE),
    sd_ODEBT   = sd(ODEBT_H2S_NH4_INTERPOLATED_volsp, na.rm = TRUE),
    n          = sum(!is.na(ODEBT_H2S_NH4_INTERPOLATED_volsp)),
    se_period  = sd_ODEBT / sqrt(n),
    .groups = "drop"
  )
# mean_ODEBT = your period mean per basin
# se_period = uncertainty of that mean (simple SE)

GES_by_basin <- c("Bornholm" = 6.37, "Central BS" = 8.66)
# Proposed from this work:
#GES_by_basin <- c("Bornholm" = 7.48, "Central BS" = 8.44)

period_probs <- means_by_basin %>%
  mutate(
    GES = GES_by_basin[Basin],
    p_good = pnorm(GES, mean = mean_ODEBT, sd = se_period),  # P(true mean <= GES)
    p_bad  = 1 - p_good,                                     # P(true mean > GES)
    class = ifelse(mean_ODEBT > GES, "Bad", "Good"),          # classification based on mean
    p_correct = ifelse(class == "Bad", p_bad, p_good)         # probability classification is correct
  )


period_probs <- period_probs %>%
  mutate(
    p_good_pct = 100 * p_good,
    p_bad_pct = 100 * p_bad,
    p_correct_pct = 100 * p_correct
  )
# Converts probabilities (0–1) to percentages (0–100).

# Get only basin and accuracy infro from table
probs <- period_probs[, c("Basin", "p_correct_pct")]


# Add accuarcy to the df_sum table
df_sum <- df_sum %>%
  left_join(probs, by = c("Basin"))

df_sum$p_correct_pct[is.na(df_sum$spatial_sal_model)] <- NA

# Get mean of yearly spatial confidences
df_sum$spatial_yr <- rowMeans(df_sum[, c("spatial_sal_model", "un_stations")], na.rm = TRUE)
df_sum$spatial_yr[is.na(df_sum$spatial_sal_model)] <- NA


# Average by basin and add classes

conf_by_basin <- df_sum %>%
  group_by(Basin) %>%
  summarise(
    spatial = mean(spatial_yr, na.rm = TRUE),
    temporal   = mean(temporal_months, na.rm = TRUE),
    methodical = mean(methodical, na.rm = TRUE),
    accuracy   = mean(p_correct_pct, na.rm = TRUE),
    .groups = "drop"
  )

conf_by_basin$confidence <- rowMeans(conf_by_basin[, c("spatial", "temporal", "methodical","accuracy")], na.rm = TRUE)

# Create classes per numeric confidence values
conf_by_basin$class <- ""

conf_by_basin <- conf_by_basin %>%
  mutate(
    class = case_when(
      confidence >= 50 ~ "MODERATE",
      confidence >= 75 ~ "HIGH",
      confidence < 50 ~ "LOW"
    )
  )

# Save results
# Yearly:
write.csv(
  df_sum,
  file.path(outputPath, "confidence_yearly.csv"),
  row.names = FALSE
)


write.csv(
  conf_by_basin,
  file.path(outputPath, "confidence_per_period.csv"),
  row.names = FALSE
)



