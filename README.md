# ODEBT 2026
# A code set of a modified version of the HELCOM "Oxygen debt" core indicator
# Modifications by Stella-Theresa Luik (stella.luik@taltech.ee)

# This code is built using a dataset downloaded from ICES in January 2026.
# All necessary input files to run the code are in: https://drive.google.com/file/d/1_tPKg-WIw0G0dAUtJnKvosoavBz4G4_B/view?usp=sharing

# To run the code set:
#   Open the .Rproj file

#   Or run one-by-one 
#   Run order:
#     01_header.R
#       02_setup.R
#     03_define_assessment.R
#          Here define the assessment period.

#     04_data_download.R
#           Here define the names of CTD and bottle files downloaded from ICES. 
#           Reference to the auxilliary file an to the HELCOM assessment area file
#           and download Baltsem_utm34.zip and BALTIC_BATHY_BALTSEM.zip

#     05_make_assessment_area.R
#           Here only necessary assessment areas are selected.

#     06_make_bathymetry_layer.R
#           Here bathymetry data is tied to assessment areas.

#     07_data_preparation.R
#           Here good quality data is selected, duplicates removed, ctd and bottle data joined (ctd kept over bottle). 
#           Oxygen saturation concentration and then oxygen deficiency (debt) calculated.
#           Negative oxygen debt below 30 m is set to NA, since supersaturation in deeper water is unrealistic.
#           CTD oxygen values below 1 mg/l have a censor mark = 1, meaning they are unreliable (because such small values create complications in the profile estimations).
#           If H2S is NA and oxygen is 0, then censor = 1.

#     08_model_profiles.R
#           Here individual salinity profiles are modeled to find change points.
#           Using the modeled salinity profiles, halocline change points are found.
#           Oxygen deficit is found be subtracting oxygen from oxygen saturation concentrations.
#           Oxygen deficit is also found by adding negative DO found based on H2S and NH4 (Rolff et al., 2022).
#           Oxygen deficit values are found for change points.
#           Oxygen deficit slope below the halocline layer is found.

#     09_interpolate_spatially.R
#           Here parameters are interpolated spatially, using IDW interpolator (Inverse Distance Weighting): nearby observations influence a grid cell more than far ones.
#           The IDW power parameter (in gstat it’s idp) controls how quickly a station’s influence drops off with distance.Now, idp = 1, which is low power, resulting in smoother maps.
#           Limiting neighbors (nmax) - each grid cell uses only the nearest X number of stations. Now, nmax = 30.

#     10_visualize_spatially_interpolated_data_get_means.R
#           Here the lower depth of the halocline is found by adding the halocline and depth gradient variables.
#           The volume of water below the halocline layer is found. Grid is 1km x 1km.
#           The average DO deficit concentration below the halocline layer is found by first calculating the DO deficit at bottom (DO deficit below halocline layer + (slope * distance to bottom)). Then taking the average of the DO deficit below halocline layer and at bottom.
#           This average DO is then used to calculate the grid cell specific DO deficit (mean Do deficit * volume below halocline layer)
#           Results table includes year-basin means of variables and the mean DO deficit values found based on summed volume and summed grid cell specific DO deficit.
#           For visualization, figures of depth at halocline bottom and tiled layout of DO deficit values are plotted. 

#     11_assessment_confidence.R
#           Here the confidence of the assessment is calculated using a proposed methods which takes into account, temporal, spatial, methodological, and accuracy aspects of the assessments.
#           Confidence is assessed first yearly, for the temporal and spatial aspects. And then averaged per assessment period.
#           Spatial aspect consists of the success % of modelled profiles and the amount of unique stations(based on coordinates, lon and lat rounded to 5 decimals).
#           The amount of unique stations get confidence values as follows:
#             less than 50 unique stations per year and basin = 0%
#             >= 50 & < 100 unique stations per year and basin = 50%
#             >= 100 & < 100 unique stations per year and basin = 100%
#           Temporal confidence takes into account the amounts of months covered per year.
#             If all months are covered, then the corresponding year gets 100%
#             If up to 2 months are missing, then 50%.
#             If 2+ months missing, then 0%.
#             There is a condition set, that if for half of the years in the assessment period, all months are covered, then the temporal confidence for said assessment period is 100%.
#           Methodological confidence is set to 100% because regional monitoring guidelines are followed when collecting and handling samples and data.
#           Accuracy aspect considers the found assessment period mean, standard deviation and GES value to find the probability that the found mean value is classified correctly (that it belong the GES/nonGES).
#           Assessment period confidence relies on the mean of the 2 spatial confidences, which are then averaged with the temporal, methodological, and accuracy aspects.
#           The resulting assessment condifence is classified as follows: HIGH if confidence >= 75%; MODERATE if >= 50% & < 75%; and LOW if < 50%.


