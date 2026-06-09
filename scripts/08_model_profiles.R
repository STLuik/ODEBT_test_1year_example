# scripts/08_model_profiles.R
# This script models profiles based on the cleaned data.

options(project_clean_workspace = FALSE)
# This prevents accidental workspace wiping when you run this script by itself during testing.

source("scripts/01_header.R")
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
outputPath <- output_subset_dir

# Remove unnecessary data/values/functions
keep <- c("assessment", "end_year", "start_year", "outputPath", "inputPath", "proj", "repo_url", "O2satFun","auxilliaryFile")
# List the object names you want to keep (write yours here).

rm(list = setdiff(ls(envir = .GlobalEnv), keep), envir = .GlobalEnv)
# Removes everything in the global environment EXCEPT the objects in keep.

# Read in data
oxy <- read.csv(file.path(outputPath, "oxy_clean.csv"))

# Add auxilliary information to data
aux <- read.csv(file.path(inputPath, "Auxilliary.csv"))
# Makes sure both are data.tables:
setDT(oxy); setDT(aux)
# In aux, change the value "Bornholm Basin" to "Bornholm" in the Basin column.
aux[Basin == "Bornholm Basin", Basin := "Bornholm"]
# In aux, change the value "Baltic Proper" to "Central BS" in the Basin column.
aux[Basin == "Baltic Proper", Basin := "Central BS"]
# Join aux onto oxy by matching aux$Basin to oxy$F2_Name.
# This keeps all rows from oxy and adds columns from aux.
oxy2 <- aux[oxy, on = .(Basin = F2_Name)]

# Make sure oxy2 is a data frame:
setDF(oxy2)


########## DEFINE THE FUNCTION THAT WILL ESTIMATE/MODEL THE PROFILES ONE-BY-ONE

MODEL_PROFILE <- function(data, debug = FALSE, ID) {
  if (debug) {
    cat(rep(" ", 30), "\rID:", data$ID[1])
  }
  
  ##############################################################
  #ID = 4095
  #data <- oxy2[oxy2$ID == ID,]
  #data$censor <- 0
  
  
  
  
  
  
  ##############################################################
  
  
  # SORT DATA BASED ON DEPTH:
  data <- data[order(data$Depth_m), ]
  
  # CREATE AN OUTPUT CONTAINER:
  fit <- data.frame(ID = ID,
                    # Surface salinity:
                    sali_surf = NA,
                    
                    # Salinity difference, halocline = halocline layer midpoint depth, depth_gradient *2 = halocline layer width:
                    sali_dif = NA, halocline = NA, depth_gradient = NA,
                    sali_dif.se = NA, halocline.se = NA, depth_gradient.se = NA,
                    
                    # Change points of the salinity profiles - halocline layer bounds:
                    depth_change_point1 = NA,
                    depth_change_point2 = NA, 
                    depth_change_point2.se = NA,
                    
                    # O2def below halocline layer, O2def change rate below halocline layer:
                    O2def_below_halocline = NA, 
                    O2def_slope_below_halocline = NA,
                    O2def_slope_below_halocline.se = NA,
                    
                    # O2def, where linearly interpolated negative DO from H2S and NH4 have been added, below halocline layer, O2def change rate below halocline layer:
                    O2def_H2S_NH4_INTERPOLATED_below_halocline = NA,
                    O2def_H2S_NH4_INTERPOLATED_slope_below_halocline = NA,
                    O2def_H2S_NH4_INTERPOLATED_slope_below_halocline.se = NA,
                    
                    # O2def, where linearly interpolated negative DO from H2S has been added, below halocline layer, O2def change rate below halocline layer:
                    O2def_H2S_INTERPOLATED_below_halocline = NA,
                    O2def_H2S_INTERPOLATED_slope_below_halocline = NA,
                    O2def_H2S_INTERPOLATED_slope_below_halocline.se = NA,
                    
                    # O2def, where linearly interpolated negative DO from NH4 has been added, below halocline layer, O2def change rate below halocline layer:
                    O2def_NH4_INTERPOLATED_below_halocline = NA,
                    O2def_NH4_INTERPOLATED_slope_below_halocline = NA,
                    O2def_NH4_INTERPOLATED_slope_below_halocline.se = NA,
                    
                    # O2def, where negative DO from H2S and NH4 have been added, below halocline layer, O2def change rate below halocline layer:
                    O2def_H2S_NH4_below_halocline = NA,
                    O2def_H2S_NH4_slope_below_halocline = NA,
                    O2def_H2S_NH4_slope_below_halocline.se = NA,
                    
                    # O2def, where negative DO from H2S has been added, below halocline layer, O2def change rate below halocline layer:
                    O2def_H2S_below_halocline = NA,
                    O2def_H2S_slope_below_halocline = NA,
                    O2def_H2S_slope_below_halocline.se = NA,
                    
                    # O2def, where negative DO from NH4 has been added, below halocline layer, O2def change rate below halocline layer:
                    O2def_NH4_below_halocline = NA,
                    O2def_NH4_slope_below_halocline = NA,
                    O2def_NH4_slope_below_halocline.se = NA,
                    
                    notes = "")
  
  #-------------------------------------------------------------------------------
  # CHECK NUMBER OF SURFACE SALINITY OBSERVATIONS:
  # This block ensures that there are enough salinity measurements in the surface layer 
  # to proceed with estimating surface salinity. If not, it exits early and logs a note 
  # explaining why.(data$Depth >= data$surfacedepth1 & data$Depth <= data$surfacedepth2)
  wk <- data$Salinity_psu[data$Depth_m >= data$surfacedepth1[1] &
                            data$Depth_m <= data$surfacedepth2[1]]
  if (sum(!is.na(wk)) < 2) {
    fit$notes <- "CHECK 1: Too few salinity observations in surface layer (Depth_m >= surfacedepth1[1] & Depth_m <= surfacedepth2[1])."
    return(fit)
  }
  
  #-------------------------------------------------------------------------------
  # COMPUTE SURFACE SALINITY:
  # (sali_surf) (this should change by basin: data$Depth >= data$surfacedepth1 & data$Depth <= data$surfacedepth2)
  data$sali_surf <-
    fit$sali_surf <-
    mean(data$Salinity_psu[data$Depth_m >= data$surfacedepth1[1] &
                             data$Depth_m <= data$surfacedepth2[1]], na.rm = TRUE)
  
  #-------------------------------------------------------------------------------
  # CHECK NUMBER OF SALINITY OBSERVATIONS BELOW SURFACE
  # This block ensures that there are enough valid salinity observations below 20 meters and in sufficiently deep locations (data$max_depth >= data$salmod_max_depth) to proceed with modeling. If not, it logs a note and exits early.
  sal <- data[!is.na(data$Salinity_psu) & data$Depth_m >= 20 & data$max_depth_m >= data$salmod_max_depth[1],]
  
  if (nrow(sal) < 4) {
    fit$notes <- "CHECK 2: Too few salinity observations below surface (Depth_m >= 20) & in sufficiently deep depths (max_depth_m >= salmod_max_depth[1])."
    return(fit)
  }
  
  #-------------------------------------------------------------------------------
  # Define lower bound for halocline (hlower <- sal$salmod_halocline_lower[1])
  hlower <- sal$salmod_halocline_lower[1]
  
  #-------------------------------------------------------------------------------
  # SET UP WEIGHTS FOR OBSERVATIONS
  # This code is assigning custom weights to salinity data based on depth, for use in a weighted model or analysis. The idea is to give more or less importance to certain depth ranges when fitting a model or calculating statistics. (sal$weights[sal$Depth >= sal$salmod_wt_depth1 & sal$Depth <= sal$salmod_wt_depth2] <- sal$salmod_wt[1])
  sal$weights <- 1
  sal$weights[sal$Depth >= sal$salmod_wt_depth1 & sal$Depth <= sal$salmod_wt_depth2] <- sal$salmod_wt[1]
  #-------------------------------------------------------------------------------
  # FIT SALINITY PROFILE
  # It models salinity as a function of depth, assuming a smooth transition in salinity around the halocline. 
  # The transition is modeled using the cumulative distribution function of the normal distribution (pnorm), which creates a sigmoid-like curve.
  
  # Salinity ~ sali_surf + sali_dif * pnorm(Depth, halocline, depth_gradient): This models the salinity transition with depth, where:
  #   Depth is in meters
  #   halocline is the mean of the distribution (also in meters)
  #   depth_gradient is the standard deviation of the distribution
  #   In a normal distribution, the standard deviation has the same unit as the variable being modeled. Since Depth and halocline are in meters, depth_gradient must also be in meters.
  model <- try(
    nls(Salinity_psu ~ sali_surf + sali_dif * pnorm(Depth_m, halocline, depth_gradient), # This models salinity as a function of depth using a normal cumulative distribution function (pnorm), which is often used to model smooth transitions like a halocline (a layer of rapid salinity change).
        data = sal, # Uses the sal data frame.
        lower = list(sali_dif = -0.5, halocline =  hlower, depth_gradient =   1), # Bounds for the parameters, used with the "port" algorithm which supports constrained optimization.
        start = list(sali_dif =  4  , halocline =      50, depth_gradient =  10), # Initial guesses for the parameters.
        upper = list(sali_dif =  Inf, halocline =     120, depth_gradient = 100), # Bounds for the parameters, used with the "port" algorithm which supports constrained optimization.
        weights = sal$weights, #  Applies weights to the observations to give more importance to certain depth measurements.
        algorithm = "port"), silent = TRUE) # Uses the PORT algorithm, which allows for parameter bounds.
  
  if (inherits(model, "try-error")) {
    fit$notes <- "CHECK 3: Salinity model failure to converge."
    return(fit)
  }
  
  #-------------------------------------------------------------------------------
  # ADD MODEL COEFFICIENTS TO THE OUTPUT CONTAINER 'fit'
  # coef(model): Extracts the named coefficients from the fitted model (e.g., sali_dif, halocline, depth_gradient).
  # names(coef(model)): Gets the names of those coefficients.
  # fit[...] <- ...: Assigns the values of the coefficients to the corresponding named elements in the fit object.
  fit[names(coef(model))] <- coef(model)
  
  #-------------------------------------------------------------------------------
  # CALCULATE CHANGE POINTS
  # with(fit, ...): Allows you to refer to elements inside fit without repeatedly writing fit$...
  # halocline -/+ 1.0 * depth_gradient: Computes a point one standard deviation shallower/deeper than the halocline depth.
  # fit$depth_change_point1 <- ...: Stores the result in a new variable called depth_change_point1/depth_change_point2 inside the fit object.
  fit$depth_change_point1 <- with(fit, halocline - 1.0 * depth_gradient)
  fit$depth_change_point2 <- with(fit, halocline + 1.0 * depth_gradient)
  
  #-------------------------------------------------------------------------------
  # DESCRIBE ABOVE AS A MATRIX OPERATION
  # c(0, 1, 1) = a vector of values to fill the matrix.
  # 1 = number of rows.
  # 3 = number of columns.
  # matrix(...) = turns the vector into a matrix, filling it column by column by default.
  # So this matrix is often used in modeling or linear algebra, maybe as a row of predictors or coefficients.
  X <- matrix(c(0,1,1), 1, 3) # i.e. depth_change_point2 = X %*% coef(model)
  
  #-------------------------------------------------------------------------------
  # CALCULATE STANDARD ERROR OF 'depth_change_point2' by assuming normal errors for parameter estimates
  # Check if the model fit is reliable. If it's not, it sets some error values to 1000 (a big number to say “don’t trust this”). If it is reliable, it calculates standard errors (SEs) for the model parameters and a derived value.
  if (any(diag(model$m$Rmat()) == 0)) { # This checks if the model is singular (i.e., something went wrong with the math). diag(model$m$Rmat()) gets diagonal values from an internal matrix. If any of those are 0, the model is not trustworthy.
    fit$depth_change_point2.se <- 1000 # singular fit - unreliable - set SE to a very large value
    fit$sali_dif.se <- 1000 # singular fit - unreliable - set SE to a very large value
  } else {
    fit$depth_change_point2.se <- sqrt(drop(X %*% vcov(model) %*% t(X))) # This calculates the standard error for a derived value (depth_change_point2) using matrix math. vcov(model) = variance-covariance matrix of the model (used to estimate uncertainty). X is a matrix defined earlier: matrix(c(0, 1, 1), 1, 3).
    fit$sali_dif.se <- sqrt(diag(vcov(model)))["sali_dif"]
    fit$halocline.se <- sqrt(diag(vcov(model)))["halocline"]
    fit$depth_gradient.se <- sqrt(diag(vcov(model)))["depth_gradient"]
  }
  
  #-------------------------------------------------------------------------------
  # COMPUTE OXYGEN DEFICIT
  # Get rows, where 'Oxygen_deficit' values are not missing.
  O2 <- data[!is.na(data$Oxygen_debt_mgl),] 
  
  # If there are no DO deficit values, oxygen saturation was not calculated due to missing salinity and/or temperature values, then exit function.
  if (nrow(O2) == 0) {
    fit$notes <- "CHECK 4: No DO deficit values (salinity and/or temperature missing, so no DO saturation was caluclated)."
    return(fit)
  }
  
  #-------------------------------------------------------------------------------
  # ADD NEGATIVE OXYGEN BASED ON MEASURED H2S
  # 1 H2S = 2 O2 or 1 μmol L-1 H2S = - 0.04478 mL L-1 O2 (Fonselius, 1981). H2S is in umol l-1 (mg/L = µmol/L × 0.03408)
  O2$negat_DO_H2S_mll[!is.na(O2$Hydrogen_Sulphide_umoll)] <- O2$Hydrogen_Sulphide_umoll[!is.na(O2$Hydrogen_Sulphide_umoll)] * - 0.04478
  
  # Oxygen observations are in mg/l - convert ml/l to mg/l
  O2$negat_DO_H2S_mgl[!is.na(O2$negat_DO_H2S_mll)] <- O2$negat_DO_H2S_mll[!is.na(O2$negat_DO_H2S_mll)] * 1.428 # or / 0.700
  
  # Remove  O2$negat_DO_H2S_mgl values, where O2$Hydrogen_Sulphide_umoll <= 4 (According to Rolff et al., 2022: Negative oxygen was calculated only if H2S > 4 μmol L-1, since measurements below this level were considered uncertain.)
  O2$negat_DO_H2S_mgl[O2$Hydrogen_Sulphide_umoll <= 4] <- NA
  
  # If O2$Oxygen_mgl != NA & O2$Hydrogen_Sulphide_umoll <= 4, then oxygen values are used, but they are assumed to be zero.
  z <- which(!is.na(O2$Oxygen_mgl) & O2$Hydrogen_Sulphide_umoll <= 4)
  
  if (length(z) > 0) {
    O2$Hydrogen_Sulphide_umoll[z] <- NA
    O2$Oxygen_mgl[z] <- 0
  }
  #-------------------------------------------------------------------------------
  # CALCULATE DO DEFICIT AGAIN
  # For correction, oxygen debt (deficit) is calculated here again, according to the previous if function the oxygen values could change (If O2$Oxygen_mgl != NA & O2$Hydrogen_Sulphide_umoll <= 4, then oxygen values are used, but they are assumed to be zero.)
  # Compute oxygen deficit
  O2$Oxygen_debt_mgl <- O2satFun(O2$Temperature_degreesC) - O2$Oxygen_mgl
  
  #------------------------------------------------------------------------------
  # ADD NEGATIVE OXYGEN BASED ON MEASURED NH4
  # Convert NH4 µmol/L to mg/l
  O2$Ammonium_Nitrogen_mgl[!is.na(O2$Ammonium_Nitrogen_umoll)] <- O2$Ammonium_Nitrogen_umoll[!is.na(O2$Ammonium_Nitrogen_umoll)] * (14.0067 / 1000)
  
  # Calculate negative oxygen according to Rolff et al., 2022
  O2$negat_DO_NH4_mgl[!is.na(O2$Ammonium_Nitrogen_mgl)] <- O2$Ammonium_Nitrogen_mgl[!is.na(O2$Ammonium_Nitrogen_mgl)] * -4.57
  
  # O2$negat_DO_NH4 values that are above halocline layer (depths smaller than depth_change_point1) are set to NA. In Rolff et al., 2022, NH4 is considered for negative DO at depths > 65m
  O2$negat_DO_NH4_mgl[O2$Depth_m < fit$depth_change_point1] <- NA
  
  #------------------------------------------------------------------------------
  # POPULATE NEGATIVE DO PROFILES WITH DATA FOR DEPTHS BETWEEN THE SHALLOWEST AND DEEPEST MEASURED POINTS AND ADD THE LAST MEASURED VALUE AS A CONSTANT FOR DEPTHS TOWARDS BOTTOM
  # Interpolate using R's built-in approx() function
  # Define function
  interpolate_profile_approx <- function(depth, parameter, rule = 2) {
    valid <- !is.na(depth) & !is.na(parameter) # Remove paired NAs, i.e., identify valid data points
    
    if (sum(valid) < 2) { # If not enough valid points, return original
      return(parameter)
    }
    
    min_measured_depth <- min(depth[valid]) # Find the shallowest depth that was actually measured
    
    result <- approx( # Perform interpolation. rule=2 means extrapolation is used for points outside the range (extrapolated upwards and downwards - the shallow values interpolated here are set to NA later). rule=1 would return NA for points outside the range
      x = depth[valid], 
      y = parameter[valid], 
      xout = depth, 
      method = "linear", 
      rule = rule
    )
    
    #result[depth < min_measured_depth] <- NA #"Clean up" the surface: If the depth is shallower than our first measurement, force it back to NA
    
    return(result$y)
  }
  
  # Interpolate missing H2S values
  valid <- !is.na(O2$Depth_m) & !is.na(O2$negat_DO_NH4_mgl)
  if (sum(valid) >= 2) {
    O2$negat_DO_NH4_mgl_INTERPOLATED <- interpolate_profile_approx(O2$Depth_m,O2$negat_DO_NH4_mgl)
    # Set shallow values (above the first measured observation) to NA
    min_measured_depth <- min(O2$Depth_m[valid])
    O2$negat_DO_NH4_mgl_INTERPOLATED[O2$Depth_m < min_measured_depth]  <- NA
  } else {
    O2$negat_DO_NH4_mgl_INTERPOLATED <- NA
  }
  
  # Interpolate missing NH4 values
  valid <- !is.na(O2$Depth_m) & !is.na(O2$negat_DO_H2S_mgl)
  if (sum(valid) >= 2) {
    O2$negat_DO_H2S_mgl_INTERPOLATED <- interpolate_profile_approx(O2$Depth_m,O2$negat_DO_H2S_mgl)
    # Set shallow values (above the first measured observation) to NA
    min_measured_depth <- min(O2$Depth_m[valid])
    O2$negat_DO_H2S_mgl_INTERPOLATED[O2$Depth_m < min_measured_depth]  <- NA
  } else {
    O2$negat_DO_H2S_mgl_INTERPOLATED <- NA
  }
  
  #------------------------------------------------------------------------------
  # ADD NEGATIVE DO TO DO DEFICIT VARIABLE
  # CREATE VARS WITH JUST OBSERVATIONAL DATA
  # Populate DO deficit H2S var with DO deficit values
  O2$Oxygen_debt_mgl_H2S <- O2$Oxygen_debt_mgl
  # Add negative DO (multiplied with -1 to get positive values, since DO decifit is positive) to created var
  O2$Oxygen_debt_mgl_H2S[!is.na(O2$negat_DO_H2S_mgl)] <- O2$Oxygen_debt_mgl[!is.na(O2$negat_DO_H2S_mgl)] + O2$negat_DO_H2S_mgl[!is.na(O2$negat_DO_H2S_mgl)] * -1
  
  
  # Populate DO deficit NH4 var with DO deficit values
  O2$Oxygen_debt_mgl_NH4 <- O2$Oxygen_debt_mgl
  # Add negative DO (multiplied with -1 to get positive values, since DO decifit is positive) to created var
  O2$Oxygen_debt_mgl_NH4[!is.na(O2$negat_DO_NH4_mgl)] <- O2$Oxygen_debt_mgl[!is.na(O2$negat_DO_NH4_mgl)] + O2$negat_DO_NH4_mgl[!is.na(O2$negat_DO_NH4_mgl)] * -1
  
  
  # Populate DO deficit H2S+NH4 var with DO deficit values
  O2$Oxygen_debt_mgl_H2S_NH4 <- O2$Oxygen_debt_mgl_H2S
  # Add negative DO (multiplied with -1 to get positive values, since DO decifit is positive) to created var
  O2$Oxygen_debt_mgl_H2S_NH4[!is.na(O2$negat_DO_NH4_mgl)] <- O2$Oxygen_debt_mgl_H2S_NH4[!is.na(O2$negat_DO_NH4_mgl)] + O2$negat_DO_NH4_mgl[!is.na(O2$negat_DO_NH4_mgl)] * -1
  
  
  # CREATE VARS WITH INTERPOLATED DATA
  # Populate interpolated DO deficit H2S var with DO deficit values
  O2$Oxygen_debt_mgl_H2S_INTERPOLATED <- O2$Oxygen_debt_mgl
  # Add negative DO (multiplied with -1 to get positive values, since DO decifit is positive) to created var
  O2$Oxygen_debt_mgl_H2S_INTERPOLATED[!is.na(O2$negat_DO_H2S_mgl_INTERPOLATED)] <- O2$Oxygen_debt_mgl[!is.na(O2$negat_DO_H2S_mgl_INTERPOLATED)] + O2$negat_DO_H2S_mgl_INTERPOLATED[!is.na(O2$negat_DO_H2S_mgl_INTERPOLATED)] * -1
  
  
  # Populate interpolated DO deficit NH4 var with DO deficit values
  O2$Oxygen_debt_mgl_NH4_INTERPOLATED <- O2$Oxygen_debt_mgl
  # Add negative DO (multiplied with -1 to get positive values, since DO decifit is positive) to created var
  O2$Oxygen_debt_mgl_NH4_INTERPOLATED[!is.na(O2$negat_DO_NH4_mgl_INTERPOLATED)] <- O2$Oxygen_debt_mgl[!is.na(O2$negat_DO_NH4_mgl_INTERPOLATED)] + O2$negat_DO_NH4_mgl_INTERPOLATED[!is.na(O2$negat_DO_NH4_mgl_INTERPOLATED)] * -1
  
  
  # Populate interpolated DO deficit H2s+NH4 var with DO deficit values
  O2$Oxygen_debt_mgl_H2S_NH4_INTERPOLATED <- O2$Oxygen_debt_mgl_H2S_INTERPOLATED
  # Add negative DO (multiplied with -1 to get positive values, since DO decifit is positive) to created var
  O2$Oxygen_debt_mgl_H2S_NH4_INTERPOLATED[!is.na(O2$negat_DO_NH4_mgl_INTERPOLATED)] <- O2$Oxygen_debt_mgl_H2S_NH4_INTERPOLATED[!is.na(O2$negat_DO_NH4_mgl_INTERPOLATED)] + O2$negat_DO_NH4_mgl_INTERPOLATED[!is.na(O2$negat_DO_NH4_mgl_INTERPOLATED)] * -1
  
  
  ##############################################################################
  # For checking individual profiles
  #fwrite(O2, file.path(outputPath, "example_ID_INTERPOLATE_negative_DO.csv"))
  ##############################################################################
  
  #------------------------------------------------------------------------------
  # CALCULATE DO DEFICIT FOR SECOND CHANGE POINT (at depth = halocline + depth_gradient)
  # Checking whether it's safe and meaningful to calculate oxygen values below a certain depth (called the second change point).
  id <- which(O2$Depth_m > fit$depth_change_point2)[1] # This finds the first row in your cleaned data (O2) where the depth is deeper than the second change point. It saves the row number as id.
  if (nrow(O2) < 2 || is.na(id) || id == 1) { # Checks whether there are no data above change point or no data below change point.
    fit$notes <- "CHECK 5: No O2 data below change point 2nd change point, i.e., no O2 data below the halocline layer." # Adds a note to the fit object explaining the issue.
    return(fit) # Stops the function and returns the fit object early.
  }
  
  # Get indices
  id <- id  + -1:0 # Grab two rows from data: one just above and one just below the depth where the salinity changes.
  
  
  # Define function for the salinity model (it was used being in \Utils, but doesn't work like this at the moment, so added it here)
  sali_profile <- function(depth, pars) {
    with(pars, sali_surf + sali_dif * pnorm(depth, halocline, depth_gradient)) # requires a vector 'pars' with names: sali_surf, sali_dif, halocline, depth_gradient
  }
  
  # Use linear interpolation based on depth data or use the salinity curve.
  if (diff(O2$Depth_m[id]) > 30) { # Check if the two depth points around the halocline lower border (depth_change_point2 and the depth below it) are more than 30 meters apart. If yes, the gap is too big to trust the salinity model, so it uses depth-based interpolation.
    # Interpolate the oxygen deficit at the halocline depth using the two nearby depth values. approx() = linear interpolation.
    fit$O2def_below_halocline <- approx(O2$Depth_m[id], O2$Oxygen_debt_mgl[id], xout = fit$depth_change_point2)$y
    
    fit$O2def_H2S_NH4_INTERPOLATED_below_halocline <- approx(O2$Depth_m[id], O2$Oxygen_debt_mgl_H2S_NH4_INTERPOLATED[id], xout = fit$depth_change_point2)$y
    fit$O2def_H2S_INTERPOLATED_below_halocline <- approx(O2$Depth_m[id], O2$Oxygen_debt_mgl_H2S_INTERPOLATED[id], xout = fit$depth_change_point2)$y
    fit$O2def_NH4_INTERPOLATED_below_halocline <- approx(O2$Depth_m[id], O2$Oxygen_debt_mgl_NH4_INTERPOLATED[id], xout = fit$depth_change_point2)$y
    
    fit$O2def_H2S_NH4_below_halocline <- approx(O2$Depth_m[id], O2$Oxygen_debt_mgl_H2S_NH4[id], xout = fit$depth_change_point2)$y
    fit$O2def_H2S_below_halocline <- approx(O2$Depth_m[id], O2$Oxygen_debt_mgl_H2S[id], xout = fit$depth_change_point2)$y
    fit$O2def_NH4_below_halocline <- approx(O2$Depth_m[id], O2$Oxygen_debt_mgl_NH4[id], xout = fit$depth_change_point2)$y
    
  } else {
    # Use the salinity model (sali_profile) to estimate salinity at those depths.Interpolate the oxygen deficit based on salinity, not depth.
    sali_preds <- sali_profile(O2$Depth_m[id], fit)
    sali_change_point2 <- sali_profile(fit$depth_change_point2, fit)
    fit$O2def_below_halocline <- approx(sali_preds, O2$Oxygen_debt_mgl[id], xout = sali_change_point2)$y
    
    fit$O2def_H2S_NH4_INTERPOLATED_below_halocline <- approx(sali_preds, O2$Oxygen_debt_mgl_H2S_NH4_INTERPOLATED[id], xout = sali_change_point2)$y 
    fit$O2def_H2S_INTERPOLATED_below_halocline <- approx(sali_preds, O2$Oxygen_debt_mgl_H2S_INTERPOLATED[id], xout = sali_change_point2)$y 
    fit$O2def_NH4_INTERPOLATED_below_halocline <- approx(sali_preds, O2$Oxygen_debt_mgl_NH4_INTERPOLATED[id], xout = sali_change_point2)$y 
    
    fit$O2def_H2S_NH4_below_halocline <- approx(sali_preds, O2$Oxygen_debt_mgl_H2S_NH4[id], xout = sali_change_point2)$y 
    fit$O2def_H2S_below_halocline <- approx(sali_preds, O2$Oxygen_debt_mgl_H2S[id], xout = sali_change_point2)$y 
    fit$O2def_NH4_below_halocline <- approx(sali_preds, O2$Oxygen_debt_mgl_NH4[id], xout = sali_change_point2)$y 
    
  }
  
  #------------------------------------------------------------------------------
  # GET OXYGEN DEFICIT DATA BELOW THE HALOCLINE - DO DEFICIT CHANGE WITH DEPTH
  # Create a subset of data, where only depths below change_point_2 (below the halocline layer) are present.
  O2_slope <- O2[O2$Depth_m > fit$depth_change_point2,]
  
  # Check if there are enough data below the halocline.
  if (nrow(O2_slope[O2_slope$censor == 0,]) < 2) { # Filter for rows where censor == 0, which means the oxygen measurements are valid (not censored or missing). If there are fewer than 2 valid measurements, it’s not enough to do a reliable analysis.
    fit$notes <- "CHECK 6: Too few (uncensored) O2 observations below halocline layer (Depth_m > depth_change_point2 has to have at least 2 values)."
    return(fit)
  }
  
  # Recenter data to easily estimate the slope - oxygen deficit change with depth
  O2_slope$Oxygen_debt_mgl <- O2_slope$Oxygen_debt_mgl - fit$O2def_below_halocline # Take all the oxygen deficit values below the halocline, and subtract the value at the halocline. This shifts the data so that the oxygen deficit at the halocline becomes zero, and everything deeper shows how much it increases from there.
  O2_slope$Depth_m <- O2_slope$Depth_m - fit$depth_change_point2 # Shift all the depth values so that the halocline depth becomes zero.
  
  O2_slope$Oxygen_debt_mgl_H2S_NH4_INTERPOLATED <- O2_slope$Oxygen_debt_mgl_H2S_NH4_INTERPOLATED - fit$O2def_H2S_NH4_INTERPOLATED_below_halocline
  O2_slope$Oxygen_debt_mgl_H2S_INTERPOLATED <- O2_slope$Oxygen_debt_mgl_H2S_INTERPOLATED - fit$O2def_H2S_INTERPOLATED_below_halocline
  O2_slope$Oxygen_debt_mgl_NH4_INTERPOLATED <- O2_slope$Oxygen_debt_mgl_NH4_INTERPOLATED - fit$O2def_NH4_INTERPOLATED_below_halocline
  
  O2_slope$Oxygen_debt_mgl_H2S_NH4 <- O2_slope$Oxygen_debt_mgl_H2S_NH4 - fit$O2def_H2S_NH4_below_halocline
  O2_slope$Oxygen_debt_mgl_H2S <- O2_slope$Oxygen_debt_mgl_H2S - fit$O2def_H2S_below_halocline
  O2_slope$Oxygen_debt_mgl_NH4 <- O2_slope$Oxygen_debt_mgl_NH4 - fit$O2def_NH4_below_halocline
  
  
  ##############################################################################
  # For testing:
  # datas <- O2_slope
  # parameter <- datas$Oxygen_debt_mgl
  # depth_var <- datas$Depth_m
  # censor_var <- datas$censor
  ##############################################################################
  
  
  # Define function to get the slope
  calculate_slope <- function(parameter,depth_var,censor_var,censor_type = "right") {
    keep <- is.finite(parameter) & is.finite(depth_var) & !is.na(censor_var) 
    parameter <- parameter[keep] 
    depth_var <- depth_var[keep] 
    censor_var <- censor_var[keep]
    
    has_censored <- any(censor_var == 1, na.rm = TRUE) 
    
    event <- !(censor_var == 1) # TRUE = observed, FALSE = censored
    # 1. Check if any data is censored (1 = censored, 0 = observed)
    # We use !data[[censor_var]] because Surv() expects TRUE for 'observed' and FALSE for 'censored' when using type='right'
    if  (has_censored) {
      
      seg3_model <- suppressWarnings( survival::survreg( survival::Surv(parameter, event, type = censor_type) ~ depth_var - 1, 
                                                         dist = "gaussian", 
                                                         control = survival::survreg.control(maxiter = 1000) ) ) 
      slope_raw <- unname(coef(seg3_model)[[1]])
      slope_se <- unname(summary(seg3_model)$table[1, "Std. Error"])
      
    } else {
      
      seg3_model <- stats::lm(parameter ~ depth_var - 1)
      
      slope_raw <- unname(coef(seg3_model)[[1]])
      slope_se <- unname(coef(summary(seg3_model))[1, "Std. Error"])
    }
    
    slope <- pmax(0, slope_raw) # keep non-negative if that’s your physical constraint slope_se <- unname(summary(seg3_model)$coefficients[1, 2])
    
    list(slope = slope, # clamped at 0 
         slope_raw = slope_raw, # unclamped slope (what the model estimated) 
         slope_se = slope_se, 
         has_censored = has_censored, 
         censor_type = censor_type, 
         n = length(parameter), 
         model = seg3_model ) 
  }
  
  # Run the function for DO deficit
  results <- calculate_slope(
    parameter = O2_slope$Oxygen_debt_mgl,
    depth_var = O2_slope$Depth_m,
    censor_var = O2_slope$censor,
    censor_type = "right")
  # Assign results back to your 'fit' object
  fit$O2def_slope_below_halocline <- results$slope
  fit$O2def_slope_below_halocline.se <- results$slope_se
  
  # Run the function for DO deficit (interpolated H2S+NH4)
  results <- calculate_slope(
    parameter = O2_slope$Oxygen_debt_mgl_H2S_NH4_INTERPOLATED,
    depth_var = O2_slope$Depth_m,
    censor_var = O2_slope$censor,
    censor_type = "right")
  # Assign results back to your 'fit' object
  fit$O2def_H2S_NH4_INTERPOLATED_slope_below_halocline <- results$slope
  fit$O2def_H2S_NH4_INTERPOLATED_slope_below_halocline.se <- results$slope_se 
  
  # Run the function for DO deficit (interpolated H2S)
  results <- calculate_slope(
    parameter = O2_slope$Oxygen_debt_mgl_H2S_INTERPOLATED,
    depth_var = O2_slope$Depth_m,
    censor_var = O2_slope$censor,
    censor_type = "right")
  # Assign results back to your 'fit' object
  fit$O2def_H2S_INTERPOLATED_slope_below_halocline <- results$slope
  fit$O2def_H2S_INTERPOLATED_slope_below_halocline.se <- results$slope_se 
  
  # Run the function for DO deficit (interpolated NH4)
  results <- calculate_slope(
    parameter = O2_slope$Oxygen_debt_mgl_NH4_INTERPOLATED,
    depth_var = O2_slope$Depth_m,
    censor_var = O2_slope$censor,
    censor_type = "right")
  # Assign results back to your 'fit' object
  fit$O2def_NH4_INTERPOLATED_slope_below_halocline <- results$slope
  fit$O2def_NH4_INTERPOLATED_slope_below_halocline.se <- results$slope_se 
  
  # Run the function for DO deficit (H2S+NH4)
  results <- calculate_slope(
    parameter = O2_slope$Oxygen_debt_mgl_H2S_NH4,
    depth_var = O2_slope$Depth_m,
    censor_var = O2_slope$censor,
    censor_type = "right")
  # Assign results back to your 'fit' object
  fit$O2def_H2S_NH4_slope_below_halocline <- results$slope
  fit$O2def_H2S_NH4_slope_below_halocline.se <- results$slope_se 
  
  # Run the function for DO deficit (interpolated H2S)
  results <- calculate_slope(
    parameter = O2_slope$Oxygen_debt_mgl_H2S,
    depth_var = O2_slope$Depth_m,
    censor_var = O2_slope$censor,
    censor_type = "right")
  # Assign results back to your 'fit' object
  fit$O2def_H2S_slope_below_halocline <- results$slope
  fit$O2def_H2S_slope_below_halocline.se <- results$slope_se 
  
  # Run the function for DO deficit (interpolated NH4)
  results <- calculate_slope(
    parameter = O2_slope$Oxygen_debt_mgl_NH4,
    depth_var = O2_slope$Depth_m,
    censor_var = O2_slope$censor,
    censor_type = "right")
  # Assign results back to your 'fit' object
  fit$O2def_NH4_slope_below_halocline <- results$slope
  fit$O2def_NH4_slope_below_halocline.se <- results$slope_se 
  
  
  fit$notes <- "Check 7: Success."
  
  fit
}

# Run all fits:
profiles <-
  do.call(rbind,
          lapply(unique(oxy2$ID),
                 function(id) MODEL_PROFILE(subset(oxy2, ID == id), ID = id, debug = TRUE)))

# Join all profile level variables onto profiles. Get unique rows of oxy based on columns NOT shown here.
out <- unique(dplyr::select(oxy2, -Depth_m, -source, -Oxygen_mll,
                            -Temperature_degreesC, -Salinity_psu, -Oxygen_mgl, -Hydrogen_Sulphide_umoll, 
                            -Ammonium_Nitrogen_umoll, -Oxygen_debt_mgl, -censor, -n_Oxygen, -Cruise))
rownames(out) <- paste(out$ID)

# Join this onto the profiles:
profiles <- merge(profiles, out)

# Select which data are reliable:
drop_sal <- profiles$sali_dif.se > 2.5 | # Only use profiles that are based on an estimate of the salinity difference estimate with +- 5 accuracy
  profiles$depth_change_point2.se > 10 | # Only use salinity profiles that are based on an estimate of the lower halocline estimate with +- 20m accuracy
  profiles$halocline.se > 10 | # Only use salinity profiles with halocline depth estimated to +- 20m accuracy
  profiles$sali_dif < 0 | profiles$sali_dif > 17 |   # Big salinity difference estimates with reasonable precision - dubious.
  profiles$halocline == profiles$salmod_halocline_lower | profiles$halocline > 100 |
  profiles$depth_gradient > 45 |
  profiles$depth_change_point1 < profiles$surfacedepth2 | profiles$depth_change_point1 > 90

drop_sal[is.na(drop_sal)] <- FALSE
profiles$sali_dif[drop_sal] <- NA
profiles$halocline[drop_sal] <- NA
profiles$depth_gradient[drop_sal] <- NA
profiles$depth_change_point1[drop_sal] <- NA
profiles$depth_change_point2[drop_sal] <- NA

profiles$O2def_below_halocline[drop_sal] <- NA
profiles$O2def_slope_below_halocline[drop_sal] <- NA

profiles$O2def_H2S_NH4_INTERPOLATED_below_halocline[drop_sal] <- NA
profiles$O2def_H2S_NH4_INTERPOLATED_slope_below_halocline[drop_sal] <- NA

profiles$O2def_H2S_INTERPOLATED_below_halocline[drop_sal] <- NA
profiles$O2def_H2S_INTERPOLATED_slope_below_halocline[drop_sal] <- NA

profiles$O2def_NH4_INTERPOLATED_below_halocline[drop_sal] <- NA
profiles$O2def_NH4_INTERPOLATED_slope_below_halocline[drop_sal] <- NA

profiles$O2def_H2S_below_halocline[drop_sal] <- NA
profiles$O2def_H2S_slope_below_halocline[drop_sal] <- NA

profiles$O2def_NH4_below_halocline[drop_sal] <- NA
profiles$O2def_NH4_slope_below_halocline[drop_sal] <- NA

# Drop off badly estimated O2 deficit slopes:
drop_O2def <- profiles$O2def_slope_below_halocline > 1.5
drop_O2def_H2S_NH4 <- profiles$O2def_H2S_NH4_INTERPOLATED_slope_below_halocline > 1.5

profiles$O2def_slope_below_halocline[drop_O2def] <- NA
profiles$O2def_H2S_NH4_INTERPOLATED_slope_below_halocline[drop_O2def_H2S_NH4] <- NA

# Write out profile data
fwrite(profiles, file.path(outputPath, "oxy_profiles_H2S_NH4.csv"))


# GET OVEWRVIEW TABLE OF MODEL SUCCESS

# Ensures profiles is a data.table (fast grouping + reshaping).
setDT(profiles)

# Note categories present in data:
notes <- unique(profiles$notes)

# Set preferred order of notes
# Pull the number after "CHECK" from each note (e.g., 3 from "CHECK 3: ...").
check_num <- as.integer(sub("^CHECK\\s*(\\d+):.*", "\\1", notes))

# Order notes by the extracted number.
un_note <- notes[order(check_num)]

# OVERVIEW FOR 2 BASINS - BALTSEM DIVISION, ORIGINAL INDICATOR DIVISION
# Count rows for each Year + Basin + notes combination
cnt <- profiles[, .N, by = .(Year, Basin, notes)]
# .N is data.table’s “count”.

# Turn notes into columns (wide format). Now you get one row per Year×Basin, and one column per notes category.
N_Basin_Year <- dcast(
  cnt,
  Year + Basin ~ notes,
  value.var = "N",
  fill = 0
)

# Reorders the note columns to your custom order (Year and Basin stay first).
setcolorder(N_Basin_Year, c("Year", "Basin", un_note))

# Sum across note columns to get total profiles per Year×Basin.
N_Basin_Year[, number_of_profiles := rowSums(.SD), .SDcols = un_note]

success_label <- "Check 7: Success."
N_Basin_Year[, success_percent := ifelse(number_of_profiles > 0,
                                         get(success_label) * 100 / number_of_profiles,
                                         NA_real_)]
# Write the table.
fwrite(N_Basin_Year, file.path(outputPath, "summary_of_oxy_profiles_success_by_year_and_2areas.csv"))









# OVERVIEW FOR 5 BASINS - HELCOM eutro 4b division
# Count rows for each Year + Name + notes combination
cnt_name <- profiles[, .N, by = .(Year, Name, notes)]
# Counts per Year×Name×notes.

N_Name_Year <- dcast(
  cnt_name,
  Year + Name ~ notes,
  value.var = "N",
  fill = 0
)
# Wide table: one row per Year×Name.

setcolorder(N_Name_Year, c("Year", "Name", un_note))
# Put Year first, then Name, then notes in your desired order.

N_Name_Year[, number_of_profiles := rowSums(.SD), .SDcols = un_note]
# Total profiles per Year×Name.

N_Name_Year[, success_percent := ifelse(number_of_profiles > 0,
                                        get(success_label) * 100 / number_of_profiles,
                                        NA_real_)]

fwrite(N_Name_Year, file.path(outputPath, "summary_of_oxy_profiles_success_by_year_and_5basins.csv"))

