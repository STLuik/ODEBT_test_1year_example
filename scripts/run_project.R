# scripts/run_project.R
# This script runs the whole project from start to finish.

options(project_clean_workspace = TRUE)
# This tells header.R to clear the workspace for a clean run.

source("scripts/header.R")
# This loads packages (via setup.R if needed) and loads Utils functions into oxydebt_funs.

analysis_scripts <- c(
  "scripts/01_import.R",
  "scripts/02_clean.R",
  "scripts/03_analysis.R",
  "scripts/04_plots.R",
  "scripts/05_export.R"
)
# This lists your scripts in the order you want them to run.

for (s in analysis_scripts) {
  # This loop goes through each script.
  
  if (file.exists(s)) {
    # This checks the script exists.
    
    message("Running: ", s)
    # This prints which script is running.
    
    source(s)
    # This runs the script.
    
  } else {
    # This runs if the script does not exist.
    
    message("Skipping (file not found): ", s)
    # This tells you it was skipped.
  }
}

message("All done: run_project.R finished.")
# This prints a final message when everything is finished.