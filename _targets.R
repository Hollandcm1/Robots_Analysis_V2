# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  packages = c("tibble", "R.matlab", "here", "openxlsx", "dplyr", "stringr", 
               "ggplot2", "tidyr", "purrr", "flexplot") # Packages that your targets need
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source(files = "R_targets_scripts")

# Target List
list(
  # Codebook
  tar_target(
    name = codes,
    command = import_codebook()
  ),
  # Separate targets to extract each element
  tar_target(
    name = codes_conditions,
    command = codes[[1]]
  ),
  tar_target(
    name = codes_participant_conditions,
    command = codes[[2]]
  ),
  # Maps
  tar_target(
    name = maps,
    command = pull_maps(codes_participant_conditions, codes_conditions, environments)
  ),
  tar_target(
    name = maps_plotted,
    command = plot_maps(maps),
    #cue = tar_cue(mode = "always")
  ),
  # Condense Data
  tar_target(
    name = compiled_data,
    command = condense_data(codes_participant_conditions)
  ),
  # Participant Error Correction
  tar_target(
    name = corrected_data,
    command = participant_error_correction(compiled_data)
  ),
  # Plot All Trials
  tar_target(
    name = all_trials_plots,
    command = plot_all_trials(corrected_data)
  )
  
)

# tar_manifest()
# tar_visnetwork()
# tar_make()
